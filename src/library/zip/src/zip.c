
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <stdarg.h>
#include <stdio.h>

#ifdef _WIN32
#include <direct.h>		/* _mkdir */
#include <windows.h>
#endif

#include "miniz.h"
#include "zip.h"

#define ZIP_ERROR_BUFFER_SIZE 1000
static char zip_error_buffer[ZIP_ERROR_BUFFER_SIZE];

static const char *zip_error_strings[] = {
  /* 0 R_ZIP_ESUCCESS     */ "Success",
  /* 1 R_ZIP_EOPEN        */ "Cannot open zip file `%s` for reading",
  /* 2 R_ZIP_ENOMEM       */ "Cannot extract zip file `%s`, out of memory",
  /* 3 R_ZIP_ENOENTRY     */ "Cannot find file `%s` in zip archive `%s`",
  /* 4 R_ZIP_EBROKEN      */ "Cannot extract zip archive `%s`",
  /* 5 R_ZIP_EBROKENENTRY */ "Cannot extract entry `%s` from archive `%s`",
  /* 6 R_ZIP_EOVERWRITE   */ "Not overwriting `%s` when extracting `%s`",
  /* 7 R_ZIP_ECREATEDIR   */
     "Cannot create directory `%s` to extract `%s` from arghive `%s`",
  /* 8 R_ZIP_ESETPERM     */
     "Cannot set permissions for `%s` from archive `%s`",
  /* 9 R_ZIP_ESETMTIME    */
      "Failed to set mtime on `%s` while extracting `%s`",
  /*10 R_ZIP_EOPENWRITE   */ "Cannot open zip file `%s` for writing",
  /*11 R_ZIP_EOPENWRITE   */ "Cannot open zip file `%s` for appending",
  /*12 R_ZIP_EADDDIR      */ "Cannot add directory `%s` to archive `%s`",
  /*13 R_ZIP_EADDFILE     */ "Cannot add file `%s` to archive `%s`",
  /*14 R_ZIP_ESETZIPPERM  */
      "Cannot set permission on file `%s` in archive `%s`",
  /*15 R_ZIP_ECREATE      */ "Could not create zip archive `%s`",
  /*16 R_ZIP_EOPENX       */ "Cannot extract file `%s`",
  /*17 R_ZIP_FILESIZE     */ "Cannot determine size of `%s`"
};

static zip_error_handler_t *zip_error_handler = 0;

void zip_set_error_handler(zip_error_handler_t *handler) {
  zip_error_handler = handler;
}

void zip_error(int errorcode, const char *file, int line, ...) {
  va_list va;
  int err = errno;
  va_start(va, line);
  vsnprintf(zip_error_buffer, ZIP_ERROR_BUFFER_SIZE - 1,
	    zip_error_strings[errorcode], va);
  zip_error_handler(zip_error_buffer, file, line, errorcode, err);
}

#define ZIP_ERROR(c, ...) do {				\
  zip_error((c), __FILE__, __LINE__, __VA_ARGS__);	\
  return 1; }  while(0)

int zip_set_permissions(mz_zip_archive *zip_archive, mz_uint file_index,
			const char *filename) {

  /* We only do this on Unix currently*/
#ifdef _WIN32
  return 0;
#else
  mz_uint16 version_by;
  mz_uint32 external_attr;
  struct stat st;

  if (! mz_zip_get_version_made_by(zip_archive, file_index, &version_by) ||
      ! mz_zip_get_external_attr(zip_archive, file_index, &external_attr)) {
    return 1;
  }

  if (stat(filename, &st)) return 1;

  version_by &= 0x00FF;
  version_by |= 3 << 8;

  /* We need to set the created-by version here, apparently, otherwise
     miniz will not set it properly.... */
  version_by |= 23;

  external_attr &= 0x0000FFFF;
  external_attr |= (st.st_mode & 0777) << 16;

  if (! mz_zip_set_version_made_by(zip_archive, file_index, version_by) ||
      ! mz_zip_set_external_attr(zip_archive, file_index, external_attr)) {
    return 1;
  }

  return 0;
#endif
}

#ifdef _WIN32
int zip_get_permissions(mz_zip_archive_file_stat *stat, mode_t *mode) {
  *mode = stat->m_is_directory ? 0700 : 0600;
  return 0;
}
#else
int zip_get_permissions(mz_zip_archive_file_stat *stat, mode_t *mode) {
  mz_uint16 version_by = (stat->m_version_made_by >> 8) & 0xFF;
  mz_uint32 external_attr = (stat->m_external_attr >> 16) & 0xFFFF;

  /* If it is not made by Unix, or the permission field is zero,
     we ignore them. */
  if (version_by != 3 || external_attr == 0) {
    *mode = stat->m_is_directory ? 0700 : 0600;
    return 1;
  } else {
    *mode = (mode_t) external_attr & 0777;
  }

  return 0;
}
#endif

int zip_unzip(const char *czipfile, const char **cfiles, int num_files,
	      int coverwrite, int cjunkpaths, const char *cexdir) {

  int allfiles = cfiles == NULL;
  int i, n;
  mz_zip_archive zip_archive;
  memset(&zip_archive, 0, sizeof(zip_archive));

  zip_char_t *buffer = NULL;
  size_t buffer_size = 0;

  FILE *zfh = zip_open_utf8(czipfile, ZIP__READ, &buffer, &buffer_size);
  if (zfh == NULL) ZIP_ERROR(R_ZIP_EOPEN, czipfile);
  if (!mz_zip_reader_init_cfile(&zip_archive, zfh, 0, 0)) {
    if (buffer) free(buffer);
    fclose(zfh);
    ZIP_ERROR(R_ZIP_EOPEN, czipfile);
  }

  n = allfiles ? mz_zip_reader_get_num_files(&zip_archive) : num_files;

  for (i = 0; i < n; i++) {
    mz_uint32 idx = -1;
    const char *key = 0;
    mz_zip_archive_file_stat file_stat;

    if (allfiles) {
      idx = (mz_uint32) i;
    } else {
      key = cfiles[i];
      if (!mz_zip_reader_locate_file_v2(&zip_archive, key, /* pComment= */ 0,
				       /* flags= */ 0, &idx)) {
	      mz_zip_reader_end(&zip_archive);
	      if (buffer) free(buffer);
	      fclose(zfh);
	      ZIP_ERROR(R_ZIP_ENOENTRY, key, czipfile);
      }
    }

    if (! mz_zip_reader_file_stat(&zip_archive, idx, &file_stat)) {
      mz_zip_reader_end(&zip_archive);
      if (buffer) free(buffer);
      fclose(zfh);
      ZIP_ERROR(R_ZIP_EBROKEN, czipfile);
    }
    key = file_stat.m_filename;

    if (zip_str_file_path(cexdir, key, &buffer, &buffer_size, cjunkpaths)) {
      mz_zip_reader_end(&zip_archive);
      if (buffer) free(buffer);
      fclose(zfh);
      ZIP_ERROR(R_ZIP_ENOMEM, czipfile);
    }

    if (file_stat.m_is_directory) {
      if (! cjunkpaths && zip_mkdirp(buffer, 1)) {
	      mz_zip_reader_end(&zip_archive);
	      if (buffer) free(buffer);
	      fclose(zfh);
	      ZIP_ERROR(R_ZIP_EBROKENENTRY, key, czipfile);
      }

    } else {
      if (!coverwrite && zip_file_exists(buffer)) {
	      mz_zip_reader_end(&zip_archive);
	      if (buffer) free(buffer);
	      fclose(zfh);
	      ZIP_ERROR(R_ZIP_EOVERWRITE, key, czipfile);
      }

      if (! cjunkpaths && zip_mkdirp(buffer, 0)) {
	      mz_zip_reader_end(&zip_archive);
	      if (buffer) free(buffer);
	      fclose(zfh);
	      ZIP_ERROR(R_ZIP_ECREATEDIR, key, czipfile);
      }

      FILE *fh = NULL;
#ifdef _WIN32
      fh = _wfopen(buffer, L"wb");
#else
      fh = fopen(buffer, "wb");
#endif
      if (fh == NULL) {
        mz_zip_reader_end(&zip_archive);
        if (buffer) free(buffer);
        fclose(zfh);
        ZIP_ERROR(R_ZIP_EOPENX, key);
      }

      if (!mz_zip_reader_extract_to_cfile(&zip_archive, idx, fh, 0)) {
	      mz_zip_reader_end(&zip_archive);
	      if (buffer) free(buffer);
	      fclose(fh);
	      fclose(zfh);
	      ZIP_ERROR(R_ZIP_EBROKENENTRY, key, czipfile);
      }
      fclose(fh);
    }
#ifndef _WIN32
    mode_t mode;
    /* returns 1 if there are no permissions. In that case we don't call
       call chmod() and leave the permissions as they are, the file was
       created with the default umask. */
    int ret = zip_get_permissions(&file_stat, &mode);
    if (!ret) {
      if (chmod(buffer, mode)) {
        mz_zip_reader_end(&zip_archive);
        if (buffer) free(buffer);
        fclose(zfh);
        ZIP_ERROR(R_ZIP_ESETPERM, key, czipfile);
      }
    }
#endif
  }

  /* Round two, to set the mtime on directories. We skip handling most
     of the errors here, because the central directory is unchanged, and
     if we got here, then it must be still good. */

  for (i = 0; ! cjunkpaths &&  i < n; i++) {
    mz_uint32 idx = -1;
    const char *key = 0;
    mz_zip_archive_file_stat file_stat;

    if (allfiles) {
      idx = (mz_uint32) i;
    } else {
      key = cfiles[i];
      mz_zip_reader_locate_file_v2(&zip_archive, key, /* pComment= */ 0,
				   /* flags= */ 0, &idx);
    }

    mz_zip_reader_file_stat(&zip_archive, idx, &file_stat);
    key = file_stat.m_filename;

    zip_str_file_path(cexdir, key, &buffer, &buffer_size, cjunkpaths);
    if (zip_set_mtime(buffer, file_stat.m_time)) {
      if (buffer) free(buffer);
      mz_zip_reader_end(&zip_archive);
      fclose(zfh);
      ZIP_ERROR(R_ZIP_ESETMTIME, key, czipfile);
    }
  }

  if (buffer) free(buffer);
  mz_zip_reader_end(&zip_archive);
  fclose(zfh);

  /* TODO: return info */
  return 0;
}

int zip_zip(const char *czipfile, int num_files, const char **ckeys,
	    const char **cfiles, int *cdirs, double *cmtimes,
	    int compression_level, int cappend) {

  mz_uint ccompression_level = (mz_uint) compression_level;
  int i, n = num_files;
  mz_zip_archive zip_archive;
  memset(&zip_archive, 0, sizeof(zip_archive));
  zip_char_t *filenameu16 = NULL;
  size_t filenameu16_len = 0;

  FILE *zfh = NULL;

  if (cappend) {
    zfh = zip_open_utf8(czipfile, ZIP__APPEND, &filenameu16,
                        &filenameu16_len);
    if (zfh == NULL) {
      if (filenameu16) free(filenameu16);
      ZIP_ERROR(R_ZIP_EOPENAPPEND, czipfile);
    }
    if (!mz_zip_reader_init_cfile(&zip_archive, zfh, 0, 0) ||
	      !mz_zip_writer_init_from_reader(&zip_archive, NULL)) {
          if (filenameu16) free(filenameu16);
          fclose(zfh);
          ZIP_ERROR(R_ZIP_EOPENAPPEND, czipfile);
    }
  } else {
    zfh = zip_open_utf8(czipfile, ZIP__WRITE, &filenameu16,
                        &filenameu16_len);
    if (zfh == NULL) {
      if (filenameu16) free(filenameu16);
      ZIP_ERROR(R_ZIP_EOPENWRITE, czipfile);
    }
    if (!mz_zip_writer_init_cfile(&zip_archive, zfh, 0)) {
      if (filenameu16) free(filenameu16);
      fclose(zfh);
      ZIP_ERROR(R_ZIP_EOPENWRITE, czipfile);
    }
  }

  for (i = 0; i < n; i++) {
    const char *key = ckeys[i];
    const char *filename = cfiles[i];
    int directory = cdirs[i];
    MZ_TIME_T cmtime = (MZ_TIME_T) cmtimes[i];
    if (directory) {
      if (!mz_zip_writer_add_mem_ex_v2(&zip_archive, key, 0, 0, 0, 0,
				       ccompression_level, 0, 0, &cmtime, 0, 0,
				       0, 0)) {
	      mz_zip_writer_end(&zip_archive);
        if (filenameu16) free(filenameu16);
        fclose(zfh);
        ZIP_ERROR(R_ZIP_EADDDIR, key, czipfile);
      }

    } else {
      FILE* fh = zip_open_utf8(filename, ZIP__READ, &filenameu16,
                               &filenameu16_len);
      if (fh == NULL) {
        if (filenameu16) free(filenameu16);
        fclose(zfh);
        ZIP_ERROR(R_ZIP_EADDFILE, key, czipfile);
      }
      mz_uint64 uncomp_size = 0;
      if (zip_file_size(fh, &uncomp_size)) {
        if (filenameu16) free(filenameu16);
        fclose(zfh);
        ZIP_ERROR(R_ZIP_FILESIZE, filename);
      }

      int ret = mz_zip_writer_add_cfile(&zip_archive, key, fh,
          /* size_to_all= */ uncomp_size, /* pFile_time = */ &cmtime,
          /* pComment= */ NULL, /* comment_size= */ 0,
          /* level_and_flags = */ ccompression_level,
          /* user_extra_data_local = */ NULL,
          /* user_extra_data_local_len = */ 0,
          /* user_extra_data_central = */ NULL,
          /* user_extra_data_central_len = */ 0);
      fclose(fh);
      if (!ret) {
        if (filenameu16) free(filenameu16);
        mz_zip_writer_end(&zip_archive);
        ZIP_ERROR(R_ZIP_EADDFILE, key, czipfile);
      }
    }

    if (zip_set_permissions(&zip_archive, i, filename)) {
      if (filenameu16) free(filenameu16);
      mz_zip_writer_end(&zip_archive);
      fclose(zfh);
      ZIP_ERROR(R_ZIP_ESETZIPPERM, key, czipfile);
    }
  }

  if (!mz_zip_writer_finalize_archive(&zip_archive)) {
    if (filenameu16) free(filenameu16);
    mz_zip_writer_end(&zip_archive);
    fclose(zfh);
    ZIP_ERROR(R_ZIP_ECREATE, czipfile);
  }

  if (!mz_zip_writer_end(&zip_archive)) {
    if (filenameu16) free(filenameu16);
    fclose(zfh);
    ZIP_ERROR(R_ZIP_ECREATE, czipfile);
  }

  if (filenameu16) free(filenameu16);
  fclose(zfh);

  /* TODO: return info */
  return 0;
}
