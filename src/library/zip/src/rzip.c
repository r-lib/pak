
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/time.h>

#ifdef _WIN32
#include <direct.h>		/* _mkdir */
#include <windows.h>
#endif

#include <Rinternals.h>

#include "miniz.h"
#include "zip.h"

SEXP R_zip_list(SEXP zipfile) {
  const char *czipfile = CHAR(STRING_ELT(zipfile, 0));
  size_t num_files;
  unsigned int i;
  SEXP result = R_NilValue;
  mz_bool status;
  mz_zip_archive zip_archive;

  FILE *fh;
  wchar_t *uzipfile = NULL;

#ifdef _WIN32
  #define R_ZIP_FSEEK64 _fseeki64
  #define R_ZIP_FTELL64 _ftelli64
  size_t uzipfile_len = 0;
  if (zip__utf8_to_utf16(czipfile, &uzipfile, &uzipfile_len)) {
    if (uzipfile) free(uzipfile);
    error("Cannot convert zip file name to unicode");
  }
  fh = zip_long_wfopen(uzipfile, L"rb");
#else
  #define R_ZIP_FSEEK64 fseek
  #define R_ZIP_FTELL64 ftell
  fh = fopen(czipfile, "rb");
#endif

  if (fh == NULL) {
    if (uzipfile) free(uzipfile);
    error("Cannot open zip file `%s`", czipfile);
  }

  R_ZIP_FSEEK64(fh, 0, SEEK_END);
  mz_uint64 file_size = R_ZIP_FTELL64(fh);
  R_ZIP_FSEEK64(fh, 0, SEEK_SET);

  memset(&zip_archive, 0, sizeof(zip_archive));
  status = mz_zip_reader_init_cfile(&zip_archive, fh, file_size, 0);
  if (!status) {
    fclose(fh);
    free(uzipfile);
    error("Cannot open zip file `%s`", czipfile);
  }

  num_files = mz_zip_reader_get_num_files(&zip_archive);
  result = PROTECT(allocVector(VECSXP, 7));
  SET_VECTOR_ELT(result, 0, allocVector(STRSXP, num_files));
  SET_VECTOR_ELT(result, 1, allocVector(REALSXP, num_files));
  SET_VECTOR_ELT(result, 2, allocVector(REALSXP, num_files));
  SET_VECTOR_ELT(result, 3, allocVector(INTSXP, num_files));
  SET_VECTOR_ELT(result, 4, allocVector(INTSXP, num_files));
  SET_VECTOR_ELT(result, 5, allocVector(INTSXP, num_files));
  SET_VECTOR_ELT(result, 6, allocVector(REALSXP, num_files));

  for (i = 0; i < num_files; i++) {
    mz_zip_archive_file_stat file_stat;
    mode_t mode;
    status = mz_zip_reader_file_stat (&zip_archive, i, &file_stat);
    if (!status) goto cleanup;

    SET_STRING_ELT(VECTOR_ELT(result, 0), i, mkChar(file_stat.m_filename));
    REAL(VECTOR_ELT(result, 1))[i] = file_stat.m_comp_size;
    REAL(VECTOR_ELT(result, 2))[i] = file_stat.m_uncomp_size;
    INTEGER(VECTOR_ELT(result, 3))[i] = (int) file_stat.m_time;
    zip_get_permissions(&file_stat, &mode);
    INTEGER(VECTOR_ELT(result, 4))[i] = (int) mode;
    INTEGER(VECTOR_ELT(result, 5))[i] = (int) file_stat.m_crc32;
    REAL(VECTOR_ELT(result, 6))[i] = (double) file_stat.m_local_header_ofs;
  }

  fclose(fh);
  free(uzipfile);
  mz_zip_reader_end(&zip_archive);
  UNPROTECT(1);
  return result;

 cleanup:
  fclose(fh);
  mz_zip_reader_end(&zip_archive);
  error("Cannot list zip entries, corrupt zip file?");
  return result;
}

void R_zip_error_handler(const char *reason, const char *file,
			 int line, int zip_errno, int eno) {
  error("zip error: %s in file %s:%i", reason, file, line);
}

SEXP R_zip_zip(SEXP zipfile, SEXP keys, SEXP files, SEXP dirs, SEXP mtime,
	       SEXP compression_level, SEXP append) {

  const char *czipfile = CHAR(STRING_ELT(zipfile, 0));
  const char **ckeys = 0, **cfiles = 0;
  int *cdirs = INTEGER(dirs);
  double *cmtimes = REAL(mtime);
  int ccompression_level = INTEGER(compression_level)[0];
  int cappend = LOGICAL(append)[0];
  int i, n = LENGTH(keys);

  /* The reason we allocate n+1 here is that otherwise R_alloc will
     return a NULL pointer for n == 0, and zip_unzip interprets that
     as extracting the whole archive. */

  ckeys  = (const char **) R_alloc(n + 1, sizeof(char*));
  cfiles = (const char **) R_alloc(n + 1, sizeof(char*));
  for (i = 0; i < n; i++) {
    ckeys [i] = CHAR(STRING_ELT(keys,  i));
    cfiles[i] = CHAR(STRING_ELT(files, i));
  }

  zip_set_error_handler(R_zip_error_handler);

  zip_zip(czipfile, n, ckeys, cfiles, cdirs, cmtimes, ccompression_level,
	  cappend);

  return R_NilValue;
}

SEXP R_zip_unzip(SEXP zipfile, SEXP files, SEXP overwrite, SEXP junkpaths,
		 SEXP exdir) {

  const char *czipfile = CHAR(STRING_ELT(zipfile, 0));
  int coverwrite = LOGICAL(overwrite)[0];
  int cjunkpaths = LOGICAL(junkpaths)[0];
  const char *cexdir = CHAR(STRING_ELT(exdir, 0));
  int allfiles = isNull(files);
  int i, n = allfiles ? 0 : LENGTH(files);
  const char **cfiles = 0;

  if (!isNull(files)) {
    /* The reason we allocate n+1 here is that otherwise R_alloc will
       return a NULL pointer for n == 0, and zip_unzip interprets that
       as extracting the whole archive. */
    cfiles = (const char**) R_alloc(n + 1, sizeof(char*));
    for (i = 0; i < n; i++) cfiles[i] = CHAR(STRING_ELT(files, i));
  }

  zip_set_error_handler(R_zip_error_handler);
  zip_unzip(czipfile, cfiles, n, coverwrite, cjunkpaths, cexdir);

  return R_NilValue;
}


#ifdef __APPLE__
#include <fcntl.h>
#include <unistd.h>
#endif


#ifdef _WIN32

int zip__utf8_to_utf16(const char* s, wchar_t** buffer,
                       size_t *buffer_size);

#endif

SEXP R_make_big_file(SEXP filename, SEXP mb) {

#ifdef _WIN32

  const char *cfilename = CHAR(STRING_ELT(filename, 0));
  LARGE_INTEGER li;

  wchar_t *wfilename = NULL;
  size_t wfilename_size = 0;

  if (zip__utf8_to_utf16(cfilename, &wfilename, &wfilename_size)) {
    error("utf8 -> utf16 conversion");
  }

  HANDLE h = CreateFileW(
    wfilename,
    GENERIC_WRITE,
    FILE_SHARE_DELETE,
    NULL,
    CREATE_NEW,
    FILE_ATTRIBUTE_NORMAL,
    NULL);
  if (h == INVALID_HANDLE_VALUE) {
    if (wfilename) free(wfilename);
    error("Cannot create big file");
  }

  li.QuadPart = INTEGER(mb)[0] * 1024.0 * 1024.0;
  li.LowPart = SetFilePointer(h, li.LowPart, &li.HighPart, FILE_BEGIN);

  if (0xffffffff == li.LowPart && GetLastError() != NO_ERROR) {
    CloseHandle(h);
    if (wfilename) free(wfilename);
    error("Cannot create big file");
  }

  if (!SetEndOfFile(h)) {
    CloseHandle(h);
    if (wfilename) free(wfilename);
    error("Cannot create big file");
  }

  if (wfilename) free(wfilename);
  CloseHandle(h);

#endif

#ifdef __APPLE__

  const char *cfilename = CHAR(STRING_ELT(filename, 0));
  int fd = open(cfilename, O_WRONLY | O_CREAT);
  double sz = INTEGER(mb)[0] * 1024.0 * 1024.0;
  fstore_t store = { F_ALLOCATECONTIG, F_PEOFPOSMODE, 0, (off_t) sz };
  // Try to get a continous chunk of disk space
  int ret = fcntl(fd, F_PREALLOCATE, &store);
  if (-1 == ret) {
    // OK, perhaps we are too fragmented, allocate non-continuous
    store.fst_flags = F_ALLOCATEALL;
    ret = fcntl(fd, F_PREALLOCATE, &store);
    if (-1 == ret) error("Cannot create big file");
  }

  if (ftruncate(fd, (off_t) sz)) {
    close(fd);
    error("Cannot create big file");
  }

  close(fd);

#endif

#ifndef _WIN32
#ifndef __APPLE__
  error("cannot create big file (only implemented for windows and macos");
#endif
#endif

  return R_NilValue;
}

SEXP R_inflate(SEXP buffer, SEXP pos, SEXP size) {
  int status;
  mz_stream stream;
  size_t cpos = INTEGER(pos)[0] - 1;
  size_t csize;
  const char *nms[] = { "output", "bytes_read", "bytes_written", "" };
  SEXP result = PROTECT(Rf_mkNamed(VECSXP, nms));
  if (isNull(size)) {
    csize = (LENGTH(buffer) - cpos) * 2;
  } else {
    csize = INTEGER(size)[0];
  }
  if (csize < 10) csize = 10;
  SEXP output = PROTECT(allocVector(RAWSXP, csize));

  memset(&stream, 0, sizeof(stream));
  stream.next_in = RAW(buffer) + cpos;
  stream.avail_in = LENGTH(buffer) - cpos;
  stream.next_out = RAW(output);
  stream.avail_out = csize;

  status = mz_inflateInit2(&stream, MZ_DEFAULT_WINDOW_BITS);

  if (status != 0) {
    error("Failed to initiaalize decompressor");
  }

  for (;;) {
    status = mz_inflate(&stream, MZ_SYNC_FLUSH);

    if (status == MZ_STREAM_END) {
      mz_inflateEnd(&stream);
      break;
    } else if (status == MZ_STREAM_ERROR) {
      mz_inflateEnd(&stream);
      error("Input stream is bogus");
    } else if (status == MZ_DATA_ERROR) {
      mz_deflateEnd(&stream);
      error("Input data is invalid");
    }

    if ((status == MZ_OK || status == MZ_BUF_ERROR) &&
        stream.avail_out == 0) {
      int newsize = csize * 1.5;
      output = Rf_lengthgets(output, newsize);
      UNPROTECT(1);
      PROTECT(output);
      stream.next_out = RAW(output) + csize;
      stream.avail_out = newsize - csize;
      csize = newsize;
      continue;
    }

    if (status == MZ_OK) {
      mz_inflateEnd(&stream);
      break;
    }

    if (status != MZ_OK) {
      mz_inflateEnd(&stream);
      error("Failed to inflate data");
    }
  }

  output = PROTECT(Rf_lengthgets(output, stream.total_out));

  SET_VECTOR_ELT(result, 0, output);
  SET_VECTOR_ELT(result, 1, Rf_ScalarInteger(stream.total_in));
  SET_VECTOR_ELT(result, 2, Rf_ScalarInteger(stream.total_out));
  UNPROTECT(3);
  return result;
}

SEXP R_deflate(SEXP buffer, SEXP level, SEXP pos, SEXP size) {
  int clevel = INTEGER(level)[0];
  int status;
  mz_stream stream;
  size_t cpos = INTEGER(pos)[0] - 1;
  size_t csize;
  const char *nms[] = { "output", "bytes_read", "bytes_written", "" };
  SEXP result = PROTECT(Rf_mkNamed(VECSXP, nms));

  if (isNull(size)) {
    csize = (LENGTH(buffer) - cpos);
  } else {
    csize = INTEGER(size)[0];
  }
  if (csize < 10) csize = 10;
  SEXP output = PROTECT(allocVector(RAWSXP, csize));

  memset(&stream, 0, sizeof(stream));
  stream.next_in = RAW(buffer) + cpos;
  stream.avail_in = LENGTH(buffer) - cpos;
  stream.next_out = RAW(output);
  stream.avail_out = csize;

  status = mz_deflateInit2(
    &stream,
    clevel,
    MZ_DEFLATED,
    MZ_DEFAULT_WINDOW_BITS,
    /* mem_level= */ 9,
    MZ_DEFAULT_STRATEGY
  );

  if (status != 0) {
    error("Failed to initiaalize compressor");
  }

  for (;;) {
    status = mz_deflate(&stream, MZ_SYNC_FLUSH);

    if (status == MZ_STREAM_END) {
      mz_deflateEnd(&stream);
      break;
    } else if (status == MZ_STREAM_ERROR) {
      mz_deflateEnd(&stream);
      error("Input stream is bogus");
    } else if (status == MZ_DATA_ERROR) {
      mz_deflateEnd(&stream);
      error("Input data is invalid");
    }

    if ((status == MZ_OK || status == MZ_BUF_ERROR) &&
        stream.avail_out == 0) {
      int newsize = csize * 1.5;
      output = Rf_lengthgets(output, newsize);
      UNPROTECT(1);
      PROTECT(output);
      stream.next_out = RAW(output) + csize;
      stream.avail_out = newsize - csize;
      csize = newsize;
      continue;
    }

    if (status == MZ_OK) {
      mz_deflateEnd(&stream);
      break;
    }

    if (status != MZ_OK) {
      mz_deflateEnd(&stream);
      error("Failed to deflate data");
    }
  }

  output = PROTECT(Rf_lengthgets(output, stream.total_out));

  SET_VECTOR_ELT(result, 0, output);
  SET_VECTOR_ELT(result, 1, Rf_ScalarInteger(stream.total_in));
  SET_VECTOR_ELT(result, 2, Rf_ScalarInteger(stream.total_out));
  UNPROTECT(3);
  return result;
}
