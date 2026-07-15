
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
#include <R_ext/Riconv.h>

#include "errors.h"
#include "miniz.h"
#include "zip.h"
#include "crypto.h"
#include "cleancall.h"
#include <cli/progress.h>

#ifndef S_IFLNK
#define S_IFLNK         0120000         /* [XSI] symbolic link */
#endif
#ifndef S_IFSOCK
#define S_IFSOCK        0140000         /* [XSI] socket */
#endif

#ifndef S_IFMT
#define S_IFMT          0170000
#endif
#ifndef S_ISLNK
#define S_ISLNK(m)      (((m) & S_IFMT) == S_IFLNK)     /* symbolic link */
#endif
#ifndef S_ISSOCK
#define S_ISSOCK(m)     (((m) & S_IFMT) == S_IFSOCK)    /* socket */
#endif

static int r_check_encoding(const char *encoding) {
  if (!encoding || encoding[0] == '\0') return 0;
  void *cd = Riconv_open("UTF-8", encoding);
  if (cd == (void *) -1) return -1;
  Riconv_close(cd);
  return 0;
}

static char *r_decode_filename(const char *src, void *data) {
  const char *encoding = (const char *) data;
  void *cd = Riconv_open("UTF-8", encoding);
  if (cd == (void *) -1) return NULL;

  size_t srclen = strlen(src);
  size_t dstlen = srclen * 4 + 1;
  char *result = malloc(dstlen);
  if (!result) { Riconv_close(cd); return NULL; }

  const char *inbuf = src;
  char *outbuf = result;
  size_t inbytesleft = srclen;
  size_t outbytesleft = dstlen - 1;

  if (Riconv(cd, &inbuf, &inbytesleft, &outbuf, &outbytesleft) == (size_t) -1) {
    Riconv_close(cd);
    free(result);
    return NULL;
  }
  *outbuf = '\0';
  Riconv_close(cd);
  return result;
}

SEXP R_zip_list(SEXP zipfile, SEXP encoding) {
  const char *czipfile = CHAR(STRING_ELT(zipfile, 0));
  const char *cencoding = isNull(encoding) ? NULL : CHAR(STRING_ELT(encoding, 0));
  if (r_check_encoding(cencoding))
    R_THROW_ERROR("zip: unsupported encoding: '%s'", cencoding);
  size_t num_files;
  unsigned int i;
  SEXP result = R_NilValue;
  mz_bool status;
  mz_zip_archive zip_archive;

  FILE *fh;
  wchar_t *uzipfile = NULL;

#ifdef _WIN32
  size_t uzipfile_len = 0;
  if (zip__utf8_to_utf16(czipfile, &uzipfile, &uzipfile_len)) {
    if (uzipfile) free(uzipfile);
    R_THROW_ERROR("Cannot convert zip file name to unicode");
  }
  fh = zip_long_wfopen(uzipfile, L"rb");
#else
  fh = fopen(czipfile, "rb");
#endif

  if (fh == NULL) {
    if (uzipfile) free(uzipfile);
    R_THROW_ERROR("Cannot open zip file `%s`", czipfile);
  }

  zip_fseek(fh, 0, SEEK_END);
  mz_uint64 file_size = zip_ftell(fh);
  zip_fseek64(fh, 0);

  memset(&zip_archive, 0, sizeof(zip_archive));
  status = mz_zip_reader_init_cfile(&zip_archive, fh, file_size, 0);
  if (!status) {
    const char *mz_err =
      mz_zip_get_error_string(mz_zip_get_last_error(&zip_archive));
    mz_zip_reader_end(&zip_archive);
    fclose(fh);
    if (uzipfile) free(uzipfile);
    R_THROW_ERROR("Cannot open zip file `%s`: %s", czipfile, mz_err);
  }

  num_files = mz_zip_reader_get_num_files(&zip_archive);
  result = PROTECT(allocVector(VECSXP, 9));
  SET_VECTOR_ELT(result, 0, allocVector(STRSXP, num_files));
  SET_VECTOR_ELT(result, 1, allocVector(REALSXP, num_files));
  SET_VECTOR_ELT(result, 2, allocVector(REALSXP, num_files));
  SET_VECTOR_ELT(result, 3, allocVector(INTSXP, num_files));
  SET_VECTOR_ELT(result, 4, allocVector(INTSXP, num_files));
  SET_VECTOR_ELT(result, 5, allocVector(INTSXP, num_files));
  SET_VECTOR_ELT(result, 6, allocVector(REALSXP, num_files));
  SET_VECTOR_ELT(result, 7, allocVector(INTSXP, num_files));
  SET_VECTOR_ELT(result, 8, allocVector(INTSXP, num_files));

  for (i = 0; i < num_files; i++) {
    mz_zip_archive_file_stat file_stat;
    mode_t mode;
    status = mz_zip_reader_file_stat (&zip_archive, i, &file_stat);
    if (!status) goto cleanup;

    const char *fname = file_stat.m_filename;
    char *fname_utf8 = NULL;
    if (!(file_stat.m_bit_flag & 0x800)) {
      fname_utf8 = cencoding
        ? r_decode_filename(fname, (void *) cencoding)
        : zip_cp437_to_utf8(fname);
      if (fname_utf8) fname = fname_utf8;
    }
    SET_STRING_ELT(VECTOR_ELT(result, 0), i, mkCharCE(fname, CE_UTF8));
    if (fname_utf8) free(fname_utf8);
    REAL(VECTOR_ELT(result, 1))[i] = file_stat.m_comp_size;
    REAL(VECTOR_ELT(result, 2))[i] = file_stat.m_uncomp_size;
    INTEGER(VECTOR_ELT(result, 3))[i] = (int) file_stat.m_time;
    zip_get_permissions(&file_stat, &mode);
    INTEGER(VECTOR_ELT(result, 4))[i] = (int) mode;
    INTEGER(VECTOR_ELT(result, 5))[i] = (int) file_stat.m_crc32;
    REAL(VECTOR_ELT(result, 6))[i] = (double) file_stat.m_local_header_ofs;
    INTEGER(VECTOR_ELT(result, 7))[i] = 0;
    mz_uint32 attr = file_stat.m_external_attr >> 16;
    /* miniz flags directories from the trailing slash or the DOS directory
       bit, which catches entries whose Unix mode bits lack S_IFDIR. */
    if (file_stat.m_is_directory) {
      INTEGER(VECTOR_ELT(result, 7))[i] = 3;
    } else if (S_ISBLK(attr)) {
      INTEGER(VECTOR_ELT(result, 7))[i] = 1;
    } else if (S_ISCHR(attr)) {
      INTEGER(VECTOR_ELT(result, 7))[i] = 2;
    } else if (S_ISDIR(attr)) {
      INTEGER(VECTOR_ELT(result, 7))[i] = 3;
    } else if (S_ISFIFO(attr)) {
      INTEGER(VECTOR_ELT(result, 7))[i] = 4;
    } else if (S_ISREG(attr)) {
      INTEGER(VECTOR_ELT(result, 7))[i] = 0;
    } else if (S_ISLNK(attr)) {
      INTEGER(VECTOR_ELT(result, 7))[i] = 5;
    } else if (S_ISSOCK(attr)) {
      INTEGER(VECTOR_ELT(result, 7))[i] = 6;
    }
    INTEGER(VECTOR_ELT(result, 8))[i] = zip_entry_encryption_type(fh, &file_stat);
  }

  fclose(fh);
  free(uzipfile);
  mz_zip_reader_end(&zip_archive);
  UNPROTECT(1);
  return result;

 cleanup:
  {
    const char *mz_err =
      mz_zip_get_error_string(mz_zip_get_last_error(&zip_archive));
    fclose(fh);
    if (uzipfile) free(uzipfile);
    mz_zip_reader_end(&zip_archive);
    UNPROTECT(1);
    R_THROW_ERROR("Cannot list zip entries in `%s`: %s", czipfile, mz_err);
  }
  return result;
}

void R_zip_error_handler(const char *reason, const char *file,
			 int line, int zip_errno, int eno) {
  R_THROW_ERROR("%s", reason);
}

static void r_zip_progress_fn(mz_uint64 bytes_done, void *data) {
  cli_progress_set((SEXP) data, (double) bytes_done);
}

static SEXP make_progress_bar(double total, const char *name) {
  SEXP bar = PROTECT(cli_progress_bar(total, R_NilValue));
  cli_progress_set_name(bar, name);
  UNPROTECT(1);
  return bar;
}

SEXP R_zip_cp437_to_utf8(SEXP bytes) {
  R_xlen_t n = XLENGTH(bytes);
  char *src = malloc(n + 1);
  if (!src) R_THROW_ERROR("Cannot decode CP437 file name, out of memory");
  memcpy(src, RAW(bytes), n);
  src[n] = '\0';

  char *utf8 = zip_cp437_to_utf8(src);
  free(src);
  if (!utf8) R_THROW_ERROR("Cannot decode CP437 file name, out of memory");

  SEXP result = PROTECT(ScalarString(mkCharCE(utf8, CE_UTF8)));
  free(utf8);
  UNPROTECT(1);
  return result;
}

SEXP R_zip_zip(SEXP zipfile, SEXP keys, SEXP files, SEXP dirs, SEXP mtime,
	       SEXP compression_level, SEXP append, SEXP total_bytes,
	       SEXP password, SEXP encryption) {

  const char *czipfile = CHAR(STRING_ELT(zipfile, 0));
  const char **ckeys = 0, **cfiles = 0;
  int *cdirs = INTEGER(dirs);
  double *cmtimes = REAL(mtime);
  int ccompression_level = INTEGER(compression_level)[0];
  int cappend = LOGICAL(append)[0];
  double ctotal_bytes = REAL(total_bytes)[0];
  int cshow_progress = !ISNA(ctotal_bytes);
  /* password is a raw vector of the password bytes, or NULL for no encryption.
     encryption is the scheme/strength (see zip_encryption_t): 0 = none,
     1/2/3 = WinZip AES-128/192/256. */
  const unsigned char *cpassword = isNull(password) ? NULL : RAW(password);
  size_t cpassword_len = isNull(password) ? 0 : (size_t) LENGTH(password);
  int cencryption = isNull(password) ? 0 : INTEGER(encryption)[0];
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

  SEXP bar = PROTECT(cshow_progress ? make_progress_bar(ctotal_bytes, "Zipping") : R_NilValue);

  zip_set_error_handler(R_zip_error_handler);

  zip_zip(czipfile, n, ckeys, cfiles, cdirs, cmtimes, ccompression_level,
	  cappend, cpassword, cpassword_len, cencryption,
	  cshow_progress ? r_zip_progress_fn : NULL, bar);

  cli_progress_done(bar);
  UNPROTECT(1);
  return R_NilValue;
}

/* Passed to r_unzip_entry_fn; result is pre-allocated by R_zip_unzip. */
typedef struct {
  SEXP   result;
  int    num_entries; /* expected length of result vectors; checked in callback */
  SEXP   bar;
#ifdef _WIN32
  char  *path_utf8;
  size_t path_utf8_size;
#endif
} r_unzip_data_t;

static void r_unzip_entry_fn(int n, int i,
                              const mz_zip_archive_file_stat *stat,
                              const char *fname_utf8,
                              const zip_char_t *path,
                              void *data_ptr) {
  r_unzip_data_t *d = (r_unzip_data_t *) data_ptr;
  if (i >= d->num_entries)
    error("zip: archive changed between directory scan and extraction");
  SEXP result = d->result;

  SET_STRING_ELT(VECTOR_ELT(result, 0), i, mkCharCE(fname_utf8, CE_UTF8));
  REAL(VECTOR_ELT(result, 1))[i]    = stat->m_comp_size;
  REAL(VECTOR_ELT(result, 2))[i]    = stat->m_uncomp_size;
  INTEGER(VECTOR_ELT(result, 3))[i] = (int) stat->m_time;
  mode_t mode;
  zip_get_permissions((mz_zip_archive_file_stat *) stat, &mode);
  INTEGER(VECTOR_ELT(result, 4))[i] = (int) mode;
  INTEGER(VECTOR_ELT(result, 5))[i] = (int) stat->m_crc32;
  REAL(VECTOR_ELT(result, 6))[i]    = (double) stat->m_local_header_ofs;

  INTEGER(VECTOR_ELT(result, 7))[i] = 0;
  mz_uint32 attr = stat->m_external_attr >> 16;
  /* see R_zip_list: trust miniz's directory flag before the Unix mode bits */
  if (stat->m_is_directory) INTEGER(VECTOR_ELT(result, 7))[i] = 3;
  else if (S_ISBLK(attr))   INTEGER(VECTOR_ELT(result, 7))[i] = 1;
  else if (S_ISCHR(attr))   INTEGER(VECTOR_ELT(result, 7))[i] = 2;
  else if (S_ISDIR(attr))   INTEGER(VECTOR_ELT(result, 7))[i] = 3;
  else if (S_ISFIFO(attr))  INTEGER(VECTOR_ELT(result, 7))[i] = 4;
  else if (S_ISLNK(attr))  INTEGER(VECTOR_ELT(result, 7))[i] = 5;
  else if (S_ISSOCK(attr)) INTEGER(VECTOR_ELT(result, 7))[i] = 6;

#ifdef _WIN32
  zip__utf16_to_utf8(path, &d->path_utf8, &d->path_utf8_size);
  SET_STRING_ELT(VECTOR_ELT(result, 8), i, mkCharCE(d->path_utf8, CE_UTF8));
#else
  SET_STRING_ELT(VECTOR_ELT(result, 8), i, mkCharCE(path, CE_UTF8));
#endif

  if (d->bar != R_NilValue) cli_progress_update(d->bar, -1, 1, 0);
}

SEXP R_zip_unzip(SEXP zipfile, SEXP files, SEXP overwrite, SEXP junkpaths,
		 SEXP exdir, SEXP encoding, SEXP show_progress, SEXP password) {

  const char *czipfile = CHAR(STRING_ELT(zipfile, 0));
  int coverwrite = LOGICAL(overwrite)[0];
  int cjunkpaths = LOGICAL(junkpaths)[0];
  const char *cexdir = CHAR(STRING_ELT(exdir, 0));
  const char *cencoding = isNull(encoding) ? NULL : CHAR(STRING_ELT(encoding, 0));
  int cshow_progress = LOGICAL(show_progress)[0];
  const unsigned char *cpassword = isNull(password) ? NULL : RAW(password);
  size_t cpassword_len = isNull(password) ? 0 : (size_t) LENGTH(password);
  if (r_check_encoding(cencoding))
    R_THROW_ERROR("zip: unsupported encoding: '%s'", cencoding);
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

  /* When extracting all files, open the archive briefly to count entries so
     we can pre-allocate the result vectors before extraction begins. */
  int num_entries = n; /* known already when files != NULL */
  if (allfiles) {
    mz_zip_archive tmp_archive;
    memset(&tmp_archive, 0, sizeof(tmp_archive));
    zip_char_t *tmp_buf = NULL;
    size_t tmp_buf_size = 0;
    FILE *tmp_fh = zip_open_utf8(czipfile, ZIP__READ, &tmp_buf, &tmp_buf_size);
    if (tmp_fh == NULL) error("Cannot open zip file `%s`", czipfile);
    if (!mz_zip_reader_init_cfile(&tmp_archive, tmp_fh, 0, 0)) {
      if (tmp_buf) free(tmp_buf);
      fclose(tmp_fh);
      error("Cannot open zip file `%s`", czipfile);
    }
    num_entries = (int) mz_zip_reader_get_num_files(&tmp_archive);
    mz_zip_reader_end(&tmp_archive);
    if (tmp_buf) free(tmp_buf);
    fclose(tmp_fh);
  }

  SEXP bar = PROTECT(cshow_progress ? make_progress_bar((double) num_entries, "Unzipping") : R_NilValue);

  /* Allocate result vectors. PROTECT and UNPROTECT are both in this function. */
  SEXP result = PROTECT(allocVector(VECSXP, 9));
  SET_VECTOR_ELT(result, 0, allocVector(STRSXP,  num_entries)); /* filename */
  SET_VECTOR_ELT(result, 1, allocVector(REALSXP, num_entries)); /* compressed_size */
  SET_VECTOR_ELT(result, 2, allocVector(REALSXP, num_entries)); /* uncompressed_size */
  SET_VECTOR_ELT(result, 3, allocVector(INTSXP,  num_entries)); /* timestamp */
  SET_VECTOR_ELT(result, 4, allocVector(INTSXP,  num_entries)); /* permissions */
  SET_VECTOR_ELT(result, 5, allocVector(INTSXP,  num_entries)); /* crc32 */
  SET_VECTOR_ELT(result, 6, allocVector(REALSXP, num_entries)); /* offset */
  SET_VECTOR_ELT(result, 7, allocVector(INTSXP,  num_entries)); /* type */
  SET_VECTOR_ELT(result, 8, allocVector(STRSXP,  num_entries)); /* path */

  r_unzip_data_t data;
  memset(&data, 0, sizeof(data));
  data.result = result;
  data.num_entries = num_entries;
  data.bar = bar;

  zip_set_error_handler(R_zip_error_handler);
  zip_unzip(czipfile, cfiles, n, coverwrite, cjunkpaths, cexdir,
            cencoding ? r_decode_filename : NULL, (void *) cencoding,
            r_unzip_entry_fn, &data,
            cpassword, cpassword_len);

#ifdef _WIN32
  if (data.path_utf8) free(data.path_utf8);
#endif

  cli_progress_done(bar);
  UNPROTECT(2); /* bar + result */
  return result;
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

SEXP R_inflate(SEXP buffer, SEXP pos, SEXP size, SEXP raw) {
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

  int wbits = LOGICAL(raw)[0] ? -MZ_DEFAULT_WINDOW_BITS : MZ_DEFAULT_WINDOW_BITS;
  status = mz_inflateInit2(&stream, wbits);

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

/* ------------------------------------------------------------------------ */
/* Test shims for the WinZip AES crypto primitives in crypto.c.             */
/* These exist so the published test vectors (PBKDF2 RFC 6070, HMAC-SHA1    */
/* RFC 2202, AES, WinZip key block derivation) can be checked from          */
/* testthat. They are not part of the package's public API.                 */
/* ------------------------------------------------------------------------ */

SEXP R_crypto_pbkdf2_sha1(SEXP password, SEXP salt, SEXP iterations,
                          SEXP dklen) {
  size_t cdklen = (size_t) INTEGER(dklen)[0];
  SEXP output = PROTECT(allocVector(RAWSXP, cdklen));
  int ret = zip_pbkdf2_sha1(
    RAW(password), (size_t) LENGTH(password),
    RAW(salt), (size_t) LENGTH(salt),
    (unsigned int) INTEGER(iterations)[0],
    RAW(output), cdklen
  );
  if (ret != 0) {
    error("PBKDF2-HMAC-SHA1 failed (mbedtls error %d)", ret);
  }
  UNPROTECT(1);
  return output;
}

SEXP R_crypto_hmac_sha1(SEXP key, SEXP data) {
  SEXP output = PROTECT(allocVector(RAWSXP, 20));
  int ret = zip_hmac_sha1(
    RAW(key), (size_t) LENGTH(key),
    RAW(data), (size_t) LENGTH(data),
    RAW(output)
  );
  if (ret != 0) {
    error("HMAC-SHA1 failed (mbedtls error %d)", ret);
  }
  UNPROTECT(1);
  return output;
}

SEXP R_crypto_aes_ctr(SEXP key, SEXP data) {
  int keybits = LENGTH(key) * 8;
  size_t len = (size_t) LENGTH(data);
  SEXP output;
  int ret;
  if (keybits != 128 && keybits != 192 && keybits != 256) {
    error("AES key must be 16, 24 or 32 bytes, not %d", LENGTH(key));
  }
  output = PROTECT(allocVector(RAWSXP, len));
  ret = zip_aes_ctr_crypt(
    RAW(key), keybits, RAW(data), RAW(output), len
  );
  if (ret != 0) {
    error("AES-CTR failed (mbedtls error %d)", ret);
  }
  UNPROTECT(1);
  return output;
}

SEXP R_crypto_winzip_keys(SEXP password, SEXP salt, SEXP strength) {
  int cstrength = INTEGER(strength)[0];
  int keylen = zip_winzip_key_len(cstrength);
  const char *nms[] = { "enc_key", "mac_key", "verifier", "" };
  SEXP result, enc_key, mac_key, verifier;
  int ret;

  if (keylen < 0) {
    error("Invalid WinZip AES strength %d (must be 1, 2 or 3)", cstrength);
  }

  result = PROTECT(Rf_mkNamed(VECSXP, nms));
  enc_key = PROTECT(allocVector(RAWSXP, keylen));
  mac_key = PROTECT(allocVector(RAWSXP, keylen));
  verifier = PROTECT(allocVector(RAWSXP, 2));

  ret = zip_winzip_aes_keys(
    RAW(password), (size_t) LENGTH(password),
    RAW(salt), (size_t) LENGTH(salt),
    cstrength, RAW(enc_key), RAW(mac_key), RAW(verifier)
  );
  if (ret != 0) {
    error("WinZip AES key derivation failed (mbedtls error %d)", ret);
  }

  SET_VECTOR_ELT(result, 0, enc_key);
  SET_VECTOR_ELT(result, 1, mac_key);
  SET_VECTOR_ELT(result, 2, verifier);
  UNPROTECT(4);
  return result;
}
