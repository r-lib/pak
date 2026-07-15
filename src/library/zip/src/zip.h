
#ifndef R_ZIP_H
#define R_ZIP_H

#if defined(_MSC_VER)
#  define ZIP_THREAD_LOCAL __declspec(thread)
#elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
#  define ZIP_THREAD_LOCAL _Thread_local
#else
#  define ZIP_THREAD_LOCAL __thread
#endif

#include <sys/types.h>
#include <time.h>
#include <stdio.h>

#include "miniz.h"

typedef enum zip_error_codes {
  R_ZIP_ESUCCESS     =  0,
  R_ZIP_EOPEN        =  1,
  R_ZIP_ENOMEM       =  2,
  R_ZIP_ENOENTRY     =  3,
  R_ZIP_EBROKEN      =  4,
  R_ZIP_EBROKENENTRY =  5,
  R_ZIP_EOVERWRITE   =  6,
  R_ZIP_ECREATEDIR   =  7,
  R_ZIP_ESETPERM     =  8,
  R_ZIP_ESETMTIME    =  9,
  R_ZIP_EOPENWRITE   = 10,
  R_ZIP_EOPENAPPEND  = 11,
  R_ZIP_EADDDIR      = 12,
  R_ZIP_EADDFILE     = 13,
  R_ZIP_ESETZIPPERM  = 14,
  R_ZIP_ECREATE      = 15,
  R_ZIP_EOPENX       = 16,
  R_ZIP_FILESIZE     = 17,
  R_ZIP_ECREATELINK    = 18,
  R_ZIP_EENCRYPT       = 19,
  R_ZIP_EWRONGPASSWORD = 20,
  R_ZIP_EBADHMAC       = 21,
  R_ZIP_ENOPASSWORD    = 22
} zip_error_codes_t;

/* Encryption scheme for zip_zip(). The non-zero values double as the WinZip
   AES strength byte (1 = AES-128, 2 = AES-192, 3 = AES-256). */
typedef enum zip_encryption {
  ZIP_ENCRYPTION_NONE      = 0,
  ZIP_ENCRYPTION_AES128    = 1,
  ZIP_ENCRYPTION_AES192    = 2,
  ZIP_ENCRYPTION_AES256    = 3,
  ZIP_ENCRYPTION_ZIPCRYPTO = 4
} zip_encryption_t;

typedef void zip_error_handler_t(const char *reason, const char *file,
				 int line, int zip_errno, int eno);

void zip_set_error_handler(zip_error_handler_t *handler);

int zip_get_permissions(mz_zip_archive_file_stat *stat, mode_t *mode);

int zip_set_permissions(mz_zip_archive *zip_archive, mz_uint file_index,
			const char *filename);

typedef void (*zip_progress_fn)(mz_uint64 bytes_done, void *data);

int zip_zip(const char *czipfile, int num_files, const char **ckeys,
	    const char **cfiles, int *cdirs, double *cmtimes,
	    int compression_level, int cappend,
	    const unsigned char *cpassword, size_t cpassword_len,
	    int cencryption,
	    zip_progress_fn progress_fn, void *progress_data);

typedef char *(*zip_decode_fn)(const char *src, void *data);

#ifdef _WIN32

#include <windows.h>

#define zip_char_t wchar_t

int zip__utf8_to_utf16(const char* s, wchar_t** buffer,
                       size_t *buffer_size);
int zip__utf16_to_utf8(const wchar_t *ws, char** buffer,
                       size_t *buffer_size);

FILE* zip_long_wfopen(const wchar_t *filename,
                     const wchar_t *mode);

#define ZIP__READ   L"rb"
#define ZIP__WRITE  L"wb"
#define ZIP__APPEND L"r+b"

/* Platform-portable 64-bit fseek / ftell. */
#define zip_fseek(fh, ofs, whence) _fseeki64((fh), (__int64)(ofs), (whence))
#define zip_ftell(fh)              _ftelli64(fh)

#else

#define zip_char_t char

#define ZIP__READ   "rb"
#define ZIP__WRITE  "wb"
#define ZIP__APPEND "r+b"

#define zip_fseek(fh, ofs, whence) fseek((fh), (long)(ofs), (whence))
#define zip_ftell(fh)              ftell(fh)

#endif

/* Absolute seek from start of file (SEEK_SET). */
#define zip_fseek64(fh, ofs) zip_fseek((fh), (ofs), SEEK_SET)

/* Called once per extracted entry after the entry is fully written.
   n  = total number of entries being extracted
   i  = 0-based index of this entry
   stat  = miniz file stat for this entry
   fname_utf8 = UTF-8 decoded filename (same string used for the filesystem path)
   path = full extracted path (zip_char_t: wchar_t on Windows, char elsewhere) */
typedef void (*zip_entry_fn)(
    int n,
    int i,
    const mz_zip_archive_file_stat *stat,
    const char *fname_utf8,
    const zip_char_t *path,
    void *data
);

int zip_unzip(const char *czipfile, const char **cfiles, int num_files,
	      int coverwrite, int cjunkpaths, const char *exdir,
	      zip_decode_fn decode_fn, void *decode_data,
	      zip_entry_fn entry_fn, void *entry_data,
	      const unsigned char *cpassword, size_t cpassword_len);

int zip_entry_encryption_type(FILE *fh, const mz_zip_archive_file_stat *fs);

char *zip_cp437_to_utf8(const char *src);

FILE *zip_open_utf8(const char *filename, const zip_char_t *mode,
                    zip_char_t **buffer, size_t *buffer_size);
int zip_str_file_path(const char *cexdir, const char *key,
                      zip_char_t **buffer, size_t *buffer_size,
                      int cjunkpaths);
int zip_mkdirp(zip_char_t *path, int complete);
int zip_set_mtime(const zip_char_t *filename, time_t mtime);
int zip_file_exists(zip_char_t *filename);
int zip_file_size(FILE *fh, mz_uint64 *size);

#endif
