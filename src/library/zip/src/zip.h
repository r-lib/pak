
#ifndef R_ZIP_H
#define R_ZIP_H

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
  R_ZIP_ECREATELINK  = 18
} zip_error_codes_t;

typedef void zip_error_handler_t(const char *reason, const char *file,
				 int line, int zip_errno, int eno);

void zip_set_error_handler(zip_error_handler_t *handler);

int zip_get_permissions(mz_zip_archive_file_stat *stat, mode_t *mode);

int zip_set_permissions(mz_zip_archive *zip_archive, mz_uint file_index,
			const char *filename);

int zip_zip(const char *czipfile, int num_files, const char **ckeys,
	    const char **cfiles, int *cdirs, double *cmtimes,
	    int compression_level, int cappend);

int zip_unzip(const char *czipfile, const char **cfiles, int num_files,
	      int coverwrite, int cjunkpaths, const char *exdir);

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

#else

#define zip_char_t char

#define ZIP__READ   "rb"
#define ZIP__WRITE  "wb"
#define ZIP__APPEND "r+b"

#endif

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
