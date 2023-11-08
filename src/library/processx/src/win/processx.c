
#include <R.h>
#include <R_ext/Rdynload.h>

#include "../processx.h"

#include <wchar.h>

static HANDLE processx__global_job_handle = NULL;

static void processx__init_global_job_handle(void) {
  /* Create a job object and set it up to kill all contained processes when
   * it's closed. Since this handle is made non-inheritable and we're not
   * giving it to anyone, we're the only process holding a reference to it.
   * That means that if this process exits it is closed and all the
   * processes it contains are killed. All processes created with processx
   * that are spawned without the cleanup flag are assigned to this job.
   *
   * We're setting the JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK flag so only
   * the processes that we explicitly add are affected, and *their*
   * subprocesses are not. This ensures that our child processes are not
   * limited in their ability to use job control on Windows versions that
   * don't deal with nested jobs (prior to Windows 8 / Server 2012). It
   * also lets our child processes create detached processes without
   * explicitly breaking away from job control (which processx_exec
   * doesn't do, either). */

  SECURITY_ATTRIBUTES attr;
  JOBOBJECT_EXTENDED_LIMIT_INFORMATION info;

  memset(&attr, 0, sizeof attr);
  attr.bInheritHandle = FALSE;

  memset(&info, 0, sizeof info);
  info.BasicLimitInformation.LimitFlags =
      JOB_OBJECT_LIMIT_BREAKAWAY_OK |
      JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK |
      JOB_OBJECT_LIMIT_DIE_ON_UNHANDLED_EXCEPTION |
      JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;

  processx__global_job_handle = CreateJobObjectW(&attr, NULL);
  if (processx__global_job_handle == NULL) {
    R_THROW_SYSTEM_ERROR("Creating global job object");
  }

  if (!SetInformationJobObject(processx__global_job_handle,
                               JobObjectExtendedLimitInformation,
                               &info,
                               sizeof info)) {
    R_THROW_SYSTEM_ERROR("Setting up global job object");
  }
}

void R_init_processx_win(void) {
  /* Nothing to do currently */
}

SEXP processx__unload_cleanup(void) {

  if (processx__connection_iocp) CloseHandle(processx__connection_iocp);
  if (processx__iocp_thread) TerminateThread(processx__iocp_thread, 0);
  if (processx__thread_start) CloseHandle(processx__thread_start);
  if (processx__thread_done) CloseHandle(processx__thread_done);

  processx__connection_iocp = processx__iocp_thread =
    processx__thread_start = processx__thread_done = NULL;

  if (processx__global_job_handle) {
    TerminateJobObject(processx__global_job_handle, 1);
    CloseHandle(processx__global_job_handle);
    processx__global_job_handle = NULL;
  }
  return R_NilValue;
}

int processx__utf8_to_utf16_alloc(const char* s, WCHAR** ws_ptr) {
  int ws_len, r;
  WCHAR* ws;

  ws_len = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ s,
    /* cbMultiByte =    */ -1,
    /* lpWideCharStr =  */ NULL,
    /* cchWideChar =    */ 0);

  if (ws_len <= 0) { return GetLastError(); }

  ws = (WCHAR*) R_alloc(ws_len,  sizeof(WCHAR));
  if (ws == NULL) { return ERROR_OUTOFMEMORY; }

  r = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ s,
    /* cbMultiBytes =   */ -1,
    /* lpWideCharStr =  */ ws,
    /* cchWideChar =    */ ws_len);

  if (r != ws_len) {
    R_THROW_ERROR("processx error interpreting UTF8 command or arguments: '%s'", s);
  }

  *ws_ptr = ws;
  return 0;
}

WCHAR* processx__quote_cmd_arg(const WCHAR *source, WCHAR *target) {
  size_t len = wcslen(source);
  size_t i;
  int quote_hit;
  WCHAR* start;

  if (len == 0) {
    /* Need double quotation for empty argument */
    *(target++) = L'"';
    *(target++) = L'"';
    return target;
  }

  if (NULL == wcspbrk(source, L" \t\"")) {
    /* No quotation needed */
    wcsncpy(target, source, len);
    target += len;
    return target;
  }

  if (NULL == wcspbrk(source, L"\"\\")) {
    /*
     * No embedded double quotes or backlashes, so I can just wrap
     * quote marks around the whole thing.
     */
    *(target++) = L'"';
    wcsncpy(target, source, len);
    target += len;
    *(target++) = L'"';
    return target;
  }

  /*
   * Expected input/output:
   *   input : hello"world
   *   output: "hello\"world"
   *   input : hello""world
   *   output: "hello\"\"world"
   *   input : hello\world
   *   output: hello\world
   *   input : hello\\world
   *   output: hello\\world
   *   input : hello\"world
   *   output: "hello\\\"world"
   *   input : hello\\"world
   *   output: "hello\\\\\"world"
   *   input : hello world\
   *   output: "hello world\\"
   */

  *(target++) = L'"';
  start = target;
  quote_hit = 1;

  for (i = len; i > 0; --i) {
    *(target++) = source[i - 1];

    if (quote_hit && source[i - 1] == L'\\') {
      *(target++) = L'\\';
    } else if(source[i - 1] == L'"') {
      quote_hit = 1;
      *(target++) = L'\\';
    } else {
      quote_hit = 0;
    }
  }
  target[0] = L'\0';
  wcsrev(start);
  *(target++) = L'"';
  return target;
}

static int processx__make_program_args(SEXP args, int verbatim_arguments,
				       WCHAR **dst_ptr) {
  const char* arg;
  WCHAR* dst = NULL;
  WCHAR* temp_buffer = NULL;
  size_t dst_len = 0;
  size_t temp_buffer_len = 0;
  WCHAR* pos;
  int arg_count = LENGTH(args);
  int err = 0;
  int i;

  /* Count the required size. */
  for (i = 0; i < arg_count; i++) {
    DWORD arg_len;
    arg = CHAR(STRING_ELT(args, i));

    arg_len = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ arg,
    /* cbMultiBytes =   */ -1,
    /* lpWideCharStr =  */ NULL,
    /* cchWideChar =    */ 0);

    if (arg_len == 0) { return GetLastError(); }

    dst_len += arg_len;

    if (arg_len > temp_buffer_len) { temp_buffer_len = arg_len; }
  }

  /* Adjust for potential quotes. Also assume the worst-case scenario */
  /* that every character needs escaping, so we need twice as much space. */
  dst_len = dst_len * 2 + arg_count * 2;

  /* Allocate buffer for the final command line. */
  dst = (WCHAR*) R_alloc(dst_len, sizeof(WCHAR));

  /* Allocate temporary working buffer. */
  temp_buffer = (WCHAR*) R_alloc(temp_buffer_len, sizeof(WCHAR));

  pos = dst;
  for (i = 0; i < arg_count; i++) {
    DWORD arg_len;
    arg = CHAR(STRING_ELT(args, i));

    /* Convert argument to wide char. */
    arg_len = MultiByteToWideChar(
    /* CodePage =       */ CP_UTF8,
    /* dwFlags =        */ 0,
    /* lpMultiByteStr = */ arg,
    /* cbMultiBytes =   */ -1,
    /* lpWideCharStr =  */ temp_buffer,
    /* cchWideChar =    */ (int) (dst + dst_len - pos));

    if (arg_len == 0) {
      err = GetLastError();
      goto error;
    }

    if (verbatim_arguments) {
      /* Copy verbatim. */
      wcscpy(pos, temp_buffer);
      pos += arg_len - 1;
    } else {
      /* Quote/escape, if needed. */
      pos = processx__quote_cmd_arg(temp_buffer, pos);
    }

    *pos++ = i < arg_count - 1 ? L' ' : L'\0';
  }

  *dst_ptr = dst;
  return 0;

error:
  return err;
}

/*
 * The way windows takes environment variables is different than what C does;
 * Windows wants a contiguous block of null-terminated strings, terminated
 * with an additional null.
 *
 * Windows has a few "essential" environment variables. winsock will fail
 * to initialize if SYSTEMROOT is not defined; some APIs make reference to
 * TEMP. SYSTEMDRIVE is probably also important. We therefore ensure that
 * these get defined if the input environment block does not contain any
 * values for them.
 *
 * Also add variables known to Cygwin to be required for correct
 * subprocess operation in many cases:
 * https://github.com/Alexpux/Cygwin/blob/b266b04fbbd3a595f02ea149e4306d3ab9b1fe3d/winsup/cygwin/environ.cc#L955
 *
 */

typedef struct env_var {
  const WCHAR* const wide;
  const WCHAR* const wide_eq;
  const size_t len; /* including null or '=' */
} env_var_t;

#define E_V(str) { L##str, L##str L"=", sizeof(str) }

static const env_var_t required_vars[] = { /* keep me sorted */
  E_V("HOMEDRIVE"),
  E_V("HOMEPATH"),
  E_V("LOGONSERVER"),
  E_V("PATH"),
  E_V("SYSTEMDRIVE"),
  E_V("SYSTEMROOT"),
  E_V("TEMP"),
  E_V("USERDOMAIN"),
  E_V("USERNAME"),
  E_V("USERPROFILE"),
  E_V("WINDIR"),
};
static size_t n_required_vars = ARRAY_SIZE(required_vars);

int env_strncmp(const wchar_t* a, int na, const wchar_t* b) {
  wchar_t* a_eq;
  wchar_t* b_eq;
  wchar_t* A;
  wchar_t* B;
  int nb;
  int r;

  if (na < 0) {
    a_eq = wcschr(a, L'=');
    na = (int)(long)(a_eq - a);
  } else {
    na--;
  }
  b_eq = wcschr(b, L'=');
  nb = b_eq - b;

  A = alloca((na+1) * sizeof(wchar_t));
  B = alloca((nb+1) * sizeof(wchar_t));

  r = LCMapStringW(LOCALE_INVARIANT, LCMAP_UPPERCASE, a, na, A, na);
  if (!r) R_THROW_SYSTEM_ERROR("make environment for process");
  A[na] = L'\0';
  r = LCMapStringW(LOCALE_INVARIANT, LCMAP_UPPERCASE, b, nb, B, nb);
  if (!r) R_THROW_SYSTEM_ERROR("make environment for process");
  B[nb] = L'\0';

  while (1) {
    wchar_t AA = *A++;
    wchar_t BB = *B++;
    if (AA < BB) {
      return -1;
    } else if (AA > BB) {
      return 1;
    } else if (!AA && !BB) {
      return 0;
    }
  }
}

static int qsort_wcscmp(const void *a, const void *b) {
  wchar_t* astr = *(wchar_t* const*)a;
  wchar_t* bstr = *(wchar_t* const*)b;
  return env_strncmp(astr, -1, bstr);
}

static int processx__add_tree_id_env(const char *ctree_id, WCHAR **dst_ptr) {
  WCHAR *env = GetEnvironmentStringsW();
  int len = 0, len2 = 0;
  WCHAR *ptr = env;
  WCHAR *id = 0;
  int err;
  int idlen;
  WCHAR *dst_copy;

  if (!env) return GetLastError();

  err = processx__utf8_to_utf16_alloc(ctree_id, &id);
  if (err) {
    FreeEnvironmentStringsW(env);
    return(err);
  }

  while (1) {
    WCHAR *prev = ptr;
    if (!*ptr) break;
    while (*ptr) ptr++;
    ptr++;
    len += (ptr - prev);
  }

  /* Plus the id */
  idlen = wcslen(id) + 1;
  len2 = len + idlen;

  /* Allocate, copy */
  dst_copy = (WCHAR*) R_alloc(len2 + 1, sizeof(WCHAR)); /* +1 for final zero */
  memcpy(dst_copy, env, len * sizeof(WCHAR));
  memcpy(dst_copy + len, id, idlen * sizeof(WCHAR));

  /* Final \0 */
  *(dst_copy + len2) = L'\0';
  *dst_ptr = dst_copy;

  FreeEnvironmentStringsW(env);
  return 0;
}

static int processx__make_program_env(SEXP env_block, const char *tree_id,
                                      WCHAR** dst_ptr, const char *cname) {
  WCHAR* dst;
  WCHAR* ptr;
  size_t env_len = 0;
  int len;
  size_t i;
  DWORD var_size;
  size_t env_block_count = 1; /* 1 for null-terminator */
  WCHAR* dst_copy;
  WCHAR** ptr_copy;
  WCHAR** env_copy;
  DWORD* required_vars_value_len = alloca(n_required_vars * sizeof(DWORD*));
  int j, num = LENGTH(env_block);

  /* first pass: determine size in UTF-16 */
  for (j = 0; j < num; j++) {
    const char *env = CHAR(STRING_ELT(env_block, j));
    if (strchr(env, '=')) {
      len = MultiByteToWideChar(CP_UTF8,
                                0,
                                env,
                                -1,
                                NULL,
                                0);
      if (len <= 0) {
        return GetLastError();
      }
      env_len += len;
      env_block_count++;
    }
  }

  /* Plus the tree id */
  len = MultiByteToWideChar(CP_UTF8, 0, tree_id, -1, NULL, 0);
  if (len <= 0) return GetLastError();
  env_len += len;
  env_block_count++;

  /* second pass: copy to UTF-16 environment block */
  dst_copy = (WCHAR*) R_alloc(env_len, sizeof(WCHAR));
  env_copy = alloca(env_block_count * sizeof(WCHAR*));

  ptr = dst_copy;
  ptr_copy = env_copy;
  for (j = 0; j < num; j++) {
    const char *env = CHAR(STRING_ELT(env_block, j));
    if (strchr(env, '=')) {
      len = MultiByteToWideChar(CP_UTF8,
                                0,
                                env,
                                -1,
                                ptr,
                                (int) (env_len - (ptr - dst_copy)));
      if (len <= 0) {
        DWORD err = GetLastError();
        return err;
      }
      *ptr_copy++ = ptr;
      ptr += len;
    }
  }
  /* Plus the tree id */
  len = MultiByteToWideChar(CP_UTF8, 0, tree_id, -1, ptr,
			    (int) (env_len  - (ptr - dst_copy)));
  if (len <= 0) return GetLastError();
  *ptr_copy++ = ptr;
  ptr += len;

  *ptr_copy = NULL;

  /* sort our (UTF-16) copy */
  qsort(env_copy, env_block_count-1, sizeof(wchar_t*), qsort_wcscmp);

  /* third pass: check for required variables */
  for (ptr_copy = env_copy, i = 0; i < n_required_vars; ) {
    int cmp;
    if (!*ptr_copy) {
      cmp = -1;
    } else {
      cmp = env_strncmp(required_vars[i].wide_eq,
                       required_vars[i].len,
                        *ptr_copy);
    }
    if (cmp < 0) {
      /* missing required var */
      var_size = GetEnvironmentVariableW(required_vars[i].wide, NULL, 0);
      required_vars_value_len[i] = var_size;
      if (var_size != 0) {
        env_len += required_vars[i].len;
        env_len += var_size;
      }
      i++;
    } else {
      ptr_copy++;
      if (cmp == 0)
        i++;
    }
  }

  /* final pass: copy, in sort order, and inserting required variables */
  dst = (WCHAR*) R_alloc(1 + env_len, sizeof(WCHAR));

  for (ptr = dst, ptr_copy = env_copy, i = 0;
       *ptr_copy || i < n_required_vars;
       ptr += len) {
    int cmp;
    if (i >= n_required_vars) {
      cmp = 1;
    } else if (!*ptr_copy) {
      cmp = -1;
    } else {
      cmp = env_strncmp(required_vars[i].wide_eq,
                        required_vars[i].len,
                        *ptr_copy);
    }
    if (cmp < 0) {
      /* missing required var */
      len = required_vars_value_len[i];
      if (len) {
        wcscpy(ptr, required_vars[i].wide_eq);
        ptr += required_vars[i].len;
        var_size = GetEnvironmentVariableW(required_vars[i].wide,
                                           ptr,
                                           (int) (env_len - (ptr - dst)));
        if (var_size != len-1) { /* race condition? */
          R_THROW_SYSTEM_ERROR("GetEnvironmentVariableW for process '%s'",
                               cname);
        }
      }
      i++;
    } else {
      /* copy var from env_block */
      len = wcslen(*ptr_copy) + 1;
      wmemcpy(ptr, *ptr_copy, len);
      ptr_copy++;
      if (cmp == 0)
        i++;
    }
  }

  /* Terminate with an extra NULL. */
  *ptr = L'\0';

  *dst_ptr = dst;
  return 0;
}

static WCHAR* processx__search_path_join_test(const WCHAR* dir,
					      size_t dir_len,
					      const WCHAR* name,
					      size_t name_len,
					      const WCHAR* ext,
					      size_t ext_len,
					      const WCHAR* cwd,
					      size_t cwd_len) {
  WCHAR *result, *result_pos;
  DWORD attrs;
  if (dir_len > 2 && dir[0] == L'\\' && dir[1] == L'\\') {
    /* It's a UNC path so ignore cwd */
    cwd_len = 0;
  } else if (dir_len >= 1 && (dir[0] == L'/' || dir[0] == L'\\')) {
    /* It's a full path without drive letter, use cwd's drive letter only */
    cwd_len = 2;
  } else if (dir_len >= 2 && dir[1] == L':' &&
      (dir_len < 3 || (dir[2] != L'/' && dir[2] != L'\\'))) {
    /* It's a relative path with drive letter (ext.g. D:../some/file)
     * Replace drive letter in dir by full cwd if it points to the same drive,
     * otherwise use the dir only.
     */
    if (cwd_len < 2 || _wcsnicmp(cwd, dir, 2) != 0) {
      cwd_len = 0;
    } else {
      dir += 2;
      dir_len -= 2;
    }
  } else if (dir_len > 2 && dir[1] == L':') {
    /* It's an absolute path with drive letter
     * Don't use the cwd at all
     */
    cwd_len = 0;
  }

  /* Allocate buffer for output */
  result = result_pos = (WCHAR*) R_alloc(
    (cwd_len + 1 + dir_len + 1 + name_len + 1 + ext_len + 1),
    sizeof(WCHAR));

  /* Copy cwd */
  wcsncpy(result_pos, cwd, cwd_len);
  result_pos += cwd_len;

  /* Add a path separator if cwd didn't end with one */
  if (cwd_len && wcsrchr(L"\\/:", result_pos[-1]) == NULL) {
    result_pos[0] = L'\\';
    result_pos++;
  }

  /* Copy dir */
  wcsncpy(result_pos, dir, dir_len);
  result_pos += dir_len;

  /* Add a separator if the dir didn't end with one */
  if (dir_len && wcsrchr(L"\\/:", result_pos[-1]) == NULL) {
    result_pos[0] = L'\\';
    result_pos++;
  }

  /* Copy filename */
  wcsncpy(result_pos, name, name_len);
  result_pos += name_len;

  if (ext_len) {
    /* Add a dot if the filename didn't end with one */
    if (name_len && result_pos[-1] != '.') {
      result_pos[0] = L'.';
      result_pos++;
    }

    /* Copy extension */
    wcsncpy(result_pos, ext, ext_len);
    result_pos += ext_len;
  }

  /* Null terminator */
  result_pos[0] = L'\0';

  attrs = GetFileAttributesW(result);

  if (attrs != INVALID_FILE_ATTRIBUTES &&
      !(attrs & FILE_ATTRIBUTE_DIRECTORY)) {
    return result;
  }

  return NULL;
}


/*
 * Helper function for search_path
 */
static WCHAR* processx__path_search_walk_ext(const WCHAR *dir,
					     size_t dir_len,
					     const WCHAR *name,
					     size_t name_len,
					     WCHAR *cwd,
					     size_t cwd_len,
					     int name_has_ext) {
  WCHAR* result;

  /* If the name itself has a nonempty extension, try this extension first */
  if (name_has_ext) {
    result = processx__search_path_join_test(dir, dir_len,
					     name, name_len,
					     L"", 0,
					     cwd, cwd_len);
    if (result != NULL) {
      return result;
    }
  }

  /* Try .com extension */
  result = processx__search_path_join_test(dir, dir_len,
					   name, name_len,
					   L"com", 3,
					   cwd, cwd_len);
  if (result != NULL) {
    return result;
  }

  /* Try .exe extension */
  result = processx__search_path_join_test(dir, dir_len,
					   name, name_len,
					   L"exe", 3,
					   cwd, cwd_len);
  if (result != NULL) {
    return result;
  }

  return NULL;
}


/*
 * search_path searches the system path for an executable filename -
 * the windows API doesn't provide this as a standalone function nor as an
 * option to CreateProcess.
 *
 * It tries to return an absolute filename.
 *
 * Furthermore, it tries to follow the semantics that cmd.exe, with this
 * exception that PATHEXT environment variable isn't used. Since CreateProcess
 * can start only .com and .exe files, only those extensions are tried. This
 * behavior equals that of msvcrt's spawn functions.
 *
 * - Do not search the path if the filename already contains a path (either
 *   relative or absolute).
 *
 * - If there's really only a filename, check the current directory for file,
 *   then search all path directories.
 *
 * - If filename specified has *any* extension, search for the file with the
 *   specified extension first.
 *
 * - If the literal filename is not found in a directory, try *appending*
 *   (not replacing) .com first and then .exe.
 *
 * - The path variable may contain relative paths; relative paths are relative
 *   to the cwd.
 *
 * - Directories in path may or may not end with a trailing backslash.
 *
 * - CMD does not trim leading/trailing whitespace from path/pathex entries
 *   nor from the environment variables as a whole.
 *
 * - When cmd.exe cannot read a directory, it will just skip it and go on
 *   searching. However, unlike posix-y systems, it will happily try to run a
 *   file that is not readable/executable; if the spawn fails it will not
 *   continue searching.
 *
 * UNC path support: we are dealing with UNC paths in both the path and the
 * filename. This is a deviation from what cmd.exe does (it does not let you
 * start a program by specifying an UNC path on the command line) but this is
 * really a pointless restriction.
 *
 */
static WCHAR* processx__search_path(const WCHAR *file,
				    WCHAR *cwd,
				    const WCHAR *path) {
  int file_has_dir;
  WCHAR* result = NULL;
  WCHAR *file_name_start;
  WCHAR *dot;
  const WCHAR *dir_start, *dir_end, *dir_path;
  size_t dir_len;
  int name_has_ext;

  size_t file_len = wcslen(file);
  size_t cwd_len = wcslen(cwd);

  /* If the caller supplies an empty filename,
   * we're not gonna return c:\windows\.exe -- GFY!
   */
  if (file_len == 0
      || (file_len == 1 && file[0] == L'.')) {
    return NULL;
  }

  /* Find the start of the filename so we can split the directory from the */
  /* name. */
  for (file_name_start = (WCHAR*)file + file_len;
       file_name_start > file
           && file_name_start[-1] != L'\\'
           && file_name_start[-1] != L'/'
           && file_name_start[-1] != L':';
       file_name_start--);

  file_has_dir = file_name_start != file;

  /* Check if the filename includes an extension */
  dot = wcschr(file_name_start, L'.');
  name_has_ext = (dot != NULL && dot[1] != L'\0');

  if (file_has_dir) {
    /* The file has a path inside, don't use path */
    result = processx__path_search_walk_ext(
        file, file_name_start - file,
        file_name_start, file_len - (file_name_start - file),
        cwd, cwd_len,
        name_has_ext);

  } else {
    dir_end = path;

    /* The file is really only a name; look in cwd first, then scan path */
    result = processx__path_search_walk_ext(L"", 0,
					    file, file_len,
					    cwd, cwd_len,
					    name_has_ext);

    while (result == NULL) {
      if (*dir_end == L'\0') {
        break;
      }

      /* Skip the separator that dir_end now points to */
      if (dir_end != path || *path == L';') {
        dir_end++;
      }

      /* Next slice starts just after where the previous one ended */
      dir_start = dir_end;

      /* Slice until the next ; or \0 is found */
      dir_end = wcschr(dir_start, L';');
      if (dir_end == NULL) {
        dir_end = wcschr(dir_start, L'\0');
      }

      /* If the slice is zero-length, don't bother */
      if (dir_end - dir_start == 0) {
        continue;
      }

      dir_path = dir_start;
      dir_len = dir_end - dir_start;

      /* Adjust if the path is quoted. */
      if (dir_path[0] == '"' || dir_path[0] == '\'') {
        ++dir_path;
        --dir_len;
      }

      if (dir_path[dir_len - 1] == '"' || dir_path[dir_len - 1] == '\'') {
        --dir_len;
      }

      result = processx__path_search_walk_ext(dir_path, dir_len,
					      file, file_len,
					      cwd, cwd_len,
					      name_has_ext);
    }
  }

  return result;
}

void processx__collect_exit_status(SEXP status, DWORD exitcode);

DWORD processx__terminate(processx_handle_t *handle, SEXP status) {
  DWORD err;

  err = TerminateProcess(handle->hProcess, 2);
  if (err) processx__collect_exit_status(status, 2);

  WaitForSingleObject(handle->hProcess, INFINITE);
  CloseHandle(handle->hProcess);
  handle->hProcess = 0;
  return err;
}

void processx__finalizer(SEXP status) {
  processx_handle_t *handle = (processx_handle_t*) R_ExternalPtrAddr(status);

  if (!handle) return;

  if (handle->cleanup && !handle->collected) {
    /* Just in case it is running */
    processx__terminate(handle, status);
  }

  if (handle->hProcess) CloseHandle(handle->hProcess);
  handle->hProcess = NULL;
  R_ClearExternalPtr(status);
  processx__handle_destroy(handle);
}

SEXP processx__make_handle(SEXP private, int cleanup) {
  processx_handle_t * handle;
  SEXP result;

  handle = (processx_handle_t*) malloc(sizeof(processx_handle_t));
  if (!handle) { R_THROW_ERROR("Out of memory when creating subprocess"); }
  memset(handle, 0, sizeof(processx_handle_t));

  result = PROTECT(R_MakeExternalPtr(handle, private, R_NilValue));
  R_RegisterCFinalizerEx(result, processx__finalizer, 1);
  handle->cleanup = cleanup;

  UNPROTECT(1);
  return result;
}

void processx__handle_destroy(processx_handle_t *handle) {
  if (!handle) return;
  if (handle->child_stdio_buffer) free(handle->child_stdio_buffer);
  free(handle);
}

SEXP processx_exec(SEXP command, SEXP args, SEXP pty, SEXP pty_options,
		               SEXP connections, SEXP env, SEXP windows_verbatim_args,
                   SEXP windows_hide, SEXP windows_detached_process,
                   SEXP private, SEXP cleanup, SEXP wd, SEXP encoding,
                   SEXP tree_id) {

  const char *ccommand = CHAR(STRING_ELT(command, 0));
  const char *cencoding = CHAR(STRING_ELT(encoding, 0));
  const char *ccwd = isNull(wd) ? 0 : CHAR(STRING_ELT(wd, 0));
  const char *ctree_id = CHAR(STRING_ELT(tree_id, 0));

  int err = 0;
  WCHAR *path;
  WCHAR *application_path = NULL, *application = NULL, *arguments = NULL,
    *cenv = NULL, *cwd = NULL;
  processx_options_t options;
  STARTUPINFOW startup = { 0 };
  PROCESS_INFORMATION info = { 0 };
  DWORD process_flags;

  processx_handle_t *handle;
  int ccleanup = INTEGER(cleanup)[0];
  SEXP result;
  DWORD dwerr;

  options.windows_verbatim_args = LOGICAL(windows_verbatim_args)[0];
  options.windows_hide = LOGICAL(windows_hide)[0];

  err = processx__utf8_to_utf16_alloc(CHAR(STRING_ELT(command, 0)),
				      &application);
  if (err) {
    R_THROW_SYSTEM_ERROR_CODE(err, "utf8 -> utf16 conversion '%s'",
                              ccommand);
  }

  err = processx__make_program_args(
      args,
      options.windows_verbatim_args,
      &arguments);
  if (err) { R_THROW_SYSTEM_ERROR_CODE(err, "making program args '%s'", ccommand); }

  if (isNull(env)) {
    err = processx__add_tree_id_env(ctree_id, &cenv);
  } else {
    err = processx__make_program_env(env, ctree_id, &cenv, ccommand);
  }
  if (err) R_THROW_SYSTEM_ERROR_CODE(err, "making environment '%s'", ccommand);

  if (ccwd) {
    /* Explicit cwd */
    err = processx__utf8_to_utf16_alloc(ccwd, &cwd);
    if (err) {
      R_THROW_SYSTEM_ERROR("convert current directory encoding '%s'", ccommand);
    }

  } else {
    /* Inherit cwd */
    DWORD cwd_len, r;

    cwd_len = GetCurrentDirectoryW(0, NULL);
    if (!cwd_len) {
      R_THROW_SYSTEM_ERROR("get current directory length '%s'", ccommand);
    }

    cwd = (WCHAR*) R_alloc(cwd_len, sizeof(WCHAR));

    r = GetCurrentDirectoryW(cwd_len, cwd);
    if (r == 0 || r >= cwd_len) {
      R_THROW_SYSTEM_ERROR("get current directory '%s'", ccommand);
    }
  }

  /* Get PATH environment variable */
  {
    DWORD path_len, r;

    path_len = GetEnvironmentVariableW(L"PATH", NULL, 0);
    if (!path_len) {
      R_THROW_SYSTEM_ERROR("get env var length '%s'", ccommand);
    }

    path = (WCHAR*) R_alloc(path_len, sizeof(WCHAR));

    r = GetEnvironmentVariableW(L"PATH", path, path_len);
    if (r == 0 || r >= path_len) {
      R_THROW_SYSTEM_ERROR("get PATH env var for '%s'", ccommand);
    }
  }

  result = PROTECT(processx__make_handle(private, ccleanup));
  handle = R_ExternalPtrAddr(result);

  int inherit_std = 0;
  err = processx__stdio_create(handle, connections,
			       &handle->child_stdio_buffer, private,
			       cencoding, ccommand, &inherit_std);
  if (err) { R_THROW_SYSTEM_ERROR_CODE(err, "setup stdio for '%s'", ccommand); }

  application_path = processx__search_path(application, cwd, path);

  /* If a UNC Path, then we try to flip the forward slashes, if any.
   * It is apparently enough to flip the first two slashes, the rest
   * are not important. */
  if (! application_path && wcslen(path) >= 2 &&
    application[0] == L'/' && application[1] == L'/') {
    application[0] = L'\\';
    application[1] = L'\\';
    application_path = processx__search_path(application, cwd, path);
  }

  if (!application_path) {
    R_ClearExternalPtr(result);
    processx__stdio_destroy(handle->child_stdio_buffer);
    free(handle);
    R_THROW_ERROR("Command '%s' not found", ccommand);
  }

  startup.cb = sizeof(startup);
  startup.lpReserved = NULL;
  startup.lpDesktop = NULL;
  startup.lpTitle = NULL;
  startup.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;

  startup.cbReserved2 = processx__stdio_size(handle->child_stdio_buffer);
  startup.lpReserved2 = (BYTE*) handle->child_stdio_buffer;

  startup.hStdInput = processx__stdio_handle(handle->child_stdio_buffer, 0);
  startup.hStdOutput = processx__stdio_handle(handle->child_stdio_buffer, 1);
  startup.hStdError = processx__stdio_handle(handle->child_stdio_buffer, 2);
  startup.wShowWindow = options.windows_hide ? SW_HIDE : SW_SHOWDEFAULT;

  process_flags = CREATE_UNICODE_ENVIRONMENT | CREATE_SUSPENDED;

  /* We only use CREATE_NO_WINDOW if none of stdin, stdout and stderr
   * are inherited, because if there is no window, then inherited
   * handles do not work. Other inherited handles should be fine,
   * I think. See https://github.com/gaborcsardi/win32-console-docs
   * for more about CREATE_NO_WINDOW. */

  if (! inherit_std) process_flags |= CREATE_NO_WINDOW;

  if (!ccleanup) {
    /* Note that we're not setting the CREATE_BREAKAWAY_FROM_JOB flag. That
     * means that processx might not let you create a fully deamonized
     * process when run under job control. However the type of job control
     * that processx itself creates doesn't trickle down to subprocesses
     * so they can still daemonize.
     *
     * A reason to not do this is that CREATE_BREAKAWAY_FROM_JOB makes the
     * CreateProcess call fail if we're under job control that doesn't
     * allow breakaway.
     */

    process_flags |= CREATE_NEW_PROCESS_GROUP;
  }

  if (LOGICAL(windows_detached_process)[0]) {
    process_flags |= DETACHED_PROCESS;
  }

  err = CreateProcessW(
    /* lpApplicationName =    */ application_path,
    /* lpCommandLine =        */ arguments,
    /* lpProcessAttributes =  */ NULL,
    /* lpThreadAttributes =   */ NULL,
    /* bInheritHandles =      */ 1,
    /* dwCreationFlags =      */ process_flags,
    /* lpEnvironment =        */ cenv,
    /* lpCurrentDirectory =   */ cwd,
    /* lpStartupInfo =        */ &startup,
    /* lpProcessInformation = */ &info);

  if (!err) { R_THROW_SYSTEM_ERROR("create process '%s'", ccommand); }

  handle->hProcess = info.hProcess;
  handle->dwProcessId = info.dwProcessId;

  /* Query official creation time. On Windows this is not used as
     an id, since the pid itself is valid until the process handle
     is released. */
  handle->create_time = processx__create_time(handle->hProcess);

  /* If the process isn't spawned as detached, assign to the global job */
  /* object so windows will kill it when the parent process dies. */
  if (ccleanup) {
    if (! processx__global_job_handle) processx__init_global_job_handle();

    if (!AssignProcessToJobObject(processx__global_job_handle, info.hProcess)) {
      /* AssignProcessToJobObject might fail if this process is under job
       * control and the job doesn't have the
       * JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK flag set, on a Windows
       * version that doesn't support nested jobs.
       *
       * When that happens we just swallow the error and continue without
       * establishing a kill-child-on-parent-exit relationship, otherwise
       * there would be no way for R/processx applications run under job
       * control to spawn processes at all.
       */
      DWORD err = GetLastError();
      if (err != ERROR_ACCESS_DENIED) {
	R_THROW_SYSTEM_ERROR_CODE(err, "Assign to job object '%s'",
                                  ccommand);
      }
    }
  }

  dwerr = ResumeThread(info.hThread);
  if (dwerr == (DWORD) -1) {
    R_THROW_SYSTEM_ERROR("resume thread for '%s'", ccommand);
  }
  CloseHandle(info.hThread);

  processx__stdio_destroy(handle->child_stdio_buffer);
  handle->child_stdio_buffer = NULL;

  UNPROTECT(1);
  return result;
}

void processx__collect_exit_status(SEXP status, DWORD exitcode) {
  processx_handle_t *handle = R_ExternalPtrAddr(status);
  handle->exitcode = exitcode;
  handle->collected = 1;
}

SEXP processx_wait(SEXP status, SEXP timeout, SEXP name) {
  int ctimeout = INTEGER(timeout)[0], timeleft = ctimeout;
  const char *cname = isNull(name) ? "???" : CHAR(STRING_ELT(name, 0));
  processx_handle_t *handle = R_ExternalPtrAddr(status);
  DWORD err, err2, exitcode;

  if (!handle) return ScalarLogical(1);
  if (handle->collected) return ScalarLogical(1);

  err2 = WAIT_TIMEOUT;
  while (ctimeout < 0 || timeleft > PROCESSX_INTERRUPT_INTERVAL) {
    err2 = WaitForSingleObject(handle->hProcess, PROCESSX_INTERRUPT_INTERVAL);
    if (err2 != WAIT_TIMEOUT) break;
    R_CheckUserInterrupt();
    timeleft -= PROCESSX_INTERRUPT_INTERVAL;
  }

  /* Maybe there is some time left from the timeout */
  if (err2 == WAIT_TIMEOUT && timeleft >= 0) {
    err2 = WaitForSingleObject(handle->hProcess, timeleft);
  }

  if (err2 == WAIT_FAILED) {
    R_THROW_SYSTEM_ERROR("failed to wait on process '%s'", cname);
  } else if (err2 == WAIT_TIMEOUT) {
    return ScalarLogical(FALSE);
  }

  /* Collect  */
  err = GetExitCodeProcess(handle->hProcess, &exitcode);
  if (!err) {
    R_THROW_SYSTEM_ERROR("cannot get exit code after wait for '%s'", cname);
  }

  processx__collect_exit_status(status, exitcode);

  return ScalarLogical(TRUE);
}

SEXP processx_is_alive(SEXP status, SEXP name) {
  processx_handle_t *handle = R_ExternalPtrAddr(status);
  const char *cname = isNull(name) ? "???" : CHAR(STRING_ELT(name, 0));
  DWORD err, exitcode;

  /* This might happen if it was finalized at the end of the session,
     even though there are some references to the R object. */
  if (!handle) return ScalarLogical(0);

  if (handle->collected) return ScalarLogical(0);

  /* Otherwise try to get exit code */
  err = GetExitCodeProcess(handle->hProcess, &exitcode);
  if (!err) {
    R_THROW_SYSTEM_ERROR("failed to get exit code to check if ",
                         "'%s' is alive", cname);
  }

  if (exitcode == STILL_ACTIVE) {
    return ScalarLogical(1);
  } else {
    processx__collect_exit_status(status, exitcode);
    return ScalarLogical(0);
  }
}

SEXP processx_get_exit_status(SEXP status, SEXP name) {
  processx_handle_t *handle = R_ExternalPtrAddr(status);
  const char *cname = isNull(name) ? "???" : CHAR(STRING_ELT(name, 0));
  DWORD err, exitcode;

  /* This might happen if it was finalized at the end of the session,
     even though there are some references to the R object. */
  if (!handle) return R_NilValue;

  if (handle->collected) return ScalarInteger(handle->exitcode);

  /* Otherwise try to get exit code */
  err = GetExitCodeProcess(handle->hProcess, &exitcode);
  if (!err) {R_THROW_SYSTEM_ERROR("get exit status failed for '%s'", cname); }

  if (exitcode == STILL_ACTIVE) {
    return R_NilValue;
  } else {
    processx__collect_exit_status(status, exitcode);
    return ScalarInteger(handle->exitcode);
  }
}

SEXP processx_signal(SEXP status, SEXP signal, SEXP name) {
  processx_handle_t *handle = R_ExternalPtrAddr(status);
  const char *cname = isNull(name) ? "???" : CHAR(STRING_ELT(name, 0));
  DWORD err, exitcode = STILL_ACTIVE;

  if (!handle) return ScalarLogical(0);
  if (handle->collected) return ScalarLogical(0);

  switch (INTEGER(signal)[0]) {

  case 15:   /* SIGTERM */
  case 9:    /* SIGKILL */
  case 2: {  /* SIGINT */
    /* Call GetExitCodeProcess to see if it is done */
    /* TODO: there is a race condition here, might finish right before
       we are terminating it... */
    err = GetExitCodeProcess(handle->hProcess, &exitcode);
    if (!err) {
      R_THROW_SYSTEM_ERROR("get exit code after signal for '%s'", cname);
    }

    if (exitcode == STILL_ACTIVE) {
      err = processx__terminate(handle, status);
      return ScalarLogical(err != 0);

    } else {
      processx__collect_exit_status(status, exitcode);
      return ScalarLogical(0);
    }
  }

  case 0: {
    /* Health check: is the process still alive? */
    err = GetExitCodeProcess(handle->hProcess, &exitcode);
    if (!err) {
      R_THROW_SYSTEM_ERROR("get exit code for signal 0 for '%s'", cname);
    }

    if (exitcode == STILL_ACTIVE) {
      return ScalarLogical(1);
    } else {
      return ScalarLogical(0);
    }
  }

  default:
    R_THROW_ERROR("Unsupported signal on this platform for '%s'", cname);
    return R_NilValue;
  }
}

SEXP processx_interrupt(SEXP status, SEXP name) {
  R_THROW_ERROR("Internal processx error, `processx_interrupt()` should not be called");
  return R_NilValue;
}

SEXP processx_kill(SEXP status, SEXP grace, SEXP name) {
  return processx_signal(status, ScalarInteger(9), name);
}

SEXP processx_get_pid(SEXP status) {
  processx_handle_t *handle = R_ExternalPtrAddr(status);

  /* This might happen if it was finalized at the end of the session,
     even though there are some references to the R object. */
  if (!handle) return ScalarInteger(NA_INTEGER);

  return ScalarInteger(handle->dwProcessId);
}

SEXP processx__process_exists(SEXP pid) {
  DWORD cpid = INTEGER(pid)[0];
  HANDLE proc = OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, cpid);
  if (proc == NULL) {
    DWORD err = GetLastError();
    if (err == ERROR_INVALID_PARAMETER) return ScalarLogical(0);
    R_THROW_SYSTEM_ERROR_CODE(err, "open process '%d' to check if it exists", cpid);
    return R_NilValue;
  } else {
    /* Maybe just finished, and in that case we still have a valid handle.
       Let's see if this is the case. */
    DWORD exitcode;
    DWORD err = GetExitCodeProcess(proc, &exitcode);
    CloseHandle(proc);
    if (!err) {
      R_THROW_SYSTEM_ERROR("get exit code to check if process '%d' exists", cpid);
    }
    return ScalarLogical(exitcode == STILL_ACTIVE);
  }
}
