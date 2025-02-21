
/* Avoid warning about empty compilation unit. */
void keyring_wincred_dummy(void) { }

#ifdef _WIN32

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <windows.h>
#include <wincred.h>

#include <string.h>

void keyring_wincred_handle_status(const char *func, BOOL status) {
  if (status == FALSE) {
    DWORD errorcode = GetLastError();
    LPVOID lpMsgBuf;
    char *msg;

    FormatMessage(
        FORMAT_MESSAGE_ALLOCATE_BUFFER |
        FORMAT_MESSAGE_FROM_SYSTEM |
        FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL,
        errorcode,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        (LPTSTR) &lpMsgBuf,
        0, NULL );

    msg = R_alloc(1, strlen(lpMsgBuf) + 1);
    strcpy(msg, lpMsgBuf);
    LocalFree(lpMsgBuf);

    error("Windows credential store error in '%s': %s", func, msg);
  }
}

SEXP keyring_wincred_get(SEXP target) {

  const char *ctarget = CHAR(STRING_ELT(target, 0));
  CREDENTIAL *cred;
  BOOL status = CredRead(ctarget, CRED_TYPE_GENERIC, /* Flags = */ 0, &cred);
  keyring_wincred_handle_status("get", status);

  SEXP result = PROTECT(allocVector(RAWSXP, cred->CredentialBlobSize));
  memcpy(RAW(result), cred->CredentialBlob, cred->CredentialBlobSize);

  CredFree(cred);
  UNPROTECT(1);
  return result;
}

SEXP keyring_wincred_exists(SEXP target) {

  const char *ctarget = CHAR(STRING_ELT(target, 0));
  CREDENTIAL *cred = NULL;
  BOOL status = CredRead(ctarget, CRED_TYPE_GENERIC, /* Flags = */ 0, &cred);
  DWORD errorcode = status ? 0 : GetLastError();
  if (errorcode != 0 && errorcode != ERROR_NOT_FOUND) {
    keyring_wincred_handle_status("exists", status);
  }

  if (cred) CredFree(cred);
  return ScalarLogical(errorcode == 0);
}

SEXP keyring_wincred_set(SEXP target, SEXP password, SEXP username,
			 SEXP session) {

  const char *ctarget = CHAR(STRING_ELT(target, 0));
  const char *cusername =
    isNull(username) ? NULL : CHAR(STRING_ELT(username, 0));
  int csession = LOGICAL(session)[0];

  CREDENTIAL cred = { 0 };
  BOOL status;

  cred.Type = CRED_TYPE_GENERIC;
  cred.TargetName = (char*) ctarget;
  cred.CredentialBlobSize = LENGTH(password);
  cred.CredentialBlob = (LPBYTE) RAW(password);
  cred.Persist = csession ? CRED_PERSIST_SESSION : CRED_PERSIST_LOCAL_MACHINE;
  cred.UserName = (char*) cusername;

  status = CredWrite(&cred, /* Flags = */ 0);

  keyring_wincred_handle_status("set", status);

  return R_NilValue;
}

SEXP keyring_wincred_delete(SEXP target) {

  const char* ctarget = CHAR(STRING_ELT(target, 0));
  BOOL status = CredDelete(ctarget, CRED_TYPE_GENERIC, /* Flags = */ 0);
  keyring_wincred_handle_status("delete", status);

  return R_NilValue;
}

SEXP keyring_wincred_enumerate(SEXP filter) {
  const char *cfilter = CHAR(STRING_ELT(filter, 0));

  DWORD count;
  PCREDENTIAL *creds = NULL;

  BOOL status = CredEnumerate(cfilter, /* Flags = */ 0, &count, &creds);
  DWORD errorcode = status ? 0 : GetLastError();

  /* If there are no keys, then an error is thrown. But for us this is
     a normal result, and we just return an empty table. */
  if (status == FALSE && errorcode == ERROR_NOT_FOUND) {
    SEXP result = PROTECT(allocVector(STRSXP, 0));
    UNPROTECT(1);
    return result;

  } else if (status == FALSE) {
    if (creds != NULL) CredFree(creds);
    keyring_wincred_handle_status("list", status);
    return R_NilValue;

  } else {
    size_t i, num = (size_t) count;
    SEXP result = PROTECT(allocVector(STRSXP, num));
    for (i = 0; i < count; i++) {
      SET_STRING_ELT(result, i, mkChar(creds[i]->TargetName));
    }
    CredFree(creds);

    UNPROTECT(1);
    return result;
  }
}

#else

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP keyring_wincred_get(SEXP target) {
  error("only works on Windows");
  return R_NilValue;
}

SEXP keyring_wincred_exists(SEXP target) {
  error("only works on Windows");
  return R_NilValue;
}

SEXP keyring_wincred_set(SEXP target, SEXP password, SEXP username,
                         SEXP session) {
  error("only works on Windows");
  return R_NilValue;
}

SEXP keyring_wincred_delete(SEXP target) {
  error("only works on Windows");
  return R_NilValue;
}

SEXP keyring_wincred_enumerate(SEXP filter) {
  error("only works on Windows");
  return R_NilValue;
}

#endif // _WIN32
