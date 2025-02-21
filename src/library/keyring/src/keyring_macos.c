
#ifdef __APPLE__

#include <CoreFoundation/CoreFoundation.h>
#include <Security/Security.h>

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

#include <sys/param.h>
#include <string.h>

void keyring_macos_error(const char *func, OSStatus status) {
  CFStringRef str = SecCopyErrorMessageString(status, NULL);
  CFIndex length = CFStringGetLength(str);
  CFIndex maxSize =
    CFStringGetMaximumSizeForEncoding(length, kCFStringEncodingUTF8) + 1;
  char *buffer = R_alloc(maxSize, 1);

  if (CFStringGetCString(str, buffer, maxSize, kCFStringEncodingUTF8)) {
    error("keyring error (macOS Keychain), %s: %s", func, buffer);

  } else {
    error("keyring error (macOS Keychain), %s", func);
  }
}

void keyring_macos_handle_status(const char *func, OSStatus status) {
  if (status != errSecSuccess) keyring_macos_error(func, status);
}

SecKeychainRef keyring_macos_open_keychain(const char *pathName) {
  SecKeychainRef keychain;
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  OSStatus status = SecKeychainOpen(pathName, &keychain);
# pragma GCC diagnostic pop
  keyring_macos_handle_status("cannot open keychain", status);

  /* We need to query the status, because SecKeychainOpen succeeds,
     even if the keychain file does not exists. (!) */
  SecKeychainStatus keychainStatus = 0;
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  status = SecKeychainGetStatus(keychain, &keychainStatus);
# pragma GCC diagnostic pop
  keyring_macos_handle_status("cannot open keychain", status);

  return keychain;
}

SEXP keyring_macos_get(SEXP keyring, SEXP service, SEXP username) {

  const char* empty = "";
  const char* cservice = CHAR(STRING_ELT(service, 0));
  const char* cusername =
    isNull(username) ? empty :CHAR(STRING_ELT(username, 0));

  void *data;
  UInt32 length;
  SEXP result;

  SecKeychainRef keychain =
    isNull(keyring) ? NULL :
    keyring_macos_open_keychain(CHAR(STRING_ELT(keyring, 0)));

# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  OSStatus status = SecKeychainFindGenericPassword(
    keychain,
    (UInt32) strlen(cservice), cservice,
    (UInt32) strlen(cusername), cusername,
    &length, &data,
    /* itemRef = */ NULL);
# pragma GCC diagnostic pop

  if (keychain != NULL) CFRelease(keychain);

  keyring_macos_handle_status("cannot get password", status);

  result = PROTECT(allocVector(RAWSXP, length));
  memcpy(RAW(result), data, length);
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  SecKeychainItemFreeContent(NULL, data);
# pragma GCC diagnostic pop

  UNPROTECT(1);
  return result;
}

SEXP keyring_macos_set(SEXP keyring, SEXP service, SEXP username,
		       SEXP password) {

  const char* empty = "";
  const char* cservice = CHAR(STRING_ELT(service, 0));
  const char* cusername =
    isNull(username) ? empty : CHAR(STRING_ELT(username, 0));
  SecKeychainItemRef item;

  SecKeychainRef keychain =
    isNull(keyring) ? NULL :
    keyring_macos_open_keychain(CHAR(STRING_ELT(keyring, 0)));

  /* Try to find it, and it is exists, update it */

# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  OSStatus status = SecKeychainFindGenericPassword(
    keychain,
    (UInt32) strlen(cservice), cservice,
    (UInt32) strlen(cusername), cusername,
    /* passwordLength = */ NULL, /* passwordData = */ NULL,
    &item);
# pragma GCC diagnostic pop

  if (status == errSecItemNotFound) {
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
    status = SecKeychainAddGenericPassword(
      keychain,
      (UInt32) strlen(cservice), cservice,
      (UInt32) strlen(cusername), cusername,
      (UInt32) LENGTH(password), RAW(password),
      /* itemRef = */ NULL);
# pragma GCC diagnostic pop

  } else {
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
    status = SecKeychainItemModifyAttributesAndData(
      item,
      /* attrList= */ NULL,
      (UInt32) LENGTH(password), RAW(password));
# pragma GCC diagnostic pop
    CFRelease(item);
  }

  if (keychain != NULL) CFRelease(keychain);

  keyring_macos_handle_status("cannot set password", status);

  return R_NilValue;
}

SEXP keyring_macos_delete(SEXP keyring, SEXP service, SEXP username) {

  const char* empty = "";
  const char* cservice = CHAR(STRING_ELT(service, 0));
  const char* cusername =
    isNull(username) ? empty : CHAR(STRING_ELT(username, 0));

  SecKeychainRef keychain =
    isNull(keyring) ? NULL : keyring_macos_open_keychain(CHAR(STRING_ELT(keyring, 0)));
  SecKeychainItemRef item;

# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  OSStatus status = SecKeychainFindGenericPassword(
    keychain,
    (UInt32) strlen(cservice), cservice,
    (UInt32) strlen(cusername), cusername,
    /* *passwordLength = */ NULL, /* *passwordData = */ NULL,
    &item);
# pragma GCC diagnostic pop

  if (status != errSecSuccess) {
    if (keychain != NULL) CFRelease(keychain);
    keyring_macos_error("cannot delete password", status);
  }

# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  status = SecKeychainItemDelete(item);
# pragma GCC diagnostic pop
  if (status != errSecSuccess) {
    if (keychain != NULL) CFRelease(keychain);
    keyring_macos_error("cannot delete password", status);
  }

  if (keychain != NULL) CFRelease(keychain);
  CFRelease(item);

  return R_NilValue;
}

static void keyring_macos_list_item(SecKeychainItemRef item, SEXP result,
				    int idx) {
  SecItemClass class;
  SecKeychainAttribute attrs[] = {
    { kSecServiceItemAttr },
    { kSecAccountItemAttr }
  };
  SecKeychainAttributeList attrList = { 2, attrs };

  /* This should not happen, not a keychain... */
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  if (SecKeychainItemGetTypeID() != CFGetTypeID(item)) {
    SET_STRING_ELT(VECTOR_ELT(result, 0), idx, mkChar(""));
    SET_STRING_ELT(VECTOR_ELT(result, 1), idx, mkChar(""));
    return;
  }
# pragma GCC diagnostic pop

# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  OSStatus status = SecKeychainItemCopyContent(item, &class, &attrList,
					       /* length = */ NULL,
					       /* outData = */ NULL);
# pragma GCC diagnostic pop
  keyring_macos_handle_status("cannot list passwords", status);
  SET_STRING_ELT(VECTOR_ELT(result, 0), idx,
		 mkCharLen(attrs[0].data, attrs[0].length));
  SET_STRING_ELT(VECTOR_ELT(result, 1), idx,
		 mkCharLen(attrs[1].data, attrs[1].length));
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  SecKeychainItemFreeContent(&attrList, NULL);
# pragma GCC diagnostic pop
}

CFArrayRef keyring_macos_list_get(const char *ckeyring,
				  const char *cservice) {

  CFStringRef cfservice = NULL;

  CFMutableDictionaryRef query = CFDictionaryCreateMutable(
    kCFAllocatorDefault, 0,
    &kCFTypeDictionaryKeyCallBacks,
    &kCFTypeDictionaryValueCallBacks);

  CFDictionarySetValue(query, kSecMatchLimit, kSecMatchLimitAll);
  CFDictionarySetValue(query, kSecReturnData, kCFBooleanFalse);
  CFDictionarySetValue(query, kSecReturnRef, kCFBooleanTrue);
  CFDictionarySetValue(query, kSecClass, kSecClassGenericPassword);

  CFArrayRef searchList = NULL;
  if (ckeyring) {
    SecKeychainRef keychain = keyring_macos_open_keychain(ckeyring);
    searchList = CFArrayCreate(NULL, (const void **) &keychain, 1,
			       &kCFTypeArrayCallBacks);
    CFDictionaryAddValue(query, kSecMatchSearchList, searchList);
  }

  if (cservice) {
    cfservice = CFStringCreateWithBytes(
      /* alloc = */ NULL,
      (const UInt8*) cservice, strlen(cservice),
      kCFStringEncodingUTF8,
      /* isExternalRepresentation = */ 0);
    CFDictionaryAddValue(query, kSecAttrService, cfservice);
  }

  CFArrayRef resArray = NULL;
  OSStatus status = SecItemCopyMatching(query, (CFTypeRef*) &resArray);
  CFRelease(query);
  if (cfservice != NULL) CFRelease(cfservice);
  if (searchList != NULL) CFRelease(searchList);

  /* If there are no elements in the keychain, then SecItemCopyMatching
     returns with an error, so we need work around that and return an
     empty list instead. */

  if (status == errSecItemNotFound) {
    resArray = CFArrayCreate(NULL, NULL, 0, NULL);
    return resArray;

  } else if (status != errSecSuccess) {
    if (resArray != NULL) CFRelease(resArray);
    keyring_macos_handle_status("cannot list passwords", status);
    return NULL;

  } else {
    return resArray;
  }
}

SEXP keyring_macos_list(SEXP keyring, SEXP service) {

  const char *ckeyring =
    isNull(keyring) ? NULL : CHAR(STRING_ELT(keyring, 0));
  const char *cservice =
    isNull(service) ? NULL : CHAR(STRING_ELT(service, 0));

  CFArrayRef resArray = keyring_macos_list_get(ckeyring, cservice);
  CFIndex i, num = CFArrayGetCount(resArray);
  SEXP result;
  PROTECT(result = allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, allocVector(STRSXP, num));
  SET_VECTOR_ELT(result, 1, allocVector(STRSXP, num));
  for (i = 0; i < num; i++) {
    SecKeychainItemRef item =
      (SecKeychainItemRef) CFArrayGetValueAtIndex(resArray, i);
    keyring_macos_list_item(item, result, (int) i);
  }

  CFRelease(resArray);
  UNPROTECT(1);
  return result;
}

SEXP keyring_macos_create(SEXP keyring, SEXP password) {
  const char *ckeyring = CHAR(STRING_ELT(keyring, 0));
  const char *cpassword = CHAR(STRING_ELT(password, 0));

  SecKeychainRef result = NULL;

# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  OSStatus status = SecKeychainCreate(
# pragma GCC diagnostic pop
    ckeyring,
    /* passwordLength = */ (UInt32) strlen(cpassword),
    (const void*) cpassword,
    /* promptUser = */ 0, /* initialAccess = */ NULL,
    &result);

  keyring_macos_handle_status("cannot create keychain", status);

  CFArrayRef keyrings = NULL;
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  status = SecKeychainCopyDomainSearchList(
    kSecPreferencesDomainUser,
    &keyrings);
# pragma GCC diagnostic pop

  if (status) {
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
    SecKeychainDelete(result);
# pragma GCC diagnostic pop
    if (result != NULL) CFRelease(result);
    keyring_macos_handle_status("cannot create keychain", status);
  }

  /* We need to add the new keychain to the keychain search list,
     otherwise applications like Keychain Access will not see it.
     There is no API to append it, we need to query the current
     search list, add it, and then set the whole new search list.
     This is of course a race condition. :/ */

  CFIndex count = CFArrayGetCount(keyrings);
  CFMutableArrayRef newkeyrings =
    CFArrayCreateMutableCopy(NULL, count + 1, keyrings);
  CFArrayAppendValue(newkeyrings, result);
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  status = SecKeychainSetDomainSearchList(
    kSecPreferencesDomainUser,
    newkeyrings);
# pragma GCC diagnostic pop

  if (status) {
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
    SecKeychainDelete(result);
# pragma GCC diagnostic pop
    if (result) CFRelease(result);
    if (keyrings) CFRelease(keyrings);
    if (newkeyrings) CFRelease(newkeyrings);
    keyring_macos_handle_status("cannot create keychain", status);
  }

  CFRelease(result);
  CFRelease(keyrings);
  CFRelease(newkeyrings);

  return R_NilValue;
}

SEXP keyring_macos_list_keyring(void) {
  CFArrayRef keyrings = NULL;
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  OSStatus status =
    SecKeychainCopyDomainSearchList(kSecPreferencesDomainUser, &keyrings);
# pragma GCC diagnostic pop
  keyring_macos_handle_status("cannot list keyrings", status);

  CFIndex i, num = CFArrayGetCount(keyrings);

  SEXP result = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(result, 0, allocVector(STRSXP, num));
  SET_VECTOR_ELT(result, 1, allocVector(INTSXP, num));
  SET_VECTOR_ELT(result, 2, allocVector(LGLSXP, num));

  for (i = 0; i < num; i++) {
    SecKeychainRef keychain =
      (SecKeychainRef) CFArrayGetValueAtIndex(keyrings, i);
    UInt32 pathLength = MAXPATHLEN;
    char pathName[MAXPATHLEN + 1];
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
    status = SecKeychainGetPath(keychain, &pathLength, pathName);
# pragma GCC diagnostic pop
    pathName[pathLength] = '\0';
    if (status) {
      CFRelease(keyrings);
      keyring_macos_handle_status("cannot list keyrings", status);
    }
    SET_STRING_ELT(VECTOR_ELT(result, 0), i, mkCharLen(pathName, pathLength));

    CFArrayRef resArray =
      keyring_macos_list_get(pathName, /* cservice = */ NULL);
    CFIndex numitems = CFArrayGetCount(resArray);
    CFRelease(resArray);
    INTEGER(VECTOR_ELT(result, 1))[i] = (int) numitems;

    SecKeychainStatus kstatus;
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
    status = SecKeychainGetStatus(keychain, &kstatus);
# pragma GCC diagnostic pop
    if (status) {
      LOGICAL(VECTOR_ELT(result, 2))[i] = NA_LOGICAL;
    } else {
      LOGICAL(VECTOR_ELT(result, 2))[i] =
	! (kstatus & kSecUnlockStateStatus);
    }
  }

  CFRelease(keyrings);

  UNPROTECT(1);
  return result;
}

SEXP keyring_macos_delete_keyring(SEXP keyring) {

  const char *ckeyring = CHAR(STRING_ELT(keyring, 0));

  /* Need to remove it from the search list as well */

  CFArrayRef keyrings = NULL;
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  OSStatus status = SecKeychainCopyDomainSearchList(
    kSecPreferencesDomainUser,
    &keyrings);
# pragma GCC diagnostic pop
  keyring_macos_handle_status("cannot delete keyring", status);

  CFIndex i, count = CFArrayGetCount(keyrings);
  CFMutableArrayRef newkeyrings =
    CFArrayCreateMutableCopy(NULL, count, keyrings);
  for (i = 0; i < count; i++) {
    SecKeychainRef item =
      (SecKeychainRef) CFArrayGetValueAtIndex(keyrings, i);
    UInt32 pathLength = MAXPATHLEN;
    char pathName[MAXPATHLEN + 1];
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
    status = SecKeychainGetPath(item, &pathLength, pathName);
# pragma GCC diagnostic pop
    pathName[pathLength] = '\0';
    if (status) {
      CFRelease(keyrings);
      CFRelease(newkeyrings);
      keyring_macos_handle_status("cannot delete keyring", status);
    }
    if (!strcmp(pathName, ckeyring)) {
      CFArrayRemoveValueAtIndex(newkeyrings, (CFIndex) i);
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
      status = SecKeychainSetDomainSearchList(
        kSecPreferencesDomainUser,
	newkeyrings);
# pragma GCC diagnostic pop
      if (status) {
	CFRelease(keyrings);
	CFRelease(newkeyrings);
	keyring_macos_handle_status("cannot delete keyring", status);
      }
    }
  }

  /* If we haven't found it on the search list,
     then we just keep silent about it ... */

  CFRelease(keyrings);
  CFRelease(newkeyrings);

  /* And now remove the file as well... */
  SecKeychainRef keychain = keyring_macos_open_keychain(ckeyring);
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  status = SecKeychainDelete(keychain);
# pragma GCC diagnostic pop
  CFRelease(keychain);
  keyring_macos_handle_status("cannot delete keyring", status);

  return R_NilValue;
}

SEXP keyring_macos_lock_keyring(SEXP keyring) {
  SecKeychainRef keychain =
    isNull(keyring) ? NULL :
    keyring_macos_open_keychain(CHAR(STRING_ELT(keyring, 0)));
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  OSStatus status = SecKeychainLock(keychain);
# pragma GCC diagnostic pop
  if (keychain) CFRelease(keychain);
  keyring_macos_handle_status("cannot lock keychain", status);
  return R_NilValue;
}

SEXP keyring_macos_unlock_keyring(SEXP keyring, SEXP password) {
  const char *cpassword = CHAR(STRING_ELT(password, 0));
  SecKeychainRef keychain =
    isNull(keyring) ? NULL :
    keyring_macos_open_keychain(CHAR(STRING_ELT(keyring, 0)));
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  OSStatus status = SecKeychainUnlock(
    keychain,
    (UInt32) strlen(cpassword),
     (const void*) cpassword,
    /* usePassword = */ TRUE);
# pragma GCC diagnostic pop

  if (keychain) CFRelease(keychain);
  keyring_macos_handle_status("cannot unlock keychain", status);
  return R_NilValue;
}

SEXP keyring_macos_is_locked_keyring(SEXP keyring) {
  SecKeychainRef keychain =
    isNull(keyring) ? NULL :
    keyring_macos_open_keychain(CHAR(STRING_ELT(keyring, 0)));

  SecKeychainStatus kstatus;
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wdeprecated-declarations"
  OSStatus status = SecKeychainGetStatus(keychain, &kstatus);
# pragma GCC diagnostic pop
  if (status) keyring_macos_error("cannot get lock information", status);

  return ScalarLogical(! (kstatus & kSecUnlockStateStatus));
}

#else

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

SEXP keyring_macos_get(SEXP keyring, SEXP service, SEXP username) {
  error("only works on macOS");
  return R_NilValue;
}

SEXP keyring_macos_set(SEXP keyring, SEXP service, SEXP username,
                       SEXP password) {
  error("only works on macOS");
  return R_NilValue;
}

SEXP keyring_macos_delete(SEXP keyring, SEXP service, SEXP username) {
  error("only works on macOS");
  return R_NilValue;
}

SEXP keyring_macos_list(SEXP keyring, SEXP service) {
  error("only works on macOS");
  return R_NilValue;
}

SEXP keyring_macos_create(SEXP keyring, SEXP password) {
  error("only works on macOS");
  return R_NilValue;
}

SEXP keyring_macos_list_keyring(void) {
  error("only works on macOS");
  return R_NilValue;
}

SEXP keyring_macos_delete_keyring(SEXP keyring) {
  error("only works on macOS");
  return R_NilValue;
}

SEXP keyring_macos_lock_keyring(SEXP keyring) {
  error("only works on macOS");
  return R_NilValue;
}

SEXP keyring_macos_unlock_keyring(SEXP keyring, SEXP password) {
  error("only works on macOS");
  return R_NilValue;
}

SEXP keyring_macos_is_locked_keyring(SEXP keyring) {
  error("only works on macOS");
  return R_NilValue;
}

#endif // __APPLE__
