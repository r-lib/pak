#include <CoreFoundation/CoreFoundation.h>
#import <AppKit/AppKit.h>

#include <R.h>
#include <Rinternals.h>

SEXP ps__list_apps(void) {
  // need to run the event loop a bit, to process updates for the
  // currently running applications
  CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0, true);
  NSWorkspace *ws = [NSWorkspace sharedWorkspace];
  NSArray *apps = [ws runningApplications];

  const char *nms[] = {
    "name",
    "bundle_identifier",
    "bundle_url",
    "arch",
    "executable_url",
    "launch_date",
    "finished_launching",
    "pid",
    "active",
    "activation_policy",
    ""
  };
  SEXP res = PROTECT(Rf_mkNamed(VECSXP, nms));
  NSUInteger count = [apps count];
  SET_VECTOR_ELT(res, 0, Rf_allocVector(STRSXP, count));
  SET_VECTOR_ELT(res, 1, Rf_allocVector(STRSXP, count));
  SET_VECTOR_ELT(res, 2, Rf_allocVector(STRSXP, count));
  SET_VECTOR_ELT(res, 3, Rf_allocVector(INTSXP, count));
  SET_VECTOR_ELT(res, 4, Rf_allocVector(STRSXP, count));
  SET_VECTOR_ELT(res, 5, Rf_allocVector(STRSXP, count));
  SET_VECTOR_ELT(res, 6, Rf_allocVector(LGLSXP, count));
  SET_VECTOR_ELT(res, 7, Rf_allocVector(INTSXP, count));
  SET_VECTOR_ELT(res, 8, Rf_allocVector(LGLSXP, count));
  SET_VECTOR_ELT(res, 9, Rf_allocVector(STRSXP, count));

  for (NSUInteger i = 0; i < count; i++) {
    NSRunningApplication *app = [apps objectAtIndex:i];

    const char *name = [app.localizedName UTF8String];
    SET_STRING_ELT(VECTOR_ELT(res, 0), i, name ? Rf_mkCharCE(name, CE_UTF8) : NA_STRING);
    const char *bid = [app.bundleIdentifier UTF8String];
    SET_STRING_ELT(VECTOR_ELT(res, 1), i, bid ? Rf_mkCharCE(bid, CE_UTF8) : NA_STRING);
    const char *burl = app.bundleURL ? [app.bundleURL.absoluteString UTF8String] : 0;
    SET_STRING_ELT(VECTOR_ELT(res, 2), i, burl ? Rf_mkCharCE(burl, CE_UTF8) : NA_STRING);
    INTEGER(VECTOR_ELT(res, 3))[i] = app.executableArchitecture;
    const char *eurl = app.executableURL ? [app.executableURL.absoluteString UTF8String] : 0;
    SET_STRING_ELT(VECTOR_ELT(res, 4), i, eurl ? Rf_mkCharCE(eurl, CE_UTF8) : NA_STRING);
    const char *ld = app.launchDate ? [app.launchDate.description UTF8String] : 0;
    SET_STRING_ELT(VECTOR_ELT(res, 5), i, ld ? Rf_mkCharCE(ld, CE_UTF8) : NA_STRING);
    LOGICAL(VECTOR_ELT(res, 6))[i] = app.finishedLaunching;
    INTEGER(VECTOR_ELT(res, 7))[i] = app.processIdentifier;
    LOGICAL(VECTOR_ELT(res, 8))[i] = app.active;

    if (app.activationPolicy == NSApplicationActivationPolicyRegular) {
      SET_STRING_ELT(VECTOR_ELT(res, 9), i, Rf_mkChar("regular"));
    } else if (app.activationPolicy == NSApplicationActivationPolicyAccessory) {
      SET_STRING_ELT(VECTOR_ELT(res, 9), i, Rf_mkChar("accessory"));
    } else if (app.activationPolicy == NSApplicationActivationPolicyProhibited) {
      SET_STRING_ELT(VECTOR_ELT(res, 9), i, Rf_mkChar("prohibited"));
    } else {
      SET_STRING_ELT(VECTOR_ELT(res, 9), i, NA_STRING);
    }
  }

  UNPROTECT(1);
  return res;
}
