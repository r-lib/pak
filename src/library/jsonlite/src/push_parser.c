#include <Rinternals.h>
#include <Rversion.h>
#include <R_ext/Connections.h>
#include <string.h>
#include <yajl_tree.h>
#include <yajl_parse.h>
#include "push_parser.h"

#define bufsize 32768
SEXP R_parse_connection(SEXP sConn, SEXP bigint_as_char){
  int first = 1;
  char errbuf[bufsize];
  unsigned char * errstr;
  yajl_handle push_parser = push_parser_new();
  SEXP call = PROTECT(Rf_lang4(
    PROTECT(Rf_install("readBin")),
    sConn,
    PROTECT(Rf_allocVector(RAWSXP, 0)),
    PROTECT(Rf_ScalarInteger(bufsize))));
  while(1){
    SEXP out = PROTECT(Rf_eval(call, R_BaseEnv));
    int len = Rf_length(out);
    if(len <= 0){
      UNPROTECT(1);
      break;
    }
    unsigned char * ptr = RAW(out);

    //strip off BOM
    if(first && len > 3 && ptr[0] == 239 && ptr[1] == 187 && ptr[2] == 191){
      warningcall(R_NilValue, "JSON string contains (illegal) UTF8 byte-order-mark!");
      ptr += 3;
      len -= 3;
    }

    //strip off rfc7464 record separator
    if(first && len > 1 && ptr[0] == 30){
      ptr += 1;
      len -= 1;
    }

    first = 0;

    /* parse and check for errors */
    if (yajl_parse(push_parser, ptr, len) != yajl_status_ok){
      errstr = yajl_get_error(push_parser, 1, ptr, len);
      goto JSON_FAIL;
    }
    UNPROTECT(1);
  }
  UNPROTECT(4);

  /* complete parse */
  if (yajl_complete_parse(push_parser) != yajl_status_ok){
    errstr = yajl_get_error(push_parser, 1, NULL, 0);
    goto JSON_FAIL;
  }

  /* get output */
  yajl_val tree = push_parser_get(push_parser);
  SEXP out = PROTECT(ParseValue(tree, asLogical(bigint_as_char)));
  yajl_tree_free(tree);
  yajl_free(push_parser);
  UNPROTECT(1);
  return out;

  JSON_FAIL:
    strncpy(errbuf, (char *) errstr, bufsize - 1);
    yajl_free_error(push_parser, errstr);
    yajl_free(push_parser);
    Rf_error("%s", errbuf);
}
