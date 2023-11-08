#include <Rinternals.h>
#include <string.h>
#include <yajl_parse.h>

SEXP R_validate(SEXP x) {
    /* get data from R */
    const char* json = translateCharUTF8(asChar(x));

    /* test for BOM */
    if(json[0] == '\xEF' && json[1] == '\xBB' && json[2] == '\xBF'){
      SEXP output = PROTECT(duplicate(ScalarLogical(0)));
      SEXP msg = PROTECT(Rf_mkString("JSON string contains UTF8 byte-order-mark."));
      setAttrib(output, install("err"), msg);
      UNPROTECT(2);
      return(output);
    }

    /* allocate a parser */
    yajl_handle hand = yajl_alloc(NULL, NULL, NULL);

    /* parser options */
    //yajl_config(hand, yajl_dont_validate_strings, 1);

    /* go parse */
    const size_t rd = strlen(json);
    yajl_status stat = yajl_parse(hand, (const unsigned char*) json, rd);
    if(stat == yajl_status_ok) {
      stat = yajl_complete_parse(hand);
    }

    SEXP output = PROTECT(duplicate(ScalarLogical(!stat)));

    //error message
    if (stat != yajl_status_ok) {
        unsigned char* str = yajl_get_error(hand, 1, (const unsigned char*) json, rd);
        SEXP errstr = PROTECT(mkString((const char *) str));
        SEXP offset = PROTECT(ScalarInteger(yajl_get_bytes_consumed(hand)));
        yajl_free_error(hand, str);
        setAttrib(output, install("offset"), offset);
        setAttrib(output, install("err"), errstr);
        UNPROTECT(2);
    }

    /* return boolean vec (0 means no errors, means is valid) */
    yajl_free(hand);
    UNPROTECT(1);
    return output;
}
