/*
   This function uses the YAJL tree parser to parse the entire document
   before converting it to an R list. It might be faster to use the YAJL
   callback mechanism instead to construct the R list immediately while
   parsing the JSON. But that looks very complicated.

*/

#include <Rinternals.h>
#include <yajl_tree.h>

static SEXP ParseObject(yajl_val node, int bigint);
static SEXP ParseArray(yajl_val node, int bigint);
SEXP ParseValue(yajl_val node, int bigint);

SEXP R_parse(SEXP x, SEXP bigint_as_char) {
    /* get data from R */
    const char* json = translateCharUTF8(asChar(x));
    const int bigint = asLogical(bigint_as_char);

    /* ignore BOM as suggested by RFC */
    if(json[0] == '\xEF' && json[1] == '\xBB' && json[2] == '\xBF'){
      warningcall(R_NilValue, "JSON string contains (illegal) UTF8 byte-order-mark!");
      json = json + 3;
    }

    /* ignore rfc7464 record separator */
    if(json[0] == '\x1E'){
      json = json + 1;
    }

    /* parse json */
    char errbuf[1024];
    yajl_val node = yajl_tree_parse(json, errbuf, sizeof(errbuf));

    /* parser error */
    if (!node) {
      Rf_errorcall(R_NilValue, "%s", errbuf);
    }
    SEXP out = ParseValue(node, bigint);
    yajl_tree_free(node);
    return(out);
}

SEXP ParseValue(yajl_val node, int bigint){
  if(YAJL_IS_NULL(node)){
    return R_NilValue;
  }
  if(YAJL_IS_STRING(node)){
    SEXP tmp = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(tmp, 0, mkCharCE(YAJL_GET_STRING(node),  CE_UTF8));
    UNPROTECT(1);
    return tmp;
  }
  if(YAJL_IS_INTEGER(node)){
    long long int val = YAJL_GET_INTEGER(node);
    /* 2^53 is highest int stored as double without loss */
    if(bigint && (val > 9007199254740992 || val < -9007199254740992)){
      char buf[32];
      snprintf(buf, 32, "%lld", val);
      return mkString(buf);
    /* see .Machine$integer.max in R */
    } else if(val > 2147483647 || val < -2147483647){
      return ScalarReal(val);
    } else {
      return ScalarInteger(val);
    }
  }
  if(YAJL_IS_DOUBLE(node)){
    return(ScalarReal(YAJL_GET_DOUBLE(node)));
  }
  if(YAJL_IS_NUMBER(node)){
    /* A number that is not int or double (very rare) */
    /* This seems to correctly round to Inf/0/-Inf */
    return(ScalarReal(YAJL_GET_DOUBLE(node)));
  }
  if(YAJL_IS_TRUE(node)){
    return(ScalarLogical(1));
  }
  if(YAJL_IS_FALSE(node)){
    return(ScalarLogical(0));
  }
  if(YAJL_IS_OBJECT(node)){
    return(ParseObject(node, bigint));
  }
  if(YAJL_IS_ARRAY(node)){
    return(ParseArray(node, bigint));
  }
  error("Invalid YAJL node type.");
}

static SEXP ParseObject(yajl_val node, int bigint){
  int len = YAJL_GET_OBJECT(node)->len;
  SEXP keys = PROTECT(allocVector(STRSXP, len));
  SEXP vec = PROTECT(allocVector(VECSXP, len));
  for (int i = 0; i < len; ++i) {
    SET_STRING_ELT(keys, i, mkCharCE(YAJL_GET_OBJECT(node)->keys[i], CE_UTF8));
    SET_VECTOR_ELT(vec, i, ParseValue(YAJL_GET_OBJECT(node)->values[i], bigint));
  }
  setAttrib(vec, R_NamesSymbol, keys);
  UNPROTECT(2);
  return vec;
}

static SEXP ParseArray(yajl_val node, int bigint){
  int len = YAJL_GET_ARRAY(node)->len;
  SEXP vec = PROTECT(allocVector(VECSXP, len));
  for (int i = 0; i < len; ++i) {
    SET_VECTOR_ELT(vec, i, ParseValue(YAJL_GET_ARRAY(node)->values[i], bigint));
  }
  UNPROTECT(1);
  return vec;
}
