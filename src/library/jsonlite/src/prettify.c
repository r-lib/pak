#include <Rinternals.h>
#include <string.h>
#include <yajl_parse.h>
#include <yajl_gen.h>

static int s_streamReformat = 0;

#define GEN_AND_RETURN(func){\
    yajl_gen_status __stat = func;\
    if (__stat == yajl_gen_generation_complete && s_streamReformat) {\
      yajl_gen_reset(g, "\n");\
      __stat = func;\
    }\
    return __stat == yajl_gen_status_ok;\
}

static int reformat_null(void * ctx)
{
    yajl_gen g = (yajl_gen) ctx;
    GEN_AND_RETURN(yajl_gen_null(g));
}

static int reformat_boolean(void * ctx, int boolean)
{
    yajl_gen g = (yajl_gen) ctx;
    GEN_AND_RETURN(yajl_gen_bool(g, boolean));
}

static int reformat_number(void * ctx, const char * s, size_t l)
{
    yajl_gen g = (yajl_gen) ctx;
    GEN_AND_RETURN(yajl_gen_number(g, s, l));
}

static int reformat_string(void * ctx, const unsigned char * stringVal,
                           size_t stringLen)
{
    yajl_gen g = (yajl_gen) ctx;
    GEN_AND_RETURN(yajl_gen_string(g, stringVal, stringLen));
}

static int reformat_map_key(void * ctx, const unsigned char * stringVal,
                            size_t stringLen)
{
    yajl_gen g = (yajl_gen) ctx;
    GEN_AND_RETURN(yajl_gen_string(g, stringVal, stringLen));
}

static int reformat_start_map(void * ctx)
{
    yajl_gen g = (yajl_gen) ctx;
    GEN_AND_RETURN(yajl_gen_map_open(g));
}

static int reformat_end_map(void * ctx)
{
    yajl_gen g = (yajl_gen) ctx;
    GEN_AND_RETURN(yajl_gen_map_close(g));
}

static int reformat_start_array(void * ctx)
{
    yajl_gen g = (yajl_gen) ctx;
    GEN_AND_RETURN(yajl_gen_array_open(g));
}

static int reformat_end_array(void * ctx)
{
    yajl_gen g = (yajl_gen) ctx;
    GEN_AND_RETURN(yajl_gen_array_close(g));
}

static yajl_callbacks callbacks = {
    reformat_null,
    reformat_boolean,
    NULL,
    NULL,
    reformat_number,
    reformat_string,
    reformat_start_map,
    reformat_map_key,
    reformat_end_map,
    reformat_start_array,
    reformat_end_array
};

SEXP R_reformat(SEXP x, SEXP pretty, SEXP indent_string) {
    yajl_status stat;
    yajl_handle hand;
    yajl_gen g;
    SEXP output;

    /* init generator */
    g = yajl_gen_alloc(NULL);
    yajl_gen_config(g, yajl_gen_beautify, asInteger(pretty));
    yajl_gen_config(g, yajl_gen_indent_string, translateCharUTF8(asChar(indent_string)));
    yajl_gen_config(g, yajl_gen_validate_utf8, 0);
    yajl_gen_config(g, yajl_gen_escape_solidus, 1); //modified to only escape for "</"

    /* init parser */
    hand = yajl_alloc(&callbacks, NULL, (void *) g);
    yajl_config(hand, yajl_allow_comments, 1);

    /* get data from R */
    const char* json = translateCharUTF8(asChar(x));

    /* ignore BOM */
    if(json[0] == '\xEF' && json[1] == '\xBB' && json[2] == '\xBF'){
      json = json + 3;
    }

    /* Get length (after removing bom) */
    const size_t rd = strlen(json);

    /* parse */
    stat = yajl_parse(hand, (const unsigned char*) json, rd);
    if(stat == yajl_status_ok) {
      stat = yajl_complete_parse(hand);
    }

    //error message
    if (stat != yajl_status_ok) {
      unsigned char* str = yajl_get_error(hand, 1, (const unsigned char*) json, rd);
      output = PROTECT(mkString((const char*) str));
      yajl_free_error(hand, str);
    } else {
      //create R object
      const unsigned char* buf;
      size_t len;
      yajl_gen_get_buf(g, &buf, &len);

      //force as UTF8 string
      output = PROTECT(allocVector(STRSXP, 1));
      SET_STRING_ELT(output, 0, mkCharCE((const char*) buf, CE_UTF8));
      setAttrib(output, R_ClassSymbol, mkString("json"));
    }

    /* clean up */
    yajl_gen_clear(g);
    yajl_gen_free(g);
    yajl_free(hand);

    /* return boolean vec (0 means no errors, means is valid) */
    SEXP vec = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(vec, 0, ScalarInteger(stat));
    SET_VECTOR_ELT(vec, 1, output);
    UNPROTECT(2);
    return vec;
}
