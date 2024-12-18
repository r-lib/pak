#include "curl-common.h"

struct curl_httppost* make_form(SEXP form){
  struct curl_httppost* post = NULL;
  struct curl_httppost* last = NULL;
  SEXP ln = PROTECT(Rf_getAttrib(form, R_NamesSymbol));
  for(int i = 0; i < Rf_length(form); i++){
    const char *name = Rf_translateCharUTF8(STRING_ELT(ln, i));
    SEXP val = VECTOR_ELT(form, i);
    if(TYPEOF(val) == RAWSXP){
      long datalen = Rf_length(val);
      if(datalen > 0){
        unsigned char * data = RAW(val);
        curl_formadd(&post, &last, CURLFORM_COPYNAME, name, CURLFORM_COPYCONTENTS, data, CURLFORM_CONTENTSLENGTH, datalen, CURLFORM_END);
      } else {
        //Note if 'CURLFORM_CONTENTLEN == 0' then libcurl assumes strlen() !
        curl_formadd(&post, &last, CURLFORM_COPYNAME, name, CURLFORM_COPYCONTENTS, "", CURLFORM_END);
      }
    } else if(Rf_isVector(val) && Rf_length(val)){
      if(Rf_isString(VECTOR_ELT(val, 0))){
        //assume a form_file upload
        const char *path = CHAR(Rf_asChar(VECTOR_ELT(val, 0)));
        if(Rf_isString(VECTOR_ELT(val, 1))) {
          const char *content_type = CHAR(Rf_asChar(VECTOR_ELT(val, 1)));
          if(Rf_isString(VECTOR_ELT(val, 2))) {
            const char *file_name = CHAR(Rf_asChar(VECTOR_ELT(val, 2)));
            curl_formadd(&post, &last, CURLFORM_COPYNAME, name, CURLFORM_FILE, path, CURLFORM_CONTENTTYPE, content_type, CURLFORM_FILENAME, file_name, CURLFORM_END);
          } else {
            curl_formadd(&post, &last, CURLFORM_COPYNAME, name, CURLFORM_FILE, path, CURLFORM_CONTENTTYPE, content_type, CURLFORM_END);
          }
        } else {
          if(Rf_isString(VECTOR_ELT(val, 2))) {
            const char *file_name = CHAR(Rf_asChar(VECTOR_ELT(val, 2)));
            curl_formadd(&post, &last, CURLFORM_COPYNAME, name, CURLFORM_FILE, path, CURLFORM_FILENAME, file_name, CURLFORM_END);
          } else {
            curl_formadd(&post, &last, CURLFORM_COPYNAME, name, CURLFORM_FILE, path, CURLFORM_END);
          }
        }
      } else {
        //assume a form_value upload
        unsigned char * data = RAW(VECTOR_ELT(val, 0));
        long datalen = Rf_length(VECTOR_ELT(val, 0));
        if(Rf_isString(VECTOR_ELT(val, 1))){
          const char * content_type = CHAR(Rf_asChar(VECTOR_ELT(val, 1)));
          curl_formadd(&post, &last, CURLFORM_COPYNAME, name, CURLFORM_COPYCONTENTS, data, CURLFORM_CONTENTSLENGTH, datalen, CURLFORM_CONTENTTYPE, content_type, CURLFORM_END);
        } else {
          curl_formadd(&post, &last, CURLFORM_COPYNAME, name, CURLFORM_COPYCONTENTS, data, CURLFORM_CONTENTSLENGTH, datalen, CURLFORM_END);
        }
      }
    } else {
      Rf_error("form value %s not supported", name);
    }
  }
  UNPROTECT(1);
  return post;
}
