#include "curl-common.h"

int r_curl_is_string_option(CURLoption x){
  return curl_easy_option_by_id(x)->type == CURLOT_STRING;
}

int r_curl_is_slist_option(CURLoption x){
  return curl_easy_option_by_id(x)->type == CURLOT_SLIST;
}

int r_curl_is_long_option(CURLoption x){
  const curl_easytype type = curl_easy_option_by_id(x)->type;
  return type == CURLOT_LONG || type == CURLOT_VALUES;
}

int r_curl_is_off_t_option(CURLoption x){
  return curl_easy_option_by_id(x)->type == CURLOT_OFF_T;
}

int r_curl_is_postfields_option(CURLoption x){
  return curl_easy_option_by_id(x)->type == CURLOT_OBJECT;
}
