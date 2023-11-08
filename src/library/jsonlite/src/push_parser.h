yajl_handle push_parser_new(void);
yajl_val push_parser_get(yajl_handle handle);
SEXP ParseValue(yajl_val node, int bigint_as_char);
