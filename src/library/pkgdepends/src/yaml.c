#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "yaml.h"

static SEXP handle_scalar(yaml_event_t *event) {
  SEXP s = PROTECT(Rf_mkCharLenCE(
    (const char*) event->data.scalar.value,
    event->data.scalar.length,
    CE_UTF8
  ));
  SEXP s2 = Rf_ScalarString(s);
  UNPROTECT(1);
  return s2;
}

SEXP yaml_parse_scalar(SEXP rx) {
  const char *x = CHAR(STRING_ELT(rx, 0));
  yaml_parser_t parser;
  yaml_event_t event;

  yaml_parser_initialize(&parser);
  yaml_parser_set_input_string(&parser, (const unsigned char*) x, strlen(x));

  SEXP res = R_NilValue;
  int done = 0;

  while (!done) {
    if (yaml_parser_parse(&parser, &event)) {
      switch (event.type) {
        case YAML_NO_EVENT:
        case YAML_STREAM_START_EVENT:
        case YAML_STREAM_END_EVENT:
        case YAML_DOCUMENT_START_EVENT:
        case YAML_DOCUMENT_END_EVENT:
          break;

        case YAML_SCALAR_EVENT:
          res = handle_scalar(&event);
          yaml_event_delete(&event);
          yaml_parser_delete(&parser);
          done = 1;
          break;

        case YAML_SEQUENCE_START_EVENT:
        case YAML_SEQUENCE_END_EVENT:
        case YAML_MAPPING_START_EVENT:
        case YAML_MAPPING_END_EVENT:
        default:
          yaml_event_delete(&event);
          yaml_parser_delete(&parser);
          Rf_error("Failed to parse YAML string");
          break;
      }

    } else {
      yaml_event_delete(&event);
      yaml_parser_delete(&parser);
      Rf_error("Failed to parse YAML string");
    }
  }

  return res;
}
