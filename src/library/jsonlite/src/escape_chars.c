#include <Rinternals.h>
#include <stdlib.h>

/*
Fast escaping of character vectors (Winston Chang)
https://gist.github.com/wch/e3ec5b20eb712f1b22b2
http://stackoverflow.com/questions/25609174/fast-escaping-deparsing-of-character-vectors-in-r/25613834#25613834
*/

SEXP C_escape_chars_one(SEXP x) {

  // Make a cursor pointer
  const char * cur = CHAR(x);
  const char * end = CHAR(x) + Rf_length(x);

  // Count the number of matches
  int matches = 0;
  while (cur < end) {
    switch(*cur) {
      case '\\':
      case '"':
      case '\n':
      case '\r':
      case '\t':
      case '\b':
      case '\f':
        matches++;
        break;
      case '/':
        if(cur > CHAR(x) && cur[-1] == '<')
          matches++;
        break;
      default:
        if (*cur >= 0x00 && *cur <= 0x1f)
          matches += 5; //needs explicit \u00xx escaping
    }
    cur++;
  }

  // Calculate output length, 2 for double quotes
  size_t outlen = Rf_length(x) + matches + 2;
  char * newstr = malloc(outlen);

  // Reset cursor to beginning
  cur = CHAR(x);

  // Allocate string memory; add 2 for start and end quotes.
  char * outcur = newstr;
  *outcur++ = '"';

  while(cur < end) {
    switch(*cur) {
      case '\\':
        *outcur++ = '\\';
        *outcur = '\\';
        break;
      case '"':
        *outcur++ = '\\';
        *outcur = '"';
        break;
      case '\n':
        *outcur++ = '\\';
        *outcur = 'n';
        break;
      case '\r':
        *outcur++ = '\\';
        *outcur = 'r';
        break;
      case '\t':
        *outcur++ = '\\';
        *outcur = 't';
        break;
      case '\b':
        *outcur++ = '\\';
        *outcur = 'b';
        break;
      case '\f':
        *outcur++ = '\\';
        *outcur = 'f';
        break;
      case '/':
        if(cur > CHAR(x) && cur[-1] == '<'){
          *outcur++ = '\\';
          *outcur = '/';
          break;
        } //FALL THROUGH!
      default:
        //control characters need explicit \u00xx escaping
        if (*cur >= 0x00 && *cur <= 0x1f){
          snprintf(outcur, 7, "\\u%04x", *cur);
          outcur += 5; //extra length
          break;
        }
        //simply copy char from input
        *outcur = *cur;
    }

    //increment input and output cursors to next character
    cur++;
    outcur++;
  }

  //Close quote and create R string
  *outcur = '"';
  SEXP out = mkCharLenCE(newstr, outlen, getCharCE(x));
  free(newstr);
  return out;
}

SEXP C_escape_chars(SEXP x) {
  if (!isString(x))
    error("x must be a character vector.");
  if (x == R_NilValue || length(x) == 0)
    return x;

  int len = length(x);
  SEXP out = PROTECT(allocVector(STRSXP, len));

  for (int i=0; i<len; i++) {
    SET_STRING_ELT(out, i, C_escape_chars_one(STRING_ELT(x, i)));
  }
  UNPROTECT(1);
  return out;
}
