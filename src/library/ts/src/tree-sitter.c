#include <stdio.h>

#define R_NO_REMAP
#include "R.h"
#include "Rinternals.h"
#include "cleancall.h"

#include "tree_sitter/api.h"

static void r_free(void *data) {
  free(data);
}

static SEXP new_lookahead_sym(
  TSSymbol sym,
  const char *csym,
  TSSymbolType symtype
) {
  const char *nms[] = {
    "symbol", "name", "type", ""
  };
  SEXP res = PROTECT(Rf_mkNamed(VECSXP, nms));
  SET_VECTOR_ELT(res, 0, Rf_ScalarInteger((int) sym));
  SET_VECTOR_ELT(res, 1, Rf_mkString(csym));
  SET_VECTOR_ELT(res, 2, Rf_ScalarInteger((int) symtype));
  UNPROTECT(1);
  return res;
}

static TSRange *get_ranges(SEXP rranges, uint32_t *count) {
  TSRange *ranges = NULL;
  *count = 0;
  if (!Rf_isNull(rranges)) {
    if (Rf_length(rranges) != 6) {
      Rf_error("Invalid ranges, must be a data frame of 6 integer columns");
    }
    for (int i = 0; i < 6; i++) {
      if (TYPEOF(VECTOR_ELT(rranges, i)) != INTSXP) {
        Rf_error("Invalid ranges, must be a data frame of 6 integer columns");
      }
    }
    *count = Rf_length(VECTOR_ELT(rranges, 0));
    ranges = malloc(sizeof(TSRange) * (*count));
    if (!ranges) {
      Rf_error("Out of memory"); // # nocov
    }
    int *start_row = INTEGER(VECTOR_ELT(rranges, 0));
    int *start_col = INTEGER(VECTOR_ELT(rranges, 1));
    int *end_row = INTEGER(VECTOR_ELT(rranges, 2));
    int *end_col = INTEGER(VECTOR_ELT(rranges, 3));
    int *start_byte = INTEGER(VECTOR_ELT(rranges, 4));
    int *end_byte = INTEGER(VECTOR_ELT(rranges, 5));
    r_call_on_exit(r_free, ranges);
    for (uint32_t i = 0; i < *count; i++) {
      ranges[i].start_point.row = start_row[i] - 1;
      ranges[i].start_point.column = start_col[i] - 1;
      ranges[i].end_point.row = end_row[i] - 1;
      ranges[i].end_point.column = end_col[i];    // no -1!
      ranges[i].start_byte = start_byte[i] - 1;
      ranges[i].end_byte = end_byte[i];           // no -1!
    }
  }
  return ranges;
}

struct ts_xtree {
  const TSLanguage *language;
  TSParser *parser;
  TSTree *tree;
};

void ts_xtree_free(SEXP rxtree) {
  struct ts_xtree *tree = (struct ts_xtree*) R_ExternalPtrAddr(rxtree);
  if (tree->tree) {
    ts_tree_delete(tree->tree);
  }
  if (tree->parser) {
    ts_parser_delete(tree->parser);
  }
  if (tree->language) {
    // languages are static, do not delete here
  }
}

SEXP s_expr(SEXP rotree) {
  SEXP rtree = Rf_getAttrib(rotree, Rf_install("tree"));
  struct ts_xtree *xtree = (struct ts_xtree*) R_ExternalPtrAddr(rtree);
  TSTree *tree = xtree->tree;
  TSNode root = ts_tree_root_node(tree);
  char *code = ts_node_string(root);

  SEXP result = Rf_mkString(code);
  r_free(code);
  return result;
}

SEXP parse(SEXP input, SEXP rlanguage, SEXP rranges) {
  if (TYPEOF(rlanguage) != EXTPTRSXP) {
    Rf_error("ts `language` must be an external pointer");
  }
  const TSLanguage *language = (const TSLanguage*)
    R_ExternalPtrAddr(rlanguage);
  if (!language) {
    Rf_error("ts `language` cannot be a NULL pointer"); // # nocov
  }
  TSParser *parser = NULL;

  parser = ts_parser_new();
  if (!ts_parser_set_language(parser, language)) {
    Rf_error("Failed to set R language, internal error."); // # nocov
  }
  r_call_on_early_exit((cleanup_fn_t) ts_parser_delete, parser);

  uint32_t count;
  TSRange *ranges = get_ranges(rranges, &count);
  if (ranges) {
    if (!ts_parser_set_included_ranges(parser, ranges, count)) {
      Rf_error("Invalid ranges for tree-sitter parser");
    }
  }

  const char *c_input = (const char*) RAW(input);
  uint32_t length = Rf_length(input);
  TSTree *tree = ts_parser_parse_string(parser, NULL, c_input, length);
  r_call_on_early_exit((cleanup_fn_t) ts_tree_delete, tree);
  TSNode root = ts_tree_root_node(tree);
  uint32_t num_nodes = ts_node_descendant_count(root);
  const char *nms[] = {
    "id", "parent", "field_name", "type", "code", "start_byte", "end_byte",
    "start_row", "start_column", "end_row", "end_column", "is_missing",
    "has_error", "expected", ""
  };
  SEXP res = PROTECT(Rf_mkNamed(VECSXP, nms));
  SET_VECTOR_ELT(res, 0, Rf_allocVector(INTSXP, num_nodes));
  SEXP res_id = VECTOR_ELT(res, 0);
  SET_VECTOR_ELT(res, 1, Rf_allocVector(INTSXP, num_nodes));
  SEXP res_parent = VECTOR_ELT(res, 1);
  SET_VECTOR_ELT(res, 2, Rf_allocVector(STRSXP, num_nodes));
  SEXP res_field_name = VECTOR_ELT(res, 2);
  SET_VECTOR_ELT(res, 3, Rf_allocVector(STRSXP, num_nodes));
  SEXP res_type = VECTOR_ELT(res, 3);
  SET_VECTOR_ELT(res, 4, Rf_allocVector(STRSXP, num_nodes));
  SEXP res_code = VECTOR_ELT(res, 4);
  SET_VECTOR_ELT(res, 5, Rf_allocVector(INTSXP, num_nodes));
  SEXP res_start_byte = VECTOR_ELT(res, 5);
  SET_VECTOR_ELT(res, 6, Rf_allocVector(INTSXP, num_nodes));
  SEXP res_end_byte = VECTOR_ELT(res, 6);
  SET_VECTOR_ELT(res, 7, Rf_allocVector(INTSXP, num_nodes));
  SEXP res_start_row = VECTOR_ELT(res, 7);
  SET_VECTOR_ELT(res, 8, Rf_allocVector(INTSXP, num_nodes));
  SEXP res_start_column = VECTOR_ELT(res, 8);
  SET_VECTOR_ELT(res, 9, Rf_allocVector(INTSXP, num_nodes));
  SEXP res_end_row = VECTOR_ELT(res, 9);
  SET_VECTOR_ELT(res, 10, Rf_allocVector(INTSXP, num_nodes));
  SEXP res_end_column = VECTOR_ELT(res, 10);
  SET_VECTOR_ELT(res, 11, Rf_allocVector(LGLSXP, num_nodes));
  SEXP res_is_missing = VECTOR_ELT(res, 11);
  SET_VECTOR_ELT(res, 12, Rf_allocVector(LGLSXP, num_nodes));
  SEXP res_has_error = VECTOR_ELT(res, 12);
  SET_VECTOR_ELT(res, 13, Rf_allocVector(VECSXP, num_nodes));
  SEXP res_expected = VECTOR_ELT(res, 13);

  TSTreeCursor cursor = ts_tree_cursor_new(root);
  r_call_on_exit((cleanup_fn_t) ts_tree_cursor_delete, &cursor);

  size_t idx = 0;
  bool ok = true;
  uint32_t parent = 0;
  while (ok) {
    // record current node
    TSNode crnt = ts_tree_cursor_current_node(&cursor);
    INTEGER(res_id)[idx] = idx + 1;
    INTEGER(res_parent)[idx] = parent + 1;
    const char *field_name = ts_tree_cursor_current_field_name(&cursor);
    SET_STRING_ELT(res_field_name, idx,
      field_name ? Rf_mkChar(field_name) : R_NaString);
    const char *type = ts_node_type(crnt);
    SET_STRING_ELT(res_type, idx, Rf_mkChar(type));
    uint32_t sb = ts_node_start_byte(crnt);
    INTEGER(res_start_byte)[idx] = sb;
    uint32_t eb = ts_node_end_byte(crnt);
    INTEGER(res_end_byte)[idx] = eb;
    TSPoint spt = ts_node_start_point(crnt);
    INTEGER(res_start_row)[idx] = spt.row;
    INTEGER(res_start_column)[idx] = spt.column;
    TSPoint ept = ts_node_end_point(crnt);
    INTEGER(res_end_row)[idx] = ept.row;
    INTEGER(res_end_column)[idx] = ept.column;
    if (ts_node_named_child_count(crnt) == 0) {
      SET_STRING_ELT(res_code, idx, Rf_mkCharLen(c_input + sb, eb - sb));
    } else {
      SET_STRING_ELT(res_code, idx, R_NaString);
    }
    LOGICAL(res_is_missing)[idx] = ts_node_is_missing(crnt);
    LOGICAL(res_has_error)[idx] = ts_node_has_error(crnt);

    if (ts_node_is_missing(crnt)) {
      TSStateId state = ts_node_parse_state(crnt);
      TSLookaheadIterator *lait = ts_lookahead_iterator_new(language, state);
      size_t laidx = 0;
      do {
        laidx++;
      } while (ts_lookahead_iterator_next(lait));
      SET_VECTOR_ELT(res_expected, idx, Rf_allocVector(VECSXP, laidx));
      SEXP exp = VECTOR_ELT(res_expected, idx);

      laidx = 0;
      ts_lookahead_iterator_reset_state(lait, state);
      do {
        TSSymbol sym = ts_lookahead_iterator_current_symbol(lait);
        const char *csym = ts_lookahead_iterator_current_symbol_name(lait);
        TSSymbolType symtype = ts_language_symbol_type(language, sym);
        SET_VECTOR_ELT(exp, laidx++, new_lookahead_sym(sym, csym, symtype));
      } while (ts_lookahead_iterator_next(lait));

      ts_lookahead_iterator_delete(lait);
    }

    // find next node. if there is a child, use that.
    // otherwise if there is a sibling use that.
    // otherwise go up until there is a sibling. or until cannot go up.
    ok = ts_tree_cursor_goto_first_child(&cursor);
    if (ok) {
      parent = idx;
    } else {
      // parent stays the same
      ok = ts_tree_cursor_goto_next_sibling(&cursor);
    }
    while (!ok) {
      ok = ts_tree_cursor_goto_parent(&cursor);
      if (!ok) break;
      parent = INTEGER(res_parent)[parent] - 1;
      ok = ts_tree_cursor_goto_next_sibling(&cursor);
    }

    idx++;
  }

  // no parent for root node
  INTEGER(res_parent)[0] = NA_INTEGER;

  SEXP rnms = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(rnms)[0] = NA_INTEGER;
  INTEGER(rnms)[1] = - num_nodes;
  Rf_setAttrib(res, R_RowNamesSymbol, rnms);
  UNPROTECT(1);

  SEXP cls = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(cls, 0, Rf_mkChar("tbl"));
  SET_STRING_ELT(cls, 1, Rf_mkChar("data.frame"));
  Rf_setAttrib(res, R_ClassSymbol, cls);

  struct ts_xtree *rxtree = malloc(sizeof(struct ts_xtree));
  *rxtree = (struct ts_xtree){ language, parser, tree };
  SEXP rtree = Rf_protect(R_MakeExternalPtr(
    (void*) rxtree,
    R_NilValue,
    R_NilValue
  ));
  R_RegisterCFinalizerEx(rtree, (R_CFinalizer_t) ts_xtree_free, TRUE);
  SEXP sym_tree = Rf_protect(Rf_install("tree"));
  Rf_setAttrib(res, sym_tree, rtree);

  UNPROTECT(4);
  return res;
}

typedef enum {
  EQ = 0,
  NOT_EQ,
  ANY_EQ,
  ANY_NOT_EQ,
  MATCH,
  NOT_MATCH,
  ANY_MATCH,
  ANY_NOT_MATCH,
  ANY_OF,
  NOT_ANY_OF
} predicate_type;

struct query_match_t {
  const TSQuery *query;
  const TSQueryMatch *match;
  uint32_t pattern_index;
  const TSQueryPredicateStep *preds;
  uint32_t num_pred_steps;
  const char *text;
  uint32_t text_length;
  const uint32_t *capture_map;
  const uint32_t *capture_map_pattern;
};

#define CODEEQ(s1, l1, s2, l2) \
  (((l1) == (l2)) && !strncmp(qm->text + (s1), qm->text + (s2), (l1)))

bool check_predicate_eq(const struct query_match_t *qm, predicate_type op,
                        uint32_t st, uint32_t first_id,
                        uint32_t first_nodes_count) {

  if (qm->preds[st].type == TSQueryPredicateStepTypeCapture) {
    uint32_t second_id = qm->preds[st].value_id;
    uint32_t second_nodes_count = 0;
    if (qm->capture_map_pattern[second_id] == qm->pattern_index + 1) {
      second_nodes_count++;
      uint32_t second_idx = qm->capture_map[second_id];
      for (uint32_t i = second_idx + 1; i < qm->match->capture_count; i++) {
        if (qm->match->captures[i].index != second_id) break;
        second_nodes_count++;
      }
    }
    // Need to compare two sets of nodes
    uint32_t first_idx = qm->capture_map[first_id];
    uint32_t second_idx = qm->capture_map[second_id];
    // TODO: this is simpler for EQ etc., no need to cross-compare all
    for (uint32_t i = first_idx; i < first_idx + first_nodes_count; i++) {
      TSNode first_node = qm->match->captures[i].node;
      uint32_t first_start = ts_node_start_byte(first_node);
      uint32_t first_end = ts_node_end_byte(first_node);
      uint32_t first_length = first_end - first_start;
      for (uint32_t j = second_idx; j < second_idx + second_nodes_count; j++) {
        TSNode second_node = qm->match->captures[j].node;
        uint32_t second_start = ts_node_start_byte(second_node);
        uint32_t second_end = ts_node_end_byte(second_node);
        uint32_t second_length = second_end - second_start;
        bool eq = CODEEQ(first_start, first_length, second_start, second_length);
        if (op == EQ) {
          if (!eq) return false;
        } else if (op == NOT_EQ) {
          if (eq) return false;
        } else if (op == ANY_EQ) {
          if (eq) return true;
        } else if (op == ANY_NOT_EQ) {
          if (!eq) return true;
        }
      }
    }
    // all combinarions checked, no evidence found against
    if (op == EQ || op == NOT_EQ) {
      return true;
    } else { // op == ANY_EQ || op == ANY_NOT_EQ
      return false;
    }

  } else if (qm->preds[st].type == TSQueryPredicateStepTypeString) {
    uint32_t str_length;
    const char *str = ts_query_string_value_for_id(
      qm->query,
      qm->preds[st].value_id,
      &str_length
    );
    uint32_t first_idx = qm->capture_map[first_id];
    for (uint32_t i = first_idx; i < first_idx + first_nodes_count; i++) {
      TSNode first_node = qm->match->captures[i].node;
      uint32_t first_start = ts_node_start_byte(first_node);
      uint32_t first_end = ts_node_end_byte(first_node);
      uint32_t first_length = first_end - first_start;
      bool eq = first_length == str_length &&
        !strncmp(qm->text + first_start, str, first_length);
      if (op == EQ) {
        if (!eq) return false;
      } else if (op == NOT_EQ) {
        if (eq) return false;
      } else if (op == ANY_EQ) {
        if (eq) return true;
      } else if (op == ANY_NOT_EQ) {
        if (!eq) return true;
      }
    }
    // all combinarions checked, no evidence found against
    if (op == EQ || op == NOT_EQ) {
      return true;
    } else { // op == ANY_EQ || op == ANY_NOT_EQ
      return false;
    }

  } else {
    // this should not happen
    // # nocov start
    Rf_error("Missing second argument for tree-sitter query");
    // # nocov end
  }
}

bool r_grepl(const char *text, uint32_t text_length, const char *pattern,
             uint32_t pattern_length) {
  SEXP rtext = PROTECT(Rf_ScalarString(Rf_mkCharLenCE(
    text, text_length, CE_UTF8)));
  SEXP rpattern = PROTECT(Rf_ScalarString(Rf_mkCharLenCE(
    pattern, pattern_length, CE_UTF8)));
  SEXP f = PROTECT(Rf_ScalarLogical(0));
  SEXP t = PROTECT(Rf_ScalarLogical(1));
  SEXP call = PROTECT(Rf_lang5(Rf_install("grepl"), rpattern, rtext, f, t));
  SEXP ans = PROTECT(Rf_eval(call, R_BaseEnv));
  bool cans = LOGICAL(ans)[0];
  UNPROTECT(6);
  return cans;
}

bool check_predicate_match(const struct query_match_t *qm,
                           predicate_type op, uint32_t st,
                           uint32_t first_id,
                           uint32_t first_nodes_count) {
  uint32_t pattern_length;
  const char *pattern = ts_query_string_value_for_id(
    qm->query,
    qm->preds[st].value_id,
    &pattern_length
  );

  uint32_t first_idx = qm->capture_map[first_id];
  for (uint32_t i = first_idx; i < first_idx + first_nodes_count; i++) {
    TSNode first_node = qm->match->captures[i].node;
    uint32_t first_start = ts_node_start_byte(first_node);
    uint32_t first_end = ts_node_end_byte(first_node);
    uint32_t first_length = first_end - first_start;
    bool eq = r_grepl(
      qm->text + first_start, first_length, pattern, pattern_length);
    if (op == MATCH) {
      if (!eq) return false;
    } else if (op == NOT_MATCH) {
      if (eq) return false;
    } else if (op == ANY_MATCH) {
      if (eq) return true;
    } else if (op == ANY_NOT_MATCH) {
      if (!eq) return true;
    }
  }
  // all combinarions checked, no evidence found against
  if (op == MATCH || op == NOT_MATCH) {
    return true;
  } else { // op == ANY_MATCH || op == ANY_NOT_MATCH
    return false;
  }
}

bool check_predicate_any_of(const struct query_match_t *qm,
                            predicate_type op, uint32_t st,
                            uint32_t first_id,
                            uint32_t first_nodes_count) {

  uint32_t first_idx = qm->capture_map[first_id];
  for (uint32_t i = first_idx; i < first_idx + first_nodes_count; i++) {
    TSNode first_node = qm->match->captures[i].node;
    uint32_t first_start = ts_node_start_byte(first_node);
    uint32_t first_end = ts_node_end_byte(first_node);
    uint32_t first_length = first_end - first_start;
    bool ifound = false;
    for (uint32_t sti = st;
         qm->preds[sti].type != TSQueryPredicateStepTypeDone;
         sti++) {
      uint32_t str_length;
      const char *str = ts_query_string_value_for_id(
        qm->query,
        qm->preds[sti].value_id,
        &str_length
      );

      bool eq = first_length == str_length &&
        !strncmp(qm->text + first_start, str, first_length);
      if (eq) {
        ifound = true;
        break;
      }
    }
    if (op == ANY_OF) {
      if (!ifound) return false;
    } else { // op == NOT_ANY_OF
      if (ifound) return false;
    }
  }
  // all nodes are ok
  return true;
}

bool check_predicates(const struct query_match_t *qm) {
  for (uint32_t st = 0; st < qm->num_pred_steps; st++) {
    // Operation, like #eq? etc. ------------------------------------------
    if (qm->preds[st].type != TSQueryPredicateStepTypeString) {
      Rf_error("First predicate step must be a string");
    }
    uint32_t l;
    const char *ops = ts_query_string_value_for_id(
      qm->query,
      qm->preds[st].value_id,
      &l
    );
    st++;
    predicate_type op;
    if (!strcasecmp("eq?", ops)) {
      op = EQ;
    } else if (!strcasecmp("not-eq?", ops)) {
      op = NOT_EQ;
    } else if (!strcasecmp("any-eq?", ops)) {
      op = ANY_EQ;
    } else if (!strcasecmp("any-not-eq?", ops)) {
      op = ANY_NOT_EQ;
    } else if (!strcasecmp("match?", ops)) {
      op = MATCH;
    } else if (!strcasecmp("not-match?", ops)) {
      op = NOT_MATCH;
    } else if (!strcasecmp("any-match?", ops)) {
      op = ANY_MATCH;
    } else if (!strcasecmp("any-not-match?", ops)) {
      op = ANY_NOT_MATCH;
    } else if (!strcasecmp("any-of?", ops)) {
      op = ANY_OF;
    } else if (!strcasecmp("not-any-of?", ops)) {
      op = NOT_ANY_OF;
    } else {
      Rf_error("Unknown predicate: #%s", ops);
    }

    // First argument must be a capture. ----------------------------------
    // Possibly 0-n nodes
    if (qm->preds[st].type != TSQueryPredicateStepTypeCapture) {
      Rf_error("First argument of a predicate must be a capture");
    }
    uint32_t first_id = qm->preds[st].value_id;
    uint32_t first_nodes_count = 0;
    if (qm->capture_map_pattern[first_id] == qm->pattern_index + 1) {
      first_nodes_count++;
      uint32_t first_idx = qm->capture_map[first_id];
      for (uint32_t i = first_idx + 1; i < qm->match->capture_count; i++) {
        if (qm->match->captures[i].index != first_id) break;
        first_nodes_count++;
      }
    }
    st++;

    if (op == ANY_OF || op == NOT_ANY_OF) {
      if (! check_predicate_any_of(qm, op, st, first_id, first_nodes_count)) {
        return false;
      }

    } else if (op == MATCH || op == NOT_MATCH ||
               op == ANY_MATCH || op == ANY_NOT_MATCH) {
      if (!check_predicate_match(qm, op, st, first_id, first_nodes_count)) {
        return false;
      }

    } else {
      if (!check_predicate_eq(qm, op, st, first_id, first_nodes_count)) {
        return false;
      }
    }
    // move to the next predicate;
    while (st < qm->num_pred_steps &&
           qm->preds[st].type != TSQueryPredicateStepTypeDone) st++;
  }
  return true;
}

SEXP code_query(SEXP rotree, SEXP rquery) {
  SEXP rtree = Rf_getAttrib(rotree, Rf_install("tree"));
  struct ts_xtree *xtree = (struct ts_xtree*) R_ExternalPtrAddr(rtree);
  const TSLanguage *language = xtree->language;
  TSTree *tree = xtree->tree;

  SEXP rinput = Rf_getAttrib(rotree, Rf_install("text"));
  const char *c_input = (const char*) RAW(rinput);
  uint32_t length = Rf_length(rinput);

  const char *cpattern = CHAR(STRING_ELT(rquery, 0));
  uint32_t error_offset;
  TSQueryError error_type;
  TSQuery *query = ts_query_new(
    language,
    cpattern,
    strlen(cpattern),
    &error_offset,
    &error_type
  );
  if (!query) {
    Rf_error("Failed to parse TS query at char %d.", (int) error_offset);
  }
  r_call_on_exit((cleanup_fn_t) ts_query_delete, query);

  uint32_t num_patterns = ts_query_pattern_count(query);
  const TSQueryPredicateStep **preds =
    malloc(sizeof(TSQueryPredicateStep*) * num_patterns);
  if (!preds) {
    Rf_error("Out of memory");
  }
  r_call_on_exit(r_free, preds);
  uint32_t *num_steps = malloc(sizeof(uint32_t) * num_patterns);
  for (uint32_t pt = 0; pt < num_patterns; pt++) {
    preds[pt] = ts_query_predicates_for_pattern(query, pt, num_steps + pt);
  }

  uint32_t capture_count = ts_query_capture_count(query);
  uint32_t *capture_map = malloc(sizeof(uint32_t) * capture_count);
  if (!capture_map) {
    Rf_error("Out of memory");
  }
  r_call_on_exit(r_free, capture_map);
  uint32_t *capture_map_pattern = malloc(sizeof(uint32_t) * capture_count);
  if (!capture_map_pattern) {
    Rf_error("Out of memory");
  }
  r_call_on_exit(r_free, capture_map_pattern);
  memset(capture_map_pattern, 0, sizeof(uint32_t) * capture_count);

  TSNode root = ts_tree_root_node(tree);

  uint32_t pattern_count = ts_query_pattern_count(query);
  SEXP result_matches = PROTECT(Rf_allocVector(VECSXP, 3));
  SET_VECTOR_ELT(result_matches, 0, Rf_allocVector(STRSXP, pattern_count));
  SET_VECTOR_ELT(result_matches, 1, Rf_allocVector(INTSXP, pattern_count));
  SET_VECTOR_ELT(result_matches, 2, Rf_allocVector(INTSXP, pattern_count));
  for (uint32_t i = 0; i < pattern_count; i++) {
    uint32_t start = ts_query_start_byte_for_pattern(query, i);
    uint32_t end = ts_query_end_byte_for_pattern(query, i);
    SET_STRING_ELT(
      VECTOR_ELT(result_matches, 0), i,
      Rf_mkCharLenCE(cpattern + start, end - start, CE_UTF8)
    );
    INTEGER(VECTOR_ELT(result_matches, 2))[i] = start + 1;
  }
  memset(
    INTEGER(VECTOR_ELT(result_matches, 1)),
    0,
    sizeof(int) * pattern_count
  );

  // TODO: we should allocate a DF here, probably
  PROTECT_INDEX rpi;
  SEXP result_matched_captures = Rf_allocVector(VECSXP, 100);
  PROTECT_WITH_INDEX(result_matched_captures, &rpi);
  uint32_t total_capture_count = 0, residx = 0;

  TSQueryCursor *cursor = ts_query_cursor_new();
  ts_query_cursor_exec(cursor, query, root);
  r_call_on_exit((cleanup_fn_t) ts_query_cursor_delete, cursor);
  TSQueryMatch match;
  uint32_t match_idx = 0;
  while (ts_query_cursor_next_match(cursor, &match)) {
    // Create a capture id -> capture_idx in match mapping
    // We point to the last node that has this capture id, and then we can
    // work backwards
    for (uint16_t cc = 0; cc < match.capture_count; cc++) {
      uint32_t cidx = match.captures[cc].index;
      // point to the first node
      if (capture_map_pattern[cidx] != match.pattern_index + 1) {
        capture_map_pattern[cidx] = match.pattern_index + 1;
        capture_map[cidx] = cc;
      }
    }

    // evaluate the predicates
    const TSQueryPredicateStep *mpreds = preds[match.pattern_index];
    uint32_t mnum_steps = num_steps[match.pattern_index];
    struct query_match_t qm = {
      query, &match, match.pattern_index, mpreds, mnum_steps, c_input,
      length, capture_map, capture_map_pattern
    };
    if (!check_predicates(&qm)) continue;

    match_idx++;
    INTEGER(VECTOR_ELT(result_matches, 1))[match.pattern_index] += 1;
    total_capture_count += match.capture_count;
    if (total_capture_count > Rf_length(result_matched_captures)) {
      REPROTECT(
        result_matched_captures = Rf_xlengthgets(
          result_matched_captures,
          total_capture_count * 2
        ),
        rpi
      );
    }

    // collect the results
    for (uint16_t cc = 0; cc < match.capture_count; cc++) {
      SEXP res1 = PROTECT(Rf_allocVector(VECSXP, 12));
      SET_VECTOR_ELT(result_matched_captures, residx++, res1);
      UNPROTECT(1);

      SET_VECTOR_ELT(res1, 0, Rf_ScalarInteger(match.pattern_index + 1));
      SET_VECTOR_ELT(res1, 1, Rf_ScalarInteger(match_idx));
      SET_VECTOR_ELT(res1, 2, Rf_ScalarInteger(match.captures[cc].index + 1));

      uint32_t cnl;
      const char *cn = ts_query_capture_name_for_id(
        query,
        match.captures[cc].index,
        &cnl
      );
      SET_VECTOR_ELT(res1, 3, Rf_ScalarString(Rf_mkCharLenCE(
        cn,
        cnl,
        CE_UTF8
      )));

      TSNode node = match.captures[cc].node;
      uint32_t start_byte = ts_node_start_byte(node);
      uint32_t end_byte = ts_node_end_byte(node);
      SET_VECTOR_ELT(res1, 4, Rf_ScalarString(Rf_mkCharLenCE(
        c_input + start_byte,
        end_byte - start_byte,
        CE_UTF8
      )));
      SET_VECTOR_ELT(res1, 5, Rf_ScalarInteger(start_byte));
      SET_VECTOR_ELT(res1, 6, Rf_ScalarInteger(end_byte));
      TSPoint start_point = ts_node_start_point(node);
      SET_VECTOR_ELT(res1, 7, Rf_ScalarInteger(start_point.row));
      SET_VECTOR_ELT(res1, 8, Rf_ScalarInteger(start_point.column));
      TSPoint end_point = ts_node_end_point(node);
      SET_VECTOR_ELT(res1, 9, Rf_ScalarInteger(end_point.row));
      SET_VECTOR_ELT(res1, 10, Rf_ScalarInteger(end_point.column));
      SET_VECTOR_ELT(res1, 11, Rf_mkString(ts_node_type(node)));
    }
  }

  REPROTECT(
    result_matched_captures = Rf_xlengthgets(
      result_matched_captures,
      total_capture_count),
    rpi
  );

  uint32_t query_capture_count = ts_query_capture_count(query);
  SEXP result_captures = Rf_protect(Rf_allocVector(STRSXP, query_capture_count));
  for (uint32_t i = 0; i < query_capture_count; i++) {
    uint32_t len;
    const char *cn = ts_query_capture_name_for_id(query, i, &len);
    SET_STRING_ELT(result_captures, i, Rf_mkCharLenCE(cn, len, CE_UTF8));
  }

  SEXP result = PROTECT(Rf_allocVector(VECSXP, 3));
  SET_VECTOR_ELT(result, 0, result_matches);
  SET_VECTOR_ELT(result, 1, result_captures);
  SET_VECTOR_ELT(result, 2, result_matched_captures);
  UNPROTECT(4);
  return result;
}
