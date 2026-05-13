#' @ts ts_tree_select_true TOML example
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml('
#'   a = 1
#'   b = [10, 20, 30]
#'   [c]
#'   c1 = true
#'   c2 = []
#' ')
#' toml |> ts_tree_select(c("b", "c"), TRUE)
#' ```
#'
#' @ts ts_tree_select_character TOML example
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml('
#'   a = 1
#'   b = [10, 20, 30]
#'   [c]
#'   c1 = true
#'   c2 = []
#' ')
#' toml |> ts_tree_select(c("a", "c"), "c1")
#' ```
#'
#' @ts ts_tree_select_integer TOML example
#'
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml('
#'   a = 1
#'   b = [10, 20, 30]
#'   [c]
#'   c1 = true
#'   c2 = []
#' ')
#' toml |> ts_tree_select(c("b", "c"), -1)
#' ```
#'
#' @ts ts_tree_select_regex TOML example
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml(
#'  'apple = 1\nalmond = 2\nbanana = 3\ncherry = 4\n'
#' )
#' toml |> ts_tree_select(regex = "^a")
#' ```
#'
#' @ts ts_tree_select_tsquery TOML example
#'
#' See [tstoml::ts_language_toml()] for details on the TOML grammar.
#'
#' <p>
#'
#' This example selects all integers in the TOML document.
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml(
#'   'a = 1\nb = [10, 20, 30]\nc = { c1 = true, c2 = 100 }\n'
#' )
#' toml |> ts_tree_select(query = "(integer) @integer")
#' ```
#'
#' @ts ts_tree_select_ids TOML example
#'
#' ```{asciicast}
#' toml <- tstoml::ts_parse_toml(
#'   'a = 1\nb = [10, 20, 30]\nc = { c1 = true, c2 = [] }\n'
#' )
#' ts_tree_dom(toml)
#' ```
#'
#' ```{asciicast}
#' toml |> ts_tree_select(I(9))
#' ```
#'
#' @ts ts_tree_select_examples TOML examples
#'
#' ## Examples
#'
#' ```{asciicast}
#' #| results: hide
#' txt <- r"(
#' # This is a TOML document
#'
#' title = "TOML Example"
#'
#' [owner]
#' name = "Tom Preston-Werner"
#' dob = 1979-05-27T07:32:00-08:00
#'
#' [database]
#' enabled = true
#' ports = [ 8000, 8001, 8002 ]
#' data = [ ["delta", "phi"], [3.14] ]
#' temp_targets = { cpu = 79.5, case = 72.0 }
#'
#' [servers]
#'
#' [servers.alpha]
#' ip = "10.0.0.1"
#' role = "frontend"
#'
#' [servers.beta]
#' ip = "10.0.0.2"
#' role = "backend"
#' )"
#' toml <- ts_parse_toml(text = txt)
#' ```
#'
#' Pretty print a tstoml object:
#'
#' ```{asciicast}
#' #| label: print-toml
#' toml
#' ```
#'
#' Select elements in a tstoml object
#'
#' ```{asciicast}
#' #| label: select-table
#' ts_tree_select(toml, "owner")
#' ```
#'
#' Select element(s) inside elements:
#'
#' ```{asciicast}
#' #| label: select-select
#' ts_tree_select(toml, "owner", "name")
#' ```
#'
#' Select element(s) of an array:
#'
#' ```{asciicast}
#' #| label: select-array
#' ts_tree_select(toml, "database", "ports", 1:2)
#' ```
#'
#' Select multiple keys from a table:
#'
#' ```{asciicast}
#' #| label: select-multiple
#' ts_tree_select(toml, "owner", c("name", "dob"))
#' ```
NULL

#' @export

ts_tree_select1.ts_tree_toml.ts_tree_selector_default <- function(
  tree,
  node,
  slt
) {
  1L
}

#' @export

ts_tree_mark_selection1.ts_tree_toml <- function(tree, node) {
  parent <- tree$parent[node]
  if (tree$type[node] == "table_array_element") {
    tree$parent[tree$dom_children[[node]]]
  } else if (
    !is.na(parent) &&
      tree$type[parent] == "table_array_element" &&
      tree$type[node] %in% c("bare_key", "quoted_key", "dotted_key")
  ) {
    # this is AOT _element_, select it like a table, but not the [[ ]] tokens
    tree$children[[parent]][c(-1, -3)]
  } else if (tree$type[node] %in% c("bare_key", "quoted_key")) {
    get_dom_subtree(tree, node, with_root = FALSE)
  } else {
    node
  }
}

get_dom_subtree <- function(tree, id, with_root = FALSE) {
  sel <- c(if (with_root) id, tree$dom_children[[id]])
  while (TRUE) {
    sel2 <- unique(c(sel, unlist(tree$dom_children[sel])))
    if (length(sel2) == length(sel)) {
      return(sel)
    }
    sel <- sel2
  }
}

#' @title
#' Select parts of a TOML tree-sitter tree
#' @usage
#' \method{ts_tree_select}{ts_tree_toml}(tree, ..., refine = FALSE)
#' @param tree
#' \eval{ts:::doc_insert("ts::ts_tree_select_param_tree", "tstoml")}
#' @param ...
#' \eval{ts:::doc_insert("ts::ts_tree_select_param_dots", "tstoml")}
#' @param refine
#' \eval{ts:::doc_insert("ts::ts_tree_select_param_refine", "tstoml")}
#' @return
#' \eval{ts:::doc_insert("ts::ts_tree_select_return", "tstoml")}
#'
#' @description
#' \eval{ts:::doc_insert("ts::ts_tree_select_description", "tstoml")}
#'
#' This is the S3 method of the [ts::ts_tree_select()] generic,
#' for [tstoml] objects.
#'
#' @details
#' \eval{ts:::doc_insert("ts::ts_tree_select_details", "tstoml")}
#' \eval{ts:::doc_insert("tstoml::ts_tree_select_examples", "tstoml")}
#' \eval{ts:::doc_extra()}
#' @export

ts_tree_select.ts_tree_toml <- function(tree, ..., refine = FALSE) {
  NextMethod()
}
