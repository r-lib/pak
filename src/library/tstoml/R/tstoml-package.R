#' @keywords internal
"_PACKAGE"

#' @import ts
NULL

## usethis namespace: start
#' @useDynLib tstoml, .registration = TRUE, .fixes = "c_"
## usethis namespace: end
NULL

#' @name quickstart
#' @title tstoml quickstart
#' @details
#'
#' ```{r, child = "tools/man/quickstart.Rmd"}
#' ```
NULL

#' Example TOML text
#' @export
#' @examples
#' writeLines(toml_example_text())

toml_example_text <- function() {
  '# This is a TOML document

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00-08:00

[database]
enabled = true
ports = [ 8000, 8001, 8002 ]
data = [ ["delta", "phi"], [3.14] ]
temp_targets = { cpu = 79.5, case = 72.0 }

[servers]

[servers.alpha]
ip = "10.0.0.1"
role = "frontend"

[servers.beta]
ip = "10.0.0.2"
role = "backend"
'
}
