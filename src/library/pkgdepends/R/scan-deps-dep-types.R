get_dep_type_from_path <- function(paths) {
  type <- rep("prod", length(paths))
  type[paths == "man/roxygen/meta.R"] <- "dev"
  type[startsWith(paths, "tests/") | startsWith(paths, "test/")] <- "test"
  type
}
