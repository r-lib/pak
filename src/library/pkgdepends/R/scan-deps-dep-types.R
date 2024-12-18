get_dep_type_from_path <- function(paths, orig = NULL) {
  tps <- rep("prod", length(paths))
  tps[paths == "man/roxygen/meta.R"] <- "dev"
  tps[startsWith(paths, "tests/") | startsWith(paths, "test/")] <- "test"
  if (!is.null(orig)) {
    # for DESCRIPTION we detect the type from the file itself
    dsc <- basename(paths) == "DESCRIPTION"
    tps[dsc] <- orig[dsc]
  }
  tps
}

get_dep_type_from_description_field <- function(fields) {
  tps <- rep("dev", length(fields))
  tps[fields %in% c("Depends", "Imports", "LinkingTo")] <- "prod"
  tps[fields %in% c("Suggests", "Enhanced")] <- "test"
  tps[fields == "Config/Needs/coverage"] <- "test"
  tps
}
