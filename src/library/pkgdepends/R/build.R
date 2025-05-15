#' @importFrom utils modifyList

build_package <- function(path, build_args = list()) {
  default_args <- list(
    path = path,
    dest_path = NULL,
    binary = FALSE,
    vignettes = TRUE,
    manual = TRUE,
    args = NULL,
    quiet = TRUE
  )
  args <- modifyList(default_args, build_args)
  zip_path <- system.file(package = "zip", "bin", .Platform$r_arch)
  withr_with_path(zip_path, do.call(pkgbuild::build, args))
}
