
if (is.null(sys.calls())) {
  mydir <- dirname(sub("^--file=", "", grep("^--file", commandArgs(), value = TRUE)[1]))
  package <- pkgload::pkg_name(mydir)
  testthat::test_dir(
    mydir,
    package = package,
    reporter = "progress",
    load_package = "source"
  )
}
