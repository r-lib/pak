test_that("cross-compilation dummies cover all embedded package imports", {
  src_dir <- file.path(testthat::test_path(), "..", "..", "src")
  skip_if_not(
    file.exists(file.path(src_dir, "install-embedded.R")),
    "Source tree not available"
  )

  base_pkgs <- rownames(installed.packages(priority = c("base", "recommended")))

  # Packages available during cross-compilation:
  # 1. Dummy stubs from src/dummy/
  dummy_pkgs <- dir(file.path(src_dir, "dummy"))
  # 2. Real packages installed into the dummy lib by install_dummies().
  #    Parse install-embedded.R to find "library/<pkg>" strings (excluding
  #    the paste0("library/", pkg) pattern used by install_one()).
  source_lines <- readLines(file.path(src_dir, "install-embedded.R"))
  lib_lines <- grep('^\\s*"library/[^"]+",?\\s*$', source_lines, value = TRUE)
  real_in_dummy <- sub('.*"library/([^"]+)".*', "\\1", lib_lines)

  expect_true(
    length(real_in_dummy) > 0,
    label = "Parsed at least one real package from install_dummies()"
  )

  available <- c(dummy_pkgs, real_in_dummy)

  # Check every embedded library package
  library_pkgs <- basename(list.dirs(
    file.path(src_dir, "library"), full.names = TRUE, recursive = FALSE
  ))

  missing <- character()
  for (pkg in library_pkgs) {
    desc_file <- file.path(src_dir, "library", pkg, "DESCRIPTION")
    if (!file.exists(desc_file)) next

    dcf <- read.dcf(desc_file, fields = "Imports")
    imports_raw <- dcf[1, "Imports"]
    if (is.na(imports_raw)) next

    imports <- trimws(strsplit(imports_raw, ",")[[1]])
    imports <- sub("\\s*\\(.*\\)$", "", imports)

    for (imp in imports) {
      if (imp %in% base_pkgs) next
      if (imp %in% available) next
      missing <- c(missing, paste0(pkg, " -> ", imp))
    }
  }

  expect_equal(
    missing, character(),
    info = paste(
      "These imports are not satisfied during cross-compilation.",
      "Add a dummy in src/dummy/<pkg>/ (DESCRIPTION + empty NAMESPACE),",
      "or install the real package in install_dummies().",
      "See src/install-embedded.R for details.",
      paste(missing, collapse = "\n  "),
      sep = "\n  "
    )
  )
})
