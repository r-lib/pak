
build_pak_binary_linux <- function() {
  ver <- as.character(utils::packageVersion("pak"))
  rver <- paste0("R", gsub(".", "-", getRversion()[,1:2], fixed = TRUE))
  platform <- R.Version()$platform
  platform <- sub("-(gnu|musl)$", "", platform)
  lib <- dirname(system.file(package = "pak"))
  pkg_file <- paste0("pak_", ver, "_", rver, "_", platform, ".tar.gz")

  withr::with_dir(lib, {
      utils::tar(
          pkg_file,
          files = "pak",
          tar = "internal",
          compression = "gzip",
          compression_level = 9
      )
  })

  local <- file.path("/tmp", pkg_file)

  file.copy(file.path(lib, pkg_file), local, overwrite = TRUE)

  pkg_file
}
