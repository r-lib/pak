build_pak_binary_linux <- function(lib) {
  ver <- as.character(utils::packageVersion("pak", lib.loc = lib))
  rver <- paste0("R", gsub(".", "-", getRversion()[, 1:2], fixed = TRUE))
  curlpkg <- readRDS(
    file.path(lib, "pak", "library", "curl", "Meta", "package.rds")
  )
  platform <- curlpkg[["Built"]][["Platform"]]
  platform <- sub("-(gnu|musl)$", "", platform)
  pkg_file <- paste0("pak_", ver, "_", rver, "_", platform, ".tar.gz")

  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(lib)

  utils::tar(
    pkg_file,
    files = "pak",
    tar = "internal",
    compression = "gzip",
    compression_level = 9
  )
  setwd(wd)

  local <- file.path("/tmp", pkg_file)

  file.copy(file.path(lib, pkg_file), local, overwrite = TRUE)

  local
}
