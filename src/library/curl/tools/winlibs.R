if(!file.exists("curl.o") && !file.exists("../.deps/libcurl/include/curl/curl.h")){
  unlink("../.deps", recursive = TRUE)
  url <- if(grepl("aarch", R.version$platform)){
    "https://github.com/r-windows/bundles/releases/download/curl-8.14.1/curl-8.14.1-clang-aarch64.tar.xz"
  } else if(grepl("clang", Sys.getenv('R_COMPILED_BY'))){
    "https://github.com/r-windows/bundles/releases/download/curl-8.14.1/curl-8.14.1-clang-x86_64.tar.xz"
  } else if(getRversion() >= "4.2") {
    "https://github.com/r-windows/bundles/releases/download/curl-8.14.1/curl-8.14.1-ucrt-x86_64.tar.xz"
  } else {
    "https://github.com/rwinlib/libcurl/archive/v7.84.0.tar.gz"
  }
  download.file(url, basename(url), quiet = TRUE)
  dir.create("../.deps", showWarnings = FALSE)
  untar(basename(url), exdir = "../.deps", tar = 'internal')
  unlink(basename(url))
  setwd("../.deps")
  file.rename(list.files(), 'libcurl')
  invisible()
}
