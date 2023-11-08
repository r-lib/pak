if(!file.exists("../windows/libcurl/include/curl/curl.h")){
  unlink("../windows", recursive = TRUE)
  url <- if(grepl("aarch", R.version$platform)){
    "https://github.com/r-windows/bundles/releases/download/curl-8.3.0/curl-8.3.0-clang-aarch64.tar.xz"
  } else if(grepl("clang", Sys.getenv('R_COMPILED_BY'))){
    "https://github.com/r-windows/bundles/releases/download/curl-8.3.0/curl-8.3.0-clang-x86_64.tar.xz"
  } else if(getRversion() >= "4.2") {
    "https://github.com/r-windows/bundles/releases/download/curl-8.3.0/curl-8.3.0-ucrt-x86_64.tar.xz"
  } else {
    "https://github.com/rwinlib/libcurl/archive/v7.84.0.tar.gz"
  }
  download.file(url, basename(url), quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  untar(basename(url), exdir = "../windows", tar = 'internal')
  unlink(basename(url))
  setwd("../windows")
  file.rename(list.files(), 'libcurl')
}
