.onAttach <- function(libname, pkgname){
  version <- curl_version()
  ssl <- sub("\\(.*\\)\\W*", "", version$ssl_version)
  msg <- paste("Using libcurl", version$version, "with", ssl)
  packageStartupMessage(msg)
  if(grepl("redhat", R.version$platform) && !('smtp' %in% version$protocols)){
    packageStartupMessage(c("Your system runs libcurl-minimal which does not support all protocols: ",
                          "See also https://github.com/jeroen/curl/issues/350"))
  }
}

.onLoad <- function(libname, pkgname){
  assign("option_type_table", make_option_type_table(), environment(.onLoad))
}
