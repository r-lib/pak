
embed_ca_certs <- function() {
    certfile <- file.path(system.file(package = "pak"), "curl-ca-bundle.crt")
    # utils::download.file("https://curl.se/ca/cacert.pem", certfile  )
    download_file("https://curl.se/ca/cacert.pem",   certfile , method = NULL    )
}
get_env_safe <-function(key  , default = NULL ){
 try({
    default = Sys.getenv(key )
  })
  default
}

download_file<-function(url,
                        destfile ,
                        method = NULL  ,
                        quiet = FALSE,
                        mode = "w",
                        cacheOK = TRUE,
                        extra = getOption("download.file.extra"),
                        headers = NULL,
                        ...){

  if(is.null(method)){
    method <- get_env_safe( "pak_ENV_download_file_method" , default = "libcurl" )
  }
    
  utils::download.file(url ,
                       destfile ,
                       method = method  , 
                       quiet = quiet ,
                       mode = mode ,
                       cacheOK = cacheOK ,
                       extra = extra, 
                       headers  =headers
                       , ...  )
}
