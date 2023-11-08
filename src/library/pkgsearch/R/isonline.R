
is_rcmd_check <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    FALSE
  } else {
    Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
  }
}

is_online <- local({
  online <- TRUE
  expires <- Sys.time()
  function() {
    if (is_rcmd_check()) return(FALSE)
    t <- Sys.time()
    if (t >= expires) {
      online <<- pingr::is_online()
      expires <<- t + as.difftime(10, units = "secs")
    }
    online
  }
})
