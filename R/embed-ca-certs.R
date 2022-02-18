
embed_ca_certs <- function() {
    certfile <- file.path(system.file(package = "pak"), "curl-ca-bundle.crt")
    utils::download.file("https://curl.se/ca/cacert.pem", certfile)
}
