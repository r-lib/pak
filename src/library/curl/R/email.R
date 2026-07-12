#' Send email
#'
#' Use the curl SMTP client to send an email. The `message` argument must be
#' properly formatted [RFC2822](https://www.rfc-editor.org/rfc/rfc2822) email message with From/To/Subject headers and CRLF
#' line breaks.
#'
#' @section Specifying the server, port, and protocol:
#'
#' The `smtp_server` argument takes a hostname, or an SMTP URL:
#'
#' \itemize{
#'   \item `mail.example.com` - hostname only
#'   \item `mail.example.com:587` - hostname and port
#'   \item `smtp://mail.example.com` - protocol and hostname
#'   \item `smtp://mail.example.com:587` - full SMTP URL
#'   \item `smtps://mail.example.com:465` - full SMTPS URL
#' }
#'
#' By default, the port will be 25, unless `smtps://` is specified--then
#' the default will be 465 instead.
#'
#' For internet SMTP servers you probably need to pass a
#' [username](https://curl.se/libcurl/c/CURLOPT_USERNAME.html) and
#' [passwords](https://curl.se/libcurl/c/CURLOPT_PASSWORD.html) option.
#' For some servers you also need to pass a string with
#' [login_options](https://curl.se/libcurl/c/CURLOPT_LOGIN_OPTIONS.html)
#' for example `login_options="AUTH=NTLM"`.
#'
#' @section Encrypting connections via SMTPS or STARTTLS:
#'
#' There are two different ways in which SMTP can be encrypted: SMTPS servers
#' run on a port which only accepts encrypted connections, similar to HTTPS.
#' Alternatively, a regular insecure smtp connection can be "upgraded" to a
#' secure TLS connection using the STARTTLS command. It is important to know
#' which method your server expects.
#'
#' If your smtp server listens on port 465, then use a  `smtps://hostname:465`
#' URL. The SMTPS protocol *guarantees* that TLS will be used to protect
#' all communications from the start.
#'
#' If your email server listens on port 25 or 587, use an `smtp://` URL in
#' combination with the  `use_ssl` parameter to control if the connection
#' should be upgraded with STARTTLS. The default value `"try"` will
#' *opportunistically* try to upgrade to a secure connection if the server
#' supports it, and proceed as normal otherwise.
#'
#' @export
#' @param mail_rcpt one or more recipient email addresses. Do not include names,
#' these go into the `message` headers.
#' @param mail_from email address of the sender.
#' @param message either a string or connection with (properly formatted) email
#' message, including sender/recipient/subject headers. See example.
#' @param smtp_server hostname or address of the SMTP server, or, an
#' `smtp://` or `smtps://` URL. See "Specifying the server, port,
#' and protocol" below.
#' @param use_ssl Request to upgrade the connection to SSL using the STARTTLS command,
#' see [CURLOPT_USE_SSL](https://curl.se/libcurl/c/CURLOPT_USE_SSL.html)
#' for details. Default will try to SSL, proceed as normal otherwise.
#' @param verbose print output
#' @param ... other options passed to [handle_setopt()]. In most cases
#' you will need to set a `username` and `password` or `login_options`
#' to authenticate with the SMTP server, see details.
#' @examples \dontrun{# Set sender and recipients (email addresses only)
#' recipients <- readline("Enter your email address to receive test: ")
#' sender <- 'test@noreply.com'
#'
#' # Full email message in RFC2822 format
#' message <- 'From: "R (curl package)" <test@noreply.com>
#' To: "Roger Recipient" <roger@noreply.com>
#' Subject: Hello R user!
#'
#' Dear R user,
#'
#' I am sending this email using curl.'
#'
#' # Send the email
#' send_mail(sender, recipients, message, smtp_server = 'smtps://smtp.gmail.com',
#'   username = 'curlpackage', password  = 'qyyjddvphjsrbnlm')}
send_mail <- function(mail_from, mail_rcpt, message, smtp_server = 'smtp://localhost',
                      use_ssl = c("try", "no", "force"), verbose = TRUE, ...){
  if(!grepl('://', smtp_server)) {
    # no protocol was provided
    smtp_server <- if (grepl(":465$", smtp_server)) {
      paste0('smtps://', smtp_server)
    } else {
      paste0('smtp://', smtp_server)
    }
  }
  if (!grepl('^smtps?://', smtp_server)) {
    stop('smtp_server invalid; only smtp:// and smtps:// URLs are supported')
  }

  # Values from: curl_symbols('CURLUSESSL')
  if(is.character(use_ssl)){
    use_ssl <- switch(match.arg(use_ssl), no = 0, try = 1, force = 2)
  }

  if(is.character(message))
    message <- charToRaw(paste(message, collapse = '\r\n'))
  con <- if(is.raw(message)){
    rawConnection(message)
  } else if(inherits(message, "connection")){
    if(!isOpen(message))
      open(message, 'rb')
    message
  } else {
    stop("Body must be a string, raw vector, or connection object")
  }
  on.exit(close(con))
  total_bytes <- 0
  h <- new_handle(upload = TRUE, readfunction = function(nbytes, ...) {
    buf <- readBin(con, raw(), nbytes)
    total_bytes <<- total_bytes + length(buf)
    if(verbose){
      if(length(buf)){
        cat(sprintf("\rUploaded %.0f bytes...", total_bytes), file = stderr())
      } else {
        cat(sprintf("\rUploaded %.0f bytes... all done!\n", total_bytes), file = stderr())
      }
    }
    return(buf)
  }, mail_from = mail_from, mail_rcpt = mail_rcpt, use_ssl = use_ssl,
      verbose = verbose, ...)
  curl_fetch_memory(smtp_server, handle = h)
}
