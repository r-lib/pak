# remote messages

    Code
      invisible(remote(function() cli::cli_text("just once")))
    Message <callr_message>
      just once

---

    Code
      withCallingHandlers(invisible(remote(function() cli::cli_text("just once"))),
      message = function(m) print(m))
    Output
      <callr_message: just once
      >
    Message <callr_message>
      just once

