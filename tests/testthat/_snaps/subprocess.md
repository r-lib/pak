# remote messages

    Code
      invisible(remote(function() cli::cli_alert_info("just once")))
    Message <callr_message>
      ℹ just once

---

    Code
      withCallingHandlers(invisible(remote(function() cli::cli_alert_info("just once"))),
      message = function(m) print(m))
    Output
      <callr_message: ℹ just once
      >
    Message <callr_message>
      ℹ just once

