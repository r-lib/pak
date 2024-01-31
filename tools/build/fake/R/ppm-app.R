ppm_app <- function() {
  app <- webfakes::new_app()

  app$get("/ppmversions", function(req, res) {
    res$send_json(
      text = readLines("fixtures/ppm-versions.json")
    )
  })

  app$get("/ppmstatus", function(req, res) {
    res$send_json(
      text = readLines("fixtures/ppm-status.json")
    )
  })

  app$get("/rversions", function(req, res) {
    res$send_json(
      text = readLines("fixtures/r-versions.json")
    )
  })

  app$get("/crandb/:pkg", function(req, res) {
    if (req$params$pkg == "dplyr") {
      res$send_json(
        text = readLines(gzfile("fixtures/dplyr.json.gz"))
      )
    } else if (req$params$pkg == "bad") {
      res$send_status(401)
    } else {
      res$send_status(404)
    }
  })

  app
}
