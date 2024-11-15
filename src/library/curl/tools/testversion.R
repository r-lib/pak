ver <- libcurlVersion()
cat("Curl runtime version", ver, "\n")
q('no', status = (numeric_version(ver) < commandArgs(TRUE)))
