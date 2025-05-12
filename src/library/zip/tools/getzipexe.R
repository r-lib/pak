if (getRversion() < "3.3.0") setInternet2()

if (!file.exists("../tools/zip.exe")) {
  download.file(
    "https://github.com/rwinlib/zip/blob/master/zip.exe?raw=true",
    "../tools/zip.exe",
    quiet = TRUE,
    mode = "wb"
  )
}

file.copy("../tools/zip.exe", "tools/zip.exe")
