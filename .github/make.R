
builds <- read.csv(
  stringsAsFactors = FALSE, strip.white = TRUE,
  textConnection('
    ostype,   os,               rversion
    "macos",  "macOS-latest",   "3.3"
    "macos",  "macOS-latest",   "3.4"
    "macos",  "macOS-latest",   "3.5"
    "macos",  "macOS-latest",   "3.6"
    "macos",  "macOS-latest",   "4.0"
    "macos",  "macOS-latest",   "devel/4.1"

    "linux",  "ubuntu-latest",  "3.3"
    "linux",  "ubuntu-latest",  "3.4"
    "linux",  "ubuntu-latest",  "3.5"
    "linux",  "ubuntu-latest",  "3.6"
    "linux",  "ubuntu-latest",  "4.0"
    "linux",  "ubuntu-latest",  "devel/4.1"

    "win",    "windows-2016",   "3.3"
    "win",    "windows-2016",   "3.4"
    "win",    "windows-2016",   "3.5"
    "win",    "windows-2016",   "3.6"
    "win",    "windows-2016",   "4.0"
    "win",    "windows-2016",   "devel/4.1"
 ')
)

input <- ".github/build.tmpl"

for (i in seq_len(nrow(builds))) {
  ostype   <- builds$ostype[i]
  os       <- builds$os[i]
  rversion <- builds$rversion[i]
  numver <- sub("^[a-z]+/", "", rversion)
  output <- sprintf(".github/workflows/build-%s-R-%s.yaml", ostype, numver)
  brew::brew(input, output)
}
