version_info <- list(
  "2.11" = list(
    version_min = "2.10.0",
    version_max = "2.11.1",
    path = c("bin", "perl/bin", "MinGW/bin")
  ),
  "2.12" = list(
    version_min = "2.12.0",
    version_max = "2.12.2",
    path = c("bin", "perl/bin", "MinGW/bin", "MinGW64/bin")
  ),
  "2.13" = list(
    version_min = "2.13.0",
    version_max = "2.13.2",
    path = c("bin", "MinGW/bin", "MinGW64/bin")
  ),
  "2.14" = list(
    version_min = "2.13.0",
    version_max = "2.14.2",
    path = c("bin", "MinGW/bin", "MinGW64/bin")
  ),
  "2.15" = list(
    version_min = "2.14.2",
    version_max = "2.15.1",
    path = c("bin", "gcc-4.6.3/bin")
  ),
  "2.16" = list(
    version_min = "2.15.2",
    version_max = "3.0.0",
    path = c("bin", "gcc-4.6.3/bin")
  ),
  "3.0" = list(
    version_min = "2.15.2",
    version_max = "3.0.99",
    path = c("bin", "gcc-4.6.3/bin")
  ),
  "3.1" = list(
    version_min = "3.0.0",
    version_max = "3.1.99",
    path = c("bin", "gcc-4.6.3/bin")
  ),
  "3.2" = list(
    version_min = "3.1.0",
    version_max = "3.2.99",
    path = c("bin", "gcc-4.6.3/bin")
  ),
  "3.3" = list(
    version_min = "3.2.0",
    version_max = "3.3.99",
    path = if (using_gcc49()) {
      "bin"
    } else {
      c("bin", "gcc-4.6.3/bin")
    }
  ),
  "3.4" = list(
    version_min = "3.3.0",
    version_max = "3.6.3",
    path = "bin"
  ),
  "3.5" = list(
    version_min = "3.3.0",
    version_max = "3.6.3",
    path = "bin"
  ),
  "4.0" = list(
    version_min = "4.0.0",
    version_max = "4.1.99",
    path = c("usr/bin", "ucrt64/bin")
  ),
  "4.2" = list(
    version_min = "4.2.0",
    version_max = "4.2.99",
    path = "usr/bin"
  ),
  "4.3" = list(
    version_min = "4.3.0",
    version_max = "4.3.99",
    path = "usr/bin"
  ),
  "4.4" = list(
    version_min = "4.4.0",
    version_max = "4.4.99",
    path = "usr/bin"
  ),
  "4.5" = list(
    version_min = "4.5.0",
    version_max = "99.99.99",
    path = character()
  ),
  "custom" = list(
    version_min = "2.10.0",
    version_max = "99.99.99",
    path = if (getRversion() >= "4.4.0") {
      character()
    } else if (getRversion() >= "4.0.0") {
      "usr/bin"
    } else {
      "bin"
    }
  )
)
