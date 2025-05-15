async_system_list_packages <- function(config = NULL) {
  config <- config %||% current_config()
  sysreqs_platform <- config$get("sysreqs_platform")
  pkgtool <- sysreqs2_command(sysreqs_platform, "query")
  if (pkgtool == "dpkg-query") {
    async_system_list_packages_dpkg_query(config)
  } else if (pkgtool == "apk") {
    async_system_list_packages_apk(config)
  } else {
    async_system_list_packages_rpm(config)
  }
}

# For DEB systems we need
# dpkg-query -W -f '${db:Status-Abbrev} ${Package} ${Version} ${Provides}\n' '*'
# This also lists the packages that are not installed currently.
# First field is the status code, second is package name, then version
# number (if installed), then capabilities in a comma separated list.
# Capabilities may include version requirements.

async_system_list_packages_dpkg_query <- function(config) {
  args <- c(
    "-W",
    "-f",
    "${db:Status-Abbrev} ${Package} ${Version} ${Provides}\\n",
    "*"
  )
  stdout <- tempfile()
  external_process(function(...) {
    processx::process$new(
      "dpkg-query",
      stdout = stdout,
      stderr = stdout,
      args = args,
      ...
    )
  })$then(function(ret) {
    parse_dpkg_query_output(strsplit(ret$stdout, "\n")[[1]])
  })$finally(function() unlink(stdout))
}

parse_dpkg_query_output <- function(lines) {
  # If not installed, then not interesting
  lines <- lines[grepl("^.[^n]", lines)]
  status <- trimws(sub(" .*$", "", lines))
  rest <- sub("^[^ ]+[ ]+", "", lines)
  package <- trimws(sub(" .*$", "", rest))
  rest <- sub("^[^ ]+[ ]+", "", rest)
  version <- trimws(sub(" .*$", "", rest))
  rest <- sub("^[^ ]+[ ]+", "", rest)
  provides <- lapply(strsplit(rest, ",[ ]?"), trimws)
  # just drop version requirements, we probably don't need them
  provides <- lapply(provides, sub, pattern = "[ ].*$", replacement = "")
  provides <- lapply(provides, function(x) x[x != ""])
  # sorted by default
  data_frame(
    status = status,
    package = package,
    version = version,
    provides = provides
  )
}

# For RPM, we need this query:
# rpm -qa --provides --qf '---%{NAME}\n'
# The output is a bit weird, for each package first the capabilities are
# listed, then the package name, after `---`.
# We can use %{NAME} %{VERSION} if we want the version numbers as well.

async_system_list_packages_rpm <- function(config) {
  args <- c(
    "-qa",
    "--provides",
    "--qf",
    "---%{NAME} %{VERSION}\\n"
  )
  stdout <- tempfile()
  external_process(function(...) {
    processx::process$new(
      "rpm",
      stdout = stdout,
      stderr = stdout,
      args = args,
      ...
    )
  })$then(function(ret) {
    parse_rpm_output(strsplit(ret$stdout, "\n")[[1]])
  })$finally(function() unlink(stdout))
}

parse_rpm_output <- function(lines) {
  last <- which(grepl("^---", lines))
  from <- c(1L, last[-length(last)] + 1L)
  to <- c(last)
  blocks <- mapply(from, to, FUN = function(f, t) lines[f:t])
  pkglines <- sub("^---", "", vcapply(blocks, utils::tail, 1))
  package <- trimws(sub(" .*$", "", pkglines))
  version <- trimws(sub("^[^ ]+[ ]+", "", pkglines))
  provides <- mapply(
    package,
    blocks,
    USE.NAMES = FALSE,
    FUN = function(pkg, blk) {
      blk <- utils::head(blk, -1)
      # the package itself is a capability, drop that
      # TODO: drop the pkg(arch) = version capabilties as well?
      blk <- blk[!startsWith(blk, paste0(pkg, " = "))]
      # drop version numbers
      blk <- sub(" .*$", "", blk)
      trimws(blk)
    }
  )

  pkgs <- data_frame(
    status = "ii",
    package = package,
    version = version,
    provides = provides
  )

  pkgs <- pkgs[order(tolower(pkgs$package)), ]
  pkgs
}

# For APK, we need this query:

async_system_list_packages_apk <- function(config) {
  args <- c(
    "list",
    "-I"
  )
  stdout <- tempfile()
  external_process(function(...) {
    processx::process$new(
      "apk",
      stdout = stdout,
      stderr = stdout,
      args = args,
      ...
    )
  })$then(function(ret) {
    parse_apk_output(strsplit(ret$stdout, "\n")[[1]])
  })$finally(function() unlink(stdout))
}

parse_apk_output <- function(lines) {
  package = sub("-[0-9].*", "", lines)
  version = sub(".*?-([0-9][^ ]*).*", "\\1", lines)
  provides = ""

  pkgs <- data_frame(
    status = "ii",
    package = package,
    version = version,
    provides = provides
  )

  pkgs <- pkgs[order(tolower(pkgs$package)), ]
  pkgs
}
