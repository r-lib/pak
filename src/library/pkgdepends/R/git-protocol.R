#' git protocol notes, for developers
#'
#' Assumptions, they might be relaxed or checked for later:
#' - The server must speak the smart protocol, version 1 or 2.
#'   (Although I added some functions specifically for the dumb protocol,
#'   but they are pretty limited.)
#' - We use HTTP transport, not SSH.
#' - The server should have the `shallow` capability.
#' - The server should have the `filter` capability if protocol version 2.
#' - Only SHA-1 hashing is supported.
#'
#' Improvements needed:
#' - DONE Tests. (Can always have more.)
#' - DONE Use async HTTP.
#' - DONE Support packfiles with deltas. (`ofs-delta` objects are still
#'   not supported.)
#' - DONE Optionally send authorization. Already possibly in the URL.
#' - DONE Better error messages.
#' - Better errors for non-existing user, repository, ref, PR, etc.
#'
#' Optional improvements:
#' - Support `ofs-delta` objects in packfiles. Not necessarily, unless we
#'   send this capability, the server is not sending `ofs-delta` objects.
#' - Make unpacking faster. It is not fast currently, with all the bit
#'   arithmetic in R. But it is already faster than a `tar.gz` + uncompress
#'   download from GitHub, so not really needed.
#'
#' ## Docs and other helpful links:
#' - <https://github.com/git/git/blob/master/Documentation/gitprotocol-common.txt>
#' - <https://github.com/git/git/blob/master/Documentation/gitprotocol-pack.txt>
#' - <https://github.com/git/git/blob/master/Documentation/gitprotocol-v2.txt>
#' - <https://github.com/calebsander/git-internals/blob/part2/src/main.rs>
#' - <https://dev.to/calebsander/git-internals-part-1-the-git-object-model-474m>
#'
#' @keywords internal
#' @name git-protocol
NULL

# -------------------------------------------------------------------------

git_creds_for_url <- function(url) {
  creds <- tryCatch(
    gitcreds_get(url)[c("username", "password")],
    error = function(e) NULL
  )
  if (is.null(creds)) {
    do.call(
      Sys.setenv,
      structure(list("FAIL"), names = gitcreds_cache_envvar(url))
    )
  }
  creds
}

git_http_get <- function(url, options = list(), ...) {
  options <- c(options, git_creds_for_url(url))
  http_get(url, options = options, ...)
}

git_http_post <- function(url, options = list(), ...) {
  options <- c(options, git_creds_for_url(url))
  http_post(url, options = options, ...)
}

#' List references in a remote git repository
#'
#' @details
#' * Branches have references named `refs/heads/<branch>`, e.g.
#' `refs/heads/main`.
#' * Tags have references named `refs/tags/<tag>`, e.g. `refs/tags/v1.0.2`.
#' * On GitHub pull requests have references named
#'   `refs/pull/<pr-number>/head`, e.g. `refs/pull/37/head`.
#'   For open pull requests there should be a `refs/pull/<pr-number>/merge`
#'   reference as well, which is the branch after the pull request has been
#'   merged.
#' * There is a special reference called `HEAD`. This points to the default
#'   branch on GitHub.
#'
#' @param url Repository URL, e.g. `https://github.com/r-lib/pak.git`.
#'   It might include authentication information, e.g. a GitHub token.
#' @param prefixes If not `NULL`, then only references of which one of
#'   `prefixes` is a prefix, are listed.
#' @return A list with entries: `refs` and `caps`.
#'   `caps` is a character vector of capabilities advertised by the server.
#'   `refs` is a data frame of git refs, it has columns `ref` and `hash`.
#'
#' @keywords internal
#' @section Examples:
#'
#' ```{r, git-list-refs, cache = TRUE}
#' git_list_refs("https://github.com/r-lib/filelock.git")
#' ```
#'
#' List only references having a certain prefix:
#'
#' ```{r git-list-refs-2, cache = TRUE}
#' git_list_refs("https://github.com/r-lib/filelock.git", "refs/heads/main")
#' ```
#'
#' Various services:
#' ```{r git-list-refs-2, cache = TRUE}
#' git_list_refs("https://gitlab.com/Linaro/tuxmake.git", "HEAD")
#' git_list_refs("https://bitbucket.org/gaborcsardi/cli.git")
#' git_list_refs("https://git.savannah.nongnu.org/git/administration/savane.git")
#' ```

git_list_refs <- function(url, prefixes = NULL) {
  synchronize(async_git_list_refs(url, prefixes))
}

async_git_list_refs <- function(url, prefixes = NULL) {
  async_git_list_refs_v2(url, prefixes)
}

# -------------------------------------------------------------------------

#' List files in a remote git repository
#'
#' @inheritParams git_list_refs
#' @param ref Either a SHA or a ref name. It may also be a branch name
#'   without the `refs/heads` prefix, or a partial (but unique) SHA of
#'   at least seven hexadecimal digits.
#'   See [git_list_refs()] for how branches, tags and GitHub pull
#'   requests are named.
#' @return A list with entries:
#'   * `ref`: The `ref` the function was called with.
#'   * `sha`: SHA of `ref`.
#'   * `commit`: named character vector of the data of the commit object
#'      belonging to `ref`. It has fields named: `tree`, `parent`,
#'      `author`, `committer`, `encoding`, `message`.
#'   * `tree`: SHA of the `tree` object belonging to `commit`.
#'   * `files`: a data frame of files and directories. It has columns:
#'        - `hash`: SHA hash of the tree or blob.
#'        - `type`: Either `"tree"` or `"blob"`.
#'        - `mode`: Unix mode.
#'        - `path`: Relative path from the repository's root directory.
#'
#' @keywords internal
#' @section Examples:
#' ```{r git-list-files, cache = TRUE}
#' fls <- git_list_files("https://github.com/r-lib/pak.git", "refs/heads/main")
#' fls$files
#' ```

git_list_files <- function(url, ref = "HEAD") {
  synchronize(async_git_list_files(url, ref))
}

async_git_resolve_ref <- function(url, ref) {
  url
  ref
  sha <- ref

  if (!grepl("^[0-9a-f]{40}$", ref)) {
    # Only use 'ref' as a filter if it is not a sha prefix of at least 7 chars
    filt <- if (nchar(ref) < 7 || !grepl("^[0-9a-f]+$", ref)) {
      paste0(c("", "refs/heads/", "refs/tags/"), ref)
    }
    async_git_list_refs(url, filt)$catch(
      error = function(e) async_git_list_refs_v1(url)
    )$then(function(refs) {
      result <- if (ref %in% refs$refs$ref) {
        refs$refs$hash[refs$refs$ref == ref]
      } else if (paste0("refs/tags/", ref) %in% refs$refs$ref) {
        refs$refs$hash[refs$refs$ref == paste0("refs/tags/", ref)]
      } else if (paste0("refs/heads/", ref) %in% refs$refs$ref) {
        refs$refs$hash[refs$refs$ref == paste0("refs/heads/", ref)]
      } else if (any(startsWith(refs$refs$hash, ref))) {
        sha <- unique(refs$refs$hash[startsWith(refs$refs$hash, ref)])
        if (length(sha) > 1) {
          throw(pkg_error(
            "Found multiple git refs with prefix {.val {ref}}, it is ambiguous.",
            "i" = "Matching git refs: {.val {sha}}.",
            "i" = "Specify a longer prefix to choose a single git ref."
          ))
        }
        sha
      } else {
        throw(pkg_error(
          "Unknown git ref: {.val {ref}}.",
          .class = "git_proto_error_unknown_ref",
          .data = list(ref = ref, url = redact_url(url))
        ))
      }

      attr(result, "protocol") <- if ("version 2" %in% refs$caps) 2 else 1
      attr(result, "filter") <- any(grepl("\\bfilter\\b", refs$caps))
      result
    })
  } else {
    async_constant(ref)
  }
}

async_git_list_files <- function(url, ref = "HEAD") {
  url
  ref
  sha2 <- ref
  async_git_resolve_ref(url, ref)$then(function(sha) {
    sha2 <<- sha
    async_git_fetch(url, sha)
  })$then(function(pf) async_git_list_files_process(pf, ref, sha2, url))
}

async_git_list_files_process <- function(packfile, ref, sha, url) {
  names(packfile) <- vcapply(packfile, "[[", "hash")
  types <- unname(vcapply(packfile, "[[", "type"))
  tree_sizes <- viapply(packfile, function(x) nrow(x$object) %||% NA_integer_)
  num_files <- sum(tree_sizes, na.rm = TRUE)

  files = data_frame(
    hash = character(num_files),
    type = character(num_files),
    mode = character(num_files),
    path = character(num_files)
  )

  trees <- packfile[types == "tree"]
  done <- logical(length(trees))
  idx <- 1L
  wd <- character()

  commit <- parse_commit(packfile[[which(types == "commit")]]$object)
  tree <- commit[["tree"]]

  process_tree <- function(i) {
    if (done[i]) return()
    done[i] <<- TRUE
    tr <- trees[[i]]$object
    for (l in seq_len(nrow(tr))) {
      files$hash[idx] <<- tr$hash[l]
      files$type[idx] <<- tr$type[l]
      files$mode[idx] <<- tr$mode[l]
      files$path[idx] <<- paste(c(wd, tr$path[l]), collapse = "/")
      idx <<- idx + 1L
      if (tr$type[l] == "tree") {
        tidx <- which(tr$hash[l] == names(trees))[1]
        if (is.na(tidx)) next # nocov
        wd <<- c(wd, tr$path[l])
        process_tree(tidx)
        wd <<- utils::head(wd, -1)
      }
    }
  }

  # start with the root tree
  root <- match(tree, names(trees))
  if (is.na(root)) {
    throw(pkg_error(
      "Invalid git response from {.url {url}}, cannot find commit tree"
    ))
  }
  process_tree(root)
  if (any(!done)) {
    warning(
      "Some trees are unreachable when listing files from git repo from ",
      url
    )
  }

  list(
    ref = ref,
    sha = sha,
    commit = commit,
    tree = tree,
    files = files
  )
}

# -------------------------------------------------------------------------

#' Download a blob for a remote git repository
#'
#' @inheritParams git_list_refs
#' @param sha SHA hash of the blob.
#' @param output Path where the blob will be written. It's directory is
#'   created if it does not exist.
#' @return A list that corresponds to a git packfile entry, invisibly.
#'   It has entries: `type` (always `"blob"`), `object` (raw object, the
#'   blob itself), `size`, `packed_size`, `hash`.
#'
#' @keywords internal
#' @section Examples:
#' Download a `DESCRIPTION` file from GitHub:
#' ```{r git-download-file, cache = TRUE}
#' pak_repo <- "https://github.com/r-lib/pak.git"
#' fls <- git_list_files(pak_repo, "HEAD")
#' git_download_file(
#'   pak_repo,
#'   fls$files$hash[fls$files$path == "DESCRIPTION"],
#'   output = tmp <- tempfile()
#' )
#' readLines(tmp)[1:5]
#' ```

git_download_file <- function(url, sha, output = sha) {
  synchronize(async_git_download_file(url, sha, output))
}

async_git_download_file <- function(url, sha, output = sha) {
  url
  sha
  output
  async_git_fetch(url, sha)$then(function(packfile) {
    if (length(packfile) != 1 || packfile[[1]]$type != "blob") {
      # nocov start
      throw(pkg_error(
        "Invalid response from git server, packfile should have a single blob.",
        .class = "git_proto_error_unexpected_response",
        .data = list(url = redact_url(url), sha = sha)
      ))
      # nocov end
    }
    if (!is.null(output)) {
      mkdirp(dirname(output))
      writeBin(packfile[[1]]$raw, output)
    }
    invisible(packfile[[1]])
  })
}

# -------------------------------------------------------------------------

#' Get a packfile for an object from a remote git repository
#'
#' @details
#' It uses `filter blob:none`, so extra blobs are not downloaded, only
#' trees.
#'
#' @inheritParams git_list_refs
#' @param sha SHA of the object to get.
#' @param blobs Whether we want to download all blobs as well or not.
#'   We are still always requesting a shallow (depth = 1) clone.
#' @return A list of git objects. Each element has entries:
#'   * `type`: `commit`, `tree`, `blob` or `tag`.
#'   * `object`: the object itself. It is a
#'       - character scalar for commits,
#'       - a data frame for trees, with columns: `type`, `mode`, `path`, `hash`,
#'       - a raw vector for blobs,
#'       - a raw vector for tags. (We could probably do better here.)
#'
#' @keywords internal
#' @section Examples:
#'
#' ```{r, git-fetch, cache = TRUE}
#' ft <- git_fetch(
#'   "https://github.com/r-lib/filelock.git",
#'   "9fdba75a62facaa3e818902f58891166e45eabe9"
#' )
#' ft
#' ```

git_fetch <- function(url, sha, blobs = FALSE) {
  synchronize(async_git_fetch(url, sha, blobs))
}

async_git_fetch <- function(url, sha, blobs = FALSE) {
  url
  sha

  if (!is.null(attr(sha, "protocol"))) {
    if (attr(sha, "protocol") == 1) {
      async_git_fetch_v1(url, sha, blobs)
    } else {
      async_git_fetch_v2(url, sha, blobs)
    }
  } else {
    async_git_fetch_v2(url, sha, blobs)$catch(
      error = function(err) async_git_fetch_v1(url, sha, blobs)
    )
  }
}

async_git_fetch_v1 <- function(url, sha, blobs) {
  async_git_send_message_v1(
    url,
    args = c(
      paste0("want ", sha),
      "deepen 1",
      "",
      "done",
      ""
    ),
    caps = c("multi_ack", "no-done", "no-progress", paste0("agent=", git_ua()))
  )$then(function(reply) git_fetch_process_v1(reply, url, sha))
}

async_git_fetch_v2 <- function(url, sha, blobs) {
  # If 'filter' is not supported, then we need to get the blobs
  if (!is.null(attr(sha, "filter")) && !attr(sha, "filter")) {
    blobs <- TRUE
  }
  async_git_send_message_v2(
    url,
    "fetch",
    caps = c(
      paste0("agent=", git_ua(), "\n"),
      "object-format=sha1\n"
    ),
    args = c(
      "deepen 1\n",
      "no-progress\n",
      if (!blobs) "filter blob:none\n",
      paste0("want ", sha, "\n"),
      paste0("want ", sha, "\n"),
      "done\n"
    )
  )$then(function(reply) git_fetch_process(reply, url, sha))
}

git_fetch_process_v1 <- function(reply, url, sha) {
  if (length(reply) == 0) {
    throw(pkg_error(
      "Empty reply from git server (protocol v1) at {.url {redact_url(url)}}."
    ))
  }
  if (reply[[length(reply)]]$type != "pack") {
    throw(pkg_error(
      "No PACK in git server response (protocol v1) from {.url {redact_url(url)}}."
    ))
  }

  git_unpack(reply[[length(reply)]]$data)
}

git_fetch_process <- function(reply, url, sha) {
  # https://github.com/git/git/blob/7c2ef319c52c4997256f5807564523dfd4acdfc7/Documentation/gitprotocol-v2.txt#L240
  #
  #     output = acknowledgements flush-pkt |
  # 	     [acknowledgments delim-pkt] [shallow-info delim-pkt]
  # 	     [wanted-refs delim-pkt] [packfile-uris delim-pkt]
  # 	     packfile flush-pkt
  #
  #     acknowledgments = PKT-LINE("acknowledgments" LF)
  # 		      (nak | *ack)
  # 		      (ready)
  #     ready = PKT-LINE("ready" LF)
  #     nak = PKT-LINE("NAK" LF)
  #     ack = PKT-LINE("ACK" SP obj-id LF)
  #
  #     shallow-info = PKT-LINE("shallow-info" LF)
  # 		   *PKT-LINE((shallow | unshallow) LF)
  #     shallow = "shallow" SP obj-id
  #     unshallow = "unshallow" SP obj-id
  #
  #     wanted-refs = PKT-LINE("wanted-refs" LF)
  # 		  *PKT-LINE(wanted-ref LF)
  #     wanted-ref = obj-id SP refname
  #
  #     packfile-uris = PKT-LINE("packfile-uris" LF) *packfile-uri
  #     packfile-uri = PKT-LINE(40*(HEXDIGIT) SP *%x20-ff LF)
  #
  #     packfile = PKT-LINE("packfile" LF)
  # 	       *PKT-LINE(%x01-03 *%x00-ff)
  #

  # Since we sent a 'done', there must be no acknowledgements section

  if (is.null(reply[[1]]$text) || reply[[1]]$text != "shallow-info") {
    # nocov start
    throw(pkg_error(
      "Expected {.code shallow-info} section from git server.",
      .class = "git_proto_error_unexpected_response",
      .data = list(url = redact_url(url), sha = sha)
    ))
    # nocov end
  }

  # There should be only one 'shallow' line I think
  idx <- 2L
  while (idx <= length(reply) && !identical(reply[[idx]]$text, "packfile")) {
    idx <- idx + 1L
  }
  idx <- idx + 1L

  if (idx > length(reply)) {
    # nocov start
    throw(pkg_error(
      "Response from git server does not have a {.code packgile} section.",
      .class = "git_proto_error_unexpected_response",
      .data = list(url = redact_url(url), sha = sha)
    ))
    # nocov end
  }

  # check closing flush-pkt
  if (reply[[length(reply)]]$type != "flush-pkt") {
    # nocov start
    throw(pkg_error(
      "Response from git server does not have a closing {.code flush-pkt}.",
      .class = "git_proto_error_unexpected_response",
      .data = list(url = redact_url(url), sha = sha)
    ))
    # nocov end
  }

  data <- reply[idx:(length(reply) - 1)]

  if (any(vcapply(data, "[[", "type") != "data-pkt")) {
    # nocov start
    throw(pkg_error(
      "Response from git server must contain {.code data-pkt} sections.",
      .class = "git_proto_error_unexpected_response",
      .data = list(url = redact_url(url), sha = sha)
    ))
    # nocov end
  }

  # drop progress messages, although we did not requrest them...
  stream <- viapply(data, function(x) as.integer(x$data[1]))
  if (any(stream == 3)) {
    # nocov start
    msg <- tryCatch(
      rawToChar(reply[[which(stream == 3)[1]]]$data[-1]),
      error = function(err) "error message not available"
    )
    throw(pkg_error(
      "Got fatal error from git server: {msg}.",
      .class = "git_proto_error_unexpected_response",
      .data = list(url = redact_url(url), sha = sha)
    ))
    # nocov end
  }
  data <- data[stream == 1L]

  packfile <- do.call("c", lapply(data, function(x) x$data[-1]))

  git_unpack(packfile)
}

# -------------------------------------------------------------------------

git_download_repo <- function(
  url,
  ref = "HEAD",
  output = ref,
  submodules = FALSE
) {
  synchronize(async_git_download_repo(url, ref, output, submodules))
}

async_git_download_repo <- function(
  url,
  ref = "HEAD",
  output = ref,
  submodules = FALSE
) {
  url
  ref
  async_git_resolve_ref(url, ref)$then(function(sha) {
    async_git_download_repo_sha(url, sha, output, submodules)
  })
}

async_git_download_repo_sha <- function(url, sha, output, submodules = FALSE) {
  url
  sha
  output
  p <- async_git_fetch(url, sha, blobs = TRUE)$then(
    function(packfile) unpack_packfile_repo(packfile, output, url)
  )
  if (!submodules) {
    p
  } else {
    p$then(function() async_update_git_submodules(output))
  }
}

unpack_packfile_repo <- function(parsed, output, url) {
  types <- unname(vcapply(parsed, "[[", "type"))
  trees <- parsed[types == "tree"]
  done <- logical(length(trees))
  idx <- 1L
  wd <- character()

  mkdirp(output)

  process_tree <- function(i) {
    if (done[i]) return()
    done[i] <<- TRUE
    tr <- trees[[i]]$object
    for (l in seq_len(nrow(tr))) {
      idx <<- idx + 1L
      opath <- file.path(output, paste(c(wd, tr$path[l]), collapse = "/"))
      if (tr$type[l] == "tree") {
        tidx <- which(tr$hash[l] == names(trees))[1]
        if (is.na(tidx)) {
          throw(new_error(
            # nocov
            "git tree missing from packfile: {.val {tr$hash[i]}}.", # nocov
            .class = "git_proto_error_unexpected_response" # nocov
          )) # nocov
        }
        wd <<- c(wd, tr$path[l])
        mkdirp(opath)
        process_tree(tidx)
        wd <<- utils::head(wd, -1)
      } else if (tr$type[l] == "blob") {
        # for submodules this is NULL
        if (!is.null(parsed[[tr$hash[l]]])) {
          writeBin(parsed[[tr$hash[l]]]$raw, opath)
        }
      }
    }
  }

  commit <- parse_commit(parsed[[which(types == "commit")]]$object)
  tree <- commit[["tree"]]
  root <- match(tree, names(trees))
  if (is.na(root)) {
    throw(pkg_error(
      "Invalid git response from {.url {url}}, cannot find commit tree"
    ))
  }
  process_tree(root)
  if (any(!done)) {
    warning(
      "Some trees are unreachable when listing files from git repo from ",
      url
    )
  }

  for (i in seq_along(trees)) process_tree(i)

  invisible()
}

# -------------------------------------------------------------------------
# Utility functions
# -------------------------------------------------------------------------

git_ua <- function() {
  "git/2.38.1"
}

#' Comvert raw message from a pkt_line to text, if possible
#'
#' If the input is binary it returns `NA_character_`.
#'
#' @param x Raw vector.
#' @return Character string, if `x` can be an UTF-8 string. Otherwise
#'   `NA_character_`.
#'
#' @noRd

raw_as_utf8 <- function(x) {
  if (is.raw(x)) {
    if (any(x == 0x0)) return(NA_character_)
    if (length(x) > 0 && x[[length(x)]] == 0x0a) {
      x <- x[1:(length(x) - 1)]
    }
    x <- rawToChar(x)
  }

  iconv(x, "UTF-8", "UTF-8")
}

#' Parse a message from git
#'
#' @param msg A raw vector, the full message.
#' @return A list of objects that correspond to _pkt_lines_ of the git
#' message. Each such object is a named list with potential entries:
#' * `type`: this entry is always present, and it is one of
#'   `flush-pkt`, `delim-pkt`, `response-end-pkt` or `data-pkt`. See the
#'   git protocol docs for what these are.
#' * `data`: for `data-pkt` lines this is a raw vector of the data.
#' * `text`: for `data-pkt` lines that are text, this is the text of the
#'   data. We use [raw_as_utf8()] to convert raw data to text, and sometimes
#'   it might interpret binary data as text, especially if the data is
#'   short. So this field is for convenience only.
#'
#' @noRd

git_parse_message <- function(msg) {
  assert_that(is.raw(msg))
  lines <- list()
  pos <- 1L

  while (pos <= length(msg)) {
    if (pos + 3L > length(msg)) {
      throw(pkg_error(
        "Invalid pkt-line at the enf of message from git.",
        .class = "git_proto_error_invalid_data"
      ))
    }
    pkg_len <- msg[pos:(pos + 3)]
    if (!all(pkg_len == charToRaw("PACK"))) {
      if (any(pkg_len < charToRaw('0') | pkg_len > charToRaw('f'))) {
        throw(pkg_error(
          "Invalid pkt-len field in message from git, must be four hexa digits.",
          .class = "git_proto_error_invalid_data"
        ))
      }
      len <- as.integer(as.hexmode(rawToChar(pkg_len)))
    }

    if (all(pkg_len == charToRaw("PACK"))) {
      lines[[length(lines) + 1L]] <- list(
        type = "pack",
        data = msg[pos:length(msg)]
      )
      pos <- length(msg) + 1L
    } else if (len == 0) {
      lines[[length(lines) + 1L]] <- list(type = "flush-pkt")
      pos <- pos + 4L
    } else if (len == 1) {
      lines[[length(lines) + 1L]] <- list(type = "delim-pkt")
      pos <- pos + 4L
    } else if (len == 2) {
      lines[[length(lines) + 1L]] <- list(type = "response-end-pkt") # nocov
      pos <- pos + 4L # nocov
    } else {
      if (pos + len - 1L > length(msg)) {
        throw(pkg_error(
          "Invalid pkt-payload in message from git.",
          i = "Need {len} byte{?s}, found {length(msg) - pos + 1L}.",
          .class = "git_proto_error_invalid_data"
        ))
      }

      data <- msg[(pos + 4):(pos + len - 1L)]
      lines[[length(lines) + 1L]] <- list(
        type = "data-pkt",
        data = data,
        text = raw_as_utf8(data)
      )

      pos <- pos + len
    }
  }

  lines
}

# After receiving the capability advertisement, a client can then issue a
# request to select the command it wants with any particular capabilities
# or arguments.  There is then an optional section where the client can
# provide any command specific parameters or queries.  Only a single
# command can be requested at a time.
#
#     request = empty-request | command-request
#     empty-request = flush-pkt
#     command-request = command
#                       capability-list
#                       delim-pkt
#                       command-args
#                       flush-pkt
#     command = PKT-LINE("command=" key LF)
#     command-args = *command-specific-arg
#
#     command-specific-args are packet line framed arguments defined by
#     each individual command.
#
# https://github.com/git/git/blob/master/Documentation/gitprotocol-v2.txt

#' Create a git protocol version 2 message, to be sent to git
#'
#' @param cmd Command to send. E.g. `ls-refs` or `fetch`.
#' @param caps Capabilities to advertise, a character vector.
#'   If they don't include the trailing `\n` character, it will be added.
#' @param args Arguments to send to git, a character vector. If they
#'   don't include the trailing `\n` character, it will be added.
#'
#' @return The message as a raw vector.
#'
#' @noRd

git_create_message_v2 <- function(
  cmd,
  caps = character(),
  args = character()
) {
  caps <- ifelse(last_char(caps) == "\n", caps, paste0(caps, "\n"))
  args <- ifelse(last_char(args) == "\n", args, paste0(args, "\n"))

  c(
    pkt_line("command=", cmd, "\n"),
    unlist(lapply(caps, pkt_line)),
    delim_pkt(),
    unlist(lapply(args, pkt_line)),
    flush_pkt()
  )
}

git_create_message_v1 <- function(args = character(), caps = character()) {
  if (length(args) == 0) {
    throw(pkg_error(
      "Invalid git protocol (v1) message, must have at least one argument"
    ))
  }

  args[1] <- paste0(args[1], " ", paste(caps, collapse = " "))
  args <- ifelse(
    last_char(args) == "\n" | args == "",
    args,
    paste0(args, "\n")
  )

  res <- lapply(args, pkt_line)
  res[args == ""] <- list(charToRaw("0000"))
  unlist(res)
}

#' Send a protocol version 2 message to a git server
#'
#' @inheritParams git_list_refs
#' @inheritParams git_create_message_v2
#' @return Response from git, already parsed with [git_parse_message()].
#'
#' @noRd

git_send_message_v2 <- function(
  url,
  cmd,
  caps = character(),
  args = character()
) {
  synchronize(async_git_send_message_v2(url, cmd, caps = caps, args = args)) # nocov
}

#' `async_git_send_message_v2()` is the asynchronous variant of
#' `git_send_message_v2()`.
#' @rdname git_send_message_v2
#' @noRd

async_git_send_message_v2 <- function(
  url,
  cmd,
  caps = character(),
  args = character()
) {
  msg <- git_create_message_v2(cmd, caps = caps, args = args)

  url2 <- paste0(url, "/git-upload-pack")
  headers <- c(
    "Content-Type" = "application/x-git-upload-pack-request",
    "User-Agent" = git_ua(),
    "accept-encoding" = "deflate, gzip",
    "accept" = "application/x-git-upload-pack-result",
    "git-protocol" = "version=2",
    "content-length" = as.character(length(msg))
  )

  git_http_post(
    url2,
    data = msg,
    headers = headers
  )$then(http_stop_for_status)$then(
    function(res) git_parse_message(res$content)
  )
}

async_git_send_message_v1 <- function(url, args, caps) {
  msg <- git_create_message_v1(caps = caps, args = args)

  url2 <- paste0(url, "/git-upload-pack")
  headers <- c(
    "Content-Type" = "application/x-git-upload-pack-request",
    "User-Agent" = git_ua(),
    "accept" = "application/x-git-upload-pack-result",
    "content-length" = as.character(length(msg))
  )
  git_http_post(
    url2,
    data = msg,
    headers = headers
  )$then(http_stop_for_status)$then(
    function(res) git_parse_message(res$content)
  )
}

delim_pkt <- function() {
  charToRaw("0001")
}

flush_pkt <- function() {
  charToRaw("0000")
}

#' Create a `pkt_line`, to be sent as part of a message to git
#'
#' Currently it errors if the complete payload is longer than 65516 bytes.
#'
#' @param payload Raw data, or a string to send.
#' @param ... More strings to send. `payload` and `...` are all
#'   concatenated to form the line.
#' @return Raw vector containing the `pkt_line`.
#' @noRd

pkt_line <- function(payload, ...) {
  if (!is.raw(payload)) {
    payload <- charToRaw(payload)
  }
  line <- c(
    payload,
    charToRaw(paste0("", ...))
  )
  len <- length(line) + 4L
  if (len > 65516) {
    throw(pkg_error(
      "packet line longer than 65516 bytes is not implemented yet."
    ))
  }

  line <- c(
    charToRaw(format(as.hexmode(len), width = 4)),
    line
  )

  line
}

#' List references in a remote git repositoty, protocol version 1
#'
#' We use this if we don't want to filter the references, because it
#' needs a single HTTP query, whereas with version 2, we would need two
#' queries.
#'
#' @inheritParams git_list_refs
#' @return Same as [git_list_refs()].
#' @noRd

git_list_refs_v1 <- function(url) {
  synchronize(async_git_list_refs_v1(url))
}

#' `async_git_list_refs_v1()` is the asynchronous variant of
#' `git_list_refs_v1()`.
#' @rdname git_list_refs_v1
#' @noRd

# https://github.com/git/git/blob/master/Documentation/gitprotocol-http.txt

async_git_list_refs_v1 <- function(url) {
  url
  url1 <- paste0(url, "/info/refs?service=git-upload-pack")
  git_http_get(url1, headers = c("User-Agent" = git_ua()))$then(
    http_stop_for_status
  )$then(function(response) git_list_refs_v1_process(response, url))
}

#' Process the response to a version 1 reference list query
#'
#' @param response Response from git, as returned by `async::http_get()`, which
#'   is the same as the object from `curl::curl_fetch_memory()`.
#' @return Same as [git_list_refs()].
#' @noRd

git_list_refs_v1_process <- function(response, url) {
  psd <- git_parse_message(response$content)
  psd <- drop_service_line(psd)
  check_initial_response(psd, url)

  # The stream MUST include capability declarations behind a NUL on the
  # first ref.
  nul <- which(psd[[1]]$data == 0)
  if (length(nul) != 1) {
    # nocov start
    throw(pkg_error(
      "Unexpected response from git server, third pkt-line should contain
       exactly one {.code NUL} byte.",
      .class = "git_proto_error_unexpected_response",
      .data = list(url = redact_url(url))
    ))
    # nocov end
  }

  caps <- if (nul < length(psd[[1]]$data)) {
    rawToChar(psd[[1]]$data[(nul + 1):length(psd[[1]]$data)])
  } else {
    "" # nocov
  }
  caps <- strsplit(trimws(caps), " ", fixed = TRUE)[[1]]
  psd[[1]]$data <- if (nul == 1) raw() else psd[[1]]$data[1:(nul - 1)]
  psd[[1]]$text <- rawToChar(psd[[1]]$data)

  if (psd[[length(psd)]]$type != "flush-pkt") {
    # nocov start
    throw(pkg_error(
      "Response from git server does not have a closing {.code flush-pkt}.",
      .class = "git_proto_error_unexpected_response",
      .data = list(url = redact_url(url))
    ))
    # nocov end
  }

  refs <- git_parse_pkt_line_refs(psd[1:(length(psd) - 1)], url)
  list(refs = refs, caps = caps)
}

git_list_refs_v1_process_1 <- function(response, url, prefixes) {
  # We wanted v2, but the server replied in v1, so we'll use v1 now
  refs <- git_list_refs_v1_process(response, url)
  if (length(prefixes) > 0) {
    keep <- logical(length(refs$refs$ref))
    for (pfx in prefixes) {
      keep <- keep | startsWith(refs$refs$ref, pfx)
    }
    refs$refs <- refs$refs[keep, ]
  }
  refs
}

#' Helper function to parse a `pkt_line` containing a git ref
#'
#' E.g. an `ls-refs` commans responds with such lines.
#'
#' @param lines List of `pkt_line` objects, they must be all data
#'   packets (`data-pkt`), in the right format.
#' @return Data frame with columns `ref` (name of the reference), and
#'   `hash` (SHA).
#' @noRd

git_parse_pkt_line_refs <- function(lines, url) {
  res <- data_frame(
    ref = character(length(lines)),
    hash = character(length(lines))
  )

  for (idx in seq_along(lines)) {
    line <- lines[[idx]]
    if (
      line$type != "data-pkt" ||
        is.null(line$text) ||
        !grepl("^[0-9a-f]{40} .+$", line$text)
    ) {
      # nocov start
      throw(pkg_error(
        "Expected response from git server.",
        i = "Line {idx} must be a text {.code data-pkt} with a sha and a ref name.",
        .class = "git_proto_error_unexpected_response",
        .data = list(url = redact_url(url))
      ))
      # nocov end
    }

    res$ref[idx] <- substr(line$text, 42, nchar(line$text))
    res$hash[idx] <- substr(line$text, 1, 40)
  }

  res
}

#' List references in a remote git repositoty, protocol version 2
#'
#' We use this to filter all refs using prefixes, if we are only
#' interested in a subset of refs.
#'
#' @inheritParams git_list_refs
#' @return Same as [git_list_refs()].
#' @noRd

git_list_refs_v2 <- function(url, prefixes = character()) {
  synchronize(async_git_list_refs_v2(url, prefixes))
}

#' `async_git_list_refs_v2()` is the asynchronous version of
#' `git_list_refs_v2()`.
#' @rdname git_list_refs_v2
#' @noRd

async_git_list_refs_v2 <- function(url, prefixes = character()) {
  url
  prefixes

  url1 <- paste0(url, "/info/refs?service=git-upload-pack")

  headers <- c(
    "User-Agent" = git_ua(),
    "git-protocol" = "version=2"
  )

  git_http_get(url1, headers = headers)$then(http_stop_for_status)$then(
    function(res) async_git_list_refs_v2_process_1(res, url, prefixes)
  )
}

#' Helper function to post-process the response to the initial client
#' request, with protocol version 2
#'
#' @param response The HTTP response from git, from `async::http_get()`,
#'   i.e. from `curl::curl_fetch_memory()`.
#' @param url The original URL is passed in here.
#' @param prefixes The original prefixes are passed in here.
#' @return `async_git_list_refs_v2_process_1()` will send out the
#'   second query, an `ls-refs` command, filtered for `prefixes`, and
#'   return with (the deferred value of) the HTTP response.
#'
#' @noRd

async_git_list_refs_v2_process_1 <- function(response, url, prefixes) {
  psd <- git_parse_message(response$content)
  psd <- drop_service_line(psd)
  check_initial_response(psd, url)
  async_git_list_refs_v2_process_2(response, psd, url, prefixes)
}


async_git_list_refs_v2_process_2 <- function(response, psd, url, prefixes) {
  # capability-advertisement = protocol-version
  #                capability-list
  #                flush-pkt
  #
  # protocol-version = PKT-LINE("version 2" LF)
  # capability-list = *capability
  # capability = PKT-LINE(key[=value] LF)
  #
  # key = 1*(ALPHA | DIGIT | "-_")
  # value = 1*(ALPHA | DIGIT | " -_.,?\/{}[]()<>!@#$%^&*+=:;")

  if (is.null(psd[[1]]$text)) {
    throw(pkg_error(
      "Invalid git protocol message from {.url {redact_url(url)}}."
    ))
  }

  if (is.na(psd[[1]]$text)) {
    return(git_list_refs_v1_process_1(response, url, prefixes))
  }

  if (psd[[1]]$text != "version 2") {
    throw(pkg_error(
      "Only git protocol version 2 is supported, not {psd[[1]]$text}.",
      .class = "git_proto_error_not_implemented"
    ))
  }

  if (psd[[length(psd)]]$type != "flush-pkt") {
    throw(pkg_error(
      "Response from git server does not have a closing {.code flush-pkt}.",
      .class = "git_proto_error_unexpected_response",
      .data = list(url = redact_url(url))
    ))
  }

  caps <- unlist(lapply(psd[1:(length(psd) - 1)], "[[", "text"))

  args <- if (length(prefixes)) paste0("ref-prefix ", prefixes, "\n")

  async_git_send_message_v2(url, "ls-refs", args = as.character(args))$then(
    function(res) async_git_list_refs_v2_process_3(res, caps, url)
  )
}

drop_service_line <- function(psd) {
  # Actually, this might not be present in V2 servers...
  if (
    length(psd) > 0 &&
      !is.null(psd[[1]]$text) &&
      grepl("^# service=.+", psd[[1]]$text)
  ) {
    psd <- psd[-1]
  }

  if (length(psd) > 0 && psd[[1]]$type == "flush-pkt") {
    psd <- psd[-1]
  }

  psd
}

check_initial_response <- function(psd, url) {
  if (length(psd) < 1 || psd[[1]]$type != "data-pkt") {
    throw(pkg_error(
      "Unexpected response from git server, no {.code data-pkt} line.",
      .class = "git_proto_error_unexpected_response",
      .data = list(url = redact_url(url))
    ))
  }
}

#' Helper function to process the response to an `ls-refs` command
#'
#' @param reply The parsed message from the server.
#' @param caps Capabilities that are passed in from the response to
#'   the initial client request.
#' @return Same as [git_list_refs()].
#' @noRd

async_git_list_refs_v2_process_3 <- function(reply, caps, url) {
  if (reply[[length(reply)]]$type != "flush-pkt") {
    throw(pkg_error(
      "Response from git server does not have a closing {.code flush-pkt}.",
      .class = "git_proto_error_unexpected_response",
      .data = list(url = redact_url(url))
    ))
  }

  # Any refs at all?
  refs <- if (length(reply) == 1) {
    git_parse_pkt_line_refs(list(), url)
  } else {
    git_parse_pkt_line_refs(reply[1:(length(reply) - 1)], url)
  }
  list(refs = refs, caps = caps)
}

#' Unpack a git packfile in memory
#'
#' @details
#' It cannot currently unpack packfiles with deltas.
#'
#' It checks the packfile checksum.
#'
#' @param pack The git packfile as a raw vector, or the name of the file
#'   containing the packfile.
#' @return List of git objects. Each object is a named list with entries:
#'   * `type`: `commit`, `tree`, `blob` or `tag`.
#'   * `object`: the contents of the object. For `commit` the commit
#'     data and message in a string. For `tree` a data frame, as returned
#'     from [parse_tree()]. For `blob` and `tag` a raw vector.
#'   * `size`: unpacked size.
#'   * `packed_size`: packed size (size header not included).
#'
#' @noRd

git_unpack <- function(pack) {
  # allow file names as well
  if (is.character(pack)) {
    pack <- readBin(pack, "raw", file.size(pack))
  }

  if (length(pack) < 32) {
    throw(pkg_error(
      "Invalid packfile from git, too short.",
      .class = "git_proto_error_invalid_data"
    ))
  }
  if (!all(pack[1:4] == charToRaw("PACK"))) {
    throw(pkg_error(
      "Not a git packfile, it does not have a {.code PACK} header.",
      .class = "git_proto_error_invalid_data"
    ))
  }
  if (!all(pack[5:8] == as.raw(c(0x00, 0x00, 0x00, 0x02)))) {
    throw(pkg_error(
      "Unexpected packfile version, must be version 2.",
      .class = "git_proto_error_unexpected_response"
    ))
  }

  chksum <- cli::hash_raw_sha1(utils::head(pack, -20))
  chksum_exp <- paste(format(utils::tail(pack, 20)), collapse = "")
  if (chksum != chksum_exp) {
    throw(pkg_error(
      "Checksum mismatch in git packfile.",
      .class = "git_proto_error_invalid_data"
    ))
  }

  n_obj <- parse_int32_nwb(pack[9:12])

  idx <- 13L
  objects <- vector("list", n_obj)
  object_starts <- integer()

  types <- c(
    "commit",
    "tree",
    "blob",
    "tag",
    "reserved",
    "ofs_delta",
    "ref_delta"
  )

  unpack_object <- function() {
    start <- idx
    type <- bitwShiftR(bitwAnd(as.integer(pack[idx]), 0x7f), 4L)
    size <- parse_size(pack, idx)
    idx <<- size$idx + 1
    if (type == 6L) {
      offset <- parse_ofs_delta_offset(pack, idx)
      idx <<- offset$idx + 1
    } else if (type == 7L) {
      if (idx + 19 > length(pack)) {
        # nocov start
        throw(pkg_error(
          "Invalid packfile, unexpected end of file"
        ))
        # nocov end
      }
      base <- bin_to_sha(pack[idx:(idx + 19)])
      idx <<- idx + 20
    }
    obj <- zip::inflate(pack, idx, size$size)
    idx <<- idx + obj$bytes_read
    if (type == 6L) {
      baseidx <- object_starts[[as.character(start - offset$size)]]
      deltified_object(obj$output, baseidx = baseidx)
    } else if (type == 7L) {
      deltified_object(obj$output, base = base)
    } else {
      list(
        type = types[type],
        raw = obj$output,
        size = size$size,
        packed_size = obj$bytes_read
      )
    }
  }

  deltified_object <- function(delta, base = NULL, baseidx = NULL) {
    baseidx <- baseidx %||% match(base, names(objects))
    if (is.na(baseidx) || objects[[baseidx]]$type == "delta") {
      return(list(
        type = "delta",
        data = delta,
        base = base,
        baseidx = if (is.na(baseidx)) NULL else baseidx
      ))
    }
    baseobj <- objects[[baseidx]]$raw
    didx <- 1L
    basesize <- parse_delta_size(delta, didx)
    didx <- basesize$idx + 1L
    size <- parse_delta_size(delta, didx)
    didx <- size$idx + 1L
    newobj <- raw(size$size)
    nidx <- 1L
    while (didx <= length(delta)) {
      c <- as.integer(delta[didx])
      if (c < 128) {
        datasize <- bitwAnd(c, 0x7f)
        newobj[nidx:(nidx + datasize - 1L)] <-
          delta[(didx + 1L):(didx + datasize)]
        nidx <- nidx + datasize
        didx <- didx + datasize + 1L
      } else {
        ofs <- parse_delta_offset(delta, didx)
        newobj[nidx:(nidx + ofs$size - 1L)] <-
          baseobj[(ofs$offset + 1L):(ofs$offset + ofs$size)]
        nidx <- nidx + ofs$size
        didx <- ofs$idx + 1L
      }
    }
    list(
      type = objects[[baseidx]]$type,
      raw = newobj,
      size = size,
      packed_size = length(delta)
    )
  }

  finalize_object <- function(x) {
    if (x$type == "commit") {
      x$object <- rawToChar(x$raw)
    } else if (x$type == "tree") {
      x$object <- parse_tree(x$raw)
    }

    if (x$type %in% c("commit", "tree", "blob", "tag")) {
      raw2 <- c(
        charToRaw(paste0(x$type, " ", length(x$raw))),
        as.raw(0L),
        x$raw
      )
      x$hash <- cli::hash_raw_sha1(raw2)
    } else if (x$type == "delta") {
      # do nothing
    } else {
      # nocov start
      throw(pkg_error(
        "git packfile object type {.cls {x$type}} is not
         implemented yet.",
        .class = "git_proto_error_not_implemented"
      ))
      # nocov end
    }
    x
  }

  for (i in seq_len(n_obj)) {
    object_starts[[as.character(idx)]] <- i
    objects[[i]] <- unpack_object()
    objects[[i]] <- finalize_object(objects[[i]])
    if (!is.null(objects[[i]]$hash)) names(objects)[i] <- objects[[i]]$hash
  }

  # now need to resolve the deltas
  n_delta <- sum(sapply(objects, "[[", "type") == "delta")
  while (n_delta > 0) {
    for (i in seq_len(n_obj)) {
      if (objects[[i]]$type == "delta") {
        objects[[i]] <- deltified_object(
          objects[[i]]$data,
          objects[[i]]$base,
          objects[[i]]$baseidx
        )
        objects[[i]] <- finalize_object(objects[[i]])
        if (!is.null(objects[[i]]$hash)) names(objects)[i] <- objects[[i]]$hash
      }
    }
    n_delta2 <- sum(sapply(objects, "[[", "type") == "delta")
    if (n_delta2 == n_delta) {
      throw(pkg_error(
        "Found circular references while resolving deltas in git pack file."
      ))
    }
    n_delta <- n_delta2
  }

  objects
}

git_list_pack_index <- function(idx) {
  if (is.character(idx)) {
    idx <- readBin(idx, "raw", file.size(idx))
  }

  if (length(idx) < 4 + 4 + 256) {
    throw(pkg_error(
      "Invalid pack index file, too short: {length(idx)} byte{?s}."
    ))
  }

  if (any(idx[1:4] != c(0xff, 0x74, 0x4f, 0x63))) {
    throw(pkg_error(
      "Invalid pack index file, no {.code \\377tOc} header."
    ))
  }
  if (any(idx[5:8] != c(0, 0, 0, 2))) {
    throw(pkg_error(
      "Only version 2 pack index files are supported"
    ))
  }

  tab <- matrix(as.integer(idx[9L:(9L + (256L * 4L) - 1L)]), nrow = 4)
  tab <- tab[1, ] * 256**3 + tab[2, ] * 256**2 + tab[3, ] * 256 + tab[4, ]
  n_obj <- tab[256]

  hash_off <- 256L * 4L + 9L
  hash_len <- n_obj * 20L
  hash <- matrix(
    as.character(idx[hash_off:(hash_off + hash_len - 1L)]),
    nrow = 20
  )
  hash <- apply(hash, 2, paste, collapse = "")

  crc_off <- hash_off + hash_len
  crc_len <- n_obj * 4
  crc <- matrix(as.integer(idx[crc_off:(crc_off + crc_len - 1L)]), nrow = 4)
  crc <- crc[1, ] * 256**3 + crc[2, ] * 256**2 + crc[3, ] * 256 + crc[4, ]

  off_off <- crc_off + crc_len
  off_len <- n_obj * 4
  off <- matrix(as.integer(idx[off_off:(off_off + off_len - 1L)]), nrow = 4)
  off <- off[1, ] * 256**3 + off[2, ] * 256**2 + off[3, ] * 256 + off[4, ]

  data_chksum <- idx[(off_off + off_len):(off_off + off_len + 20L - 1L)]
  data_chksum <- paste(as.character(data_chksum), collapse = "")

  idx_chksum <- idx[(off_off + off_len):(off_off + off_len + 20L - 1L) + 20L]
  idx_chksum <- paste(as.character(idx_chksum), collapse = "")

  if (length(idx) > off_off + off_len + 40L) {
    warning("Ignored 8 byte offsets in git pack file")
  }

  objects <- data.frame(
    stringsAsFactors = FALSE,
    hash = hash,
    crc = crc,
    offset = off
  )
  objects <- objects[order(objects$offset), ]
  rownames(objects) <- NULL

  list(
    objects = objects,
    data_chksum = data_chksum,
    idx_chksum = idx_chksum
  )
}

#' Parse a four byte integer in network byte order
#'
#' @param x Raw vector or four bytes.
#' @return Integer scalar.
#' @noRd

parse_int32_nwb <- function(x) {
  if (length(x) != 4L || !is.raw(x)) {
    throw(pkg_error(
      "Cannot parse integer, not raw or number of bytes is wrong."
    ))
  }
  sum(as.integer(x) * (256L**(3:0)))
}

#' Parse a variable length size field
#'
#' @details
#' As in https://git-scm.com/docs/pack-format#_size_encoding.
#' Some notes, because that docs is pretty dense.
#'
#' * First bit is zero for the last byte.
#' * Next three bits of the first byte are the object type.
#' * So we throw away the first four bits of the first byte and use the
#'   last four bits only.
#' * Then we keep processing the last seven bits of bytes, until the
#'   first bit is zero. (We still process that byte.)
#'
#' @param x Raw vector, typically the whole packfile.
#' @param idx Integer scalar, where to start reading the size field in the
#'   `x` raw vector.
#' @return Named list with entries:
#'   * `size`: parsed size,
#'   * `idx`: Integer, point to the last byte of the parsed size field.
#'
#' @noRd

parse_size <- function(x, idx) {
  c <- as.integer(x[idx])
  size <- bitwAnd(c, 0x0f)
  shft <- 4L
  while (c >= 128) {
    idx <- idx + 1L
    if (idx > length(x)) {
      # nocov start
      throw(pkg_error(
        "Invalid git packfile, invalid size field.",
        .class = "git_proto_error_invalid_data"
      ))
      # nocov end
    }
    c <- as.integer(x[idx])
    size <- size + bitwShiftL(bitwAnd(c, 0x7f), shft)
    shft <- shft + 7L
  }

  list(size = size, idx = idx)
}

parse_ofs_size <- function(x, idx) {
}

parse_delta_size <- function(x, idx) {
  c <- as.integer(x[idx])
  size <- bitwAnd(c, 0x7f)
  shft <- 7L
  while (c >= 128) {
    idx <- idx + 1L
    if (idx > length(x)) {
      # nocov start
      throw(pkg_error(
        "Invalid git packfile, invalid size field.",
        .class = "git_proto_error_invalid_data"
      ))
      # nocov end
    }
    c <- as.integer(x[idx])
    size <- size + bitwShiftL(bitwAnd(c, 0x7f), shft)
    shft <- shft + 7L
  }

  list(size = size, idx = idx)
}

parse_ofs_delta_offset <- function(x, idx) {
  c <- as.integer(x[idx])
  size <- bitwAnd(c, 0x7f)
  while (c >= 128) {
    idx <- idx + 1L
    c <- as.integer(x[idx])
    size <- size + 1L
    size <- bitwShiftL(size, 7) + (bitwAnd(c, 0x7f))
  }

  list(size = size, idx = idx)
}

parse_delta_offset <- function(x, idx) {
  c <- as.integer(x[idx])
  offset <- 0L
  size <- 0L
  if (bitwAnd(c, 0x01)) {
    idx <- idx + 1L
    offset <- offset + as.integer(x[idx])
  }
  if (bitwAnd(c, 0x02)) {
    idx <- idx + 1L
    offset <- offset + as.integer(x[idx]) * 256
  }
  if (bitwAnd(c, 0x04)) {
    idx <- idx + 1L
    offset <- offset + as.integer(x[idx]) * 256 * 256
  }
  if (bitwAnd(c, 0x08)) {
    idx <- idx + 1L
    offset <- offset + as.integer(x[idx]) * 256 * 256 * 256
  }
  if (bitwAnd(c, 0x10)) {
    idx <- idx + 1L
    size <- size + as.integer(x[idx])
  }
  if (bitwAnd(c, 0x20)) {
    idx <- idx + 1L
    size <- size + as.integer(x[idx]) * 256
  }
  if (bitwAnd(c, 0x40)) {
    idx <- idx + 1L
    size <- size + as.integer(x[idx]) * 256 * 256
  }

  # exception for easily including a block
  if (size == 0L) size <- 0x10000

  list(offset = offset, size = size, idx = idx)
}

#' Parse a git tree object
#'
#' @param tree The tree object in a raw vector, _without_ the 'tree' +
#'   content size + `\0` header. Git packfiles do not have these headers,
#'   so we don't require them here, either.
#' @return Data frame with character columns:
#'   * `type`: Either `"blob"` or `"tree"`.
#'   * `mode`: Unix permissions.
#'   * `path`: File or directory name.
#'   * `hash`: Hash of `blob` or tree`.
#'
#' @noRd

parse_tree <- function(tree) {
  nul <- which(tree == 0)
  last <- nul[1]
  for (i in seq_along(nul)[-1]) {
    nl <- nul[i]
    if (nl > last + 20) {
      last <- nl
    } else {
      nul[i] <- NA_integer_
    }
  }

  nul <- nul[!is.na(nul)]
  num <- length(nul)
  res <- data_frame(
    type = character(num),
    mode = character(num),
    path = character(num),
    hash = character(num)
  )

  beg <- c(1L, nul + 21)
  for (i in seq_along(nul)) {
    txt <- rawToChar(tree[beg[i]:nul[i]])
    res$mode[i] <- sub("[ ].*$", "", txt)
    res$path[i] <- sub("[^ ]*[ ]", "", txt)
    res$hash[i] <- bin_to_sha(tree[(nul[i] + 1):(nul[i] + 20)])
  }
  res$type <- ifelse(substr(res$mode, 1, 1) == "1", "blob", "tree")

  res
}

#' Format a raw vector as a hexa string
#'
#' @param x Raw vector.
#' @return String.
#'
#' @noRd

bin_to_sha <- function(x) {
  paste(format(x), collapse = "")
}

#' Parse a commit object
#'
#' @param commit Commit object as an UTF-8 string.
#' @return Named character vector. Names are taken from the commit object,
#'   plus `message` is the name of the commit message. The commit object
#'   should have names `tree`, `parent` (one for each parent commit),
#'   `author`, `committer`, `encoding`.
#'
#' @noRd

parse_commit <- function(commit) {
  lines <- strsplit(commit, "\n", fixed = TRUE)[[1]]
  delim <- which(lines == "")[1]
  if (delim == 1) {
    # nocov start
    throw(pkg_error(
      "Invalid git commit object, no {.code tree} field.",
      .class = "git_proto_error_invalid_data"
    ))
    # nocov end
  }

  message <- if (is.na(delim)) {
    NA_character_
  } else if (delim == length(lines)) {
    "" # nocov
  } else {
    paste(lines[(delim + 1):length(lines)], collapse = "\n")
  }

  fields <- lines[1:(delim - 1)]
  nms <- sub("[ ].*$", "", fields)
  vls <- sub("^[^ ]+ ", "", fields)

  structure(c(vls, message), names = c(nms, "message"))
}

# Returns `""` for empty strings!

last_char <- function(x) {
  nc <- nchar(x)
  substr(x, nc, nc)
}

redact_url <- function(x) {
  sub("://[^/]+@", "://<auth>@", x)
}

git_dumb_list_refs <- function(url) {
  synchronize(async_git_dumb_list_refs(url))
}

async_git_dumb_list_refs <- function(url) {
  url

  url1 <- paste0(url, "/info/refs")
  url2 <- paste0(url, "/HEAD")
  headers <- c(
    "User-Agent" = git_ua()
  )
  when_all(
    git_http_get(url1, headers = headers)$then(http_stop_for_status),
    git_http_get(url2, headers = headers)$then(http_stop_for_status)
  )$then(function(res) async_git_dumb_list_refs_process(res, url))
}

async_git_dumb_list_refs_process <- function(res, url) {
  res_refs <- res[[1]]
  res_head <- res[[2]]
  lines <- strsplit(rawToChar(res_refs$content), "\n", fixed = TRUE)[[1]]
  lines2 <- strsplit(lines, "\t", fixed = TRUE)
  ref <- vcapply(lines2, "[[", 2)
  hash <- vcapply(lines2, "[[", 1)

  head <- trimws(rawToChar(res_head$content))
  head <- sub("^[^ ]* ", "", head)
  has_head <- head != ""
  if (has_head && !head %in% ref) {
    throw(pkg_error(
      "HEAD does not refer to a ref in git repo at {.url {url}}.",
      "i" = "HEAD is {.val {head}}."
    ))
  }

  list(
    refs = data_frame(
      ref = c(if (has_head) "HEAD", ref),
      hash = c(if (has_head) hash[match(head, ref)], hash)
    ),
    caps = character()
  )
}

git_dumb_download_file <- function(url, sha, path, output = basename(path)) {
  invisible(synchronize(async_git_dumb_download_file(url, sha, path, output)))
}

# This only works for blobs at the root of the tree currently!
# Also, it only works if the object with sha is not in a pack file!
# So it is pretty limited, and can only be used with a fallback.

async_git_dumb_download_file <- function(
  url,
  sha,
  path,
  output = basename(path)
) {
  async_git_dumb_get_commit(url, sha)$then(function(cmt) {
    async_git_dumb_get_tree(url, cmt[["tree"]])
  })$then(function(tree) {
    wh <- match(path, tree$path)
    if (is.na(wh)) {
      throw(pkg_error(
        "Could not find path {.val {path}} in git repo at {.url {url}}."
      ))
    }
    if (tree$type[wh] != "blob") {
      throw(pkg_error(
        "Path {.val {path}} is not a blob in git repo at {.url {url}}."
      ))
    }
    tree$hash[wh]
  })$then(function(blob) {
    async_git_dumb_get_blob(url, blob)
  })$then(function(bytes) {
    if (!is.null(output)) {
      mkdirp(dirname(output))
      writeBin(bytes, output)
    }
    invisible(bytes)
  })
}

async_git_dumb_get_commit <- function(url, sha) {
  url1 <- paste0(
    url,
    "/objects/",
    substr(sha, 1, 2),
    "/",
    substr(sha, 3, nchar(sha))
  )
  headers <- c(
    "User-Agent" = git_ua(),
    "accept-encoding" = "deflate, gzip"
  )
  git_http_get(url = url1, headers = headers)$then(http_stop_for_status)$then(
    function(res) {
      cmt <- zip::inflate(res$content)$output
      if (any(utils::head(cmt, 6) != charToRaw("commit"))) {
        throw(pkg_error(
          "Git object {.val {substr(sha, 1, 7)}} is not a commit object
           in git repo at {.url {url}}."
        ))
      }
      nul <- which(cmt == 0)
      if (length(nul) != 1) {
        throw(pkg_error(
          "Git commit object {.val {substr(sha, 1, 7)}} is invalid, does not
           contain a single zero byte, in git repo at {.url {url}}."
        ))
      }
      parse_commit(rawToChar(cmt[(nul + 1):length(cmt)]))
    }
  )
}

async_git_dumb_get_tree <- function(url, sha) {
  url1 <- paste0(
    url,
    "/objects/",
    substr(sha, 1, 2),
    "/",
    substr(sha, 3, nchar(sha))
  )
  headers <- c(
    "User-Agent" = git_ua(),
    "accept-encoding" = "deflate, gzip"
  )
  git_http_get(url = url1, headers = headers)$then(http_stop_for_status)$then(
    function(res) {
      cmt <- zip::inflate(res$content)$output
      if (any(utils::head(cmt, 4) != charToRaw("tree"))) {
        throw(pkg_error(
          "Git object {.val {substr(sha, 1, 7)}} is not a tree object
           in git repo at {.url {url}}."
        ))
      }
      nul <- which(cmt == 0)[1]
      if (is.na(nul)) {
        throw(pkg_error(
          "Git tree object {.val {substr(sha, 1, 7)}} is invalid, does not
           contain any zero bytes, in git repo at {.url {url}}."
        ))
      }
      parse_tree(cmt[(nul + 1):length(cmt)])
    }
  )
}

async_git_dumb_get_blob <- function(url, sha) {
  url1 <- paste0(
    url,
    "/objects/",
    substr(sha, 1, 2),
    "/",
    substr(sha, 3, nchar(sha))
  )
  headers <- c(
    "User-Agent" = git_ua(),
    "accept-encoding" = "deflate, gzip"
  )
  git_http_get(url = url1, headers = headers)$then(http_stop_for_status)$then(
    function(res) {
      cmt <- zip::inflate(res$content)$output
      if (any(utils::head(cmt, 4) != charToRaw("blob"))) {
        throw(pkg_error(
          "Git object {.val {substr(sha, 1, 7)}} is not a blob object
           in git repo at {.url {url}}."
        ))
      }
      nul <- which(cmt == 0)[1]
      if (is.na(nul)) {
        throw(pkg_error(
          "Git blob object {.val {substr(sha, 1, 7)}} is invalid, does not
           contain any zero bytes, in git repo at {.url {url}}."
        ))
      }
      cmt[(nul + 1):length(cmt)]
    }
  )
}
