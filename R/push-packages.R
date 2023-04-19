
import_from <- function(env, symbol) {
  environment(get(env))[[symbol]]
}

push_packages <- local({

  const_annotations <- list(
    com.github.package.type = "r_package",
    org.opencontainers.image.authors = "Gabor Csardi",
    org.opencontainers.image.url = "https://github.com/r-lib/pak",
    org.opencontainers.image.documentation = "https://pak.r-lib.org/",
    org.opencontainers.image.source = "https://github.com/r-lib/pak",
    org.opencontainers.image.title = "pak R package",
    org.opencontainers.image.description = "Package manager for R",
    org.opencontainers.image.licenses = "GPL-3"
  )

  is_gha <- function() {
    Sys.getenv("GITHUB_ACTIONS") == "true"
  }

  const_annotations_js <- function() {
    paste0(
      glue::glue('"{names(const_annotations)}": "{const_annotations}"'),
      collapse = ",\n      "
    )
  }

  ghcr_uri <- function() {
    Sys.getenv(
      "PAK_GHCR_URI",
      "docker://ghcr.io/r-lib/pak"
    )
  }

  ghcr_user <- function() {
    Sys.getenv("PAK_GHCR_USER", "gaborcsardi")
  }

  ghcr_token <- function() {
    Sys.getenv("PAK_GHCR_TOKEN", gitcreds::gitcreds_get()$password)
  }

  package_dir <- function() {
    Sys.getenv("PAK_PACKAGE_DIR", tempfile())
  }

  git <- function (..., echo_cmd = TRUE, echo = TRUE, dry_run = FALSE,
                   stderr_to_stdout = FALSE) {
    if (dry_run) {
      cat("git", c(...), "\n")
    } else {
      processx::run("git", c(...), echo_cmd = echo_cmd, echo = echo,
                    stderr_to_stdout = stderr_to_stdout)
    }
  }

  git_pull <- function(dir, dry_run = FALSE) {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)
    setwd(dir)
    git("pull", dry_run = dry_run)
  }

  git_worktree_add <- function(dir, remote, branch, dry_run = FALSE) {
    git("worktree", "add", dir, branch, dry_run = dry_run)
  }

  git_worktree_remove <- function(dir, dry_run = FALSE) {
    git("worktree", "remove", "--force", dir, dry_run = dry_run)
  }

  init_package_dir <- function(dir, dry_run = FALSE) {
    remote <- "origin"
    branch <- "packages"
    git("remote", "set-branches", "--add", remote, branch, dry_run = dry_run)
    git("fetch", remote, branch, dry_run = dry_run)
    if (dry_run) mkdirp(dir)
    git_worktree_add(dir, remote, branch, dry_run = dry_run)
    dir
  }

  cleanup_package_dir <- function(dir, dry_run = FALSE) {
    git_worktree_remove(dir, dry_run = dry_run)
  }

  sha256 <- function(path) {
    digest::digest(path, algo = "sha256", file = TRUE)
  }

  sha256str <- function(str) {
    vapply(str, digest::digest, character(1), algo = "sha256", serialize=FALSE)
  }

  mkdirp <- function(dir, msg = NULL) {
    s <- vlapply(dir, dir.create, recursive = TRUE, showWarnings = FALSE)
    invisible(s)
  }

  rimraf <- function(path) {
    unlink(path, recursive = TRUE, force = TRUE)
  }

  canonize_arch <- function(platform) {
    arch <- strsplit(platform, "-", fixed = TRUE)[[1]][1]
    c("aarch64" = "arm64", "x86_64" = "amd64")[[arch]]
  }

  canonize_os <- function(platform) {
    os <- strsplit(platform, "-", fixed = TRUE)[[1]][3]
    if (substr(os, 1, 6) == "darwin") os <- "darwin"
    if (substr(os, 1, 5) == "mingw") os <- "windows"
    if (substr(os, 1, 7) == "solaris") os <- "solaris"
    os
  }

  get_os_version <- function(platform) {
    os <- strsplit(platform, "-", fixed = TRUE)[[1]][3]
    os
  }

  format_iso_8601 <- function(date) {
    format(as.POSIXlt(date, tz = "UTC"), "%Y-%m-%dT%H:%M:%S+00:00")
  }

  # This is not empty for brew, but it is not useful for us
  image_config <- function(pkgs) {
    rep("{}", nrow(pkgs))
  }

  # Extension from path, but keep .tar.gz
  get_ext <- function(path) {
    pcs <- strsplit(path, ".", fixed = TRUE)
    vapply(pcs, function(x) {
      x <- rev(x)
      if (length(x) == 1) return("")
      if (x[1] == "gz" && length(x) > 1 && x[2] == "tar") {
        "tar.gz"
      } else {
        x[1]
      }
    }, character(1))
  }

  image_title <- function(pkgs) {
    paste0(
      "pak--",
      gsub(".", "-", pkgs$r.version, fixed = TRUE),
      "--",
      pkgs$r.platform,
      "--",
      pkgs$pak.version,
      ".",
      vapply(pkgs$path, get_ext, character(1))
    )
  }

  extract_info <- function(pkg) {
    mkdirp(tmp <- tempfile())
    on.exit(rimraf(tmp), add = TRUE)

    # We need curl's DESCRIPTION as well, because pak's does not have the
    # arch, because R thinks that it does not have compiled code.
    untar(
      pkg,
      exdir = tmp,
      c("pak/DESCRIPTION", "pak/library/curl/DESCRIPTION")
    )
    pakdsc <- desc::desc(file = file.path(tmp, "pak", "DESCRIPTION"))
    curldsc <- desc::desc(
      file = file.path(tmp, "pak", "library", "curl", "DESCRIPTION")
    )

    data.frame(
      stringsAsFactors = FALSE,
      r.platform = curldsc$get_built()$Platform,
      r.version = as.character(pakdsc$get_built()$R[,1:2]),
      pak.version = pakdsc$get_field("Version"),
      pak.revision = "TODO",
      buildtime = format_iso_8601(file.info(pkg)$mtime),
      size = file.size(pkg),
      digest = paste0("sha256:", sha256(pkg)),
      path = pkg
    )
  }

  parse_metadata <- function(pkgs) {
    empty <- data.frame(
      stringsAsFactors = FALSE,
      r.platform = character(),
      r.version = character(),
      pak.version = character(),
      pak.revision = character(),
      buildtime = character(),
      size = integer(),
      digest = character(),
      path = character()
    )
    rbind(
      empty,
      do.call(rbind, lapply(pkgs, extract_info))
    )
  }

  read_metadata <- function(dir, tag) {
    path <- file.path(dir, "manifest.json")
    if (!file.exists(path)) return(NULL)
    mnft <- jsonlite::fromJSON(path)
    if (tag %in% mnft$tags$tag) {
      pfms <- mnft$tags$platforms[[ which(mnft$tags$tag == tag) ]]
      pfms$path <- NA_character_
    } else {
      pfms <- parse_metadata(character())
    }
    pfms
  }

  update_df <- function(old, new, by) {
    clp <- function(df) {
      vapply(seq_len(nrow(df)),
        function(i) paste0(df[i, ], collapse = ";"),
        character(1)
      )
    }
    oldkey <- clp(old[, by])
    newkey <- clp(new[, by])

    mch <- match(newkey, oldkey)
    old[na.omit(mch), ] <- new[!is.na(mch), ]
    old <- rbind(old, new[is.na(mch), ])
    old
  }

  update_metadata <- function(dir, tag, pkgs) {
    old <- read_metadata(dir, tag)
    new <- parse_metadata(pkgs)
    if (is.null(old)) {
      new
    } else {
      update_df(old, new, by = c("r.platform", "r.version"))
    }
  }

  write_file <- function(txt, path) {
    out <- file(path, open = "wb")
    on.exit(close(out), add = TRUE)
    cat(txt, file = out, sep = "")
  }

  write_files <- function(txts, paths) {
    invisible(mapply(write_file, txts, paths))
  }

  image_manifest <- function(pkgs) {

    tmpl <- '
      {
        "schemaVersion": 2,
        "config": {
          "mediaType": "application/vnd.oci.image.config.v1+json",
          "digest": "sha256:<<image_config_hash>>",
          "size": <<nchar(image_config, "bytes")>>
        },
        "layers": [
          {
            "mediaType": "application/vnd.oci.image.layer.v1.tar+gzip",
            "digest": "<<digest>>",
            "size": <<size>>,
            "annotations": {
              "org.opencontainers.image.title": "<<image_title>>"
            }
          }
        ],
        "annotations": {
           <<const_annotations_js()>>,
           "org.opencontainers.image.created": "<<buildtime>>",
           "org.opencontainers.image.version": "<<pak.version>>"
        }
     }'

    glue::glue_data(pkgs, tmpl, .open = "<<", .close = ">>")
  }

  image_index <- function(pkgs) {
    arch <- vapply(pkgs$r.platform, canonize_arch, character(1))
    os <- vapply(pkgs$r.platform, canonize_os, character(1))
    tmpl <- '
      {
        "mediaType": "application/vnd.oci.image.manifest.v1+json",
        "digest": "sha256:<<manifest_hash>>",
        "size": <<nchar(manifest, "bytes")>>,
        "platform": {
          "architecture": "<<arch>>",
          "os": "<<os>>",
          "os.version": "R <<r.version>>",
          "r.version": "<<r.version>>",
          "r.platform": "<<r.platform>>"
        },
        "annotations": {
          "org.opencontainers.image.ref.name": "<<pak.version>>--<<r.version>>--<<r.platform>>",
          "io.r-hub.package.digest": "<<digest>>"
        }
      }'
    mnfts <- glue::glue_data(pkgs, tmpl, .open = "<<", .close = ">>")

    buildtime <- max(pkgs$buildtime)
    pak.version <- pkgs$pak.version[1]

    tmpl2 <- '
      {
        "schemaVersion": 2,
        "manifests": [
           <<paste0(mnfts, collapse = ",\n")>>
        ],
        "annotations": {
           <<const_annotations_js()>>,
           "org.opencontainers.image.created": "<<buildtime>>",
           "org.opencontainers.image.version": "<<pak.version>>"
        }
      }'

    jsonlite::prettify(glue::glue(tmpl2, .open = "<<", .close = ">>"))
  }

  find_skopeo <- function() {
    path <- Sys.which("skopeo")
    if (path != "") return(path)
    if (file.exists(cand <- "/usr/local/bin/skopeo")) return(cand)
    if (file.exists(cand <- "/opt/homebrew/bin/skopeo")) return(cand)
    stop("Need skopeo to push packages")
  }

  skopeo_version <- function() {
    skopeo <- find_skopeo()
    out <- processx::run(skopeo, "--version")
    re_ver <- "[ ]([0-9]+[.][0-9]+[.][0-9]+)"
    if (!grepl(re_ver, out$stdout)) stop("Cannot determine skopeo version")
    mch <- regexpr(re_ver, out$stdout, perl = TRUE)
    beg <- attr(mch, "capture.start")[1]
    end <- beg + attr(mch, "capture.length")[1] - 1L
    package_version(substr(out$stdout, beg, end))
  }

  update_packages <- function(
    paths,
    workdir,
    tag = "devel",
    keep_old = TRUE,
    dry_run = FALSE) {

    shadir <- file.path(workdir, "blobs", "sha256")
    mkdirp(shadir)

    if (keep_old) {
      pkgs <- update_metadata(workdir, tag, paths)
    } else {
      pkgs <- parse_metadata(paths)
    }

    # Copy package files
    tocopy <- match(paths, pkgs$path)
    ret <- file.copy(
      paths,
      file.path(shadir, sub("^sha256:", "", pkgs$digest[tocopy])),
      overwrite = TRUE
    )
    if (any(!ret)) stop("Failed to copy package file(s)")

    # Create image config files. These are dummy files currenty ({}),
    # but in case we switch to proper files, we write them out properly
    pkgs$image_config <- image_config(pkgs)
    pkgs$image_config_hash <- sha256str(pkgs$image_config)
    write_files(pkgs$image_config, file.path(shadir, pkgs$image_config_hash))

    # Image manifests
    pkgs$image_title <- image_title(pkgs)
    pkgs$manifest <- image_manifest(pkgs)
    pkgs$manifest_hash <- sha256str(pkgs$manifest)
    write_files(pkgs$manifest, file.path(shadir, pkgs$manifest_hash))

    if (is_gha()) {
      for (i in seq_len(nrow(pkgs))) {
        cat("IMAGE MANIFEST ", pkgs$manifest_hash[[i]], "\n")
        cat(pkgs$manifest[[i]])
        cat("\n\n")
      }
    }

    # Image index
    imidx <- image_index(pkgs)
    imidx_hash <- sha256str(imidx)
    write_files(imidx, file.path(shadir, imidx_hash))

    if (is_gha()) {
      cat("IMAGE INDEX:\n")
      cat(imidx)
    }

    # index.json
    idxjs <- jsonlite::prettify(glue::glue(
      '{
         "schemaVersion": 2,
         "manifests": [
            {
              "mediaType": "application/vnd.oci.image.index.v1+json",
              "digest": "sha256:<<imidx_hash>>",
              "size": <<nchar(imidx, "bytes")>>
            }
         ]
       }', .open = "<<", .close = ">>"
    ))
    write_files(idxjs, file.path(workdir, "index.json"))

    # OCI version
    cat(
      '{"imageLayoutVersion": "1.0.0"}\n',
      file = file.path(workdir, "oci-layout")
    )

    policy_file <- file.path(workdir, "policy.json")
    policy <- jsonlite::prettify(
      '{
         "default": [
           {
             "type": "insecureAcceptAnything"
           }
         ],
         "transports":
           {
             "docker-daemon":
               {
                 "": [{"type":"insecureAcceptAnything"}]
               }
           }
       }')
    write_files(policy, policy_file)

    skopeo <- find_skopeo()
    skopeo_ver <- skopeo_version()
    if (skopeo_ver < "1.6.0") {
      if (any(grepl("[.]zip$", paths))) {
        stop("Need at least skopeo 1.6.0 to update .zip files")
      }
      warning("Skopeo is too old (< 1.6.0), cannot use --preserve-digests")
    }

    workdir <- normalizePath(workdir, winslash = "/")
    args <- c("copy", "--all", "--retry-times", 20)
    if (skopeo_ver >= "1.6.0") args <- c(args, "--preserve-digests")
    args <- c(args, "--policy", policy_file)
    args <- c(args, paste0("--dest-creds=", ghcr_user(), ":", ghcr_token()))
    args <- c(args, paste0("oci:", workdir), paste0(ghcr_uri(), ":", tag))

    if (is_gha()) {
      cat("BLOBS:\n")
      print(
        file.info(dir(shadir, full.names=TRUE))[, c("size"), drop = FALSE]
      )
    }

    if (dry_run) {
      cat(skopeo, args, "\n")
    } else {
      tries <- 10
      repeat {
        tries <- tries - 1
        tryCatch({
          processx::run(skopeo, args, echo_cmd = TRUE, echo = TRUE)
          break;
        }, error = function(e) {
          if (tries == 0) stop(e)
          tries <<- tries - 1
        })
      }
    }

    invisible(pkgs)
  }

  update_manifest <- function(workdir, tag, pkgs) {
    cols <- c("r.platform", "r.version", "pak.version", "pak.revision",
              "buildtime", "size", "digest")
    pfms <- pkgs[,cols]
    newtags <- data.frame(
      stringsAsFactors = FALSE,
      tag = tag,
      platforms = I(list(pfms))
    )

    path <- file.path(workdir, "manifest.json")
    if (file.exists(path)) {
      old <- jsonlite::fromJSON(path)
    } else {
      old <- jsonlite::fromJSON('{ "schemaVersion": 1, "tags": [] }')
    }
    if (length(old$tags) == 0) {
      old$tags <- data.frame(
        stringsAsFactors = FALSE,
        tag = character(),
        platforms = I(list())
      )
    }

    wh <- match(tag, old$tags$tag)
    if (!is.na(wh)) {
      old$tags[[wh, "platforms"]] <- newtags$platforms[[1]]
    } else {
      old$tags <- rbind(old$tags, newtags)
    }
    old$schemaVersion <- jsonlite::unbox(old$schemaVersion)

    json <- jsonlite::toJSON(old, pretty = TRUE)
    out <- file(path, open = "wb")
    on.exit(close(out), add = TRUE)
    cat(json, file = out)
  }

  push_manifest <- function(workdir, dry_run = FALSE) {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)
    setwd(workdir)
    git("add", "manifest.json", dry_run = dry_run)
    git("commit", "--allow-empty", "-m", "Auto-update packages", dry_run = dry_run)
    git("push", "--porcelain", "origin", stderr_to_stdout = TRUE, dry_run = dry_run)
  }

  push_packages <- function(paths, tag = "auto", keep_old = TRUE,
                            dry_run = FALSE, cleanup = TRUE) {
    dry_run

    if (tag == "auto") {
      version <- unclass(package_version(utils::packageVersion("pak")))[[1]]
      if (length(version) >= 4 && version[4] == 9999) {
        # rc is also pushed to devel, as devel should be the latest
        p1 <- push_packages(paths, "rc", keep_old, dry_run, cleanup)
        p2 <- push_packages(paths, "devel", keep_old, dry_run, cleanup)
        return(invisible(rbind(p1, p2)))

      } else if (length(version) >= 4 && version[4] >= 9000) {
        tag <- "devel"

      } else {
        # stable is also pushed to rc
        p1 <- push_packages(paths, "stable", keep_old, dry_run, cleanup)
        p2 <- push_packages(paths, "rc", keep_old, dry_run, cleanup)
        return(invisible(rbind(p1, p2)))
      }
    }

    workdir <- package_dir()

    if (!file.exists(workdir)) {
      init_package_dir(workdir, dry_run = dry_run)
      if (cleanup) {
        on.exit(cleanup_package_dir(workdir, dry_run = dry_run), add = TRUE)
      }
    }

    # We need to try the update several times, because other processes
    # might be updating the git repo as well at the same time, so we might
    # not be able to push.
    repeat {
      git_pull(workdir, dry_run = dry_run)
      pkgs <- update_packages(paths, workdir, tag, keep_old, dry_run)
      update_manifest(workdir, tag, pkgs)
      tryCatch({
        push_manifest(workdir, dry_run = dry_run)
        break
      }, error = function(err) {
        if (!grepl("(non-fast-forward|fetch first|cannot lock ref)",
                   err$stderr)) {
          stop(err)
        }
        old <- getwd()
        on.exit(setwd(old), add = TRUE)
        setwd(workdir)
        git("reset", "HEAD^", dry_run = dry_run)
        git("checkout", "--", ".", dry_run = dry_run)
      })
    }

    invisible(pkgs)
  }
})

create_pak_repo <- local({

  init_package_dir    <- import_from("push_packages", "init_package_dir")
  cleanup_package_dir <- import_from("push_packages", "cleanup_package_dir")
  git_pull            <- import_from("push_packages", "git_pull")
  mkdirp              <- import_from("push_packages", "mkdirp")
  package_dir         <- import_from("push_packages", "package_dir")
  read_metadata       <- import_from("push_packages", "read_metadata")
  sha256              <- import_from("push_packages", "sha256")

  tags <- c("stable", "rc", "devel")

  cpu_map <- c(
    "arm64" = "aarch64",
    "amd64" = "x86_64"
  )

  os_map <- c(
    "linux-musl" = "linux",
    "linux-gnu" = "linux"
  )

  # The REAL directories containing the packages are:
  # - linux/x86_64
  # - linux/aarch64
  # - darwin15.6.0/x86_64
  # - darwin17.0/x86_64
  # - darwin20/aarch64
  # - mingw32/x86_64

  # ## New form of the install command will use these repo URL and paths:
  # ```
  # source/linux-gnu/x85_64 + /src/contrib ->
  #      linux/x86_64
  # source/linux-gnu/aarch64 + /src/contrib ->
  #      linux/aarch64
  # mac.binary.big-sur-arm64/darwin20/aarch64 + /bin/macosx/big-sur-arm64/contrib/4.1 ->
  #      darwin20/aarch64
  # mac.binary/darwin17.0/x86_64 + /bin/macosx/contrib/4.1 ->
  #      darwin17.0/x86_64
  # mac.binary.el-capitan/darwin15.6.0/x86_64 + /bin/macosx/el-capitan/contrib/3.6 ->
  #      darwin15.6.0/x86_64
  # win.binary/mingw32/x86_64 + /bin/windows/contrib/4.1 ->
  #      mingw32/x86_64
  # win.binary/mingw32/i386 + /bin/windows/contrib/4.1 ->
  #      mingw32/x86_64
  # ```
  #
  # Unsupported platforms will get a non-existant path, e.g. homebrew R
  # will get source/darwin21.1.0/aarch64 or similar, which does not exist.
  # We can have an alternative repo URL for these later, so people can
  # also install from source.
  #
  # This is the compatiblity mapping for the older, generic repo URL:
  # ```
  # /src/contrib                           -> linux/x86_64
  # /bin/macosx/big-sur-arm64/contrib/4.1  -> darwin20/aarch64
  # /bin/macosx/contrib/4.1                -> darwin17.0/x86_64
  # /bin/macosx/big-sur-x86_64/contrib/4.3 -> darwin20.0/x86_64
  # /bin/macosx/el-capitan/contrib/3.6     -> darwin15.6.0/x86_64
  # /bin/windows/contrib/4.1               -> mingw32/x86_64
  # ```

  links <- c(
    # Make sure all Linux maps to the same place since we fully static pkgs
    "linux-gnu/x86_64"       = "../../linux/x86_64",
    "linux-musl/x86_64"      = "../../linux/x86_64",
    "linux-uclibc/x86_64"    = "../../linux/x86_64",
    "linux-dietlibc/x86_64"  = "../../linux/x86_64",
    "linux-unknown/x86_64"   = "../../linux/x86_64",

    "linux-gnu/aarch64"      = "../../linux/aarch64",
    "linux-musl/aarch64"     = "../../linux/aarch64",
    "linux-uclibc/aarch64"   = "../../linux/aarch64",
    "linux-dietlibc/aarch64" = "../../linux/aarch64",
    "linux-unknown/aarch64"  = "../../linux/aarch64",

    # On Windows we server bi-arch packages:
    "mingw32/i386" = "../x86_64",

    # Map the pkgType/os/arch packages to os/arch on Linux, because on
    # Linux we serve binaries as sources, but on other OSes not.
    "source/linux/x86_64/src/contrib"           = "../../../../../linux/x86_64",
    "source/linux-gnu/x86_64/src/contrib"       = "../../../../../linux/x86_64",
    "source/linux-musl/x86_64/src/contrib"      = "../../../../../linux/x86_64",
    "source/linux-uclibc/x86_64/src/contrib"    = "../../../../../linux/x86_64",
    "source/linux-dietlibc/x86_64/src/contrib"  = "../../../../../linux/x86_64",
    "source/linux-unknown/x86_64/src/contrib"   = "../../../../../linux/x86_64",

    "source/linux/aarch64/src/contrib"          = "../../../../../linux/aarch64",
    "source/linux-gnu/aarch64/src/contrib"      = "../../../../../linux/aarch64",
    "source/linux-musl/aarch64/src/contrib"     = "../../../../../linux/aarch64",
    "source/linux-uclibc/aarch64/src/contrib"   = "../../../../../linux/aarch64",
    "source/linux-dietlibc/aarch64/src/contrib" = "../../../../../linux/aarch64",
    "source/linux-unknown/aarch64/src/contrib"  = "../../../../../linux/aarch64",

    # Map the pkgType/os/arch form binaries of other OSes to the right place.
    "win.binary/mingw32/x86_64/src/contrib" = "../../../../../mingw32/x86_64",
    "mac.binary.big-sur-arm64/darwin20/aarch64/src/contrib" = "../../../../../darwin20/aarch64",
    "mac.binary/darwin17.0/x86_64/src/contrib" = "../../../../../darwin17.0/x86_64",
    "mac.binary.el-capitan/darwin15.6.0/x86_64/src/contrib" = "../../../../../darwin15.6.0/x86_64",
    "mac.binary.big-sur-x86_64/darwin20/x86_64/src/contrib" = "../../../../../darwin20/x86_64",

    "win.binary/mingw32/x86_64/bin/windows/contrib/3.4" = "../../../../../../../mingw32/x86_64",
    "win.binary/mingw32/x86_64/bin/windows/contrib/3.5" = "../../../../../../../mingw32/x86_64",
    "win.binary/mingw32/x86_64/bin/windows/contrib/3.6" = "../../../../../../../mingw32/x86_64",
    "win.binary/mingw32/x86_64/bin/windows/contrib/4.0" = "../../../../../../../mingw32/x86_64",
    "win.binary/mingw32/x86_64/bin/windows/contrib/4.1" = "../../../../../../../mingw32/x86_64",
    "win.binary/mingw32/x86_64/bin/windows/contrib/4.2" = "../../../../../../../mingw32/x86_64",
    "win.binary/mingw32/x86_64/bin/windows/contrib/4.3" = "../../../../../../../mingw32/x86_64",
    "win.binary/mingw32/x86_64/bin/windows/contrib/4.4" = "../../../../../../../mingw32/x86_64",

    "mac.binary.big-sur-arm64/darwin20/aarch64/bin/macosx/big-sur-arm64/contrib/4.1" = "../../../../../../../../darwin20/aarch64",
    "mac.binary.big-sur-arm64/darwin20/aarch64/bin/macosx/big-sur-arm64/contrib/4.2" = "../../../../../../../../darwin20/aarch64",
    "mac.binary.big-sur-arm64/darwin20/aarch64/bin/macosx/big-sur-arm64/contrib/4.3" = "../../../../../../../../darwin20/aarch64",
    "mac.binary.big-sur-arm64/darwin20/aarch64/bin/macosx/big-sur-arm64/contrib/4.4" = "../../../../../../../../darwin20/aarch64",
    "mac.binary.big-sur-x86_64/darwin20/x86_64/bin/macosx/big-sur-x86_64/contrib/4.3" = "../../../../../../../../darwin20/x86_64",
    "mac.binary.big-sur-x86_64/darwin20/x86_64/bin/macosx/big-sur-x86_64/contrib/4.4" = "../../../../../../../../darwin20/x86_64",
    "mac.binary/darwin17.0/x86_64/bin/macosx/contrib/4.0" = "../../../../../../../darwin17.0/x86_64",
    "mac.binary/darwin17.0/x86_64/bin/macosx/contrib/4.1" = "../../../../../../../darwin17.0/x86_64",
    "mac.binary/darwin17.0/x86_64/bin/macosx/contrib/4.2" = "../../../../../../../darwin17.0/x86_64",
    "mac.binary/darwin17.0/x86_64/bin/macosx/contrib/4.3" = "../../../../../../../darwin17.0/x86_64",
    "mac.binary/darwin17.0/x86_64/bin/macosx/contrib/4.4" = "../../../../../../../darwin17.0/x86_64",
    "mac.binary.el-capitan/darwin15.6.0/x86_64/bin/macosx/el-capitan/contrib/3.4" = "../../../../../../../../darwin15.6.0/x86_64",
    "mac.binary.el-capitan/darwin15.6.0/x86_64/bin/macosx/el-capitan/contrib/3.5" = "../../../../../../../../darwin15.6.0/x86_64",
    "mac.binary.el-capitan/darwin15.6.0/x86_64/bin/macosx/el-capitan/contrib/3.6" = "../../../../../../../../darwin15.6.0/x86_64",

    # Now the compatibility maps, these are for the old
    # repos = "https://r-lib.github.io/p/pak/stable" form
    "bin/windows/contrib/3.4" = "../../../../mingw32/x86_64",
    "bin/windows/contrib/3.5" = "../../../../mingw32/x86_64",
    "bin/windows/contrib/3.6" = "../../../../mingw32/x86_64",
    "bin/windows/contrib/4.0" = "../../../../mingw32/x86_64",
    "bin/windows/contrib/4.1" = "../../../../mingw32/x86_64",
    "bin/windows/contrib/4.2" = "../../../../mingw32/x86_64",
    "bin/windows/contrib/4.3" = "../../../../mingw32/x86_64",
    "bin/windows/contrib/4.4" = "../../../../mingw32/x86_64",

    "bin/macosx/big-sur-arm64/contrib/4.1" = "../../../../../darwin20/aarch64",
    "bin/macosx/big-sur-arm64/contrib/4.2" = "../../../../../darwin20/aarch64",
    "bin/macosx/big-sur-arm64/contrib/4.3" = "../../../../../darwin20/aarch64",
    "bin/macosx/big-sur-arm64/contrib/4.4" = "../../../../../darwin20/aarch64",
    "bin/macosx/contrib/4.0" = "../../../../darwin17.0/x86_64",
    "bin/macosx/contrib/4.1" = "../../../../darwin17.0/x86_64",
    "bin/macosx/contrib/4.2" = "../../../../darwin17.0/x86_64",
    "bin/macosx/contrib/4.3" = "../../../../darwin17.0/x86_64",
    "bin/macosx/contrib/4.4" = "../../../../darwin17.0/x86_64",
    "bin/macosx/el-capitan/contrib/3.4" = "../../../../../darwin15.6.0/x86_64",
    "bin/macosx/el-capitan/contrib/3.5" = "../../../../../darwin15.6.0/x86_64",
    "bin/macosx/el-capitan/contrib/3.6" = "../../../../../darwin15.6.0/x86_64",
    "bin/macosx/big-sur-x86_64/contrib/4.3" = "../../../../../darwin20/x86_64",
    "bin/macosx/big-sur-x86_64/contrib/4.4" = "../../../../../darwin20/x86_64",

    "src/contrib" = "../../linux/x86_64"
  )

  download_uri <- function() {
    Sys.getenv(
      "PAK_GHCP_DOWNLOAD_URI",
      "https://ghcr.io/v2/r-lib/pak/blobs"
    )
  }

  `%NA%` <- function(x, y) {
    ifelse(is.na(x), y, x)
  }

  seq_rows <- function(x) {
    seq_len(nrow(x))
  }

  parse_platform <- function(x) {
    pcs <- strsplit(x, "-", fixed = TRUE)
    data.frame(
      stringsAsFactors = FALSE,
      cpu = vcapply(pcs, "[", 1),
      vendor = vcapply(pcs, "[", 2),
      os = vcapply(pcs, function(y) {
        if (length(y) < 3) NA_character_ else paste(y[-(1:2)], collapse = "-")
      })
    )
  }

  pkgfile_ext <- function(x) {
    ifelse(
      x == "mingw32",
      ".zip",
      ifelse(grepl("^darwin", x), ".tgz", ".tar.gz")
    )
  }

  add_repo_links <- function(root, tag) {
    repo_root <- file.path(root, tag)
    for (idx in seq_along(links)) {
      link <- file.path(repo_root, names(links)[idx])
      mkdirp(link)
      orig <- file.path(link, links[[idx]])
      origfile <- file.path(orig, "PACKAGES")
      linkfile <- file.path(link, "PACKAGES")
      if (!file.exists(origfile)) next
      file.copy(origfile, linkfile, overwrite = TRUE)
      # TODO: properly update the file
      lines <- c(readLines(origfile), "")
      entry <- paste0("Path: ", links[[idx]], "\n")
      lines[nchar(lines) == 0] <- entry
      writeLines(lines, linkfile)
      tab <- read.dcf(linkfile, all = TRUE)
      write_dcf(tab, linkfile, quiet = TRUE)
    }
  }

  write_dcf <- function(meta, PACKAGES, quiet = FALSE) {
    if (!quiet) cat("Writing ", PACKAGES, "\n")
    meta <- as.matrix(meta)
    write.dcf(meta, PACKAGES, width = 200)
    con <- gzfile(paste0(PACKAGES, ".gz"), "wt")
    write.dcf(meta, con, width = 200)
    close(con)
    saveRDS(meta, paste0(PACKAGES, ".rds"), compress = "xz", version = 2)
  }

  create_packages_files <- function(data, root, tag) {
    data$dir <- dirname(data$path)
    data$sha <- unname(vapply(data$path, sha256, ""))
    if (any(data$digest != paste0("sha256:", data$sha))) stop("SHA mismatch")
    baseuri <- download_uri()
    field <- function(f) vapply(dscs, function(x) x$get_field(f), "")
    fix_built <- function(built, platform) {
      mapply("; ;", paste0("; ", platform, ";"), built, FUN = sub)
    }

    dscs <- lapply(data$path, desc::desc)
    data$Package <- field("Package")
    data$Version <- field("Version")
    data$Depends <- paste0(
      "R (>= ", data$r.version, "), R (<= ", data$r.version, ".99)"
    )
    data$Imports <- field("Imports")
    data$MD5sum <- unname(tools::md5sum(data$path))
    data$Sha256 <- data$sha
    data$NeedsCompilation <- "no"
    data$License <- field("License")
    data$Built <- fix_built(field("Built"), data$r.platform)
    data$File <- basename(data$path)
    data$DownloadURL <- paste0(baseuri, "/", data$digest)
    plat <- parse_platform(data$r.platform)
    data$OS <- os_map[plat$os] %NA% plat$os
    data$Arch <- cpu_map[plat$cpu] %NA% plat$cpu

    # we need to allow allow newer R versions, in case the
    # version number of R-devel is increased. This is per
    # PACKAGES file.
    for (dir in unique(data$dir)) {
      idx <- which(dir == data$dir)
      max_r_version <- as.character(max(package_version(data$r.version[idx])))
      below_r_version <- ifelse(
        data$r.version[idx] == max_r_version,
        "10.0.0",
        paste0(data$r.version[idx], ".99")
      )
      data$Depends[idx] <- paste0(
        "R (>= ", data$r.version[idx], "), R (<= ", below_r_version, ")"
      )
    }

    cols <- c("Package", "Version", "Depends", "Imports", "License",
              "MD5sum", "Sha256", "NeedsCompilation", "Built", "File",
              "DownloadURL", "OS", "Arch")
    meta <- main <- data[, cols]

    main_file <- file.path(root, tag, "metadata.json")
    json <- jsonlite::toJSON(main, pretty = TRUE)
    out <- file(main_file, open = "wb")
    on.exit(close(out), add = TRUE)
    cat(json, file = out)

    for (dir in unique(data$dir)) {
      PACKAGES <- file.path(dir, "PACKAGES")
      unlink(PACKAGES)
      local <- meta[dir == data$dir, ]
      write_dcf(local, PACKAGES)
    }
  }

  create_package_repo_tag <- function(root, workdir, tag, dry_run = FALSE) {
    data <- read_metadata(workdir, tag)
    plat <- parse_platform(data$r.platform)
    cpu <- cpu_map[plat$cpu] %NA% plat$cpu
    os <- os_map[plat$os] %NA% plat$os
    pkgdir <- file.path(root, tag, os, cpu)
    mkdirp(unique(pkgdir))

    pkgfile <- paste0(
      "pak_",
      data$pak.version, "_",
      "R-", gsub("[.]", "-", data$r.version), "_",
      cpu, "-", os,
      pkgfile_ext(os)
    )

    data$path <- file.path(pkgdir, pkgfile)
    miss <- !file.exists(data$path)
    bad <- rep(NA, length(miss))
    bad[!miss] <-
      paste0("sha256:", vcapply(data$path[!miss], sha256)) != data$digest[!miss]

    for (idx in which(miss | bad)) {
      h <- curl::new_handle()
      curl::handle_setheaders(h, Authorization = "Bearer QQ==")
      url <- paste0(download_uri(), "/", data$digest[idx])
      cat("Getting", url, "->\n    ", pkgfile[idx], "\n")
      curl::curl_download(url, data$path[idx], handle = h, quiet = FALSE)
    }

    create_packages_files(data, root, tag)

    add_repo_links(root, tag)
  }

  create_pak_repo <- function(path = "repo", dry_run = FALSE, cleanup = TRUE) {
    workdir <- package_dir()
    if (!file.exists(workdir)) {
      init_package_dir(workdir, dry_run = dry_run)
      if (cleanup) {
        on.exit(cleanup_package_dir(workdir, dry_run = dry_run), add = TRUE)
      }
    }

    git_pull(workdir, dry_run = dry_run)
    root <- file.path(path, "p", "pak")
    mkdirp(root)

    cat("\n", file = file.path(path, ".nojekyll"))

    for (tag in tags) {
      create_package_repo_tag(root, workdir, tag = tag, dry_run = dry_run)
    }

    file.symlink("stable", file.path(root, "dev"))
  }
})
