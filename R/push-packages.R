
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

  const_annotations_js <- function() {
    paste0(
      glue::glue('"{names(const_annotations)}": "{const_annotations}"'),
      collapse = ",\n      "
    )
  }

  ghcr_uri <- function() {
    Sys.getenv(
      "PAK_GHCR_URI",
      "docker://ghcr.io/gaborcsardi/playground/pak"
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

  git <- function (..., echo_cmd = TRUE, echo = TRUE, dry_run = FALSE) {
    if (dry_run) {
      cat("git", c(...), "\n")
    } else {
      processx::run("git", c(...), echo_cmd = echo_cmd, echo = echo)
    }
  }

  git_pull <- function(dir, dry_run = FALSE) {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)
    setwd(dir)
    git("pull", dry_run = dry_run)
  }

  git_worktree_add <- function(dir, remote, branch, dry_run = FALSE) {
    commit <- paste0(remote, "/", branch)
    git("worktree", "add", "--track", "-B", branch, dir, commit, dry_run = dry_run)
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
    mnft <- jsonlite::fromJSON(file.path(dir, "manifest.json"))
    if (tag %in% mnft$tags$tag) {
      pfms <- mnft$tags$platforms[[ mnft$tags$tag == tag ]]
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
    update_df(old, new, by = c("r.platform", "r.version"))
  }

  write_files <- function(txts, paths) {
    invisible(mapply(cat, txts, file = paths, sep = ""))
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

  check_skopeo <- function() {
    if (Sys.which("skopeo") == "") stop("Need skopeo to push packages")
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
    file.copy(paths, file.path(shadir, sub("^sha256:", "", pkgs$digest)))

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

    # Image index
    imidx <- image_index(pkgs)
    imidx_hash <- sha256str(imidx)
    write_files(imidx, file.path(shadir, imidx_hash))

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

    args <- c("copy", "--all")
    args <- c(args, paste0("--dest-creds=", ghcr_user(), ":", ghcr_token()))
    args <- c(args, paste0("oci:", workdir), paste0(ghcr_uri(), ":", tag))
    if (dry_run) {
      cat("skopeo", args, "\n")
    } else {
      processx::run("skopeo", args, echo_cmd = TRUE, echo = TRUE)
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
    cat(json, file = path)
  }

  push_manifest <- function(workdir, dry_run = FALSE) {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)
    setwd(workdir)
    git("add", "manifest.json", dry_run = dry_run)
    git("commit", "--allow-empty", "-m", "Auto-update packages", dry_run = dry_run)
    git("push", "origin", dry_run = dry_run)
  }

  function(paths, tag = "devel", keep_old = TRUE, dry_run = FALSE) {
    dry_run
    workdir <- package_dir()

    if (!file.exists(workdir)) {
      init_package_dir(workdir, dry_run = dry_run)
      on.exit(cleanup_package_dir(workdir, dry_run = dry_run), add = TRUE)
    }
    git_pull(workdir, dry_run = dry_run)

    pkgs <- update_packages(paths, workdir = workdir, tag, keep_old, dry_run)
    update_manifest(workdir, tag, pkgs)
    push_manifest(workdir, dry_run = dry_run)

    invisible(pkgs)
  }
})
