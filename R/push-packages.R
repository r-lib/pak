
push_packages <- local({

  package_dir <- function() {
    Sys.getenv("PAK_PACKAGE_DIR", tempfile())
  }

  git <- function (..., echo_cmd = TRUE, echo = TRUE) {
    processx::run("git", c(...), echo_cmd = echo_cmd, echo = echo)
  }

  init_package_dir <- function() {
    dir <- package_dir()
    if (!file.exists(dir)) {
      git("remote", "set-branches", "--add", "origin", "packages")
      git("fetch", remote, branch)
      github_worktree_add(dir, "origin", "packages")
    }
    dir
  }

  sha256 <- function(path) {
    digest::digest(path, algo = "sha256", file = TRUE)
  }

  diff_id <- function(path) {
    con <- gzfile(path, open = "rb")
    on.exit(close(con), add = TRUE)
    digest::digest(
      serialize = FALSE,
      algo="sha256",
      readBin(con, n = 100000000, what = "raw")
    )
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

  image_config <- function(path, meta) {
    U <- jsonlite::unbox
    diff_ids <- diff_id(path)
    config <- list(
      architecture = U(meta$platform$architecture),
      os = U(meta$platform$os),
      os.version = U(meta$platform$os.version),
      r.version = U(meta$platform$r.version),
      r.platform = U(meta$platform$r.platform),
      rootfs = list(
        type = U("layers"),
        diff_ids = paste0("sha256:", diff_ids)
      )
    )

    paste0(jsonlite::toJSON(config, pretty = TRUE), "\n")
  }

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

  image_title <- function(path, meta) {
    paste0(
      "pak--",
      gsub(".", "-", meta$platform$r.version, fixed = TRUE),
      "--",
      meta$platform$r.platform,
      "--",
      meta$annotations$org.opencontainers.image.version,
      ".",
      get_ext(path)
    )
  }

  image_manifest <- function(path, sha, meta, image_config, image_config_hash,
                             image_title) {
    U <- jsonlite::unbox
    manifest <- list(
      schemaVersion = U(2L),
      config = list(
        mediaType = U("application/vnd.oci.image.config.v1+json"),
        digest = U(paste0("sha256:", image_config_hash)),
        size = U(nchar(image_config, "bytes"))
      ),
      layers = list(
        list(
          mediaType = U("application/vnd.oci.image.layer.v1.tar+gzip"),
          digest = U(paste0("sha256:", sha)),
          size = U(file.size(path)),
          annotations = list(
            org.opencontainers.image.title = U(image_title)
          )
        )
      ),
      annotations = lapply(meta$annotations, U)
    )

    paste0(jsonlite::toJSON(manifest, pretty = TRUE), "\n")
  }

  image_index <- function(pkgs) {
    U <- jsonlite::unbox

    mfst <- function(meta, sha, manifest, manifest_hash) {
      list(
        mediaType = U("application/vnd.oci.image.manifest.v1+json"),
        digest = U(paste0("sha256:", manifest_hash)),
        size = U(nchar(manifest, "bytes")),
        platform = list(
          architecture = U(meta$platform$architecture),
          os = U(meta$platform$os),
          os.version = U(meta$platform$os.version),
          r.version = U(meta$platform$r.version),
          r.platform = U(meta$platform$r.platform)
        ),
        annotations = list(
          org.opencontainers.image.ref.name =
            U(meta$annotations$org.opencontainers.image.ref.name),
          "io.r-hub.package.digest" = U(sha)
        )
      )
    }

    manifests <- mapply(
      "mfst", SIMPLIFY = FALSE,
      pkgs$meta, pkgs$sha,  pkgs$manifest, pkgs$manifest_hash
    )

    annotations <- pkgs$meta[[1]]$annotations
    tss <- vapply(
      pkgs$meta,
      function(x) x$annotations$org.opencontainers.image.created,
      character(1)
    )
    annotations$org.opencontainers.image.created <- max(tss)

    idx <- list(
      schemaVersion = U(2L),
      manifests = manifests,
      annotations = lapply(annotations, U)
    )

    paste0(jsonlite::toJSON(idx, pretty = TRUE), "\n")
  }

  sha256str <- function(str) {
    vapply(str, digest::digest, character(1), algo = "sha256", serialize=FALSE)
  }

  get_metadata <- function(pkg, tag) {
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

    platform_string <- curldsc$get_built()$Platform

    platform <- list(
      architecture = canonize_arch(platform_string),
      os = canonize_os(platform_string),
      os.version = get_os_version(platform_string),
      r.version = as.character(pakdsc$get_built()$R[,1:2]),
      r.platform = platform_string
    )

    annotations <- list(
      com.github.package.type = "r_package",
      org.opencontainers.image.created =
        format_iso_8601(file.info(pkg)$mtime),
      org.opencontainers.image.authors = "Gabor Csardi",
      org.opencontainers.image.url = "https://github.com/r-lib/pak",
      org.opencontainers.image.documentation = "https://pak.r-lib.org/",
      org.opencontainers.image.source = "https://github.com/r-lib/pak",
      org.opencontainers.image.version = pakdsc$get_field("Version"),
      org.opencontainers.image.ref.name = tag,
      # org.opencontainers.image.revision =
      org.opencontainers.image.licenses = pakdsc$get_field("License"),
      org.opencontainers.image.title = "pak R package",
      org.opencontainers.image.description = "Package manager for R"
    )

    list(
      platform = platform,
      annotations = annotations
    )
  }

  function(pkgs, tag = "devel") {
    dir <- init_package_dir()
    shadir <- file.path(dir, "blobs", "sha256")
    mkdirp(shadir)

    # Copy package files
    pkgs <- data.frame(path = pkgs, stringsAsFactors = FALSE)
    pkgs$sha <- vapply(pkgs$path, "sha256", character(1))
    file.copy(pkgs$path, file.path(shadir, pkgs$sha))

    # Extract metadata
    pkgs$meta <- I(lapply(pkgs$path, "get_metadata", tag = tag))

    # Create image config files
    pkgs$image_config <- mapply("image_config", pkgs$path, pkgs$meta)
    pkgs$image_config_hash <- sha256str(pkgs$image_config)
    mapply(
      cat,
      pkgs$image_config,
      file = file.path(shadir, pkgs$image_config_hash),
      sep = ""
    )

    # Image manifests
    pkgs$image_title <- mapply("image_title", pkgs$path, pkgs$meta)
    pkgs$manifest <- mapply(
      "image_manifest",
      pkgs$path,
      pkgs$sha,
      pkgs$meta,
      pkgs$image_config,
      pkgs$image_config_hash,
      pkgs$image_title
    )
    pkgs$manifest_hash <- sha256str(pkgs$manifest)
    mapply(
      cat,
      pkgs$manifest,
      file = file.path(shadir, pkgs$manifest_hash),
      sep = ""
    )

    # Image index
    idx <- image_index(pkgs)
    idx_hash <- sha256str(idx)
    cat(idx, file = file.path(shadir, idx_hash))

    # index.json
    U <- jsonlite::unbox
    idx2 <- list(
      schemaVersion = U(2L),
      manifests = list(
        list(
          mediaType = U("application/vnd.oci.image.index.v1+json"),
          digest = U(paste0("sha256:", idx_hash)),
          size = U(nchar(idx, "bytes"))
        )
      )
    )

    idx2txt <- paste0(jsonlite::toJSON(idx2, pretty = TRUE), "\n")
    cat(idx2txt, file = file.path(dir, "index.json"))

    invisible(dir)
  }
})
