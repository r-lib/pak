ghcr_const_annotations <- list(
  com.github.package.type = "r_package"
  #  org.opencontainers.image.authors = "Gabor Csardi",
  #  org.opencontainers.image.url = "https://github.com/r-lib/pak",
  #  org.opencontainers.image.documentation = "https://pak.r-lib.org/",
  #  org.opencontainers.image.source = "https://github.com/r-lib/pak",
  #  org.opencontainers.image.title = "pak R package",
  #  org.opencontainers.image.description = "Package manager for R",
  #  org.opencontainers.image.licenses = "GPL-3"
)

ghcr_const_annotations_js <- function() {
  paste0(
    glue::glue('"{names(ghcr_const_annotations)}": "{ghcr_const_annotations}"'),
    collapse = ",\n      "
  )
}

ghcr_get_package_data <- function(path) {
  if (!file.exists(path)) {
    throw(pkg_error("{.path {path}} does not exist."))
  }

  plt <- pkgcache::current_r_platform_data()
  if (plt$os != "linux" && !grepl("^linux-", plt$os)) {
    throw(pkg_error(
      "{.fn ghcr_push_package} only works on Linux.",
      "i" = "It does not work on this platform ({.var {plt$platform}}) yet."
    ))
  }

  chain_error(
    dsc <- desc::desc(path),
    pkg_error(
      "Cannot read {.code DESCRIPTION} from {.file {path}}.",
      "i" = "A valid R package must have a {.code DESCRIPTION} file."
    )
  )

  chain_error(
    built <- dsc$get_built(),
    pkg_error(
      "Cannot find {.code Built} field in package at {.file {path}}",
      "i" = "A binary R package must have a {.code Built} field in
             {.code DESCRIPTION}."
    )
  )

  sha256 <- cli::hash_file_sha256(path)
  data_frame(
    path = path,
    md5 = cli::hash_file_md5(path),
    sha256 = sha256,
    digest = paste0("sha256:", sha256),
    image_title = paste("R package", dsc$get_field("Package")),
    size = file.size(path),
    buildtime = format_iso_8601(built$Date),
    r_version = as.character(built$R),
    r_platform = built$Platform,
    package_name = dsc$get_field("Package"),
    package_version = dsc$get_field("Version"),
    license = dsc$get_field("License"),
    arch = ghcr_canonize_arch(built$Platform),
    os = ghcr_canonize_os(built$Platform)
  )
}

ghcr_create_oci_repo <- function(
  pkgs,
  repo = NULL,
  oci_path = NULL,
  cleanup = TRUE
) {
  if (nrow(pkgs) > 1) {
    throw(pkg_error("Multiple package versions are not supported yet."))
  }

  repo <- repo %||% Sys.getenv("GITHUB_REPOSITORY", NA_character_)
  if (is.na(repo)) {
    throw(pkg_error("Cannot detect GitHub repository"))
  }

  oci_path <- oci_path %||% tempfile()
  mkdirp(oci_path)
  blob_path <- file.path(oci_path, "blobs", "sha256")
  mkdirp(blob_path)

  # Create oci-layout file
  cat(
    '{"imageLayoutVersion": "1.0.0"}\n',
    file = file.path(oci_path, "oci-layout")
  )

  # Copy package files to blobs
  file.copy(pkgs$path, file.path(blob_path, pkgs$sha256))

  # Config is dummy now
  pkgs$image_config <- "{}"
  pkgs$image_config_hash <- cli::hash_sha256(pkgs$image_config)
  write_files(pkgs$image_config, file.path(blob_path, pkgs$image_config_hash))

  # Create manifests
  pkgs$manifest <- ghcr_image_manifest(pkgs)
  pkgs$manifest_hash <- cli::hash_sha256(pkgs$manifest)
  write_files(pkgs$manifest, file.path(blob_path, pkgs$manifest_hash))

  # Create index
  imidx <- ghcr_image_index(pkgs)
  imidx_hash <- cli::hash_sha256(imidx)
  write_files(imidx, file.path(blob_path, imidx_hash))

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
     }',
    .open = "<<",
    .close = ">>"
  ))
  write_files(idxjs, file.path(oci_path, "index.json"))

  # policy.json
  policy_file <- file.path(oci_path, "policy.json")
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
     }'
  )
  write_files(policy, policy_file)

  oci_path
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

ghcr_push_oci_repo <- function(oci_path, ghcr_tag = NULL) {
  if (!file.exists(oci_path)) {
    throw(pkg_error("Could not find OCI repository at {.path {oci_path}}."))
  }
  ghcr_tag <- ghcr_tag %||% "TODO"

  skopeo <- check_skopeo()
  oci_path <- normalizePath(oci_path, winslash = "/")
  policy_file <- file.path(oci_path, "policy.json")
  args <- c(
    "copy",
    "--all",
    "--retry-times",
    20,
    "--preserve-digests",
    "--policy",
    policy_file,
    paste0("--dest-creds=", ghcr_user(), ":", ghcr_token()),
    paste0("oci:", oci_path),
    paste0(ghcr_uri(), ":", ghcr_tag)
  )
  "TODO"
}

ghcr_canonize_arch <- function(platform) {
  arch <- strsplit(platform, "-", fixed = TRUE)[[1]][1]
  c("aarch64" = "arm64", "x86_64" = "amd64", "s390x" = "s390s")[[arch]]
}

ghcr_canonize_os <- function(platform) {
  os <- strsplit(platform, "-", fixed = TRUE)[[1]][3]
  if (substr(os, 1, 6) == "darwin") os <- "darwin"
  if (substr(os, 1, 5) == "mingw") os <- "windows"
  if (substr(os, 1, 7) == "solaris") os <- "solaris"
  os
}

ghcr_image_index <- function(pkgs) {
  tmpl <- '
    {
      "mediaType": "application/vnd.oci.image.manifest.v1+json",
      "digest": "sha256:<<manifest_hash>>",
      "size": <<nchar(manifest, "bytes")>>,
      "platform": {
        "architecture": "<<arch>>",
        "os": "<<os>>",
        "os.version": "R <<r_version>>",
        "r_version": "<<r_version>>",
        "r_platform": "<<r_platform>>"
      },
      "annotations": {
        "org.opencontainers.image.ref.name": "<<package_version>>--<<r_version>>--<<r_platform>>",
        "io.r-hub.package.digest": "<<digest>>"
      }
    }'
  mnfts <- glue::glue_data(pkgs, tmpl, .open = "<<", .close = ">>")

  buildtime <- max(pkgs$buildtime)
  package_name <- unique(package_name)
  if (length(package_name) != 1) {
    throw(pkg_error(
      "Cannot push different packages at the same time.",
      "i" = "Found packages {.pkg {package_name}}."
    ))
  }

  package_version <- unique(pkgs$package_version)
  if (length(package_version) != 1) {
    throw(pkg_error(
      "Non-matching package versions when pushing package {.pkg {package_name}}.",
      "i" = "All package versions must be the same.",
      "i" = "Found package versions: {.var {package_version}}."
    ))
  }

  tmpl2 <- '
    {
      "schemaVersion": 2,
      "manifests": [
         <<paste0(mnfts, collapse = ",\n")>>
      ],
      "annotations": {
         <<ghcr_const_annotations_js()>>,
         "org.opencontainers.image.created": "<<buildtime>>",
         "org.opencontainers.image.version": "<<package_version>>"
      }
    }'

  jsonlite::prettify(glue::glue(tmpl2, .open = "<<", .close = ">>"))
}

ghcr_image_manifest <- function(pkgs) {
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
         <<ghcr_const_annotations_js()>>,
         "org.opencontainers.image.created": "<<buildtime>>",
         "org.opencontainers.image.version": "<<package_version>>"
      }
   }'

  glue::glue_data(pkgs, tmpl, .open = "<<", .close = ">>")
}


write_file <- function(txt, path) {
  out <- file(path, open = "wb")
  on.exit(close(out), add = TRUE)
  cat(txt, file = out, sep = "")
}

write_files <- function(txts, paths) {
  invisible(mapply(write_file, txts, paths))
}

find_skopeo <- function() {
  path <- Sys.which("skopeo")
  if (path != "") return(path)
  if (file.exists(cand <- "/usr/local/bin/skopeo")) return(cand)
  if (file.exists(cand <- "/opt/homebrew/bin/skopeo")) return(cand)
  throw(pkg_error("Need skopeo to push packages."))
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

check_skopeo <- function() {
  skopeo <- find_skopeo()
  skopeo_ver <- skopeo_version()
  if (skopeo_ver < "1.6.0") {
    throw(pkg_error(
      "Need at least skopeo 1.6.0 to push packages",
      "i" = "Found skopeo {.emph {skopoe_ver}} at {.path {skopeo}}."
    ))
  }
  skopeo[[1]]
}
