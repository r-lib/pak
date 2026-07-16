# pak Docker Quick Start

## Build and Run

```bash
cd /path/to/pak
docker build -t pak:latest .
docker run --rm pak:latest
```

## What's Included

- **R 4.4.1 base** — Debian-based R runtime
- **pak package from CRAN** — Binary installation (fast, ~5 min build)
- **System dependencies** — libcurl, libssl, libxml2 for package compilation
- **Ready to use** — interactive R shell with pak preloaded

## Interactive Usage

```bash
docker run -it --rm pak:latest R --slave

# Inside R:
> library(pak)
> pak::pkg_install("cli")
> pak::pkg_deps_tree("tibble")
```

## Install from GitHub

```bash
docker run -it --rm pak:latest R --slave -e \
  'pak::pkg_install("tidyverse/tibble")'
```

## Mount Local Cache

For persistent package cache across runs:

```bash
docker run -it --rm \
  -v ~/.pak:/root/.pak \
  -e R_PKG_CACHE_DIR=/root/.pak/cache \
  pak:latest R --slave
```

## Notes

- pak requires build tools for some packages — all included
- CRAN binary install is used for speed (no source compilation)
- For development pak source builds, extend Dockerfile to include testthat

## Troubleshooting

**"pak not found"**

```bash
docker run -it --rm pak:latest R --slave -e 'library(pak)'
```

**"Package not available"**

pak can install from GitHub, Bioconductor, or local. Check your source:

```r
pak::pkg_install("username/repo")  # GitHub
pak::pkg_install("bioc::package")  # Bioconductor
pak::local_install("./package")    # Local directory
```
