# pak + Sparky Integration Guide

## Overview

**pak** is an R package installer that resolves dependencies, performs parallel downloads, and manages packages from CRAN, Bioconductor, GitHub, and local sources.

**Sparky** (ThinkingSparky) is a file-based AI assistant that pak can delegate to for intelligent package recommendations, dependency resolution assistance, and installation strategy optimization.

## Integration Architecture

### File-Based Relay (Inbox/Outbox Pattern)

Like ComputeGate, pak would communicate with Sparky through a local file relay:

```
pak R process
    ↓ (writes JSON request)
inbox/sparky_XXXXXXXX.json
    ↓ (Sparky watches)
[Sparky AI agent processes]
    ↓ (writes JSON response)
outbox/sparky_XXXXXXXX.json
    ↓ (pak reads & polls)
pak continues
```

### Implementation in R

**File 1: `R/sparky_bridge.R` (new module)**

```r
#' Communicate with Sparky AI for package decisions
#'
#' @param prompt Character string with pak question
#' @param timeout Seconds to wait for response (default: 120)
#' @export
sparky_ask <- function(prompt, timeout = 120) {
  bridge_dir <- file.path(Sys.getenv("HOME"), ".pak", "sparky_bridge")
  inbox_dir <- file.path(bridge_dir, "inbox")
  outbox_dir <- file.path(bridge_dir, "outbox")
  
  dir.create(inbox_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(outbox_dir, recursive = TRUE, showWarnings = FALSE)
  
  request_id <- paste0("req_", sub("-", "", Sys.time(), fixed = TRUE))
  request_file <- file.path(inbox_dir, paste0(request_id, ".json"))
  response_file <- file.path(outbox_dir, paste0(request_id, ".json"))
  
  # Write request
  request <- list(
    id = request_id,
    prompt = prompt,
    timestamp = Sys.time()
  )
  write_lines(
    jsonlite::toJSON(request, pretty = TRUE),
    request_file
  )
  
  # Poll for response
  start <- Sys.time()
  while (as.numeric(Sys.time() - start) < timeout) {
    if (file.exists(response_file)) {
      response <- jsonlite::read_json(response_file)
      unlink(response_file)
      unlink(request_file)
      if (!is.null(response$error)) {
        stop("Sparky error: ", response$error)
      }
      return(response$response)
    }
    Sys.sleep(0.5)
  }
  
  unlink(request_file)
  stop("Sparky timeout after ", timeout, "s")
}

#' Suggest packages using Sparky
#'
#' Ask Sparky for package recommendations based on task description
#'
#' @param task Character string describing the task
#' @export
sparky_suggest <- function(task) {
  prompt <- paste0(
    "I need to install R packages for this task: ", task, "\n",
    "Suggest specific package names (comma-separated) that would be most useful. ",
    "Include both primary packages and key dependencies."
  )
  sparky_ask(prompt)
}

#' Resolve dependency conflict with Sparky
#'
#' Get Sparky's recommendation for resolving version conflicts
#'
#' @param packages Character vector of conflicting packages
#' @export
sparky_resolve_conflict <- function(packages) {
  prompt <- paste0(
    "These R packages have version conflicts: ", paste(packages, collapse = ", "), "\n",
    "Which version combination would work best and why? ",
    "Return a brief recommendation."
  )
  sparky_ask(prompt)
}
```

**File 2: `R/zzz.R` (initialization hook)**

```r
.onLoad <- function(libname, pkgname) {
  if (Sys.getenv("PAK_ENABLE_SPARKY", "") == "true") {
    packageStartupMessage("pak: Sparky AI assistance enabled")
  }
}
```

### Usage Examples

**In pak R code:**

```r
# When selecting packages interactively
if (Sys.getenv("PAK_ENABLE_SPARKY", "") == "true") {
  suggestions <- sparky_suggest("data wrangling and visualization")
  cli::cli_alert_info("Sparky suggests: {suggestions}")
}

# When resolving dependency conflicts
if (has_version_conflicts) {
  recommendation <- sparky_resolve_conflict(conflicting_packages)
  cli::cli_alert_warning("Sparky recommendation: {recommendation}")
}
```

## Docker Integration

### Build with Sparky Support

```dockerfile
FROM r-base:4.4.1

WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# Copy pak source
COPY . .

# Install pak
RUN R --slave -e "install.packages('pak', repos='https://cloud.r-project.org/')"

# Create Sparky bridge directory
RUN mkdir -p ~/.pak/sparky_bridge/{inbox,outbox}

# Enable Sparky in environment
ENV PAK_ENABLE_SPARKY=true
ENV R_PKG_CACHE_DIR=/tmp/pak-cache

CMD ["R", "--slave", "-e", "library(pak); cat('pak + Sparky ready\\n')"]
```

### Run with Host Sparky Access

```bash
# On host, ensure Sparky is running and watching the bridge directory
docker run -it --rm \
  -v ~/.pak/sparky_bridge:/root/.pak/sparky_bridge \
  pak:latest \
  R --slave
```

Inside R:
```r
library(pak)
# Sparky bridge will be available if Sparky process is watching the host directory
suggestions <- sparky_suggest("statistical analysis")
```

## Advantages for pak

1. **Intelligent Dependency Resolution** — Sparky can reason about which package versions minimize conflicts
2. **GitHub/Bioconductor Navigation** — Sparky helps identify which GitHub sources or Bioconductor packages are best
3. **Task-Aware Selection** — User describes their goal; Sparky recommends the right packages (not just keyword matches)
4. **Conflict Explanation** — Instead of silent resolution, Sparky explains why it chose a particular version combination
5. **Offline AI** — All decisions stay local; no cloud API calls needed

## Implementation Steps

1. Add `sparky_bridge.R` to `R/` directory
2. Add Sparky initialization to `.onLoad()` in `R/zzz.R`
3. Call `sparky_suggest()` or `sparky_resolve_conflict()` from `resolve_dependencies()` function
4. Document usage in README under "Advanced: Sparky AI Assistance"
5. Test with docker and Sparky agent running in parallel

## Testing

```bash
# Terminal 1: Start Sparky watching the bridge
sparky --watch ~/.pak/sparky_bridge

# Terminal 2: Run pak in Docker
docker run -it --rm \
  -v ~/.pak/sparky_bridge:/root/.pak/sparky_bridge \
  -e PAK_ENABLE_SPARKY=true \
  pak:latest \
  R --slave -e "pak::sparky_suggest('data analysis')"
```

## Security Considerations

- Sparky sees all package requests (no sensitive queries go to cloud)
- File-based relay is local-only; bridge directory should be `~/.pak/sparky_bridge` (user-owned)
- Add `.pak/sparky_bridge` to `.gitignore` to avoid committing bridge files
- Document that pak with Sparky should only be used in trusted environments
