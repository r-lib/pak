pkg_plan <- R6::R6Class(
  "pkg_plan",
  public = list(
    initialize = function(
      refs,
      config = list(),
      library = NULL,
      remote_types = NULL,
      lockfile = NULL
    )
      pkgplan_init(
        self,
        private,
        refs,
        config,
        library,
        remote_types,
        lockfile
      ),

    get_refs = function() private$refs,
    has_resolution = function() !is.null(private$resolution$result),
    has_clean_resolution = function()
      self$has_resolution() && (all(private$resolution$result$status == "OK")),
    has_resolution_downloads = function() !is.null(private$downloads),
    has_solution_downloads = function() !is.null(private$solution_downloads),
    has_solution = function() !is.null(private$solution),
    get_config = function() private$config,

    async_resolve = function() pkgplan_async_resolve(self, private),
    resolve = function() pkgplan_resolve(self, private),
    get_resolution = function() pkgplan_get_resolution(self, private),

    async_download_resolution = function()
      pkgplan_async_download_resolution(self, private),
    download_resolution = function() pkgplan_download_resolution(self, private),
    get_resolution_download = function()
      pkgplan_get_resolution_download(self, private),

    solve = function(policy = c("lazy", "upgrade"))
      pkgplan_solve(self, private, match.arg(policy)),
    delete_solution = function() private$solution <- NULL,
    stop_for_solve_error = function()
      pkgplan_stop_for_solve_error(self, private),
    get_solution = function() pkgplan_get_solution(self, private),
    show_solution = function(key = FALSE)
      pkgplan_show_solution(self, private, key),
    get_sysreqs = function() pkgplan_get_sysreqs(self, private),
    show_sysreqs = function() pkgplan_show_sysreqs(self, private),
    get_install_plan = function()
      pkgplan_install_plan(self, private, downloads = TRUE),
    export_install_plan = function(plan_file = "pkg.lock", version = 2)
      pkgplan_export_install_plan(self, private, plan_file, version),
    draw_solution_tree = function(pkgs = NULL, annotate = TRUE)
      pkgplan_draw_solution_tree(self, private, pkgs, annotate),

    async_download_solution = function()
      pkgplan_async_download_solution(self, private),
    download_solution = function() pkgplan_download_solution(self, private),
    get_solution_download = function()
      pkgplan_get_solution_download(self, private),
    stop_for_solution_download_error = function()
      pkgplan_stop_for_solution_download_error(self, private),
    stop_for_resolution_download_error = function()
      pkgplan_stop_for_resolution_download_error(self, private),

    update = function() pkgplan_update(self, private),
    update_sysreqs = function() pkgplan_update_sysreqs(self, private),

    print = function(...) pkgplan_print(self, private, ...)
  ),

  private = list(
    refs = NULL,
    dirty = FALSE,
    remotes = list(),
    cache = NULL,
    resolution = NULL,
    solution = NULL,
    downloads = NULL,
    solution_downloads = NULL,
    download_cache = NULL,
    config = NULL,
    progress_bar = NULL,
    progress_bar_timer = NULL,
    remote_types = NULL,
    system_packages = NULL,
    sysreqs = NULL,

    download_res = function(res, which, on_progress = NULL)
      pkgplan_download_res(self, private, res, which, on_progress),
    subset_resolution = function(which)
      pkgplan__subset_resolution(self, private, which),
    create_lp_problem = function(pkgs, policy)
      pkgplan__create_lp_problem(self, private, pkgs, policy),
    solve_lp_problem = function(problem)
      pkgplan__solve_lp_problem(self, private, problem),

    create_progress_bar = function(what) {
      bar <- pkgplan__create_progress_bar(what)
      pkgplan__init_progress_bar(bar)
      bar
    },
    update_progress_bar = function(idx, event, data)
      pkgplan__update_progress_bar(private$progress_bar, idx, event, data),
    done_progress_bar = function() {
      if (!is.null(private$progress_bar)) {
        pkgplan__done_progress_bar(private$progress_bar)
        private$progress_bar <- NULL
      }
    }
  )
)

#' @importFrom utils modifyList

pkgplan_init <- function(
  self,
  private,
  refs,
  config,
  library,
  remote_types,
  lockfile
) {
  if (!is.null(lockfile)) {
    return(pkgplan_init_lockfile(
      self,
      private,
      lockfile,
      config,
      library,
      remote_types
    ))
  }

  assert_that(is_character(refs), is_optional_path(library))

  private$refs <- refs
  private$remotes <- parse_pkg_refs(refs)
  private$config <- current_config()$update(config)
  private$config$set("library", library)
  private$remote_types <- remote_types %||% default_remote_types()

  if (!is.null(library)) {
    mkdirp(library, msg = "Creating library directory")
    library <- normalizePath(library)
  }
  mkdirp(private$download_cache <- private$config$get("cache_dir"))

  installed <- NULL
  if (!is.null(library)) {
    installed <- merge_installed_caches(
      make_installed_cache(library),
      make_installed_cache(.Library, priority = "recommended")
    )
  }

  private$cache <- list(
    metadata = pkgcache::cranlike_metadata_cache$new(
      replica_path = private$config$get("metadata_cache_dir"),
      platforms = private$config$get("platforms"),
      r_version = private$config$get("r_versions"),
      cran_mirror = private$config$get("cran_mirror"),
      update_after = private$config$get("metadata_update_after"),
      bioc = private$config$get("use_bioconductor")
    ),
    package = pkgcache::package_cache$new(private$config$get(
      "package_cache_dir"
    )),
    installed = installed
  )

  private$dirty <- TRUE
  invisible(self)
}

pkgplan_init_lockfile <- function(
  self,
  private,
  lockfile,
  config,
  library,
  remote_types
) {
  assert_that(
    is_path(lockfile),
    is_optional_path(library)
  )

  private$config <- current_config()$update(config)
  private$config$set("library", library)
  private$remote_types <- remote_types %||% default_remote_types()

  if (!is.null(library)) {
    mkdirp(library, msg = "Creating library directory")
    library <- normalizePath(library)
  }
  mkdirp(private$download_cache <- private$config$get("cache_dir"))

  raw <- jsonlite::fromJSON(readLines(lockfile), simplifyVector = FALSE)
  if (raw$lockfile_version != 1) {
    throw(pkg_error(
      "This version of {pak_or_pkgdepends()} (version {pakx_version()})
       does not support lockfile version {raw$lockfile_version}.",
      i = "This lockfile was probably created by a newer version."
    ))
  }

  pkgs <- raw$packages
  refs <- vcapply(pkgs, "[[", "ref")

  sysreqs_packages <- lapply(pkgs, function(x) {
    sq <- x[["sysreqs_packages"]]
    sq <- lapply(sq, function(sq1) {
      sq1[["sysreq"]] <- unlist(sq1[["sysreq"]])
      sq1[["packages"]] <- unlist(sq1[["packages"]])
      sq1
    })
    sq
  })

  soldata <- data_frame(
    ref = refs,
    type = vcapply(pkgs, "[[", "type"),
    direct = vlapply(pkgs, "[[", "direct"),
    directpkg = vlapply(pkgs, "[[", "directpkg"),
    status = "OK",
    package = vcapply(pkgs, "[[", "package"),
    version = vcapply(pkgs, "[[", "version"),
    license = vcapply(pkgs, function(x) x$license %||% NA_character_),
    needscompilation = vlapply(pkgs, function(x) x$needscompilation %||% NA),
    sha256 = vcapply(pkgs, function(x) x$sha256 %||% NA_character_),
    filesize = viapply(pkgs, function(x) x$filesize %||% NA_integer_),
    built = vcapply(pkgs, function(x) x$built %||% NA_character_),
    platform = vcapply(pkgs, "[[", "platform"),
    rversion = vcapply(pkgs, "[[", "rversion"),
    target = vcapply(pkgs, "[[", "target"),
    dependencies = lapply(
      pkgs,
      function(x) unlist(x$dependencies) %||% character()
    ),
    sources = lapply(pkgs, function(x) unlist(x$sources)),
    metadata = lapply(pkgs, function(x) unlist(x$metadata)),
    dep_types = lapply(pkgs, function(x) unlist(x$dep_types)),
    remote = parse_pkg_refs(refs),
    cache_status = "miss",
    lib_status = "new",
    old_version = NA_character_,
    new_version = vcapply(pkgs, "[[", "version"),
    extra = list(list()),
    install_args = lapply(
      pkgs,
      function(x) unlist(x$install_args) %||% character()
    ),
    repotype = vcapply(pkgs, function(x) x$repotype %||% NA_character_),
    params = lapply(pkgs, function(x) unlist(x$params)),
    sysreqs = vcapply(pkgs, function(x) x[["sysreqs"]] %||% NA_character_),
    sysreqs_packages = sysreqs_packages
  )

  private$refs <- refs[soldata$direct]
  private$remotes <- soldata$remote
  private$dirty <- FALSE

  private$cache <- list(
    metadata = NULL,
    package = pkgcache::package_cache$new(private$config$get(
      "package_cache_dir"
    )),
    installed = NULL
  )

  sysreqs <- raw$sysreqs
  if (!is.null(sysreqs)) {
    sysreqs$packages <- unlist(sysreqs$packages)
    sysreqs["pre_install"] <- list(unlist(sysreqs$pre_install))
    sysreqs["post_install"] <- list(unlist(sysreqs$post_install))
    sysreqs["install_scripts"] <- list(unlist(sysreqs$install_scripts))
  }

  private$resolution <- list(result = soldata)
  private$solution <- list(
    result = structure(
      class = c("pkg_solution_result", "list"),
      list(
        status = "OK",
        data = soldata,
        problem = list(pkgs = soldata),
        solution = NULL,
        sysreqs = sysreqs
      )
    )
  )

  invisible(self)
}

pkgplan_get_total_files <- function(self, private) {
  nrow(private$resolution$result)
}

pkgplan_print <- function(self, private, ...) {
  cat("<pkg_plan>\n")

  ## refs
  refs <- self$get_refs()
  cat(
    strwrap(
      paste0("- refs: ", paste(backtick(refs), collapse = ", ")),
      indent = 0,
      exdent = 4
    ),
    sep = "\n"
  )

  ## library
  if (!is.null(private$config$get("library"))) {
    cat("- library:", backtick(private$config$get("library")), "\n")
  }

  ## resolution
  if (self$has_resolution()) {
    if (self$has_clean_resolution()) {
      cat("- has resolution\n")
    } else {
      cat("- has resolution, with errors\n")
    }
  }

  ## solution
  if (!is.null(private$solution$result)) {
    if (private$solution$result$status == "OK") {
      cat("- has solution\n")
    } else {
      cat("- has solution, with errors\n")
    }
  }

  invisible(self)
}

pkgplan_update <- function(self, private) {
  libstat <- make_installed_cache(private$config$get("library"))$pkgs
  for (i in seq_len(nrow(private$solution$result$data))) {
    pkg <- private$solution$result$data$package[i]
    if (!pkg %in% libstat$package) {
      next
    }

    installed <- libstat[match(pkg, libstat$package), ]
    solution <- private$solution$result$data[i, ]

    par <- solution$params[[1]]
    if (is_true_param(par, "reinstall") || is_true_param(par, "nocache")) {
      next
    }

    if (!installedok_remote(installed, solution)) {
      next
    }

    private$solution$result$data$ref[i] <- installed$ref
    private$solution$result$data$type[i] <- "installed"
    private$solution$result$data$sources[[i]] <- character()
    private$solution$result$data$remote[[i]] <- parse_pkg_ref(installed$ref)
    private$solution$result$data$cache_status[i] <- NA
    private$solution$result$data$lib_status[i] <- "current"
    private$solution$result$data$old_version[[i]] <- installed$version
    private$solution$result$data$new_version[[i]] <- NA
  }
}

pkgplan_update_sysreqs <- function(self, private) {
  if (!private$config$get("sysreqs")) return(invisible())
  # Stop here is no sysreqs at all, no need to look up system packages
  sys <- self$get_solution()$data$sysreqs_packages
  if (length(unlist(sys)) == 0) return(invisible())

  private$solution$result$data$sysreqs_packages <-
    sysreqs_update_state(sys)
}
