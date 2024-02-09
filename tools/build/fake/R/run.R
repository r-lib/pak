message("Starting up fake apps")

env <- new.env()
apps <- list()

source("cran-app.R", local = env, verbose = FALSE)
apps[["cran"]] <- webfakes::new_app_process(
  env$cran_app(env$cran_app_pkgs),
  port = as.integer(Sys.getenv("CRAN_APP_PORT", "3100")),
  opts = webfakes::server_opts(num_threads = 4, interface = "0.0.0.0")
)

source("bioc-app.R", local = env, verbose = FALSE)
apps[["bioc"]] <- webfakes::new_app_process(
  env$bioc_app(env$bioc_app_pkgs),
  port = as.integer(Sys.getenv("BIOC_APP_PORT", "3101")),
  opts = webfakes::server_opts(num_threads = 4, interface = "0.0.0.0")
)

source("ppm-app.R", local = env, verbose = FALSE)
apps[["ppm"]] <- webfakes::local_app_process(
  env$ppm_app(),
  port = as.integer(Sys.getenv("PPM_APP_PORT", "3102")),
  opts = webfakes::server_opts(num_threads = 3, interface = "0.0.0.0")
)

source("name-check-app.R", local = env, verbose = FALSE)
apps[["name-check"]] <- webfakes::local_app_process(
  env$name_check_app(),
  port = as.integer(Sys.getenv("NAME_CHECK_APP_PORT", "3103")),
  opts = webfakes::server_opts(num_threads = 3, interface = "0.0.0.0")
)

source("git-app.R", local = env, verbose = FALSE)
apps[["git"]] <- webfakes::local_app_process(
  webfakes::git_app(env$git_root),
  port = as.integer(Sys.getenv("GIT_APP_PORT", "3104")),
  opts = webfakes::server_opts(num_threads = 3, interface = "0.0.0.0")
)

source("gh-app.R", local = env, verbose = FALSE)
apps[["gh"]] <- webfakes::local_app_process(
  env$gh_app(env$gh_app_repos),
  port = as.integer(Sys.getenv("GH_APP_PORT", "3105")),
  opts = webfakes::server_opts(num_threads = 3, interface = "0.0.0.0")
)

apps[["cran2"]] <- webfakes::new_app_process(
  env$cran_app(env$cran_app_pkgs2),
  port = as.integer(Sys.getenv("CRAN2_APP_PORT", "3106")),
  opts = webfakes::server_opts(num_threads = 4, interface = "0.0.0.0")
)

message("CRAN       app: ", apps$cran$url())
message("BIOC       app: ", apps$bioc$url())
message("PPM        app: ", apps$ppm$url())
message("Name check app:", apps[["name-check"]]$url())
message("GIT        app: ", apps$git$url())
message("GH         app: ", apps$gh$url())
message("CRAN       app: ", apps$cran2$url())

while (TRUE) {
  Sys.sleep(10000)
}
