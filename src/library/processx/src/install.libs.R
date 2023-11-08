
progs <- if (WINDOWS) {
  c(file.path("tools", c("px.exe", "interrupt.exe", "sock.exe")),
    file.path("supervisor", "supervisor.exe"))
} else {
  c(file.path("tools", c("px", "sock")),
    file.path("supervisor", "supervisor"))
}

dest <- file.path(R_PACKAGE_DIR, paste0("bin", R_ARCH))
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
file.copy(progs, dest, overwrite = TRUE)

files <- Sys.glob(paste0("*", SHLIB_EXT))
dest <- file.path(R_PACKAGE_DIR, paste0('libs', R_ARCH))
dir.create(dest, recursive = TRUE, showWarnings = FALSE)
file.copy(files, dest, overwrite = TRUE)
if (file.exists("symbols.rds")) {
  file.copy("symbols.rds", dest, overwrite = TRUE)
}
