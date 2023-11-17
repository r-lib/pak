# R CMD INSTALL happily continues even if `make` (via `Makevars`) faile,
# so we need to throw an error here explicitly.

if (!file.exists("DONE")) {
  stop("Compilation failed.")
}
