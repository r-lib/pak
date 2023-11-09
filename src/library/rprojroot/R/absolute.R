# adapted from fs
is_absolute_path <- function(x) {
  grepl("^[/\\\\~]|^[a-zA-Z]:[/\\\\]", x)
}
