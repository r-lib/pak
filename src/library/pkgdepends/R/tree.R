pkgplan_draw_solution_tree <- function(self, private, pkgs, annotate) {
  assert_that(is.null(pkgs) || is_character(pkgs))

  self$stop_for_solve_error()
  sol <- self$get_solution()$data
  sol <- sol[order(sol$package), ]
  pkgs <- pkgs %||% sol$package[sol$directpkg]

  data <- sol[, "package", drop = FALSE]

  if ("dependencies" %in% names(sol)) {
    data$deps <- sol$dependencies
  } else {
    deps <- lapply(seq_len(nrow(sol)), function(i) {
      d <- sol$deps[[i]]
      d[tolower(d$type) %in% tolower(sol$dep_types[[i]]), ]
    })
    deps <- lapply(deps, "[[", "package")
    data$deps <- deps
  }
  data$deps <- lapply(data$deps, intersect, data$package)

  ann_version <- function() {
    v <- cli::col_silver(sol$version)
    upd <- sol$lib_status == "update"
    if (any(upd)) {
      v[upd] <- cli::col_green(paste(
        sol$old_version[upd],
        "->",
        sol$version[upd]
      ))
    }
    nupd <- sol$lib_status == "no-update"
    if (any(nupd)) {
      v[nupd] <- cli::col_green(paste(
        sol$version[nupd],
        "<",
        sol$new_version[nupd]
      ))
    }
    v
  }

  data$label <- ifelse(
    sol$type %in% c("cran", "bioc", "standard", "installed"),
    data$package,
    sol$ref
  )
  data$trimmed <- data$label
  data$label <- paste(data$label, ann_version())
  data$label[sol$directpkg] <-
    cli::style_italic(cli::style_bold(cli::col_cyan(data$label[sol$directpkg])))

  if (annotate) {
    builder <- emoji("builder")
    ann <- get_tree_annotation(sol)
    data$label <- paste(
      data$label,
      annotate_tree(sol, ann, builder = builder)
    )
  }

  trees <- unlist(lapply(
    pkgs,
    function(p) c(cli::tree(data, root = p, trim = TRUE), "")
  ))

  if (annotate && any(unlist(ann))) {
    key <- paste0(
      if (any(ann$new)) paste(" |", cli::col_green(emoji("sparkles")), "new"),
      if (any(ann$upd)) paste(" |", cli::col_green(emoji("rocket")), "update"),
      if (any(ann$noupd))
        paste(" |", cli::col_green(emoji("hand")), "outdated"),
      if (any(ann$dl)) paste(" |", cli::col_green(emoji("dl")), "download"),
      if (any(ann$build)) paste(" |", cli::col_green(builder), "build"),
      if (any(ann$compile))
        paste(" |", cli::col_green(emoji("wrench")), "compile")
    )
    key <- paste0("Key: ", sub("^ [|]", "", key))
    trees <- c(trees, key)
  }

  class(trees) <- c("cli_tree", "tree", "character")
  trees
}

has_emoji <- function() {
  if (!cli::is_utf8_output()) return(FALSE)
  if (isTRUE(opt <- getOption("pkg.emoji"))) return(TRUE)
  if (identical(opt, FALSE)) return(FALSE)
  if (Sys.info()[["sysname"]] != "Darwin") return(FALSE)
  TRUE
}

get_tree_annotation <- function(sol) {
  ann <- data.frame(
    stringsAsFactors = FALSE,
    new = sol$lib_status == "new",
    upd = sol$lib_status == "update",
    noupd = sol$lib_status == "no-update",
    dl = !is.na(sol$cache_status) & sol$cache_status == "miss"
  )
  ann$build <- (ann$new | ann$upd) & sol$platform == "source"
  ann$compile <- (ann$new | ann$upd) &
    !is.na(sol$needscompilation) &
    sol$needscompilation

  ann
}

annotate_tree <- function(sol, ann, builder = NULL) {
  builder <- builder %||% emoji("builder")
  cli::col_green(paste0(
    ifelse(ann$new, emoji("sparkles"), ""),
    ifelse(ann$upd, emoji("rocket"), ""),
    ifelse(ann$noupd, emoji("hand"), ""),
    ifelse(ann$build, builder, ""),
    ifelse(ann$compile, emoji("wrench"), ""),
    ifelse(ann$dl, paste(emoji("dl"), format_file_size(sol$filesize)), "")
  ))
}

format_file_size <- function(x) {
  bts <- str_trim(format_bytes$pretty_bytes(x))
  cli::col_silver(
    ifelse(is.na(x), "(unknown size)", paste0("(", bts, ")"))
  )
}

emoji <- function(what, alt = NULL) {
  emo <- has_emoji()
  switch(
    what,
    "rocket" = if (emo) "\U1F680" else alt %||% "[upd]",
    "sparkles" = if (emo) "\u2728" else alt %||% "[new]",
    "hand" = if (emo) "\u270B" else alt %||% "[old]",
    "dl" = if (emo) " \u2B07" else alt %||% "[dl]",
    "builder" = if (emo) emo_builder() else alt %||% "[bld]",
    "wrench" = if (emo) "\U1F527" else alt %||% "[cmp]",
    "pkg" = if (emo) "\U1F4E6" else alt %||% "pkg",
    "pkgs" = if (emo) "\U1F4E6" else alt %||% "pkgs",
    ""
  )
}

emo_builder <- function(n = 1) {
  base <- "\U1F477"
  tone <- c(
    "\U1F477\U1F3FB",
    "\U1F477\U1F3FC",
    "\U1F477\U1F3FD",
    "\U1F477\U1F3FE",
    "\U1F477\U1F3FF"
  )
  gend <- c(
    "\U1F477\u200D\u2640\uFE0F",
    "\U1F477\u200D\u2642\uFE0F",
    "\U1F477\U1F3FB\u200D\u2640\uFE0F",
    "\U1F477\U1F3FB\u200D\u2642\uFE0F",
    "\U1F477\U1F3FC\u200D\u2640\uFE0F",
    "\U1F477\U1F3FC\u200D\U2642\uFE0F",
    "\U1F477\U1F3FD\u200D\u2640\uFE0F",
    "\U1F477\U1F3FD\u200D\u2642\uFE0F",
    "\U1F477\U1F3FE\u200D\u2640\uFE0F",
    "\U1F477\U1F3FE\u200D\u2642\uFE0F",
    "\U1F477\U1F3FF\u200D\u2640\uFE0F",
    "\U1F477\U1F3FF\u200D\u2642\uFE0F"
  )

  rstudio_type <- rstudio_detect()$type
  if (rstudio_type == "rstudio_terminal") {
    ppl <- base
  } else if (rstudio_type == "rstudio_console") {
    ppl <- c(base, tone)
  } else {
    ppl <- c(base, tone, gend)
  }

  sample(ppl, n, replace = TRUE)
}
