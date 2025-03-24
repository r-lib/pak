
library(css)

rstudio_theme_details_map <- list(
  "ambiance" = list(name = "Ambiance", isDark = TRUE),
  "chaos" = list(name = "Chaos", isDark = TRUE),
  "chrome" = list(name = "Chrome", isDark = FALSE),
  "clouds" = list(name = "Clouds", isDark = FALSE),
  "clouds_midnight" = list(name = "Clouds Midnight", isDark = TRUE),
  "cobalt" = list(name = "Cobalt", isDark = TRUE),
  "crimson_editor" = list(name = "Crimson Editor", isDark = FALSE),
  "dawn" = list(name = "Dawn", isDark = FALSE),
  "dracula" = list(name = "Dracula", isDark = TRUE),
  "dreamweaver" = list(name = "Dreamweaver", isDark = FALSE),
  "eclipse" = list(name = "Eclipse", isDark = FALSE),
  "idle_fingers" = list(name = "Idle Fingers", isDark = TRUE),
  "katzenmilch" = list(name = "Katzenmilch", isDark = FALSE),
  "kr_theme" = list(name = "Kr Theme", isDark = TRUE),
  "material" = list(name = "Material", isDark = TRUE),
  "merbivore" = list(name = "Merbivore", isDark = TRUE),
  "merbivore_soft" = list(name = "Merbivore Soft", isDark = TRUE),
  "mono_industrial" = list(name = "Mono Industrial", isDark = TRUE),
  "monokai" = list(name = "Monokai", isDark = TRUE),
  "pastel_on_dark" = list(name = "Pastel On Dark", isDark = TRUE),
  "solarized_dark" = list(name = "Solarized Dark", isDark = TRUE),
  "solarized_light" = list(name = "Solarized Light", isDark = FALSE),
  "textmate" = list(name = "Textmate (default)", isDark = FALSE),
  "tomorrow" = list(name = "Tomorrow", isDark = FALSE),
  "tomorrow_night" = list(name = "Tomorrow Night", isDark = TRUE),
  "tomorrow_night_blue" = list(name = "Tomorrow Night Blue", isDark = TRUE),
  "tomorrow_night_bright" = list(name = "Tomorrow Night Bright", isDark = TRUE),
  "tomorrow_night_eighties" = list(name = "Tomorrow Night 80s", isDark = TRUE),
  "twilight" = list(name = "Twilight", isDark = TRUE),
  "vibrant_ink" = list(name = "Vibrant Ink", isDark = TRUE),
  "xcode" = list(name = "Xcode", isDark = FALSE)
)

rstudio_theme_url_template <- paste0(
  "https://raw.githubusercontent.com/",
  "rstudio/rstudio/master/src/cpp/session/resources/themes/%s.rstheme"
)

## A set of operator colors to use, for each theme. Should match the name
## of the theme file in ace.
## We need to explicity set themes that should be overridden with the default
## vaue to NULL
operator_theme_map <- list(
   "solarized_light" = "#93A1A1",
   "solarized_dark" = "#B58900",
   "twilight" = "#7587A6",
   "idle_fingers" = "#6892B2",
   "clouds_midnight" = "#A53553",
   "cobalt" = "#BED6FF",
   "kr_theme" = "#A56464",
   "clouds" = NULL,
   "dawn" = NULL,
   "eclipse" = NULL,
   "katzenmilch" = NULL,
   "merbivore" = NULL,
   "merbivore_soft" = NULL,
   "monokai" = NULL,
   "pastel_on_dark" = NULL,
   "vibrant_ink" = NULL,
   "xcode" = NULL
)

## Similarly, colors for keywords that we might override.
keyword_theme_map <- list(
   "eclipse" = "#800080",
   "clouds" = "#800080"
)

# Needs https://github.com/romainfrancois/css

rstudio_css <- function(theme) {
  message("Downloading theme '", theme, "'")
  url <- sprintf(rstudio_theme_url_template, theme)
  css <- asNamespace("css")$read_css(url)

  g <- function(sel) {
    col <- vapply(
      sel,
      FUN.VALUE = "",
      function(sel1) {
        tail(c(NA_character_, css$value[grepl(sel1, css$rule) &
                                        css$property == "color"]), 1)
      }
    )

    col <- na.omit(col)[1]

    if (is.na(col) || length(col) != 1) {
      stop("Cannot get '", sel, "' from theme '", theme, "'")
    }

    col <- sub("[ ]+!important", "", col)

    ## Three digit colors are not handled by cli...
    if (grepl("^#[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]$", col)) {
      col <- paste(rep(strsplit(col, "", fixed = TRUE)[[1]], c(1, 2, 2, 2)), collapse = "")
    }
    ## rgb () form
    if (grepl("^rgb", col)) {
      col <- gsub("[^0-9 ]", "", col)
      col <- do.call(rgb, as.list(scan(text = col, quiet = TRUE) / 255))
    }
    unname(col)
  }

  if (theme %in% names(keyword_theme_map)) {
    kw <- keyword_theme_map[[theme]]
  } else {
    kw <- g("\\.ace_keyword$|\\.ace_keyword,")
  }

  if (theme %in% names(operator_theme_map)) {
    if (is.null(operator_theme_map[[theme]])) {
      if (rstudio_theme_details_map[[theme]]$isDark) {
        op <- "#aaaaaa"
      } else {
        op <- "#888888"
      }
    } else {
      op <- operator_theme_map[[theme]]
    }
  } else {
    op <- g(c(
      "^\\.ace_keyword\\.ace_operator$",
      "^\\.ace_constant\\.ace_language$",
      "^\\.ace_variable\\.ace_language$",
      "\\.ace_constant,",
      "\\.ace_constant\\.ace_buildin"
    ))
  }

  list(
    reserved_ = kw,
    number_ = g(c("\\.ace_constant\\.ace_numeric$", "\\.ace_constant,")),
    null_ = g(c("\\.ace_constant\\.ace_language$",
                "\\.ace_variable\\.ace_language$",
                "\\.ace_constant,",
                "\\.ace_constant\\.ace_buildin")),
    operator_ = op,
    call_ = NA_character_,
    string_ = g("\\.ace_string$|\\.ace_string,"),
    comment_ = g("\\.ace_comment$|\\.ace_comment,"),
    bracket_ = g(c("\\.ace_paren\\.ace_keyword\\.ace_operator",
                   "\\.ace_keyword\\.ace_operator",
                   "\\.ace_keyword"))
  )
}

create_rstudio_data <- function() {

  themes <- lapply(names(rstudio_theme_details_map), rstudio_css)
  names(themes) <- names(rstudio_theme_details_map)

  # Some substitutions
  for (nm in names(operator_theme_map)) {
    if (is.null(operator_theme_map[[nm]])) {

    } else {
      themes[[nm]]$null_ <- operator_theme_map[[nm]]
      themes[[nm]]$operator_ <- operator_theme_map[[nm]]
      themes[[nm]]$bracket_ <- operator_theme_map[[nm]]
    }
  }

  for (nm in names(keyword_theme_map)) {
    themes[[nm]]$reserved_ <- keyword_theme_map[[nm]]
  }

  # check that cli can handle these, otherwise it will error
  lapply(na.omit(unlist(themes)), cli::make_ansi_style)

  themes2 <- lapply(themes, function(theme) {
    list(
      reserved = theme$reserved_,
      number   = theme$number_,
      null     = theme$null_,
      operator = theme$operator_,
      call     = "bold",
      string   = theme$string_,
      comment  = theme$comment_,
      bracket  = list(theme$bracket_, "yellow", "blue", "cyan")
    )
  })

  names(themes2) <- vapply(rstudio_theme_details_map, "[[", "", "name")

  message("Saving sysdata.rda... ", appendLF = FALSE)
  sysenv <- new.env(parent = emptyenv())
  load(file.path("R", "sysdata.rda"), envir = sysenv)
  sysenv$rstudio_themes <- themes2
  save(
    list = ls(sysenv),
    envir = sysenv,
    file = file.path("R", "sysdata.rda"),
    version = 2
  )
  message("done.")

  invisible(themes2)
}

if (is.null(sys.calls())) {
  create_rstudio_data()
}
