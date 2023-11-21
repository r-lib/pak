
#' RStudio addin to search CRAN packages
#'
#' Call this function from RStudio for best results. You can also use it
#' without RStudio, then it will run in the web browser.
#'
#' The app has:
#' - A search tab for free text search, very much like the [pkg_search()]
#'   function.
#' - The list of recently updated packages.
#' - The list of top packages: most downloaded, most depended upon,
#'   and trending packages.
#' - Package list by maintainer.
#'
#' @param query Query string to start the addin with.
#' @param viewer Whether to show the addin within RStudio (`"dialog"`),
#'   or in a web browser (`"browser"`).
#'
#' @export
#' @section Examples:
#' ```
#' pkg_search_addin()
#'
#' # Start with a search query
#' pkg_search_addin("permutation test")
#' ```

pkg_search_addin <- function(
  query = "",
  viewer = c("dialog", "browser")) {

  needs_packages(c("memoise", "shiny", "shinyjs", "shinyWidgets", "whoami"))

  query
  maint <- whoami::email_address(fallback = "")

  wired <- character()
  data <- list(
    `cnt-search-prev` = 0L,
    `cnt-search-next` = 0L,
    `cnt-new-prev`    = 0L,
    `cnt-new-next`    = 0L,
    `cnt-topdl-prev`  = 0L,
    `cnt-topdl-next`  = 0L,
    `cnt-topdep-prev` = 0L,
    `cnt-topdep-next` = 0L,
    `cnt-trend-prev` = 0L,
    `cnt-trend-next` = 0L,
    `cnt-maint-prev` = 0L,
    `cnt-maint-next` = 0L
  )

  if (is.character(viewer)) {
    mode <- match.arg(viewer)
    if (mode == "dialog") {
      viewer <- shiny::dialogViewer(
        "Package search",
        width = 800,
        height = 600
      )
    } else {
      viewer <- shiny::browserViewer()
    }
  }

  searchQuery <- function(id) {
    shiny::textInput(
      paste0("query-", id),
      label = NULL,
      value = query,
      placeholder = "Write your search query here..."
    )
  }

  maintQuery <- function() {
    shiny::textInput(
      "query-maint",
      label = "Maintainer email address",
      value = maint,
      placeholder = "Write your email address here..."
    )
  }

  searchResults <- function(id) {
    shiny::tags$div(
      shiny::tags$div(
        id = paste0("spin-", id),
        class = "spinner",
        shiny::HTML(spin_html())
      ),
      shiny::htmlOutput(paste0("results-", id))
    )
  }

  ui <- shiny::navbarPage(
    windowTitle = "Search CRAN packages",
    title = shiny::div(
      shiny::div(
        id = "rhub-logo",
        shiny::tags$a(
          shiny::img(src = "https://cdn.jsdelivr.net/gh/r-hub/branding@master/logo/rhub-square.svg",
              width = "40px",
              height = "40px"
          ),
          href = "https://docs.r-hub.io"
        )
        ),
      shiny::div(
        id = "done-button",
        shiny::actionButton("done", label = "Done", class = "btn btn-primary")
      )
    ),
    shiny::tabPanel("Search", searchQuery("search"), searchResults("search")),
    shiny::tabPanel("New packages", searchResults("new")),
    shiny::navbarMenu("Top packages",
      shiny::tabPanel("Most downloaded", searchResults("topdl")),
      shiny::tabPanel("Most depended upon", searchResults("topdep")),
      shiny::tabPanel("Trending", searchResults("trend"))
    ),
    shiny::tabPanel("My packages", maintQuery(), searchResults("maint")),
    header = shiny::tagList(
      shiny::tags$head(
        shiny::tags$style(addin_styles()),
        shiny::tags$style(spin_css()),
        shiny::tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "https://cdn.jsdelivr.net/gh/gaborcsardi/r-font@master/rlogo.css"
        ),
        shiny::tags$script("
          Shiny.addCustomMessageHandler('selectSearch', function(message) {
            $('#query-search').select();
          });
        ")
      ),
      shinyjs::useShinyjs()
    )
  )

  reactives <- shiny::reactiveValues(
    "search-prev" = 0L,
    "search-next" = 0L,
    "new-prev" = 0L,
    "new-next" = 0L,
    "topdl-prev" = 0L,
    "topdl-next" = 0L,
    "topdep-prev" = 0L,
    "topdep-next" = 0L,
    "trend-prev" = 0L,
    "trend-next" = 0L,
    "maint-prev" = 0L,
    "maint-next" = 0L
  )

  server <- function(input, output, session) {
    session$sendCustomMessage("selectSearch", "select")

    shiny::observeEvent(input$done, shiny::stopApp())

    output$`results-search` <- shiny::renderUI({
      if (input$`query-search` != "") {
        shinyjs::runjs("$(\"#spin-search\").css(\"display\",\"block\");")
      }
      ret <- simple_search(
        input$`query-search`,
        reactives$`search-prev`,
        reactives$`search-next`
      )
      shinyjs::runjs("window.scrollTo(0, 0)")
      shinyjs::runjs("$(\"#spin-search\").css(\"display\",\"none\");")
      ret
    })

    output$`results-new` <- shiny::renderUI({
      shinyjs::runjs("$(\"#spin-new\").css(\"display\",\"block\");")
      ret <- new_search(
        reactives$`new-prev`,
        reactives$`new-next`
      )
      shinyjs::runjs("window.scrollTo(0, 0)")
      shinyjs::runjs("$(\"#spin-new\").css(\"display\",\"none\");")
      ret
    })

    output$`results-topdl` <- shiny::renderUI({
      shinyjs::runjs("$(\"#spin-topdl\").css(\"display\",\"block\");")
      ret <- topdl_search(
        reactives$`topdl-prev`,
        reactives$`topdl-next`
      )
      shinyjs::runjs("window.scrollTo(0, 0)")
      shinyjs::runjs("$(\"#spin-topdl\").css(\"display\",\"none\");")
      ret
    })

    output$`results-topdep` <- shiny::renderUI({
      shinyjs::runjs("$(\"#spin-topdep\").css(\"display\",\"block\");")
      ret <- topdep_search(
        reactives$`topdep-prev`,
        reactives$`topdep-next`
      )
      shinyjs::runjs("window.scrollTo(0, 0)")
      shinyjs::runjs("$(\"#spin-topdep\").css(\"display\",\"none\");")
      ret
    })

    output$`results-trend` <- shiny::renderUI({
      shinyjs::runjs("$(\"#spin-trend\").css(\"display\",\"block\");")
      ret <- trend_search(
        reactives$`trend-prev`,
        reactives$`trend-next`
      )
      shinyjs::runjs("window.scrollTo(0, 0)")
      shinyjs::runjs("$(\"#spin-trend\").css(\"display\",\"none\");")
      ret
    })

    output$`results-maint` <- shiny::renderUI({
      shinyjs::runjs("$(\"#spin-maint\").css(\"display\",\"block\");")
      ret <- maint_search(
        input$`query-maint`,
        reactives$`maint-prev`,
        reactives$`maint-next`
      )
      shinyjs::runjs("window.scrollTo(0, 0)")
      shinyjs::runjs("$(\"#spin-maint\").css(\"display\",\"none\");")
      ret
    })

    wire_menu(input, output)
  }

  wire_menu <- function(input, output) {
    tabs <- c("search", "new", "topdl", "topdep", "trend", "maint")
    lapply(tabs, function(tab) {
      lapply(1:10, function(i) {
        id <- paste0("btn-", tab, "-", i, "-cran")
        if (! id %in% wired) {
          wired <<- c(wired, id)
          shiny::observeEvent(input[[id]], action_cran(tab, i))
        }
      })
      lapply(1:10, function(i) {
        id <- paste0("btn-", tab, "-", i, "-home")
        if (! id %in% wired) {
          wired <<- c(wired, id)
          shiny::observeEvent(input[[id]], action_home(tab, i))
        }
      })
      lapply(1:10, function(i) {
        id <- paste0("btn-", tab, "-", i, "-metacran")
        if (! id %in% wired) {
          wired <<- c(wired, id)
          shiny::observeEvent(input[[id]], action_metacran(tab, i))
        }
      })
      lapply(1:10, function(i) {
        id <- paste0("btn-", tab, "-", i, "-source")
        if (! id %in% wired) {
          wired <<- c(wired, id)
          shiny::observeEvent(input[[id]], action_source(tab, i))
        }
      })
      id1 <- paste0(tab, "-prev")
      if (! id1 %in% wired) {
        wired <<- c(wired, id1)
        shiny::observeEvent(
          input[[id1]],
          shiny::isolate(reactives[[id1]] <- reactives[[id1]] + 1)
        )
      }
      id2 <- paste0(tab, "-next")
      if (! id2 %in% wired) {
        wired <<- c(wired, id2)
        shiny::observeEvent(
          input[[id2]],
          shiny::isolate(reactives[[id2]] <- reactives[[id2]] + 1)
        )
      }
    })
  }

  simple_search <- function(query, btn_prev, btn_next) {

    # Create dependencies
    btn_next
    btn_prev

    if (identical(query, meta(data$search)$query)) {
      if (btn_next > data$`cnt-search-next`) {
        from <- meta(data$search)$from + meta(data$search)$size
        data$`cnt-search-next` <<- btn_next
      } else {
        from <- meta(data$search)$from - meta(data$search)$size
        data$`cnt-search-prev` <<- btn_prev
      }
    } else {
      from <- 1
    }

    if (is.null(query) || nchar(query) == 0) return(NULL)

    result <- pkg_search(query, from = from)
    data$search <<- result
    format_addin_results(result, "search")
  }

  new_search <- function(btn_prev, btn_next) {

    btn_prev
    btn_next

    if (btn_next > data$`cnt-new-next`) {
      from <- meta(data$new)$from + meta(data$new)$size
      data$`cnt-new-next` <<- btn_next
    } else if (btn_prev > data$`cnt-new-prev`) {
      from <- meta(data$new)$from - meta(data$new)$size
      data$`cnt-new-prev` <<- btn_prev
    } else {
      from <- 1
    }

    result <- rectangle_events(cran_events(archivals = FALSE, from = from))
    attr(result, "metadata") <- list(from = from, size = 10, total = 15000)
    data$new <<- result
    format_addin_results(result, "new")
  }

  topdl_search <- function(btn_prev, btn_next) {

    btn_prev
    btn_next

    if (btn_next > data$`cnt-topdl-next`) {
      from <- meta(data$topdl)$from + meta(data$topdl)$size
      data$`cnt-topdl-next` <<- btn_next
    } else if (btn_prev > data$`cnt-topdl-prev`) {
      from <- meta(data$topdl)$from - meta(data$topdl)$size
      data$`cnt-topdl-prev` <<- btn_prev
    } else {
      from <- 1
    }

    result <- get_topdl(from = from)
    attr(result, "metadata") <- list(from = from, size = 10, total = 100)
    data$topdl <<- result
    format_addin_results(result, "topdl")
  }

  cran_top_dl <- memoise::memoise(
    cran_top_downloaded,
    ~ memoise::timeout(60 * 60)
  )

  get_topdl0 <- function(from) {
    dl <- cran_top_dl()
    pkgs <- cran_packages(dl$package[from:(from + 10 - 1)])
    rectangle_pkgs(pkgs)
  }

  # Cache the top downloaded packages for one hour
  get_topdl <- memoise::memoise(get_topdl0, ~ memoise::timeout(60 * 60))

  topdep_search <- function(btn_prev, btn_next) {

    btn_prev
    btn_next

    if (btn_next > data$`cnt-topdep-next`) {
      from <- meta(data$topdep)$from + meta(data$topdep)$size
      data$`cnt-topdep-next` <<- btn_next
    } else if (btn_prev > data$`cnt-topdep-prev`) {
      from <- meta(data$topdep)$from - meta(data$topdep)$size
      data$`cnt-topdep-prev` <<- btn_prev
    } else {
      from <- 1
    }

    result <- get_topdep(from = from)
    attr(result, "metadata") <- list(from = from, size = 10, total = 15000)
    data$topdep <<- result
    format_addin_results(result, "topdep")
  }

  dep_list0 <- function() {
    deps <- crandb_query("/-/deps/devel")
    data_frame(
      package = names(deps),
      count = unlist(deps)
    )
  }

  dep_list <- memoise::memoise(dep_list0, ~ memoise::timeout(60 * 60))

  get_topdep0 <- function(from) {
    deps <- dep_list()
    deps <- deps[order(deps$count, decreasing = TRUE), ]
    pkgs <- cran_packages(deps$package[from:(from + 10 - 1)])
    rectangle_pkgs(pkgs)
  }

  get_topdep <- memoise::memoise(get_topdep0, ~ memoise::timeout(60 * 60))

  trend_search <- function(btn_prev, btn_next) {

    btn_prev
    btn_next

    if (btn_next > data$`cnt-trend-next`) {
      from <- meta(data$trend)$from + meta(data$trend)$size
      data$`cnt-trend-next` <<- btn_next
    } else if (btn_prev > data$`cnt-trend-prev`) {
      from <- meta(data$trend)$from - meta(data$trend)$size
      data$`cnt-trend-prev` <<- btn_prev
    } else {
      from <- 1
    }

    result <- get_trend(from = from)
    attr(result, "metadata") <- list(from = from, size = 10, total = 15000)
    data$trend <<- result
    format_addin_results(result, "trend")
  }

  cran_trending1 <- memoise::memoise(
    cran_trending,
    ~ memoise::timeout(60 * 60)
  )

  get_trend0 <- function(from) {
    deps <- cran_trending1()
    pkgs <- cran_packages(deps$package[from:(from + 10 - 1)])
    rectangle_pkgs(pkgs)
  }

  get_trend <- memoise::memoise(get_trend0, ~ memoise::timeout(60 * 60))

  maint_search <- function(query, btn_prev, btn_next) {

    # Create dependencies
    btn_next
    btn_prev

    if (identical(query, meta(data$maint)$query)) {
      if (btn_next > data$`cnt-maint-next`) {
        from <- meta(data$maint)$from + meta(data$maint)$size
        data$`cnt-maint-next` <<- btn_next
      } else {
        from <- meta(data$maint)$from - meta(data$maint)$size
        data$`cnt-maint-prev` <<- btn_prev
      }
    } else {
      from <- 1
    }

    if (is.null(query) || nchar(query) == 0) return(NULL)
    if (!grepl("@.", query)) return(NULL)

    result <- get_maint(query, from = from)
    data$maint <<- result
    format_addin_results(result, "maint")
  }

  get_maint_data0 <- function(query) {
    ep <- "/-/maintainer"
    keys <- trim(strsplit(query, ",", fixed = TRUE)[[1]])
    keys <- map_chr(keys, utils::URLencode)
    keys <- paste0('"', keys, '"', collapse = ",")
    url <- paste0(ep, "?keys=[", keys, "]")
    ret <- crandb_query(url)
    if (length(ret)) {
      data_frame(email = ret[,1], package = ret[,2])
    } else {
      data_frame(email = character(), package = character())
    }
  }

  get_maint_data <- memoise::memoise(
    get_maint_data0,
    ~ memoise::timeout(60 * 60)
  )

  get_maint0 <- function(query, from) {
    mine <- get_maint_data(query)
    pkgs <- cran_packages(mine$package[from:(from + 10 - 1)])
    result <- rectangle_pkgs(pkgs)
    attr(result, "metadata") <- list(
      from = from,
      size = 10,
      total = nrow(mine),
      query = query
    )
    result
  }

  get_maint <- memoise::memoise(get_maint0, ~ memoise::timeout(60 * 60))

  action_cran <- function(set, row) {
    package <- data[[set]]$package[[row]]
    url <- paste0("https://cloud.r-project.org/package=", package)
    utils::browseURL(url)
  }

  action_metacran <- function(set, row) {
    package <- data[[set]]$package[[row]]
    url <- paste0("https://r-pkg.org/pkg/", package)
    utils::browseURL(url)
  }

  action_home <- function(set, row) {
    urls <- find_urls(data[[set]]$url[[row]])
    lapply(urls, utils::browseURL)
  }

  action_source <- function(set, row) {
    package <- data[[set]]$package[[row]]
    url <- paste0("https://github.com/cran/", package)
    utils::browseURL(url)
  }

  on.exit(tryCatch(shiny::stopApp(), error = function(e) NULL), add = TRUE)
  shiny::runGadget(ui, server, viewer = viewer)
}

url_regex <- function() "(https?://[^\\s,;>]+)"

find_urls <- function(txt) {
  if (is.na(txt)) return(character())
  mch <- gregexpr(url_regex(), txt, perl = TRUE)
  regmatches(txt, mch)[[1]]
}

highlight_urls <- function(txt) {
  gsub(url_regex(), "<a href=\"\\1\">\\1</a>", txt, perl = TRUE)
}

addin_styles <- function() {
  shiny::HTML("
          .packagename {
            margin-top: 30px;
          }
          .packagename .dropdown-menu {
           margin-top: 10px;
            padding: 0;
            border: 0;
          }
          .packagename li .btn {
            text-align: left;
            padding: 4px;
            font-size: 80%;
            border-radius: 0;
            width: 100%;
          }
          .btn-package {
            font-size: 140%;
            color: rgb(51, 122, 183);
            background-color: #fff;
            border: none;
            padding-left: 0px;
          }
          .packageauthor {
            color: #888;
            font-style: italic;
            font-size: 90%;
            padding-right: 10px;
            margin-top: -5px;
            margin-bottom: 5px;
          }
          .packagetitle {
            font-size: 110%;
          }
          .paginate {
            padding-top: 10px;
            padding-bottom: 10px;
          }
          .fa-cubes { color: blue }
          #rhub-logo {
            right: 10px;
            top: 0px;
            margin-top: -10px;
          }
          #done-button {
            position: absolute;
            top: 10px;
            right: 10px;
          }
          .spinner {
            display: none;
          }
          "
  )
}

spin_css <- function() {
  ".lds-ellipsis {
    display: inline-block;
    position: relative;
    width: 40px;
    height: 40px;
  }
  .lds-ellipsis div {
    position: absolute;
    top: 13px;
    width: 13px;
    height: 13px;
    border-radius: 50%;
    background: #569fde;
    animation-timing-function: cubic-bezier(0, 1, 1, 0);
  }
  .lds-ellipsis div:nth-child(1) {
    left: 8px;
    animation: lds-ellipsis1 0.6s infinite;
  }
  .lds-ellipsis div:nth-child(2) {
    left: 8px;
    animation: lds-ellipsis2 0.6s infinite;
  }
  .lds-ellipsis div:nth-child(3) {
    left: 32px;
    animation: lds-ellipsis2 0.6s infinite;
  }
  .lds-ellipsis div:nth-child(4) {
    left: 56px;
    animation: lds-ellipsis3 0.6s infinite;
  }
  @keyframes lds-ellipsis1 {
    0% {
      transform: scale(0);
    }
    100% {
      transform: scale(1);
    }
  }
  @keyframes lds-ellipsis3 {
    0% {
      transform: scale(1);
    }
    100% {
      transform: scale(0);
    }
  }
  @keyframes lds-ellipsis2 {
    0% {
      transform: translate(0, 0);
    }
    100% {
      transform: translate(24px, 0);
    }
  }
  "
}

spin_html <- function() {
  "<div class=\"lds-ellipsis\"><div></div><div></div><div></div><div></div></div>"
}

format_addin_results <- function(results, id) {
  if (is.null(results)) return(NULL)
  meta <- attr(results, "metadata")
  took <- format_took(results)

  if (nrow(results) == 0) {
    if (!is.null(took)) return(shiny::div(took))
    return(shiny::div("No packages found. :("))
  }

  pkgs <- lapply(
    seq_len(nrow(results)),
    function(i) format_pkg(results[i,], id, i, meta$from)
  )
  paginate <- format_paginate(results, id)

  do.call(
    shiny::div,
    c(list(shiny::div(took)),
      pkgs,
      list(shiny::div(paginate, class = "paginate"))
    )
  )
}

format_took <- function(results) {
  meta <- meta(results)
  page <- if (meta$from != 1) {
    paste0("Page ", (meta$from - 1)/meta$size + 1)
  }
  if (is.null(meta$took)) return(page)

  paste0(
    if (meta$from != 1) paste0(page, " of "),
    meta$total,
    if (meta$total == 1) " package, " else " packages, ",
    "found in ",
    round(meta$took / 1000, 3),
    " seconds"
  )
}

format_paginate <- function(results, id) {
  meta <- attr(results, "metadata")
  has_prev <- meta$from != 1
  has_next <- meta$from + nrow(results) - 1L < meta$total
  btns <- zap_null(list(
    if (has_prev) shiny::actionButton(paste0(id, "-prev"), "Previous"),
    if (has_next) shiny::actionButton(paste0(id, "-next"), "Next")
  ))
  btns
}

format_pkg <- function(record, id, num, from) {
  by <- paste0(
    "Version ",
    record$version,
    ", by ",
    record$maintainer_name,
    ", ",
    format_time_ago$time_ago(record$date)
  )
  urls <- find_urls(record$url)
  shiny::p(
    shiny::div(
      class = "packagetitlerow",
      shiny::span(
        shinyWidgets::dropdownButton(
          if (length(urls)) {
            shiny::actionButton(
              paste0("btn-", id, "-", num, "-home"),
              label = "View home page",
              icon = shiny::icon("home")
            )
          },
          shiny::actionButton(
            paste0("btn-", id, "-", num, "-cran"),
            label = shiny::HTML("<i class=\"icon-rlogo\"></i> View on CRAN")
          ),
          shiny::actionButton(
            paste0("btn-", id, "-", num, "-metacran"),
            label = "View on METACRAN",
            icon = shiny::icon("globe")
          ),
          shiny::actionButton(
            paste0("btn-", id, "-", num, "-source"),
            label = "Source code at CRAN@GH mirror",
            icon = shiny::icon("file-alt")
          ),
          # shiny::actionButton(
          #   paste0("btn-", id, "-", num, -install"),
          #   label = "Install, with dependencies"
          # ),
          # shiny::actionButton(
          #  paste0("btn-", id, "-", num, "-bug"),
          #  label = "Report a bug about this package"
          #),
          label = record$package,
          circle = FALSE,
          inline = TRUE,
          size = "sm",
          status = "package",
          margin = "0"
        ),
        class = "packagename"
      ),
      shiny::span("\u2014 ", record$title, class = "packagetitle")
    ),
    shiny::div(
      shiny::div(by, class = "packageauthor"),
      shiny::div(
        clean_description(record$description),
        class = "packagedescription"
      ),
      if (!is.na(record$url)) {
        shiny::div(shiny::HTML(highlight_urls(record$url)), class = "packageurl")
      }
    )
  )
}

rectangle_pkgs <- function(pkgs) {
  maintainer <- parse_maint(pkgs$Maintainer %||% character())
  data_frame(
    package =          pkgs$Package %||% character(),
    version =          pkgs$Version %||% character(),
    title =            pkgs$Title %||% character(),
    description =      pkgs$Description %||% character(),
    date =             parse_iso_8601(pkgs$date %||% character()),
    maintainer_name =  maintainer$maintainer_name,
    maintainer_email = maintainer$maintainer_email,
    license =          pkgs$License %||% character(),
    url =              pkgs$URL %||% character(),
    bugreports =       pkgs$BugReports %||% character()
  )
}

parse_maint <- function(x) {
  data_frame(
    maintainer_name = gsub("\\s*<.*$", "", x),
    maintainer_email = gsub("^.*<([^>]+)>.*$", "\\1", x, perl = TRUE)
  )
}

rectangle_events <- function(ev) {
  maintainer <- parse_maint(map_chr(ev, function(x) x$package$Maintainer %||% NA_character_))
  data_frame(
    package =          map_chr(ev, function(x) x$package$Package),
    version =          map_chr(ev, function(x) x$package$Version),
    title =            map_chr(ev, function(x) x$package$Title),
    description =      map_chr(ev, function(x) x$package$Description),
    date =             parse_iso_8601(map_chr(ev, "[[", "date")),
    maintainer_name =  maintainer$maintainer_name,
    maintainer_email = maintainer$maintainer_email,
    license =          map_chr(ev, function(x) x$package$License),
    url =
      map_chr(ev, function(x) x$package$URL %||% NA_character_),
    bugreports =
      map_chr(ev, function(x) x$package$BugReports %||% NA_character_),
    package_data =     map(ev, "[[", "package")
  )
}
