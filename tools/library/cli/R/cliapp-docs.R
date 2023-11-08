
#' @title About inline markup in the semantic cli
#'
#' @description
#' To learn how to use cli’s semantic markup, start with the ‘Building
#' a semantic CLI’ article at <https://cli.r-lib.org>.
#'
#' @section Command substitution:
#'
#' All text emitted by cli supports glue interpolation. Expressions
#' enclosed by braces will be evaluated as R code. See [glue::glue()] for
#' details.
#'
#' In addition to regular glue interpolation, cli can also add classes
#' to parts of the text, and these classes can be used in themes. For
#' example
#'
#' ```{asciicast inline-text}
#' cli_text("This is {.emph important}.")
#' ```
#'
#' adds a class to the "important" word, class `"emph"`. Note that in this
#' case the string within the braces is usually not a valid R expression.
#' If you want to mix classes with interpolation, add another pair of
#' braces:
#'
#' ```{asciicast inline-text-2}
#' adjective <- "great"
#' cli_text("This is {.emph {adjective}}.")
#' ```
#'
#' An inline class will always create a `span` element internally. So in
#' themes, you can use the `span.emph` CSS selector to change how inline
#' text is emphasized:
#'
#' ```{asciicast inline-text-3}
#' cli_div(theme = list(span.emph = list(color = "red")))
#' adjective <- "nice and red"
#' cli_text("This is {.emph {adjective}}.")
#' ```
#'
#' @section Classes:
#'
#' The default theme defines the following inline classes:
#' * `arg` for a function argument.
#' * `cls` for an S3, S4, R6 or other class name.
#' * `code` for a piece of code.
#' * `dt` is used for the terms in a definition list ([cli_dl()]).
#' * `dd` is used for the descriptions in a definition list ([cli_dl()]).
#' * `email` for an email address.
#'   If the terminal supports ANSI hyperlinks (e.g. RStudio, iTerm2, etc.),
#'   then cli creates a clickable link.
#'   See [links] for more about cli hyperlinks.
#' * `emph` for emphasized text.
#' * `envvar` for the name of an environment variable.
#' * `field` for a generic field, e.g. in a named list.
#' * `file` for a file name. If the terminal supports ANSI hyperlinks (e.g.
#'   RStudio, iTerm2, etc.), then cli creates a clickable link that opens
#'   the file in RStudio or with the default app for the file type.
#'   See [links] for more about cli hyperlinks.
#' * `fun` for a function name. If it is in the `package::function_name`
#'   form, and the terminal supports ANSI hyperlinks (e.g. RStudio,
#'   iTerm2, etc.), then cli creates a clickable link.
#'   See [links] for more about cli hyperlinks.
#' * `help` is a help page of a _function_.
#'   If the terminal supports ANSI hyperlinks to help pages (e.g. RStudio),
#'   then cli creates a clickable link. It supports link text.
#'   See [links] for more about cli hyperlinks.
#' * `href` creates a hyperlink, potentially with a link text.
#'   If the terminal supports ANSI hyperlinks (e.g. RStudio, iTerm2, etc.),
#'   then cli creates a clickable link.
#'   See [links] for more about cli hyperlinks.
#' * `key` for a keyboard key.
#' * `obj_type_friendly` formats the type of an R object in a readable way,
#'   and it should be used with `{}`, see an example below.
#' * `or` changes the string that separates the last two elements of
#'   collapsed vectors (see below) from "and" to "or".
#' * `path` for a path (the same as `file` in the default theme).
#' * `pkg` for a package name.
#' * `run` is an R expression, that is potentially clickable if the terminal
#'   supports ANSI hyperlinks to runnable code (e.g. RStudio).
#'   It supports link text. See [links] for more about cli hyperlinks.
#' * `strong` for strong importance.
#' * `topic` is a help page of a _ropic_.
#'   If the terminal supports ANSI hyperlinks to help pages (e.g. RStudio),
#'   then cli creates a clickable link. It supports link text.
#'   See [links] for more about cli hyperlinks.
#' * `type` formats the type of an R object in a readable way, and it
#'   should be used with `{}`, see an example below.
#' * `url` for a URL. If the terminal supports ANSI hyperlinks (e.g.
#'   RStudio, iTerm2, etc.), then cli creates a clickable link.
#'   See [links] for more about cli hyperlinks.
#' * `var` for a variable name.
#' * `val` for a generic "value".
#' * `vignette` is a vignette.
#'   If the terminal supports ANSI hyperlinks to help pages (e.g. RStudio),
#'   then cli creates a clickable link. It supports link text.
#'   See [links] for more about cli hyperlinks.
#'
#' ```{asciicast inline-examples}
#' ul <- cli_ul()
#' cli_li("{.emph Emphasized} text.")
#' cli_li("{.strong Strong} importance.")
#' cli_li("A piece of code: {.code sum(a) / length(a)}.")
#' cli_li("A package name: {.pkg cli}.")
#' cli_li("A function name: {.fn cli_text}.")
#' cli_li("A keyboard key: press {.kbd ENTER}.")
#' cli_li("A file name: {.file /usr/bin/env}.")
#' cli_li("An email address: {.email bugs.bunny@acme.com}.")
#' cli_li("A URL: {.url https://example.com}.")
#' cli_li("An environment variable: {.envvar R_LIBS}.")
#' cli_li("`mtcars` is {.obj_type_friendly {mtcars}}")
#' cli_end(ul)
#' ```
#'
#' You can add new classes by defining them in the theme, and then using
#' them.
#'
#' ```{asciicast inline-newclass}
#' cli_div(theme = list(
#'   span.myclass = list(color = "lightgrey"),
#'   "span.myclass" = list(before = "<<"),
#'   "span.myclass" = list(after = ">>")))
#' cli_text("This is {.myclass in angle brackets}.")
#' cli_end()
#' ```
#'
#' ## Highlighting weird-looking values
#'
#' Often it is useful to highlight a weird file or path name, e.g. one
#' that starts or ends with space characters. The built-in theme does this
#' for `.file`, `.path` and `.email` by default. You can highlight
#' any string inline by adding the `.q` class to it.
#'
#' The current highlighting algorithm
#' * adds single quotes to the string if it does not start or end with an
#'   alphanumeric character, underscore, dot or forward slash.
#' * Highlights the background colors of leading and trailing spaces on
#'   terminals that support ANSI colors.
#'
#' @section Collapsing inline vectors:
#'
#' When cli performs inline text formatting, it automatically collapses
#' glue substitutions, after formatting. This is handy to create lists of
#' files, packages, etc.
#'
#' ```{asciicast inline-collapse}
#' pkgs <- c("pkg1", "pkg2", "pkg3")
#' cli_text("Packages: {pkgs}.")
#' cli_text("Packages: {.pkg {pkgs}}.")
#' ```
#'
#' Class names are collapsed differently by default
#'
#' ```{asciicast inline-collapse-2}
#' x <- Sys.time()
#' cli_text("Hey, {.var x} has class {.cls {class(x)}}.")
#' ```
#'
#' By default cli truncates long vectors. The truncation limit is by default
#' twenty elements, but you can change it with the `vec-trunc` style.
#'
#' ```{asciicast inline-collapse-trunc}
#' nms <- cli_vec(names(mtcars), list("vec-trunc" = 5))
#' cli_text("Column names: {nms}.")
#' ```
#'
#' @section Formatting values:
#'
#' The `val` inline class formats values. By default (c.f. the built-in
#' theme), it calls the [cli_format()] generic function, with the current
#' style as the argument. See [cli_format()] for examples.
#'
#' `str` is for formatting strings, it uses [base::encodeString()] with
#' double quotes.
#'
#' @section Escaping `{` and `}`:
#'
#' It might happen that you want to pass a string to `cli_*` functions,
#' and you do _not_ want command substitution in that string, because it
#' might contain `{` and `}` characters. The simplest solution for this is
#' to refer to the string from a template:
#'
#' ```{asciicast inline-escape}
#' msg <- "Error in if (ncol(dat$y)) {: argument is of length zero"
#' cli_alert_warning("{msg}")
#' ```
#'
#' If you want to explicitly escape `{` and `}` characters, just double
#' them:
#'
#' ```{asciicast inline-escape-2}
#' cli_alert_warning("A warning with {{ braces }}.")
#' ```
#'
#' See also examples below.
#'
#' @section Pluralization:
#'
#' All cli commands that emit text support pluralization. Some examples:
#'
#' ```{asciicast inline-plural}
#' ndirs <- 1
#' nfiles <- 13
#' cli_alert_info("Found {ndirs} diretor{?y/ies} and {nfiles} file{?s}.")
#' cli_text("Will install {length(pkgs)} package{?s}: {.pkg {pkgs}}")
#' ```
#'
#' See [pluralization] for details.
#'
#' @section Wrapping:
#'
#' Most cli containers wrap the text to width the container's width,
#' while observing margins requested by the theme.
#'
#' To avoid a line break, you can use the UTF_8 non-breaking space
#' character: `\u00a0`. cli will not break a line here.
#'
#' To force a line break, insert a form feed character: `\f` or
#' `\u000c`. cli will insert a line break there.
#'
#' @name inline-markup
NULL

#' About cli containers
#'
#' Container elements may contain other elements. Currently the following
#' commands create container elements: [cli_div()], [cli_par()], the list
#' elements: [cli_ul()], [cli_ol()], [cli_dl()], and list items are
#' containers as well: [cli_li()].
#'
#' ## Themes
#'
#' A container can add a new theme, which is removed when the container
#' exits.
#'
#' ```{asciicast cnt-theme}
#' d <- cli_div(theme = list(h1 = list(color = "blue",
#'                                     "font-weight" = "bold")))
#' cli_h1("Custom title")
#' cli_end(d)
#' ```
#'
#' ## Auto-closing
#'
#' Container elements are closed with [cli_end()]. For convenience,
#' by default they are closed automatically when the function that created
#' them terminated (either regularly or with an error). The default
#' behavior can be changed with the `.auto_close` argument.
#'
#' ```{asciicast cnt-auto-close}
#' div <- function() {
#'   cli_div(class = "tmp", theme = list(.tmp = list(color = "yellow")))
#'   cli_text("This is yellow")
#' }
#' div()
#' cli_text("This is not yellow any more")
#' ```
#'
#' ## Debugging
#'
#' You can use the internal `cli:::cli_debug_doc()` function to see the
#' currently open containers.
#'
#' ```{asciicast cnt-debug, echo = -1}
#' stop_app()
#' fun <- function() {
#'   cli_div(id = "mydiv")
#'   cli_par(class = "myclass")
#'   cli:::cli_debug_doc()
#' }
#' fun()
#' ```
#'
#' @name containers
NULL

#' About cli themes
#'
#' CLI elements can be styled via a CSS-like language of selectors and
#' properties. Only a small subset of CSS3 is supported, and
#' a lot visual properties cannot be implemented on a terminal, so these
#' will be ignored as well.
#'
#' @section Adding themes:
#' The style of an element is calculated from themes from four sources.
#' These form a stack, and the themes on the top of the stack take
#' precedence, over themes in the bottom.
#'
#' 1. The cli package has a built-in theme. This is always active.
#'    See [builtin_theme()].
#' 2. When an app object is created via [start_app()], the caller can
#'    specify a theme, that is added to theme stack. If no theme is
#'    specified for [start_app()], the content of the `cli.theme` option
#'    is used. Removed when the corresponding app stops.
#' 3. The user may specify a theme in the `cli.user_theme` option. This
#'    is added to the stack _after_ the app's theme (step 2.), so it can
#'    override its settings. Removed when the app that added it stops.
#' 4. Themes specified explicitly in [cli_div()] elements. These are
#'    removed from the theme stack, when the corresponding [cli_div()]
#'    elements are closed.
#'
#' @section Writing themes:
#' A theme is a named list of lists. The name of each entry is a CSS
#' selector. Only a subset of CSS is supported:
#' * Type selectors, e.g. `input` selects all `<input>` elements.
#' * Class selectors, e.g. `.index` selects any element that has a class
#'   of "index".
#' * ID selector. `#toc` will match the element that has the ID "toc".
#' * The descendant combinator, i.e. the space, that selects nodes
#'   that are descendants of the first element. E.g. `div span` will match
#'   all `<span>` elements that are inside a `<div>` element.
#'
#' The content of a theme list entry is another named list, where the
#' names are CSS properties, e.g. `color`, or `font-weight` or
#' `margin-left`, and the list entries themselves define the values of
#' the properties. See [builtin_theme()] and [simple_theme()] for examples.
#'
#' @section Formatter callbacks:
#' For flexibility, themes may also define formatter functions, with
#' property name `fmt`. These will be called once the other styles are
#' applied to an element. They are only called on elements that produce
#' output, i.e. _not_ on container elements.
#'
#' @section Supported properties:
#' Right now only a limited set of properties are supported. These include
#' left, right, top and bottom margins, background and foreground colors,
#' bold and italic fonts, underlined text. The `before` and `after`
#' properties are supported to insert text before and after the
#' content of the element.
#'
#' The current list of properties:
#'
#' * `after`: A string literal to insert after the element. It can also be
#'   a function that returns a string literal. Supported by all inline
#'   elements, list items, alerts and rules.
#' * `background-color`: An R color name, or HTML hexadecimal color.
#'   It can be applied to most elements (inline elements, rules, text,
#'   etc.), but the background of containers is not colored properly
#'   currently.
#' * `before`: A string literal to insert before the element. It can also be
#'   a function that returns a string literal. Supported by all inline
#'   elements, list items, alerts and rules.
#' * `class-map`: Its value can be a named list, and it specifies how
#'   R (S3) class names are mapped to cli class names. E.g.
#'   `list(fs_path = "file")` specifies that `fs_path` objects (from the fs
#'   package) should always print as `.file` objects in cli.
#' * `color`: Text color, an R color name or a HTML hexadecimal color. It
#'   can be applied to most elements that are printed.
#' * `collapse`: Specifies how to collapse a vector, before applying
#'   styling. If a character string, then that is used as the separator.
#'   If a function, then it is called, with the vector as the only
#'   argument.
#' * `digits`: Number of digits after the decimal point for numeric inline
#'   element of class `.val`.
#' * `fmt`: Generic formatter function that takes an input text and returns
#'   formatted text. Can be applied to most elements. If colors are in use,
#'   the input text provided to `fmt` already includes ANSI sequences.
#' * `font-style`: If `"italic"` then the text is printed as cursive.
#' * `font-weight`: If `"bold"`, then the text is printed in boldface.
#' * `line-type`: Line type for [cli_rule()].
#' * `list-style-type`: String literal or functions that returns a string
#'   literal, to be used as a list item marker in un-ordered lists.
#' * `margin-bottom`, `margin-left`, `margin-right`, `margin-top`: Margins.
#' * `padding-left`, `padding-right`: This is currently used the same way
#'   as the margins, but this might change later.
#' * `start`: Integer number, the first element in an ordered list.
#' * `string-quote`: Quoting character for inline elements of class `.val`.
#' * `text-decoration`: If `"underline"`, then underlined text is created.
#' * `text-exdent`: Amount of indentation from the second line of wrapped
#'    text.
#' * `transform`: A function to call on glue substitutions, before
#'   collapsing them. Note that `transform` is applied prior to
#'   implementing color via ANSI sequences.
#' * `vec-last`: The last separator when collapsing vectors.
#' * `vec-sep`: The separator to use when collapsing vectors.
#' * `vec-sep2`: The separator to use for two elements when collapsing
#'   vectors. If not set, then `vec-sep` is used for these as well.
#' * `vec-trunc`: Vectors longer than this will be truncated. Defaults to
#'   100.
#' * `vec-trunc-style`: Select between two ways of collapsing vectors:
#'   - `"both-ends"` is the current default and it shows the beginning and
#'     the end of the vector.
#'   - `"head"` only shows the beginning of the vector.
#'
#' More properties might be added later. If you think that a property is
#' not applied properly to an element, please open an issue about it in
#' the cli issue tracker.
#'
#' @section Examples:
#' Color of headings, that are only active in paragraphs with an
#' 'output' class:
#' ```
#' list(
#'   "par.output h1" = list("background-color" = "red", color = "#e0e0e0"),
#'   "par.output h2" = list("background-color" = "orange", color = "#e0e0e0"),
#'   "par.output h3" = list("background-color" = "blue", color = "#e0e0e0")
#' )
#' ```
#'
#' Create a custom alert type:
#' ```
#' list(
#'   ".alert-start" = list(before = symbol$play),
#'   ".alert-stop"  = list(before = symbol$stop)
#' )
#' ```
#' @name themes

# TODO: examples

NULL

#' cli hyperlinks
#'
#' @description
#' Certain cli styles create clickable links, if your IDE or terminal
#' supports them.
#'
#' # Note: hyperlinks are currently experimental
#'
#' The details of the styles that create hyperlinks will prrobably change
#' in the near future, based on user feedback.
#'
#' # About the links in this manual page
#'
#' The hyperlinks that are included in this manual are demonstrative
#' only, except for the `https:` links. They look like a hyperlink, and
#' you can click on them, but they do nothing. I.e. a `.run` link will
#' not run the linked expression if you click on it.
#'
#' # Hyperlink Support
#'
#' As of today, the latest release of RStudio (version v2022.07.0+548)
#' supports all hyperlink types discussed here. Certain terminals, e.g.
#' iTerm on macOS, Linux terminals based on VTE (GNOME terminal) support
#' `.href`, `.email` and `.file` links.
#'
#' You can use [ansi_has_hyperlink_support()] to check if your terminal or
#' IDE has hyperlink support in general, and [ansi_hyperlink_types()] to
#' check if various types of hyperlinks are supported.
#'
#' If your hyperlink support is not detected properly in your IDE or
#' terminal, please open a cli issue at
#' <https://github.com/r-lib/cli/issues>.
#'
#' ```{asciicast links-setup, include = FALSE, cache = FALSE}
#' options(
#'   cli.hyperlink = TRUE,
#'   cli.hyperlink_run = TRUE,
#'   cli.hyperlink_help = TRUE,
#'   cli.hyperlink_vignette = TRUE
#' )
#' ```
#'
#' # Link text
#'
#' Before we delve into the various types of hyperlinks, a general comment
#' about link texts. Some link styles support a custom link text:
#'
#' * `.href`
#' * `.help`
#' * `.topic`
#' * `.vignette`
#' * `.run`
#'
#' Others, i.e. `.email`, `.file`, `.fun` and `.url` do not support custom
#' link text.
#'
#' The generic syntax for link text is the same as for Markdown hyperlinks:
#' ```
#' {.style [link text](url)}
#' ```
#'
#' ## Vectorization
#'
#' Note that it is not possible to add link text to a vector of URLs. E.g.
#' this will create a list of three URLs, all clickable:
#'
#' ```{asciicast link-example}
#' urls <- paste0("https://httpbin.org/status/", c(200, 403, 404))
#' cli::cli_text("Some httpbin URLs: {.url {urls}}.")
#' ```
#' But it is not possible to use a different link text for them.
#'
#' ## What if hyperlinks are not available?
#'
#' If ANSI hyperlinks are not available, then the link text for of these
#' styles outputs both the link text and the URL in a (hopefully) helpful
#' way. See examples below.
#'
#' # URLs
#'
#' There are two cli styles to link to generic URLs. `.url` does not
#' allow custom link text, but `\href` does.
#'
#' ```{asciicast links-url-1}
#' cli_text(
#'   "See the cli homepage at {.url https://cli.r-lib.org} for details."
#' )
#' ```
#'
#'```{asciicast links-url-2}
#' cli_text(
#'   "See the {.href [cli homepage](https://cli.r-lib.org)} for details."
#' )
#' ```
#'
#' ## Without hyperlink support
#'
#' This is how these links look without hyperlink support:
#'
#' ```{asciicast links-url-3}
#' local({
#'   withr::local_options(cli.hyperlink = FALSE)
#'   cli_text(
#'     "See the cli homepage at {.url https://cli.r-lib.org} for details."
#'   )
#'   cli_text(
#'     "See the {.href [cli homepage](https://cli.r-lib.org)} for details."
#'   )
#' })
#' ```
#'
#' ## URL encoding
#'
#' Note that cli does not encode the url, so you might need to call
#' `utils::URLencode()` on it, especially, if it is substituted in
#' via `{}`.
#'
#' ```{asciicast links-url-4}
#' weirdurl <- utils::URLencode("https://example.com/has some spaces")
#' cli_text("See more at {.url {weirdurl}}.")
#' ```
#'
#' # Files
#'
#' The `.file` style now automatically creates a `file:` hyperlink.
#' Because `file:` hyperlinks must contain an absolute path, cli tries to
#' convert relative paths, and paths starting with `~` to aboslute path.
#'
#' ```{asciicast links-file-1}
#' cli_text("... edit your {.file ~/.Rprofile} file.}")
#' ```
#'
#' ## Link text
#'
#' `.file` cannot use a custom link text. If you custom link text, then
#' you can use `.href` with a `file:` URL.
#'
#' ```{asciicast links-file-2}
#' prof <- path.expand("~/.Rprofile")
#' cli_text("... edit your {.href [R profile](file://{prof})}.")
#' ```
#'
#' ## Line and column numbers
#'
#' You may add a line number to a file name, separated by `:`. Handlers
#' typically place the cursor at that line after opening the file.
#' You may also add a column number, after the line number, separated by
#' another `:`.
#'
#' ```{asciicast links-file-3}
#' cli_text("... see line 5 in {.file ~/.Rprofile:5}.")
#' ```
#'
#' ## Default handler
#'
#' In RStudio `file:` URLs open within RStudio. If you click on a file
#' link outside of RStudio, typically the operating system is consulted
#' for the application to open it.
#'
#' ## Without hyperlink support
#'
#' One issue with using `.href` file files is that it does not look great
#' if hyperlinks are not available. This will be improved in the future:
#'
#' ```{asciicast links-file-4}
#' local({
#'   withr::local_options(cli.hyperlink = FALSE)
#'   prof <- path.expand("~/.Rprofile")
#'   cli_text("... edit your {.href [R profile](file://{prof})}.")
#' })
#' ```
#'
#' # Links to the manual
#'
#' `.fun` automatically creates links to the manual page of the function,
#' provided the function name is in the `packagename::functionname` form:
#'
#' ```{asciicast links-fun-1}
#' cli::cli_text("... see {.fun stats::lm} to learn more.")
#' ```
#'
#' ## Link text
#'
#' For a custom link text, use `.help` instead of `.fun`.
#'
#' ```{asciicast links-fun-2}
#' cli::cli_text("... see {.help [{.fun lm}](stats::lm)} to learn more.")
#' ```
#'
#' ## Without hyperlink support
#'
#' The same message without hyperlink support looks like this:
#'
#' ```{asciicast links-fun-3}
#' local({
#'   withr::local_options(cli.hyperlink = FALSE)
#'   cli::cli_text("... see {.help [{.fun lm}](stats::lm)} to learn more.")
#' })
#' ```
#'
#' ## Topics
#'
#' To link to a help topic that is not a function, use `.topic`:
#'
#' ```{asciicast links-topic}
#' cli::cli_text("... the tibble options at {.help tibble::tibble_options}.")
#' ```
#'
#' `.topic` support link text.
#'
#' ## Vignettes
#'
#' To link to a vignette, use `.vignette`:
#'
#' ```{asciicast links-vignette}
#' cli::cli_text("... see the {.vignette tibble::types} vignette.")
#' ```
#'
#' # Click to run code
#'
#' RStudio also supports a special link type that runs R code in the
#' current R session upon clicking.
#'
#' You can create these links with `.run`:
#'
#' ```{asciicast links-run}
#' cli::cli_text("Run {.run testthat::snapshot_review()} to review")
#' ```
#'
#' ## Link text
#'
#' Sometimes you want to show a slightly different expression in the link,
#' than the one that is evaluated. E.g. the evaluated expression probably
#' needs to qualify packages with `::`, but you might not want to show this:
#'
#' ```{asciicast links-run-2}
#' cli::cli_text(
#'   "Run {.run [snapshot_review()](testthat::snapshot_review())} to review"
#' )
#' ```
#'
#' ## Security considerations
#'
#' To make `.run` hyperlinks more secure, RStudio with not run code
#'
#' * that is not in the `pkg::fun(args)` form,
#' * if `args` contains `(`, `)` or `;`,
#' * if it calls a core package (base, stats, etc.),
#' * if it calls a package that is not loaded, and it is not one of
#'   testthat, devtools, usethis, or rlang, which are explicitly allowed.
#'
#' @name links
NULL
