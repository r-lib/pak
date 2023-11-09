
#' Read a DESCRIPTION file
#'
#' This is a convenience wrapper for `description$new()`.
#' Very often you want to read an existing `DESCRIPTION`
#' file, and to do this you can just supply the path to the file or its
#' directory to `desc()`.
#'
#' @param cmd A command to create a description from scratch.
#'   Currently only `"!new"` is implemented. If it does not start
#'   with an exclamation mark, it will be interpreted as the `file`
#'   argument.
#' @param file Name of the `DESCRIPTION` file to load. If all of
#'   `cmd`, `file` and `text` are `NULL` (the default), then the
#'   `DESCRIPTION` file in the current working  directory is used.
#'   The file can also be an R package (source, or
#'   binary), in which case the `DESCRIPTION` file is extracted from it, but
#'   note that in this case `$write()` cannot write the file back in
#'   the package archive.
#' @param text A character scalar containing the full DESCRIPTION.
#'   Character vectors are collapsed into a character scalar, with
#'   newline as the separator.
#' @param package If not NULL, then the name of an installed package
#'     and the DESCRIPTION file of this package will be loaded.
#'
#' @export
#' @examples
#' desc(package = "desc")
#' DESCRIPTION <- system.file("DESCRIPTION", package = "desc")
#' desc(DESCRIPTION)

desc <- function(cmd = NULL, file = NULL, text = NULL, package = NULL) {
  description$new(cmd, file, text, package)
}

#' Read, write, update, validate DESCRIPTION files
#'
#' @section Constructors:
#'
#' There are two ways of creating a description object. The first
#' is reading an already existing `DESCRIPTION` file; simply give
#' the name of the file as an argument. The default is `DESCRIPTION`:
#' ```r
#' x <- description$new()
#' x2 <- description$new("path/to/DESCRIPTION")
#' ```
#'
#' The second way is creating a description object from scratch,
#' supply `"!new"` as an argument to do this.
#' ```r
#' x3 <- description$new("!new")
#' ```
#'
#' The complete API reference:
#' ```r
#' description$new(cmd = NULL, file = NULL, text = NULL,
#'     package = NULL)
#' ```
#'
#' * `cmd`: a command to create a description from scratch.
#'   Currently only `"!new"` is implemented. If it does not start
#'   with an exclamation mark, it will be interpreted as a `file`
#'   argument.
#' * `file`: name of the `DESCRIPTION` file to load. If it is
#'   a directory, then we assume that it is inside an R package and
#'   conduct a search for the package root directory, i.e. the first
#'   directory up the tree that contains a `DESCRIPTION` file.
#'   If `cmd`, `file`, `text` and `package` are all `NULL` (the default),
#'   then the search is started from the working directory. The file can
#'   also be an R package (source, or binary), in which case the
#'   `DESCRIPTION` file is extracted from it, but note that in this case
#'   `$write()` cannot write the file back in the package archive.
#' * `text`: a character scalar containing the full DESCRIPTION.
#'   Character vectors are collapsed into a character scalar, with
#'   newline as the separator.
#' * `package`: if not NULL, then the name of an installed package
#'   and the DESCRIPTION file of this package will be loaded.
#'
#' @section Setting and Querying fields:
#' Set a field with `$set` and query it with `$get`:
#' ```r
#' x <- description$new("!new")
#' x$get("Package")
#' x$set("Package", "foobar")
#' x$set(Title = "Example Package for 'description'")
#' x$get("Package")
#' ```
#'
#' Note that `$set` has two forms. You can either give the field name
#' and new value as two arguments; or you can use a single named argument,
#' the argument name is the field name, the argument value is the field
#' value.
#'
#' The `$fields` method simply lists the fields in the object:
#' ```r
#' x$fields()
#' ```
#'
#' The `$has_fields` method checks if one or multiple fields are
#' present in a description object:
#' ```r
#' x$has_fields("Package")
#' x$has_fields(c("Title", "foobar"))
#' ```
#'
#' The `$del` method removes the specified fields:
#' ```r
#' x$set(foo = "bar")
#' x$del("foo")
#' ```
#'
#' `$get_field` is similar to `$get`, but it queries a single
#' field, it returns an unnamed vector if found, and returns the
#' specified `default` value if not. By default it throws an error
#' if the field is not found.
#'
#' The complete API reference:
#' ```r
#' description$get(keys)
#' description$get_field(key, default, trim_ws = TRUE, squish_ws = trim_ws)
#' description$set(...)
#' description$fields()
#' description$has_fields(keys)
#' description$del(keys)
#' ```
#'
#'  * `key`: a character string (length one), the key to query.
#'  * `default`: If specified and `key` is missing, this value
#'     is returned. If not specified, an error is thrown.
#'  * `trim_ws`: whether to trim leading and trailing whitespace
#'     from the returned value.
#'  * `squish_ws`: whether to reduce repeated whitespace in the
#'     returned value.
#'  * `keys`: a character vector of keys to query, check or delete.
#'  * `...`: this must be either two unnamed arguments, the key and
#'     and the value to set; or an arbitrary number of named arguments,
#'     names are used as keys, values as values to set.
#'
#' @section Normalizing:
#' Format DESCRIPTION in a standard way. `$str` formats each
#' field in a standard way and returns them (it does not change the
#' object itself), `$print` is used to print it to the
#' screen. The `$normalize` function normalizes each field (i.e.
#' it changes the object). Normalization means reformatting the fields,
#' via `{$reformat_fields()` and also reordering them via
#' `$reorder_fields()`. The format of the various fields is
#' opinionated and you might like it or not. Note that `desc` only
#' re-formats fields that it updates, and only on demand, so if your
#' formatting preferences differ, you can still manually edit
#' `DESCRIPTION` and `desc` will respect your edits.
#'
#' ```r
#' description$str(by_field = FALSE, normalize = TRUE,
#'     mode = c("file", "screen"))
#' description$normalize()
#' description$reformat_fields()
#' description$reorder_fields()
#' description$print()
#'```
#'
#' * `by_field`: whether to return the normalized format
#'     by field, or collapsed into a character scalar.
#' * `normalize`: whether to reorder and reformat the fields.
#' * `mode`: `file` mode formats the fields as they are
#'     written to a file with the `write` method. `screen`
#'     mode adds extra markup to some fields, e.g. formats the
#'     `Authors@R` field in a readable way.
#'
#' @section Writing it to file:
#' The `$write` method writes the description to a file.
#' By default it writes it to the file it was created from, if it was
#' created from a file. Otherwise giving a file name is compulsory:
#' ```r
#' x$write(file = "DESCRIPTION")
#' ```
#'
#' The API:
#' ```r
#' description$write(file = NULL)
#' ```
#'
#' * `file`: path to write the description to. If it was created
#'      from a file in the first place, then it is written to the same
#'      file. Otherwise this argument must be specified.
#'
#' @section Version numbers:
#'
#' ```r
#' description$get_version()
#' description$set_version(version)
#' description$bump_version(which = c("patch", "minor", "major", "dev"))
#' ```
#'
#' * `version`: a string or a [base::package_version] object.
#' * `which`: which component of the version number to increase.
#'     See details just below.
#'
#' These functions are simple helpers to make it easier to query, set and
#' increase the version number of a package.
#'
#' `$get_version()` returns the version number as a
#' [base::package_version] object. It throws an error if the
#' package does not have a `Version` field.
#'
#' `$set_version()` takes a string or a [base::package_version] object and
#' sets the `Version` field to it.
#'
#' `$bump_version()` increases the version number. The `which`
#' parameter specifies which component to increase.
#' It can be a string referring to a component: `major`,
#' `minor`, `patch` or `dev`, or an integer
#' scalar, for the latter components are counted from one, and the
#' beginning. I.e. component one is equivalent to `major`.
#'
#' If a component is bumped, then the ones after it are zeroed out.
#' Trailing zero components are omitted from the new version number,
#' but if the old version number had at least two or three components, then
#' the one will also have two or three.
#'
#' The bumping of the `dev` version (the fourth component) is
#' special: if the original version number had less than four components,
#' and the `dev` version is bumped, then it is set to `9000`
#' instead of `1`. This is a convention often used by R developers,
#' it was originally invented by Winston Chang.
#'
#' Both `$set_version()` and `$bump_version()` use dots to
#' separate the version number components.
#'
#' @section Dependencies:
#' These functions handle the fields that define how the R package
#' uses another R packages. See [dep_types] for the
#' list of fields in this group.
#'
#' The `$get_deps` method returns all declared dependencies, in a
#' data frame with columns: `type`, `package` and `version`.
#' `type` is the name of the dependency field, `package` is the
#' name of the R package, and `version` is the required version. If
#' no specific versions are required, then this is a `"*"`.
#'
#' The `$set_deps` method is the opposite of `$get_deps` and
#' it sets all dependencies. The input is a data frame, with the same
#' structure as the return value of `$get_deps`.
#'
#' The `$has_dep` method checks if a package is included in the
#' dependencies. It returns a logical scalar. If `type` is not
#' `any`, then it has to match as well.
#'
#' The `$del_deps` method removes all declared dependencies.
#'
#' The `$set_dep` method adds or updates a single dependency. By
#' default it adds the package to the `Imports` field.
#'
#' The API:
#' ```r
#' description$set_dep(package, type = dep_types, version = "*")
#' description$set_deps(deps)
#' description$get_deps()
#' description$has_dep(package, type = c("any", dep_types))
#' description$del_dep(package, type = c("all", dep_types))
#' description$del_deps()
#' ```
#'
#' * `package`: name of the package to add to or remove from the
#'     dependencies.
#' * `type`: dependency type, see [dep_types]. For
#'     `$del_dep` it may also be `"all"`, and then the package
#'     will be deleted from all dependency types.
#' * `version`: required version. Defaults to `"*"`, which means
#'     no explicit version requirements.
#' * `deps`: a data frame with columns `type`, `package` and
#'     `version`. `$get_deps` returns the same format.
#'
#' @section Collate fields:
#' Collate fields contain lists of file names with R source code,
#' and the package has a separate API for them. In brief, you can
#' use `$add_to_collate` to add one or more files to the main or
#' other collate field. You can use `$del_from_collate` to remove
#' it from there.
#'
#' The API:
#' ```r
#' description$set_collate(files, which = c("main", "windows", "unix"))
#' description$get_collate(which = c("main", "windows", "unix"))
#' description$del_collate(which = c("all", "main", "windows", "unix"))
#' description$add_to_collate(files, which = c("default", "all", "main",
#'   "windows", "unix"))
#' description$del_from_collate(files, which = c("all", "main",
#'   "windows", "unix"))
#' ```
#'
#' * `iles`: the files to add or remove, in a character vector.
#' * `which: which collate field to manipulate. `"default"` for
#'   `$add_to_collate` means all existing collate fields, or the
#'   main one if none exist.
#'
#' @section Authors:
#' There is a specialized API for the `Authors@R` field,
#' to add and remove authors, update their roles, change the maintainer,
#' etc.
#'
#' The API:
#' ```r
#' description$get_authors()
#' description$set_authors(authors)
#' description$get_author(role)
#' description$get_maintainer()
#' description$coerce_authors_at_r()
#' ```
#'
#' * `authors`: a `person` object, a list of authors.
#' * `role`: The role to query. See `person` for details.
#'
#' `$get_authors` returns a `person` object, the parsed
#' authors. See [utils::person()] for details.
#'
#' `$get_author` returns a `person` object, all authors with
#' the specified role.
#'
#' `$get_maintainer` returns the maintainer of the package. It works
#' with `Authors@R` fields and with traditional `Maintainer`
#' fields as well.
#'
#' `$coerce_authors_at_r` converts an `Author` field to one with
#' a `person` object. This coercion may be necessary for other
#' functions such as `$get_authors`.
#'
#' ```r
#' description$add_author(given = NULL, family = NULL, email = NULL,
#'     role = NULL, comment = NULL, orcid = NULL)
#' description$add_me(role = "ctb", comment = NULL, orcid = NULL)
#' description$add_author_gh(username, role = "ctb", comment = NULL, orcid = NULL)
#' ```
#'
#' Add a new author. The arguments correspond to the arguments of the
#' [utils::person()] function. `add_me` is a convenience
#' function, it adds the current user as an author, and it needs the
#' `whoami` package to be installed. It'll add your ORCID ID
#' if you provide it as argument or save it as `ORCID_ID` environment
#' variable in .Renviron.
#' The full name is parsed by `add_me` and `add_author_gh` using
#' `as.person` and collapsing the given name and the family name
#' in order to e.g. have the first and middle names together as given
#' name. This approach might be limited to some full name structures.
#'
#' ```r
#' description$del_author(given = NULL, family = NULL, email = NULL,
#'     role = NULL, comment = NULL, orcid = NULL)
#' ```
#'
#' Remove an author, or multiple authors. The author(s) to be removed
#' can be specified via any field(s). All authors matching all
#' specifications will be removed. E.g. if only `given = "Joe"`
#' is supplied, then all authors whole given name matches `Joe` will
#' be removed. The specifications can be (PCRE) regular expressions.
#'
#' ```r
#' description$add_role(role, given = NULL, family = NULL, email = NULL,
#'     comment = NULL, orcid = NULL)
#' description$add_orcid(orcid, given = NULL, family = NULL, email = NULL,
#'     comment = NULL, role = NULL)
#' description$del_role(role, given = NULL, family = NULL, email = NULL,
#'     comment = NULL, orcid = NULL)
#' description$change_maintainer(given = NULL, family = NULL,
#'     email = NULL, comment = NULL, orcid = NULL)
#' ```
#'
#' `role` is the role to add or delete. The other arguments
#' are used to select a subset of the authors, on which the operation
#' is performed, similarly to `$del_author`.
#'
#' @section URLs:
#'
#' We provide helper functions for manipulating URLs in the `URL`
#' field:
#'
#' ```r
#' description$get_urls()
#' description$set_urls(urls)
#' description$add_urls(urls)
#' description$del_urls(pattern)
#' description$clear_urls()
#' ```
#'
#' * `urls`: character vector of URLs to set or add.
#' * `pattern`: Perl compatible regular expression to specify the
#'     URLs to be removed.
#'
#' `$get_urls()` returns all urls in a character vector. If no URL
#' fields are present, a zero length vector is returned.
#'
#' `$set_urls()` sets the URL field to the URLs specified in the
#' character vector argument.
#'
#' `$add_urls()` appends the specified URLs to the URL field. It
#' creates the field if it does not exists. Duplicate URLs are removed.
#'
#' `$del_urls()` deletes the URLs that match the specified pattern.
#'
#' `$clear_urls()` deletes all URLs.
#'
#' @section Remotes:
#'
#' `devtools`, `remotes` and some other packages support the
#' non-standard `Remotes` field in `DESCRIPTION`. This field
#' can be used to specify locations of dependent packages: GitHub or
#' BitBucket repositories, generic git repositories, etc. Please see the
#' `Package remotes` vignette in the `devtools` package.
#'
#' `desc` has helper functions for manipulating the `Remotes`
#' field:
#'
#' ```r
#' description$get_remotes()
#' description$get_remotes()
#' description$set_remotes(remotes)
#' description$add_remotes(remotes)
#' description$del_remotes(pattern)
#' description$clear_remotes()
#' ```
#'
#' * `remotes`: character vector of remote dependency locations to
#'     set or add.
#' * `pattern`: Perl compatible regular expression to specify the
#'     remote dependency locations to remove.
#'
#' `$get_remotes()` returns all remotes in a character vector.
#' If no URL fields are present, a zero length vector is returned.
#'
#' `$set_remotes()` sets the URL field to the Remotes specified in the
#' character vector argument.
#'
#' `$add_remotes()` appends the specified remotes to the
#' `Remotes` field. It creates the field if it does not exists.
#' Duplicate remotes are removed.
#'
#' `$del_remotes()` deletes the remotes that match the specified
#' pattern.
#'
#' `$clear_remotes()` deletes all remotes.
#'
#' @section Built:
#'
#' The `Built` field is used in binary packages to store information
#' about when and how a binary package was built.
#'
#' `$get_built()` returns the built information as a list with fields
#' `R`, `Platform`, `Date`, `OStype`. It throws an
#' error if the package does not have a `Built` field.
#'
#' @section Encodings:
#' When creating a `description` object, `desc` observes the `Encoding`
#' field, if present, and uses the specified encoding to parse the file.
#' Internally, it converts all fields to UTF-8.
#'
#' When writing a `description` object to a file, `desc` uses the
#' `Encoding` field (if present), and converts all fields to the specified
#' encoding.
#'
#' We suggest that whenever you need to use non-ASCII characters in your
#' package, you use the UTF-8 encoding, for maximum portability.
#'
#' @export
#' @importFrom R6 R6Class
#' @docType class
#' @format An R6 class.
#'
#' @examples
#' ## Create a template
#' desc <- description$new("!new")
#' desc
#'
#' ## Read a file
#' desc2 <- description$new(file = system.file("DESCRIPTION",
#'                            package = "desc"))
#' desc2
#'
#' ## Remove a field
#' desc2$del("LazyData")
#'
#' ## Add another one
#' desc2$set(VignetteBuilder = "knitr")
#' desc2$get("VignetteBuilder")
#' desc2

description <- R6Class("description",
  public = list(

    ## Either from a file, or from a character vector
    initialize = function(cmd = NULL, file = NULL, text = NULL, package = NULL)
      idesc_create(self, private, cmd, file, text, package),

    write = function(file = NULL)
      idesc_write(self, private, file),

    fields = function()
      idesc_fields(self, private),

    has_fields = function(keys)
      idesc_has_fields(self, private, keys),

    get = function(keys)
      idesc_get(self, private, keys),

    get_field = function(key, default = stop("Field '", key, "' not found"),
                         trim_ws = TRUE, squish_ws = trim_ws)
      idesc_get_field(self, private, key, default, trim_ws, squish_ws),

    get_or_fail = function(keys)
      idesc_get_or_fail(self, private, keys),

    get_list = function(key, default = stop("Field '", key, "' not found"),
                        sep = ",", trim_ws = TRUE, squish_ws = trim_ws)
      idesc_get_list(self, private, key, default, sep, trim_ws, squish_ws),

    set = function(...)
      idesc_set(self, private, ...),

    set_list = function(key, list_value, sep = ", ")
      idesc_set_list(self, private, key, list_value, sep),

    del = function(keys)
      idesc_del(self, private, keys),

    validate = function()
      idesc_validate(self, private),

    print = function()
      idesc_print(self, private),

    str = function(by_field = FALSE, normalize = TRUE,
      mode = c("file", "screen"))
      idesc_str(self, private, by_field, normalize, mode),

    to_latex = function()
      idesc_to_latex(self, private),

    normalize = function()
      idesc_normalize(self, private),

    reformat_fields = function()
      idesc_reformat_fields(self, private),

    reorder_fields = function()
      idesc_reorder_fields(self, private),

    ## -----------------------------------------------------------------
    ## Version numbers

    get_version = function()
      idesc_get_version(self, private),

    set_version = function(version)
      idesc_set_version(self, private, version),

    bump_version = function(which)
      idesc_bump_version(self, private, which),

    ## -----------------------------------------------------------------
    ## Package dependencies

    set_dep = function(package, type = desc::dep_types, version = "*")
      idesc_set_dep(self, private, package, match.arg(type), version),

    set_deps = function(deps)
      idesc_set_deps(self, private, deps),

    get_deps = function()
      idesc_get_deps(self, private),

    del_dep = function(package, type = c("all", desc::dep_types))
      idesc_del_dep(self, private, package, match.arg(type)),

    del_deps = function()
      idesc_del_deps(self, private),

    has_dep = function(package, type = c("any", desc::dep_types))
      idesc_has_dep(self, private, package, match.arg(type)),

    ## -----------------------------------------------------------------
    ## Collate fields

    set_collate = function(files, which = c("main", "windows", "unix"))
      idesc_set_collate(self, private, files, match.arg(which)),

    get_collate = function(which = c("main", "windows", "unix"))
      idesc_get_collate(self, private, match.arg(which)),

    del_collate = function(which = c("all", "main", "windows", "unix"))
      idesc_del_collate(self, private, match.arg(which)),

    add_to_collate = function(files,
      which = c("default", "all", "main", "windows", "unix"))
      idesc_add_to_collate(self, private, files, match.arg(which)),

    del_from_collate = function(files,
      which = c("all", "main", "windows", "unix"))
      idesc_del_from_collate(self, private, files, match.arg(which)),

    ## -----------------------------------------------------------------
    ## Authors@R

    get_authors = function()
      idesc_get_authors(self, private),

    get_author = function(role = "cre")
      idesc_get_author(self, private, role),

    set_authors = function(authors)
      idesc_set_authors(self, private, authors),

    add_author = function(given = NULL, family = NULL, email = NULL,
                          role = NULL, comment = NULL, orcid = NULL)
      idesc_add_author(self, private, given, family, email, role, comment,
                       orcid),

    add_role = function(role, given = NULL, family = NULL, email = NULL,
                        comment = NULL, orcid = NULL)
      idesc_add_role(self, private, role, given, family, email, comment,
                     orcid),

    add_orcid = function(orcid, given = NULL, family = NULL, email = NULL,
                        comment = NULL, role = NULL)
      idesc_add_orcid(self, private, role = role, given = given, family = family,
                     email = email, comment = comment,
                     orcid = orcid),

    del_author = function(given = NULL, family = NULL, email = NULL,
                          role = NULL, comment = NULL, orcid = NULL)
      idesc_del_author(self, private, given, family, email, role, comment,
                       orcid),

    del_role = function(role, given = NULL, family = NULL, email = NULL,
                        comment = NULL, orcid = NULL)
      idesc_del_role(self, private, role, given, family, email, comment,
                     orcid),

    change_maintainer = function(given = NULL, family = NULL, email = NULL,
                                 comment = NULL, orcid = NULL)
      idesc_change_maintainer(self, private, given, family, email, comment,
                              orcid),

    add_me = function(role = "ctb", comment = NULL, orcid = NULL)
      idesc_add_me(self, private, role, comment, orcid),

    add_author_gh = function(username, role = "ctb", comment = NULL, orcid = NULL)
      idesc_add_author_gh(self, private, role = role,
                   username = username,
                   comment = comment, orcid = orcid),

    get_maintainer = function()
      idesc_get_maintainer(self, private),

    coerce_authors_at_r = function()
      idesc_coerce_authors_at_r(self, private),

    ## -----------------------------------------------------------------
    ## URL

    get_urls = function()
      idesc_get_urls(self, private),

    set_urls = function(urls)
      idesc_set_urls(self, private, urls),

    add_urls = function(urls)
      idesc_add_urls(self, private, urls),

    del_urls = function(pattern)
      idesc_del_urls(self, private, pattern),

    clear_urls = function()
      idesc_clear_urls(self, private),

    ## -----------------------------------------------------------------
    ## Remotes

    get_remotes = function()
      idesc_get_remotes(self, private),

    set_remotes = function(remotes)
      idesc_set_remotes(self, private, remotes),

    add_remotes = function(remotes)
      idesc_add_remotes(self, private, remotes),

    del_remotes = function(pattern)
      idesc_del_remotes(self, private, pattern),

    clear_remotes = function()
      idesc_clear_remotes(self, private),

    ## -----------------------------------------------------------------
    ## Built

    get_built = function()
      idesc_get_built(self, private)
  ),

  private = list(
    data = NULL,
    path = NULL,
    notws = character()                   # entries without trailing ws
  )
)

idesc_create <- function(self, private, cmd, file, text, package) {

  if (!is.null(cmd) && substring(cmd, 1, 1) != "!") {
    file <- cmd
    cmd <- NULL
  }

  if (!is.null(cmd)) {
    if (!is.null(file)) warning("file argument ignored")
    if (!is.null(text)) warning("text argument ignored")
    if (!is.null(package)) warning("package argument ignored")
    idesc_create_cmd(self, private, cmd)

  } else if (is.null(cmd) && is.null(file) && is.null(text) &&
             is.null(package)) {
    idesc_create_file(self, private, ".")

  } else if (!is.null(file)) {
    if (!is.null(text)) warning("text argument ignored")
    if (!is.null(package)) warning("package argument ignored")
    idesc_create_file(self, private, file)

  } else if (!is.null(text)) {
    if (!is.null(package)) warning("package argument ignored")
    idesc_create_text(self, private, text)

  } else {
    idesc_create_package(self, private, package)
  }

  invisible(self)
}

idesc_create_cmd <- function(self, private, cmd = c("new")) {
  stopifnot(is_constructor_cmd(cmd))

  if (cmd == "!new") {
    txt <-
'Package: {{ Package }}
Title: {{ Title }}
Version: 1.0.0
Authors@R:
    c(person(given = "Jo", family = "Doe", email = "jodoe@dom.ain",
      role = c("aut", "cre")))
Maintainer: {{ Maintainer }}
Description: {{ Description }}
License: {{ License }}
URL: {{ URL }}
BugReports: {{ BugReports }}
Encoding: UTF-8
'
    txt <- sub("Authors@R:", "Authors@R: ", txt)
    idesc_create_text(self, private, text = txt)
  }

  invisible(self)
}

idesc_create_file <- function(self, private, file) {
  stopifnot(is_path(file))

  if (file.exists(file) && is_dir(file)) file <- find_description(file)
  stopifnot(is_existing_file(file))

  if (is_package_archive(file)) {
    file <- get_description_from_package(file)

  } else {
    private$path <- normalizePath(file)
  }

  tryCatch(
    lines <- readLines(file),
    error = function(e) stop("Cannot read ", file, ": ", e$message)
  )

  idesc_create_text(self, private, lines)
}

idesc_create_text <- function(self, private, text) {
  stopifnot(is.character(text))
  con <- textConnection(text, local = TRUE, encoding = "bytes")
  on.exit(close(con), add = TRUE)
  dcf <- read_dcf(con)
  private$notws <- dcf$notws
  private$data <- dcf$dcf
  check_encoding(self, private, NULL)
}

idesc_create_package <- function(self, private, package) {
  stopifnot(is_string(package))
  path <- system.file(package = package, "DESCRIPTION")
  if (path == "") {
    stop("Cannot find DESCRIPTION for installed package ", package)
  }
  idesc_create_file(self, private, path)
}

idesc_write <- function(self, private, file) {
  if (is.null(file)) file <- private$path
  if (is.null(file)) {
    stop("Cannot write back DESCRIPTION. Note that it is not possible
          to update DESCRIPTION files within package archives")
  }

  mat <- idesc_as_matrix(private$data)

  ## Need to write to a temp file first, to preserve absense of trailing ws,
  ## and also because recently write.dcf cannot write out files in unknown
  ## encodings. So we write the file in UTF-8 and then re-encode it later.
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  ## Need to tell older R not to mess with the encoding
  ## Cannot do this for newer R, but for newer R we use useBytes = TRUE
  ## in write_dcf()
  if (getRversion() < "4.2.0") Encoding(mat) <- "unknown"
  write_dcf(mat, file = tmp, keep.white = names(private$data))

  removed <- ! names(private$notws) %in% colnames(mat)
  if (any(removed)) private$notws <- private$notws[! removed]

  postprocess_trailing_ws(tmp, names(private$notws))
  if (file.exists(file) && is_dir(file)) file <- find_description(file)

  ofile <- file(file, raw = TRUE, open = "wb+")
  on.exit(close(ofile), add = TRUE)

  lines <- readLines(tmp)
  if ("Encoding" %in% colnames(mat)) {
    encoding <- mat[, "Encoding"]
    lines <- iconv(lines, from = "UTF-8", to = encoding)
  }
  writeLines(lines, ofile, useBytes = TRUE)

  invisible(self)
}

idesc_fields <- function(self, private) {
  names(private$data)
}

idesc_has_fields <- function(self, private, keys) {
  stopifnot(is.character(keys), has_no_na(keys))
  keys %in% self$fields()
}

idesc_as_matrix <- function(data) {
  matrix(
    vapply(data, "[[", "", "value"),
    nrow = 1,
    dimnames = list(NULL, names(data))
  )
}

idesc_get <- function(self, private, keys) {
  stopifnot(is.character(keys), has_no_na(keys))
  res <- lapply(private$data[keys], "[[", "value")
  res[vapply(res, is.null, logical(1))] <- NA_character_
  res <- as.character(unlist(res))
  names(res) <- keys
  res
}

idesc_get_field <- function(self, private, key, default, trim_ws, squish_ws) {
  stopifnot(is_string(key))
  stopifnot(is_flag(trim_ws))
  val <- private$data[[key]]$value
  if (!is.null(val)) {
    if (trim_ws) val <- str_trim(val)
    if (squish_ws) val <- str_squish(val)
  }
  val %||% default
}

idesc_get_or_fail <- function(self, private, keys) {
  stopifnot(is.character(keys), has_no_na(keys))
  res <- self$get(keys)
  if (any(is.na(res))) {
    w <- is.na(res)
    msg <- paste0(
      "Could not find DESCRIPTION ",
      if (sum(w) == 1) "field: " else "fields: ",
      paste(sQuote(keys[w]), collapse = ", "),
      "."
    )
    stop(msg, call. = FALSE)
  }
  res
}

idesc_get_list <- function(self, private, key, default, sep, trim_ws, squish_ws) {
  stopifnot(is_string(key), is_flag(trim_ws), is_flag(squish_ws))
  val <- private$data[[key]]$value %||% default
  val <- strsplit(val, sep, fixed = TRUE)[[1]]
  if (trim_ws) val <- str_trim(val)
  if (squish_ws) val <- str_squish(val)
  val
}

## ... are either
## - two unnamed arguments, key and value, or
## - an arbitrary number of named arguments, the names are the keys,
##   the values are the values

idesc_set <- function(self, private, ...) {
  args <- list(...)

  if (is.null(names(args)) && length(args) == 2) {
    keys <- as_string(args[[1]])
    values <- as_string(args[[2]])

  } else if (!is.null(names(args)) && all(names(args) != "")) {
    keys <- names(args)
    values <- unlist(args)

  } else {
    stop("$set needs two unnamed args, or all named args, see docs")
  }

  fields <- create_fields(keys, enc2utf8(values))
  lapply(fields, check_field, warn = TRUE)
  check_encoding(self, private, lapply(fields, "[[", "value"))
  private$data[keys] <- fields

  invisible(self)
}

idesc_set_list <- function(self, private, key, list_value, sep) {
  stopifnot(is_string(key), is.character(list_value))
  value <- paste(list_value, collapse = sep)
  idesc_set(self, private, key, value)
}

idesc_del <- function(self, private, keys) {
  stopifnot(is.character(keys), has_no_na(keys))
  private$data <- private$data[setdiff(names(private$data), keys)]
  invisible(self)
}
