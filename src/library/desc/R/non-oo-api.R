
# No coverage calculating here, since this code
# runs during install time only.
# nocov start

#' @include description.R
#' @importFrom utils packageName

generate_api <- function(member, self = TRUE, norm = TRUE,
                         invisible = FALSE) {

  res <- function() { }

  func <- description$public_methods[[member]]

  ## Arguments
  xargs <- list(file = ".")
  if (self && norm) xargs <- c(xargs, list(normalize = FALSE))
  formals(res) <- c(formals(func), xargs)

  ## Call to member function
  member_call <- substitute(
    desc[[`_member`]](),
    list(`_member` = member)
  )
  argnames <- names(formals(func))
  dargs <- structure(lapply(argnames, as.name), names = argnames)
  if (!is.null(formals(func))) {
    member_call[1 + 1:length(formals(func))] <- dargs
    argnames2 <- c("", ifelse(argnames == "...", "", argnames))
    names(member_call) <- argnames2
  }
  desc_call <- substitute(
    result <- `_member`,
    list(`_member` = member_call)
  )

  ## Call to write, or just return the result
  write_call <- if (self && norm) {
    quote({
      if (normalize) desc$normalize()
      desc$write(file = file)
    })
  } else if (self) {
    quote(desc$write(file = file))
  }

  ## Put together

  body(res) <- substitute(
    { `_read`; `_trans`; `_write`; `_return` },
    list(
      `_read` = quote(desc <- description$new(file = file)),
      `_trans` = desc_call,
      `_write` = write_call,
      `_return` = if (invisible) quote(invisible(result)) else quote(result)
    )
  )

  environment(res) <- asNamespace(packageName())

  res
}

## -------------------------------------------------------------------

#' List all fields in a DESCRIPTION file
#'
#' @inheritParams desc_set
#' @return Character vector of fields.
#'
#' @family simple queries
#' @export

desc_fields <- generate_api("fields", self = FALSE)

#' Check if some fields are present in a DESCRIPTION file
#'
#' @param keys Character vector of keys to check.
#' @inheritParams desc_set
#' @return Logical vector.
#'
#' @family simple queries
#' @export

desc_has_fields <- generate_api("has_fields", self = FALSE)

#' Get a field from a DESCRIPTION file
#'
#' @param keys Character vector of fields to get.
#' @inheritParams desc_set
#' @return Character vector, values of the specified keys.
#'   Non-existing keys return `NA`.
#'
#' @family simple queries
#' @export

desc_get <- generate_api("get", self = FALSE)

#' Get a single field from a DESCRIPTION file, fail if not found
#'
#' `desc_get_list()` parses a comma separated list into a character
#' vector.
#'
#' @inheritParams desc_get
#' @param key The field to query.
#' @param default Value to return if `key` is not found.
#'   By default it throws an error.
#' @param trim_ws Whether to trim leading and trailing whitespace
#'   from the value. Defaults to `TRUE`.
#' @param squish_ws Whether to reduce repeated whitespace in the value.
#'   Defaults to `trim_ws`.
#' @param sep Separator string for [desc_get_list()].
#' @return Character string, the value of `key`, or `default`
#'   if `key` is not found and `default` is specified.
#'
#' @family simple queries
#' @export

desc_get_field <- generate_api("get_field", self = FALSE)

#' @rdname desc_get_field
#' @export

desc_get_or_fail <- generate_api("get_or_fail", self = FALSE)

#' @rdname desc_get_field
#' @export

desc_get_list <- generate_api("get_list", self = FALSE)

#' Set one or more fields in a DESCRIPTION file
#'
#' @details
#' `desc_set()` supports two forms, the first is two unnamed
#' arguments: the key and its value to set.
#'
#' The second form requires named arguments: names are used as keys
#' and values as values to set.
#'
#' `desc_set_list()` collapses a character vector into string,
#' separating the elements by commas.
#'
#' @param ... Values to set, see details below.
#' @param check Whether to check the validity of the new fields.
#' @param file DESCRIPTION file to use. By default the DESCRIPTION
#'    file of the current package (i.e. the package the working directory
#'    is part of) is used.
#' @param normalize Whether to "normalize" (reorder and reformat) the fields when writing back
#'   the result. See [desc_normalize()].
#' @param key Key to set in `desc_set_list()`.
#' @param list_value Character vector, to collapse in
#'   `desc_set_list()`.
#' @param sep Separator string for `desc_set_list()` list fields.
#'
#' @family simple queries
#' @export

desc_set <- generate_api("set")

#' @rdname desc_set
#' @export

desc_set_list <- generate_api("set_list")

#' Remove fields from a DESCRIPTION file
#'
#' @param keys Character vector of keys to remove.
#' @inheritParams desc_set
#'
#' @family simple queries
#' @export

desc_del <- generate_api("del")

## -------------------------------------------------------------------

#' Print the contents of a DESCRIPTION file to the screen
#'
#' @inheritParams desc_set
#' @export

desc_print <- generate_api("print", self = FALSE, invisible = TRUE)

#' Converts a DESCRIPTION file to LaTeX
#'
#' Returns the contents of the DESCRIPTION in LaTeX format.
#'
#' @inheritParams desc_set
#' @export

desc_to_latex <- generate_api("to_latex", self = FALSE)

#' Normalize a DESCRIPTION file
#'
#' Re-formats and re-orders fields in DESCRIPTION in a standard way.
#' Reorders the packages alphabetically.
#'
#' @inheritParams desc_set
#' @family repair functions
#' @export

desc_normalize <- generate_api("normalize", self = TRUE, norm = FALSE)

#' Reformat fields in a DESCRIPTION file
#'
#' Reformat the fields in DESCRIPTION in a standard way.
#'
#' @inheritParams desc_set
#' @family repair functions
#' @export

desc_reformat_fields <- generate_api("reformat_fields", self = TRUE, norm = FALSE)

#' Reorder fields in a DESCRIPTION file
#'
#' Reorder the fields in DESCRIPTION in a standard way.
#'
#' @inheritParams desc_set
#' @family repair functions
#' @export

desc_reorder_fields <- generate_api("reorder_fields", self = TRUE, norm = FALSE)

#' Validate a DESCRIPTION file
#'
#' This function is not implemented yet.
#'
#' @inheritParams desc_set
#'
#' @export

desc_validate <- generate_api("validate", self = FALSE)

## -------------------------------------------------------------------

#' List all package dependencies from a DESCRIPTION file
#'
#' @inheritParams desc_set
#' @return Data frame with columns: `type` (dependency type),
#'   `package`, and `version`. For non-versioned dependencies
#'   `version` is `*`.
#'
#' @family dependencies
#' @export

desc_get_deps <- generate_api("get_deps", self = FALSE)

#' Add a package dependency to a DESCRIPTION file
#'
#' @param package Package to depend on.
#' @param type Dependency type.
#' @param version Version to depend on, for versioned dependencies.
#' @inheritParams desc_set
#'
#' @family dependencies
#' @export

desc_set_dep <- generate_api("set_dep")

#' Set all package dependencies in DESCRIPTION
#'
#' @param deps Package dependency data frame, in the same format
#'    returned by [desc_get_deps()].
#' @inheritParams desc_set
#'
#' @family dependencies
#' @export

desc_set_deps <- generate_api("set_deps")

#' Remove a package dependency from DESCRIPTION
#'
#' @param package Package dependency to remove.
#' @param type Dependency type to remove. Sometimes a package is depended
#'   on via multiple dependency types, e.g. `LinkingTo` and
#'   `Imports`. Defaults to all types.
#' @inheritParams desc_set
#'
#' @family dependencies
#' @export

desc_del_dep <- generate_api("del_dep")

#' Remove all dependencies from DESCRIPTION
#'
#' @inheritParams desc_set
#'
#' @family dependencies
#' @export

desc_del_deps <- generate_api("del_deps")

#' Check for a dependency
#'
#' @inheritParams desc_set
#' @param package The package name.
#' @param type A dependency type or ``any`.
#' @return A logical scalar.
#'
#' @family dependencies
#' @export

desc_has_dep <- generate_api("has_dep", self = FALSE)

## -------------------------------------------------------------------

#' Set the Collate field in DESCRIPTION
#'
#' @param files Collate field to set, as a character vector.
#' @param which Which collate field to use. Collate fields can
#'   be operating system type specific.
#' @inheritParams desc_set
#'
#' @family Collate field
#' @export

desc_set_collate <- generate_api("set_collate")

#' Query the Collate field in DESCRIPTION
#'
#' @inheritParams desc_set_collate
#' @return Character vector of file names.
#'
#' @family Collate field
#' @export

desc_get_collate <- generate_api("get_collate", self = FALSE)

#' Delete the Collate field from DESCRIPTION
#'
#' @inheritParams desc_set_collate
#'
#' @family Collate field
#' @export

desc_del_collate <- generate_api("del_collate")

#' Add one or more files to the Collate field, in DESCRIPTION
#'
#' @param files Character vector, files to add.
#' @inheritParams desc_set_collate
#'
#' @family Collate field
#' @export

desc_add_to_collate <- generate_api("add_to_collate")

#' Remove files from the Collate field.
#'
#' @param files Files to remove from the Collate field.
#' @inheritParams desc_set_collate
#'
#' @family Collate field
#' @export

desc_del_from_collate <- generate_api("del_from_collate")

## -------------------------------------------------------------------

#' Query all authors in Authors@R, in DESCRIPTION
#'
#' @inheritParams desc_set
#' @return A person object, see [utils::person()].
#'
#' @family Authors@R
#' @export

desc_get_authors <- generate_api("get_authors", self = FALSE)

#' Query authors by role in Authors@R, in DESCRIPTION
#'
#' @param role Role to query. Defaults to the package maintainer.
#' @inheritParams desc_set
#' @return A person object, see [utils::person()].
#'
#' @family Authors@R
#' @export

desc_get_author <- generate_api("get_author", self = FALSE)

#' Set authors in Authors@R, in DESCRIPTION
#'
#' @param authors Authors, to set, a person object, see [utils::person()].
#' @inheritParams desc_set
#'
#' @family Authors@R
#' @export

desc_set_authors <- generate_api("set_authors")

#' Add an author to Authors@R in DESCRIPTION
#'
#' @param given Given name.
#' @param family Family name.
#' @param email Email address.
#' @param role Role.
#' @param comment Comment.
#' @param orcid ORCID.
#' @inheritParams desc_set
#'
#' @family Authors@R
#' @export

desc_add_author <- generate_api("add_author")

#' @title Add a role to one or more authors in Authors@R, in DESCRIPTION
#'
#' @description The author(s) can be specified by a combination of the `given`,
#' `family`, `email`, `comment` and `orcid` fields.
#' If multiple filters are specified, then all must match
#' to identify the author(s).
#'
#' @param role Role to add.
#' @param given Given name to filter on. Regular expression.
#' @param family Family name to filter on. Regular expression.
#' @param email Email address to filter on. Regular expression.
#' @param comment Comment field to filter on. Regular expression.
#' @param orcid ORCID field to filter on.
#' @inheritParams desc_set
#'
#' @family Authors@R
#' @export

desc_add_role <- generate_api("add_role")

#' @title Add an ORCID to one or more authors in Authors@R, in DESCRIPTION
#'
#' @description The author(s) can be specified by a combination of the `given`,
#' `family`, `email`, `comment` and `role` fields.
#' If multiple filters are specified, then all must match
#' to identify the author(s).
#'
#' @param orcid orcid to add.
#' @param given Given name to filter on. Regular expression.
#' @param family Family name to filter on. Regular expression.
#' @param email Email address to filter on. Regular expression.
#' @param comment Comment field to filter on. Regular expression.
#' @param role Role field to filter on.
#' @inheritParams desc_set
#'
#' @family Authors@R
#' @export

desc_add_orcid <- generate_api("add_orcid")

#' Remove one or more authors from DESCRIPTION.
#'
#' It uses the Authors@R field. The author(s) to be removed
#' can be specified via any field(s). All authors matching all
#' specifications will be removed. E.g. if only `given = "Joe"`
#' is supplied, then all authors whole given name matches `Joe` will
#' be removed. The specifications can be (PCRE) regular expressions.
#'
#' @param role Role to filter on. Regular expression.
#' @inheritParams desc_add_role
#'
#' @family Authors@R
#' @export

desc_del_author <- generate_api("del_author")

#' @title Delete a role of an author, in DESCRIPTION
#'
#' @inherit desc_add_role description
#'
#' @param role Role to remove.
#' @inheritParams desc_add_role
#'
#' @family Authors@R
#' @export

desc_del_role <- generate_api("del_role")

#' Change maintainer of the package, in DESCRIPTION
#'
#' Only works with the Authors@R field.
#'
#' The current maintainer is kept if they have at least another role.
#'
#' @inheritParams desc_add_author
#'
#' @family Authors@R
#' @export

desc_change_maintainer <- generate_api("change_maintainer")

#' Add the current user as an author to DESCRIPTION
#'
#' Uses the Authors@R field.
#'
#' @param role Role to set for the user, defaults to contributor.
#' @param comment Comment, empty by default.
#' @param orcid ORCID, empty by default.
#' @inheritParams desc_set
#'
#' @details
#' `desc_add_me` is a convenience function, it adds the
#'  current user as an author, and it needs the
#' `whoami` package to be installed. It'll add your ORCID ID
#' if you provide it as argument or save it as `ORCID_ID` environment
#' variable in .Renviron.
#' The full name is parsed using
#' `as.person` and collapsing the given name and the family name
#' in order to e.g. have the first and middle names together as given
#' name. This approach might be limited to some full name structures.
#'
#' @family Authors@R
#' @export

desc_add_me <- generate_api("add_me")

#' Add a GitHub user as an author to DESCRIPTION
#'
#' Uses the Authors@R field.
#'
#' @param username GitHub username of the GitHub user
#' @param role Role to set for the user, defaults to contributor.
#' @param comment Comment, empty by default.
#' @param orcid ORCID, empty by default.
#' @inheritParams desc_set
#'
#' @details
#' `desc_add_author_gh` is a convenience function, it adds the
#'  GitHub user as an author, and it needs the
#' `gh` package to be installed.
#' The full name is parsed using
#' `as.person` and collapsing the given name and the family name
#' in order to e.g. have the first and middle names together as given
#' name. This approach might be limited to some full name structures.
#'
#' @family Authors@R
#' @export

desc_add_author_gh <- generate_api("add_author_gh")

#' Query the package maintainer in DESCRIPTION
#'
#' Either from the `Maintainer` or the `Authors@R` field.
#' @inheritParams desc_set
#' @return A character scalar.
#'
#' @family Authors@R
#' @export

desc_get_maintainer <- generate_api("get_maintainer", self = FALSE)

#' Coerce Author and Maintainer Fields to Authors@R
#'
#' Convert the `Author` and `Maintainer` fields to
#' `Authors@R`, which is necessary for other functions such as
#' `desc_get_authors()`.
#'
#' @inheritParams desc_set
#'
#' @details
#' If the `Authors@R` field does not exist,
#' `desc_coerce_authors_at_r` tries to parse the `Author` and
#' `Maintainer` fields with [utils::as.person()] and writes
#' them to the `Authors@R` field.
#' Note that `Author` and `Maintainer` are free-form fields, so
#' parsing them may fail.
#'
#' @export
#' @family Authors@R
desc_coerce_authors_at_r <- generate_api("coerce_authors_at_r")


## -------------------------------------------------------------------

#' Query the URL field in DESCRIPTION
#'
#' @inheritParams desc_set
#' @return A character vectors or URLs. A length zero vector is returned
#'   if there is no URL field in the package.
#'
#' @export

desc_get_urls <- generate_api("get_urls", self = FALSE)

#' Set the URL field in DESCRIPTION
#'
#' The specified urls replace the current ones. The URL field is created
#' if it does not exist currently.
#'
#' @param urls A character vector of urls to set.
#' @inheritParams desc_set
#'
#' @export

desc_set_urls <- generate_api("set_urls")

#' Add URLs to the URL field in DESCRIPTION
#'
#' @param urls Character vector of URLs to add. Duplicate URLs are
#'   eliminated.
#' @inheritParams desc_set
#'
#' @export

desc_add_urls <- generate_api("add_urls")

#' Delete URLs from the URL field in DESCRIPTION
#'
#' All URLs matching the specified pattern are deleted.
#'
#' @param pattern Perl-compatible regular expression, all URLs
#'   matching this expression will be deleted.
#' @inheritParams desc_set
#'
#' @export

desc_del_urls <- generate_api("del_urls")

#' Remove all URLs from the URL field of DESCRIPTION
#'
#' @inheritParams desc_set
#'
#' @export

desc_clear_urls <- generate_api("clear_urls")

## -------------------------------------------------------------------

#' List the locations in the Remotes field in DESCRIPTION
#'
#' @inheritParams desc_set
#' @return A character vectors or remote locations. A length zero vector
#'   is returned if there is no Remotes field in the package.
#'
#' @export

desc_get_remotes <- generate_api("get_remotes", self = FALSE)

#' Set the Remotes field in DESCRIPTION
#'
#' The specified locations replace the current ones. The Remotes field is
#' created if it does not exist currently.
#'
#' @param remotes A character vector of remote locations to set.
#' @inheritParams desc_set
#'
#' @export

desc_set_remotes <- generate_api("set_remotes")

#' Add locations in the Remotes field in DESCRIPTION
#'
#' @param remotes Character vector of remote locations to add.
#'   Duplicate locations are eliminated. Note that existing locations
#'   are not updated, so if you want to _change_ a remote location
#'   of a package, you need to delete the old location first and then add
#'   the new one.
#' @inheritParams desc_set
#'
#' @export

desc_add_remotes <- generate_api("add_remotes")

#' Delete locations from the Remotes field in DESCRIPTION
#'
#' All locations matching the specified pattern are deleted.
#'
#' @param pattern Perl-compatible regular expression, all locations
#'   matching this expression will be deleted.
#' @inheritParams desc_set
#'
#' @export

desc_del_remotes <- generate_api("del_remotes")

#' Remove all locations from the Remotes field of DESCRIPTION
#'
#' This simply means that the field is deleted.
#'
#' @inheritParams desc_set
#'
#' @export

desc_clear_remotes <- generate_api("clear_remotes")

## -------------------------------------------------------------------

#' Query the package version in DESCRIPTION
#'
#' If the file has no `Version` field, or it is an invalid
#' version string, then it throws an error.
#'
#' @inheritParams desc_set
#' @return A [base::package_version] object.
#'
#' @export
#' @family version numbers

desc_get_version <- generate_api("get_version", self = FALSE)

#' Set the package version in DESCRIPTION
#'
#' Both `$set_version()` and `$bump_version()` use dots to
#' separate the version number components.
#'
#' @param version A string or a [base::package_version] object.
#' @inheritParams desc_set
#'
#' @export
#' @family version numbers

desc_set_version <- generate_api("set_version")

#' Increase the version number in DESCRIPTION
#'
#' The `which` parameter specifies which component to increase.
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
#' @param which Which component to increase. See details below.
#' @inheritParams desc_set
#'
#' @export
#' @family version numbers

desc_bump_version <- generate_api("bump_version")

## -------------------------------------------------------------------

#' Query the built field in DESCRIPTION
#'
#' If the file has no `Built` field then it throws an error.
#'
#' @inheritParams desc_set
#' @return A list with fields `R`, `Platform`, `Date`,
#' `OStype`.
#'
#' @export
#' @family built
desc_get_built <- generate_api("get_built")

## -------------------------------------------------------------------
# nocov end
