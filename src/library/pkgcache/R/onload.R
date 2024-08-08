
## nocov start

pkgenv <- new.env(parent = emptyenv())

pkgenv$r_versions <- list(
  list(version = "0.60",   date = "1997-12-04T08:47:58.000000Z"),
  list(version = "0.61",   date = "1997-12-21T13:09:22.000000Z"),
  list(version = "0.61.1", date = "1998-01-10T00:31:55.000000Z"),
  list(version = "0.61.2", date = "1998-03-14T19:25:55.000000Z"),
  list(version = "0.61.3", date = "1998-05-02T07:58:17.000000Z"),
  list(version = "0.62",   date = "1998-06-14T12:56:20.000000Z"),
  list(version = "0.62.1", date = "1998-06-14T22:13:25.000000Z"),
  list(version = "0.62.2", date = "1998-07-10T11:13:45.000000Z"),
  list(version = "0.62.3", date = "1998-08-28T09:02:19.000000Z"),
  list(version = "0.62.4", date = "1998-10-23T12:08:41.000000Z"),
  list(version = "0.63",   date = "1998-11-13T14:37:19.000000Z"),
  list(version = "0.63.1", date = "1998-12-04T13:06:28.000000Z"),
  list(version = "0.63.2", date = "1999-01-11T12:55:50.000000Z"),
  list(version = "0.63.3", date = "1999-03-05T14:27:14.000000Z"),
  list(version = "0.64",   date = "1999-04-07T13:19:41.000000Z"),
  list(version = "0.64.1", date = "1999-05-07T13:25:43.000000Z"),
  list(version = "0.64.2", date = "1999-07-02T12:23:15.000000Z"),
  list(version = "0.65",   date = "1999-08-27T10:29:29.000000Z"),
  list(version = "0.65.1", date = "1999-10-06T12:13:04.000000Z"),
  list(version = "0.90",   date = "1999-11-22T12:25:14.000000Z"),
  list(version = "0.90.1", date = "1999-12-15T12:29:07.000000Z"),
  list(version = "0.99",   date = "2000-02-07T11:24:50.000000Z"),
  list(version = "1.0",    date = "2000-02-29T08:55:23.000000Z"),
  list(version = "1.0.1",  date = "2000-04-14T08:44:18.000000Z"),
  list(version = "1.1",    date = "2000-06-15T08:43:21.000000Z"),
  list(version = "1.1.1",  date = "2000-08-15T08:54:18.000000Z"),
  list(version = "1.2",    date = "2000-12-15T10:19:25.000000Z"),
  list(version = "1.2.1",  date = "2001-01-15T10:18:01.000000Z"),
  list(version = "1.2.2",  date = "2001-02-26T12:43:25.000000Z"),
  list(version = "1.2.3",  date = "2001-04-26T11:29:47.000000Z"),
  list(version = "1.3",    date = "2001-06-22T10:41:02.000000Z"),
  list(version = "1.3.1" , date = "2001-08-31T12:45:52.000000Z"),
  list(version = "1.4"   , date = "2001-12-19T10:14:54.000000Z"),
  list(version = "1.4.1",  date = "2002-01-30T11:57:35.000000Z"),
  list(version = "1.5.0",  date = "2002-04-29T10:01:26.000000Z"),
  list(version = "1.5.1",  date = "2002-06-17T11:20:33.000000Z"),
  list(version = "1.6.0",  date = "2002-10-01T10:06:31.000000Z"),
  list(version = "1.6.1",  date = "2002-11-01T10:33:17.000000Z"),
  list(version = "1.6.2",  date = "2003-01-10T15:34:34.000000Z"),
  list(version = "1.7.0",  date = "2003-04-16T12:58:07.000000Z"),
  list(version = "1.7.1",  date = "2003-06-16T09:54:39.000000Z"),
  list(version = "1.8.0",  date = "2003-10-08T11:13:59.000000Z"),
  list(version = "1.8.1",  date = "2003-11-21T12:00:21.000000Z"),
  list(version = "1.9.0",  date = "2004-04-12T10:36:38.000000Z"),
  list(version = "1.9.1",  date = "2004-06-21T11:09:39.000000Z"),
  list(version = "2.0.0",  date = "2004-10-04T14:24:38.899055Z"),
  list(version = "2.0.1",  date = "2004-11-15T14:16:30.003793Z"),
  list(version = "2.1.0",  date = "2005-04-18T22:26:33.135566Z"),
  list(version = "2.1.1",  date = "2005-06-20T09:27:13.106513Z"),
  list(version = "2.2.0",  date = "2005-10-06T10:22:14.085752Z"),
  list(version = "2.2.1",  date = "2005-12-20T10:35:21.589612Z"),
  list(version = "2.3.0",  date = "2006-04-24T10:37:20.758200Z"),
  list(version = "2.3.1",  date = "2006-06-01T08:25:33.882724Z"),
  list(version = "2.4.0",  date = "2006-10-03T10:15:04.354469Z"),
  list(version = "2.4.1",  date = "2006-12-18T09:49:23.725060Z"),
  list(version = "2.5.0",  date = "2007-04-24T09:41:43.361786Z"),
  list(version = "2.5.1",  date = "2007-06-28T11:17:06.374019Z"),
  list(version = "2.6.0",  date = "2007-10-03T09:02:53.434461Z"),
  list(version = "2.6.1",  date = "2007-11-26T14:14:04.408327Z"),
  list(version = "2.6.2",  date = "2008-02-08T11:10:05.737877Z"),
  list(version = "2.7.0",  date = "2008-04-22T07:45:29.665494Z"),
  list(version = "2.7.1",  date = "2008-06-23T07:44:32.518990Z"),
  list(version = "2.7.2",  date = "2008-08-25T08:53:56.807981Z"),
  list(version = "2.8.0",  date = "2008-10-20T09:24:01.015723Z"),
  list(version = "2.8.1",  date = "2008-12-22T09:03:17.828643Z"),
  list(version = "2.9.0",  date = "2009-04-17T08:32:48.144754Z"),
  list(version = "2.9.1",  date = "2009-06-26T12:10:57.017685Z"),
  list(version = "2.9.2",  date = "2009-08-24T08:22:34.737538Z"),
  list(version = "2.10.0", date = "2009-10-26T09:02:22.255015Z"),
  list(version = "2.10.1", date = "2009-12-14T10:28:24.741988Z"),
  list(version = "2.11.0", date = "2010-04-22T08:11:21.939620Z"),
  list(version = "2.11.1", date = "2010-05-31T08:10:25.280185Z"),
  list(version = "2.12.0", date = "2010-10-15T08:41:57.974589Z"),
  list(version = "2.12.1", date = "2010-12-16T09:12:04.607865Z"),
  list(version = "2.12.2", date = "2011-02-25T11:07:19.316500Z"),
  list(version = "2.13.0", date = "2011-04-13T08:31:27.165034Z"),
  list(version = "2.13.1", date = "2011-07-08T09:37:08.653178Z"),
  list(version = "2.13.2", date = "2011-09-30T07:05:56.091789Z"),
  list(version = "2.14.0", date = "2011-10-31T08:09:09.353781Z"),
  list(version = "2.14.1", date = "2011-12-22T08:10:18.809127Z"),
  list(version = "2.14.2", date = "2012-02-29T08:10:10.445478Z"),
  list(version = "2.15.0", date = "2012-03-30T07:16:05.708046Z"),
  list(version = "2.15.1", date = "2012-06-22T07:09:44.415136Z"),
  list(version = "2.15.2", date = "2012-10-26T07:11:16.605580Z"),
  list(version = "2.15.3", date = "2013-03-01T08:28:29.088755Z"),
  list(version = "3.0.0",  date = "2013-04-03T07:12:36.801147Z"),
  list(version = "3.0.1",  date = "2013-05-16T07:11:33.885209Z"),
  list(version = "3.0.2",  date = "2013-09-25T07:11:09.016418Z"),
  list(version = "3.0.3",  date = "2014-03-06T08:12:33.995105Z"),
  list(version = "3.1.0",  date = "2014-04-10T07:11:10.831155Z"),
  list(version = "3.1.1",  date = "2014-07-10T07:11:09.316022Z"),
  list(version = "3.1.2",  date = "2014-10-31T08:11:32.082768Z"),
  list(version = "3.1.3",  date = "2015-03-09T08:12:20.229070Z"),
  list(version = "3.2.0",  date = "2015-04-16T07:13:33.144514Z"),
  list(version = "3.2.1",  date = "2015-06-18T07:15:04.589869Z"),
  list(version = "3.2.2",  date = "2015-08-14T07:13:18.272871Z"),
  list(version = "3.2.3",  date = "2015-12-10T08:13:08.415370Z"),
  list(version = "3.2.4",  date = "2016-03-10T08:15:45.901354Z"),
  list(version = "3.2.5",  date = "2016-04-14T15:59:38.833914Z"),
  list(version = "3.3.0",  date = "2016-05-03T07:13:28.102867Z"),
  list(version = "3.3.1",  date = "2016-06-21T07:21:38.894907Z"),
  list(version = "3.3.2",  date = "2016-10-31T08:13:15.868949Z"),
  list(version = "3.3.3",  date = "2017-03-06T08:16:31.646592Z"),
  list(version = "3.4.0",  date = "2017-04-21T07:14:45.366247Z"),
  list(version = "3.4.1",  date = "2017-06-30T07:04:11.824142Z"),
  list(version = "3.4.2",  date = "2017-09-28T07:04:35.796221Z"),
  list(version = "3.4.3",  date = "2017-11-30T08:05:05.204665Z"),
  list(version = "3.4.4",  date = "2018-03-15T08:04:27.234564Z"),
  list(version = "3.5.0",  date = "2018-04-23T07:04:38.341063Z"),
  list(version = "3.5.1",  date = "2018-07-02T07:04:31.629927Z"),
  list(version = "3.5.2",  date = "2018-12-20T08:04:40.536010Z"),
  list(version = "3.5.3",  date = "2019-03-11T08:04:49.379300Z"),
  list(version = "3.6.0",  date = "2019-04-26T07:05:03.899333Z"),
  list(version = "3.6.1",  date = "2019-07-05T07:05:03.918895Z"),
  list(version = "3.6.2",  date = "2019-12-12T08:05:03.679160Z"),
  list(version = "3.6.3",  date = "2020-02-29T08:05:16.744223Z"),
  list(version = "4.0.0",  date = "2020-04-24T07:05:34.612930Z"),
  list(version = "4.0.1",  date = "2020-06-06T07:05:16.469439Z"),
  list(version = "4.0.2",  date = "2020-06-22T07:05:19.236082Z"),
  list(version = "4.0.3",  date = "2020-10-10T07:05:24.661746Z"),
  list(version = "4.0.4",  date = "2021-02-15T08:05:13.579673Z"),
  list(version = "4.0.5",  date = "2021-03-31T07:05:15.035437Z"),
  list(version = "4.1.0",  date = "2021-05-18T07:05:22.435363Z"),
  list(version = "4.1.1",  date = "2021-08-10T07:05:06.632742Z"),
  list(version = "4.1.2",  date = "2021-11-01T08:05:12.078145Z"),
  list(version = "4.1.3",  date = "2022-03-10T08:05:38.083503Z"),
  list(version = "4.2.0",  date = "2022-04-22T07:05:41.508134Z"),
  list(version = "4.2.1",  date = "2022-06-23T07:05:33.441356Z"),
  list(version = "4.2.2",  date = "2022-10-31T08:05:54.268400Z"),
  list(version = "4.2.3",  date = "2023-03-15T08:06:01.008593Z"),
  list(version = "4.3.0",  date = "2023-04-21T07:06:14.217164Z")
)

pkgenv$ppm_distros_cached <-
  utils::read.table(header = TRUE, stringsAsFactors = FALSE, textConnection("
 name                    os      binary_url    distribution release   binaries
 centos7                 linux   centos7       centos       7         TRUE
 centos8                 linux   centos8       centos       8         TRUE
 rhel9                   linux   rhel9         rockylinux   9         TRUE
 opensuse15              linux   opensuse15    opensuse     15        TRUE
 opensuse152             linux   opensuse152   opensuse     15.2      TRUE
 opensuse153             linux   opensuse153   opensuse     15.3      TRUE
 opensuse154             linux   opensuse154   opensuse     15.4      TRUE
 opensuse42              linux   opensuse42    opensuse     42.3      TRUE
 rhel7                   linux   centos7       redhat       7         TRUE
 rhel8                   linux   centos8       redhat       8         TRUE
\"rhel9 (unused alias)\" linux   rhel9         redhat       9         TRUE
 sles12                  linux   opensuse42    sle          12.3      TRUE
 sles15                  linux   opensuse15    sle          15        TRUE
 sles152                 linux   opensuse152   sle          15.2      TRUE
 sles153                 linux   opensuse153   sle          15.3      TRUE
 sles154                 linux   opensuse154   sle          15.4      TRUE
 xenial                  linux   xenial        ubuntu       16.04     TRUE
 bionic                  linux   bionic        ubuntu       18.04     TRUE
 focal                   linux   focal         ubuntu       20.04     TRUE
 jammy                   linux   jammy         ubuntu       22.04     TRUE
 buster                  linux   buster        debian       10        FALSE
 bullseye                linux   bullseye      debian       11        FALSE
 windows                 windows \"\"          windows      all       TRUE
 macOS                   macOS   \"\"          macOS        all       FALSE
"))

pkgenv$ppm_r_versions_cached <- c("3.6", "4.0", "4.1", "4.2", "4.3")

pkgenv$package_versions <- new.env(parent = emptyenv())

onload_pkgcache <- function(libname, pkgname) {
  if (Sys.getenv("PKGCACHE_NO_PILLAR") == "") {
    requireNamespace("pillar", quietly = TRUE)
  }
  pkgenv$global_metadata_cache <- new.env(parent = emptyenv())
  pkgenv$archive_cache <- new.env(parent = emptyenv())
  err$onload_hook()
}

if (exists(".onLoad", inherits = FALSE)) {
  onload_old <- .onLoad
  .onLoad <- function(libname, pkgname) {
    onload_old(libname, pkgname)
    onload_pkgcache(libname, pkgname)
  }
} else {
  .onLoad <- onload_pkgcache
}

## nocov end

#' The R6 object that implements the global metadata cache
#'
#' This is used by the [meta_cache_deps()], [meta_cache_list()], etc.
#' functions.
#'
#' @export
#' @examplesIf FALSE
#' get_cranlike_metadata_cache()
#' get_cranlike_metadata_cache()$list("cli")

get_cranlike_metadata_cache <- function() {
  repos <- repo_get()
  hash <- hash_obj_md5(repos$url)
  if (is.null(pkgenv$global_metadata_cache[[hash]])) {
    pkgenv$global_metadata_cache[[hash]] <- cranlike_metadata_cache$new()
  }
  pkgenv$global_metadata_cache[[hash]]
}
