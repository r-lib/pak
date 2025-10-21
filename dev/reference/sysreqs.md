# System requirements

pak takes care of your system requirements.

## Introduction

Many R packages need external software to be present on the machine,
otherwise they do not work, or not even load. For example the RPostgres
R package uses the PostgreSQL client library, and by default dynamically
links to it on Linux systems. This means that you (or the administrators
of your system) need to install this library, typically in the form of a
system package: `libpq-dev` on Ubuntu and Debian systems, or
`postgresql-server-devel` or `postgresql-devel` on RedHat, Fedora, etc.
systems.

The good news is that pak helps you with this: - it looks up the
required system packages when installing R packages, - it checks if the
required system packages are installed, and - it installs them
automatically, if you are a superuser, or you can use password-less
`sudo` to start a superuser shell.

In addition, pak also has some functions to query system requirements
and system packages.

## Requirements, supported platforms

Call
[`pak::sysreqs_platforms()`](https://pak.r-lib.org/dev/reference/sysreqs_platforms.md)
to list all platforms that support system requirements:

    pak::sysreqs_platforms()

    ## # A data frame: 10 × 7
    ##    name        os    distribution version update_command install_command
    ##    <chr>       <chr> <chr>        <chr>   <chr>          <chr>
    ##  1 Ubuntu Lin… linux ubuntu       *       apt-get -y up… apt-get -y ins…
    ##  2 Debian Lin… linux debian       *       apt-get -y up… apt-get -y ins…
    ##  3 CentOS Lin… linux centos       *       NA             yum install -y
    ##  4 Rocky Linux linux rockylinux   *       NA             dnf install -y
    ##  5 Red Hat En… linux redhat       6       NA             yum install -y
    ##  6 Red Hat En… linux redhat       7       NA             yum install -y
    ##  7 Red Hat En… linux redhat       *       NA             dnf install -y
    ##  8 Fedora Lin… linux fedora       *       NA             dnf install -y
    ##  9 openSUSE L… linux opensuse     *       NA             zypper --non-i…
    ## 10 SUSE Linux… linux sle          *       NA             zypper --non-i…
    ## # i 1 more variable: query_command <chr>

Call
[`pak::sysreqs_is_supported()`](https://pak.r-lib.org/dev/reference/sysreqs_is_supported.md)
to see if your system is supported:

    pak::sysreqs_is_supported()

    ## [1] TRUE

This vignette was built on Ubuntu 22.04.2 LTS, which is a platform pak
does support. So in the following you will see the output of the code.

## R package installation

If you are using pak as a superuser, on a supported platform, then pak
will look up system requirements, and install the missing ones. Here is
an example:

    pak::pkg_install("RPostgres")

    ## v Loading metadata database ... done
    ##
    ## → Will install 17 packages.
    ## → All 17 packages (0 B) are cached.
    ## + DBI          1.1.3
    ## + RPostgres    1.4.5   + x libpq-dev
    ## + Rcpp         1.0.10
    ## + bit          4.0.5
    ## + bit64        4.0.5
    ## + blob         1.2.4
    ## + cli          3.6.1
    ## + generics     0.1.3
    ## + glue         1.6.2
    ## + hms          1.1.3
    ## + lifecycle    1.0.3
    ## + lubridate    1.9.2
    ## + pkgconfig    2.0.3
    ## + rlang        1.1.1
    ## + timechange   0.2.0
    ## + vctrs        0.6.2
    ## + withr        2.5.0
    ## → Will install 1 system package:
    ## + libpq-dev  - RPostgres
    ## i No downloads are needed, 17 pkgs are cached
    ## i Installing system requirements
    ## i Executing `sh -c apt-get -y update`
    ## i Executing `sh -c apt-get -y install libpq-dev`
    ## v Installed DBI 1.1.3  (1.1s)
    ## v Installed RPostgres 1.4.5  (1.2s)
    ## v Installed Rcpp 1.0.10  (1.2s)
    ## v Installed bit 4.0.5  (1.2s)
    ## v Installed bit64 4.0.5  (148ms)
    ## v Installed blob 1.2.4  (63ms)
    ## v Installed cli 3.6.1  (88ms)
    ## v Installed generics 0.1.3  (61ms)
    ## v Installed glue 1.6.2  (63ms)
    ## v Installed hms 1.1.3  (62ms)
    ## v Installed lifecycle 1.0.3  (61ms)
    ## v Installed lubridate 1.9.2  (87ms)
    ## v Installed pkgconfig 2.0.3  (62ms)
    ## v Installed rlang 1.1.1  (1.1s)
    ## v Installed timechange 0.2.0  (1.1s)
    ## v Installed vctrs 0.6.2  (1.1s)
    ## v Installed withr 2.5.0  (1.1s)
    ## v 1 pkg + 16 deps: added 17 [17.5s]

### Running R as a regular user

If you don’t want to use R as the superuser, but you can set up `sudo`
without a password, that works as well. pak will automatically detect
the password-less `sudo` capability, and use it to install system
packages, as needed.

If you run R as a regular (not root) user, and password-less `sudo` is
not available, then pak will print the system requirements, but it will
not try to install or update them. If you are installing source packages
that need to link to system libraries, then their installation will
probably fail, until you install these system packages. If you are
installing binary R packages, then the installation typically succeeds,
but you won’t be able to load these packages into R, until you install
the required system packages. Here is an example, on a system that does
not have the required system package installed for RPostgres. If you are
installing a source R package, the installation already fails:

    pak::pkg_install("RPostgres?source")

    ## + plogr       0.2.0
    ## x Missing 1 system package. You'll probably need to install it
    ## manually:
    ## + libpq-dev  - RPostgres
    ## i No downloads are needed, 2 pkgs (1.47 MB) are cached
    ## v Installed plogr 0.2.0  (1.1s)
    ## i Building RPostgres 1.4.5
    ## x Failed to build RPostgres 1.4.5
    ## Registered S3 methods overwritten by 'callr':
    ##   method                    from
    ##   format.callr_status_error
    ##   print.callr_status_error
    ## Error:
    ## ! error in pak subprocess
    ## Caused by error in `stop_task_build(state, worker)`:
    ## ! Failed to build source package 'RPostgres'
    ## Full installation output:
    ## * installing *source* package ‘RPostgres’ ...
    ## ** package ‘RPostgres’ successfully unpacked and MD5 sums checked
    ## staged installation is only possible with locking
    ## ** using non-staged installation
    ## Using PKG_CFLAGS=
    ## Using PKG_LIBS=-lpq
    ## Using PKG_PLOGR=
    ## ------------------------- ANTICONF ERROR ---------------------------
    ## Configuration failed because libpq was not found. Try installing:
    ##  * deb: libpq-dev libssl-dev (Debian, Ubuntu, etc)
    ##  * rpm: postgresql-devel (Fedora, EPEL)
    ##  * rpm: postgreql8-devel, psstgresql92-devel, postgresql93-devel, or pos
    ## tgresql94-devel (Amazon Linux)
    ##  * csw: postgresql_dev (Solaris)
    ##  * brew: libpq (OSX)
    ## If libpq is already installed, check that either:
    ## (i)  'pkg-config' is in your PATH AND PKG_CONFIG_PATH contains
    ##      a libpq.pc file; or
    ## (ii) 'pg_config' is in your PATH.
    ## If neither can detect , you can set INCLUDE_DIR
    ## and LIB_DIR manually via:
    ## R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
    ## --------------------------[ ERROR MESSAGE ]----------------------------
    ## <stdin>:1:10: fatal error: libpq-fe.h: No such file or directory
    ## compilation terminated.
    ## -----------------------------------------------------------------------
    ## ERROR: configuration failed for package ‘RPostgres’
    ## * removing ‘/tmp/RtmpsOXbPZ/pkg-lib4a492949a49e/RPostgres’
    ## ---
    ## Backtrace:
    ## 1. pak::pkg_install("RPostgres?source")
    ## 2. pak:::remote(function(...) get("pkg_install_do_plan", asNamespace("pa
    ## k")… at package.R:84:3
    ## 3. err$throw(res$error) at subprocess.R:115:5
    ## ---
    ## Subprocess backtrace:
    ##  1. base::withCallingHandlers(cli_message = function(msg) { …
    ##  2. get("pkg_install_do_plan", asNamespace("pak"))(...)
    ##  3. proposal$install()
    ##  4. pkgdepends::install_package_plan(plan, lib = private$library, num_wo
    ## rkers = nw…
    ##  5. base::withCallingHandlers({ …
    ##  6. pkgdepends:::handle_events(state, events)
    ##  7. pkgdepends:::handle_event(state, i)
    ##  8. pkgdepends:::stop_task(state, worker)
    ##  9. pkgdepends:::stop_task_build(state, worker)
    ## 10. base::throw(new_pkg_build_error("Failed to build source package {pkg
    ## }", …                                                                   
    ## 11. | base::signalCondition(cond)                                       
    ## 12. global (function (e) …                                              
    ## Execution halted                                                        

On the other hand, if you are installing binary packages, e.g. from the
Posit Package Manager, then the installation typically succeeds, but
then loading the package fails:

    pak::pkg_install("RPostgres")
    library(RPostgres)

    ## → Will install 17 packages.
    ## → All 17 packages (0 B) are cached.
    ## + DBI          1.1.3
    ## + RPostgres    1.4.5   + x libpq-dev
    ## + Rcpp         1.0.10
    ## + bit          4.0.5
    ## + bit64        4.0.5
    ## + blob         1.2.4
    ## + cli          3.6.1
    ## + generics     0.1.3
    ## + glue         1.6.2
    ## + hms          1.1.3
    ## + lifecycle    1.0.3
    ## + lubridate    1.9.2
    ## + pkgconfig    2.0.3
    ## + rlang        1.1.1
    ## + timechange   0.2.0
    ## + vctrs        0.6.2
    ## + withr        2.5.0
    ## x Missing 1 system package. You'll probably need to install it
    ## manually:
    ## + libpq-dev  - RPostgres
    ## i No downloads are needed, 17 pkgs are cached
    ## v Installed DBI 1.1.3  (1.1s)
    ## v Installed RPostgres 1.4.5  (1.1s)
    ## v Installed Rcpp 1.0.10  (1.2s)
    ## v Installed bit 4.0.5  (1.2s)
    ## v Installed bit64 4.0.5  (144ms)
    ## v Installed blob 1.2.4  (1.1s)
    ## v Installed cli 3.6.1  (1.1s)
    ## v Installed generics 0.1.3  (90ms)
    ## v Installed glue 1.6.2  (87ms)
    ## v Installed hms 1.1.3  (1.1s)
    ## v Installed lifecycle 1.0.3  (1.1s)
    ## v Installed lubridate 1.9.2  (1.1s)
    ## v Installed pkgconfig 2.0.3  (1.1s)
    ## v Installed rlang 1.1.1  (1.1s)
    ## v Installed timechange 0.2.0  (1.1s)
    ## v Installed vctrs 0.6.2  (1.1s)
    ## v Installed withr 2.5.0  (1.1s)
    ## v 1 pkg + 16 deps: added 17 [11.6s]
    ## Error: package or namespace load failed for ‘RPostgres’ in dyn.load(file
    ## , DLLpath = DLLpath, ...):
    ##  unable to load shared object '/tmp/RtmpWqZycA/lib/RPostgres/libs/RPostg
    ## res.so':
    ##   libpq.so.5: cannot open shared object file: No such file or directory
    ## Execution halted

## Query system requirements without installation

If you only want to query system requirements, without installing any
packages, use the
[`pkg_sysreqs()`](https://pak.r-lib.org/dev/reference/pkg_sysreqs.md)
function. This is similar to
[`pkg_deps()`](https://pak.r-lib.org/dev/reference/pkg_deps.md) but in
addition to looking up package dependencies, it also looks up system
dependencies, and only reports the latter:

    pak::pkg_sysreqs(c("curl", "xml2", "devtools", "CHRONOS"))

    ## v Loading metadata database ... done
    ## ── Install scripts ───────────────────────────────────── Ubuntu 22.04 ──
    ## apt-get -y update
    ## apt-get -y install libcurl4-openssl-dev libssl-dev libxml2-dev git make
    ##   libgit2-dev zlib1g-dev pandoc libfreetype6-dev libjpeg-dev libpng-dev
    ##   libtiff-dev libicu-dev libfontconfig1-dev libfribidi-dev
    ##   libharfbuzz-dev libglpk-dev libgmp3-dev default-jdk
    ## R CMD javareconf
    ## R CMD javareconf
    ##
    ## ── Packages and their system dependencies ──────────────────────────────
    ## CHRONOS     – default-jdk, pandoc
    ## credentials – git
    ## curl        – libcurl4-openssl-dev, libssl-dev
    ## fs          – make
    ## gert        – libgit2-dev
    ## gitcreds    – git
    ## httpuv      – make, zlib1g-dev
    ## igraph      – libglpk-dev, libgmp3-dev, libxml2-dev
    ## knitr       – pandoc
    ## openssl     – libssl-dev
    ## pkgdown     – pandoc
    ## png         – libpng-dev
    ## ragg        – libfreetype6-dev, libjpeg-dev, libpng-dev, libtiff-dev
    ## RCurl       – libcurl4-openssl-dev, make
    ## remotes     – git
    ## rJava       – default-jdk, make
    ## rmarkdown   – pandoc
    ## sass        – make
    ## stringi     – libicu-dev
    ## systemfonts – libfontconfig1-dev, libfreetype6-dev
    ## textshaping – libfreetype6-dev, libfribidi-dev, libharfbuzz-dev
    ## XML         – libxml2-dev
    ## xml2        – libxml2-dev

See the manual of
[`pkg_sysreqs()`](https://pak.r-lib.org/dev/reference/pkg_sysreqs.md) to
see how to programmatically extract information from its return value.

## Other queries

In addition to the automatic system package lookup and installation, pak
also has some other functions to help you with system dependencies. The
[`sysreqs_db_list()`](https://pak.r-lib.org/dev/reference/sysreqs_db_list.md)
function lists all system requirements pak knows about.

    pak::sysreqs_db_list()

    ## # A data frame: 106 × 5
    ##    name       patterns  packages  pre_install post_install
    ##    <chr>      <list>    <list>    <list>      <list>
    ##  1 QuantLib   <chr [1]> <chr [1]> <NULL>      <NULL>
    ##  2 apparmor   <chr [2]> <chr [1]> <NULL>      <NULL>
    ##  3 atk        <chr [1]> <chr [1]> <NULL>      <NULL>
    ##  4 automake   <chr [1]> <chr [1]> <NULL>      <NULL>
    ##  5 berkeleydb <chr [2]> <chr [1]> <NULL>      <NULL>
    ##  6 blender    <chr [1]> <chr [1]> <NULL>      <NULL>
    ##  7 bowtie2    <chr [1]> <chr [1]> <NULL>      <NULL>
    ##  8 bwidget    <chr [1]> <chr [1]> <NULL>      <NULL>
    ##  9 cairo      <chr [1]> <chr [1]> <NULL>      <NULL>
    ## 10 chrome     <chr [1]> <NULL>    <chr [3]>   <chr [1]>
    ## # i 96 more rows

[`sysreqs_db_match()`](https://pak.r-lib.org/dev/reference/sysreqs_db_match.md)
manually matches `SystemRequirements` fields against these system
requirements:

    sq <- pak::sysreqs_db_match("Needs libcurl and also Java.")
    sq

    ## [[1]]
    ## # A data frame: 2 × 5
    ##   spec                         sysreq  packages pre_install post_install
    ##   <chr>                        <chr>   <list>   <list>      <list>
    ## 1 Needs libcurl and also Java. java    <chr>    <NULL>      <chr [1]>
    ## 2 Needs libcurl and also Java. libcurl <chr>    <NULL>      <NULL>
    ##

    sq[[1]]$packages

    ## [[1]]
    ## [1] "default-jdk"
    ##
    ## [[2]]
    ## [1] "libcurl4-openssl-dev"
    ##

You can also use it to query system requirements for other platforms:

    sqrhel9 <- pak::sysreqs_db_match("Needs libcurl and also Java.", "redhat-9")
    sqrhel9

    ## [[1]]
    ## # A data frame: 2 × 5
    ##   spec                         sysreq  packages pre_install post_install
    ##   <chr>                        <chr>   <list>   <list>      <list>
    ## 1 Needs libcurl and also Java. java    <chr>    <NULL>      <chr [1]>
    ## 2 Needs libcurl and also Java. libcurl <chr>    <NULL>      <NULL>
    ##

    sqrhel9[[1]]$packages

    ## [[1]]
    ## [1] "java-11-openjdk-devel"
    ##
    ## [[2]]
    ## [1] "libcurl-devel"
    ##

[`sysreqs_list_system_packages()`](https://pak.r-lib.org/dev/reference/sysreqs_list_system_packages.md)
is a cross-platform way of listing all installed system packages and
capabilities:

    pak::sysreqs_list_system_packages()

    ## # A data frame: 433 × 4
    ##    status package         version         provides
    ##    <chr>  <chr>           <chr>           <list>
    ##  1 ii     adduser         3.118ubuntu5    <chr [0]>
    ##  2 ii     apt             2.4.8           <chr [1]>
    ##  3 ii     autoconf        2.71-2          <chr [0]>
    ##  4 ii     automake        1:1.16.5-1.3    <chr [2]>
    ##  5 ii     autotools-dev   20220109.1      <chr [0]>
    ##  6 ii     base-files      12ubuntu4.3     <chr [1]>
    ##  7 ii     base-passwd     3.5.52build1    <chr [0]>
    ##  8 ii     bash            5.1-6ubuntu1    <chr [0]>
    ##  9 ii     binutils        2.38-4ubuntu2.1 <chr [2]>
    ## 10 ii     binutils-common 2.38-4ubuntu2.1 <chr [0]>
    ## # i 423 more rows

[`sysreqs_check_installed()`](https://pak.r-lib.org/dev/reference/sysreqs_check_installed.md)
is a handy function that checks if all system requirements are installed
for some or all R packages that are installed in your library:

    pak::sysreqs_check_installed()

    ## system package       installed required by
    ## --------------       --        -----------
    ## git                  v         gitcreds
    ## gsfonts              v         magick
    ## imagemagick          v         magick
    ## libarchive-dev       v         archive
    ## libcurl4-openssl-dev v         curl
    ## libfontconfig1-dev   v         systemfonts
    ## libfreetype6-dev     v         ragg, systemfonts, textshaping
    ## libfribidi-dev       v         textshaping
    ## libharfbuzz-dev      v         textshaping
    ## libicu-dev           v         stringi
    ## libjpeg-dev          v         ragg
    ## libmagick++-dev      v         magick
    ## libnode-dev          v         V8
    ## libpng-dev           v         ragg
    ## libpq-dev            x         RPostgres
    ## libssl-dev           v         curl, openssl
    ## libtiff-dev          v         ragg
    ## libxml2-dev          v         xml2
    ## make                 v         fs, sass
    ## pandoc               v         knitr, rmarkdown
    ## zlib1g-dev           v         data.table

[`sysreqs_fix_installed()`](https://pak.r-lib.org/dev/reference/sysreqs_check_installed.md)
goes one step further and also tries to install the missing system
requirements.

## Build-time and run-time dependencies

The system requirements database that pak uses does not currently
differentiate between build-time and run-time dependencies. A build-time
dependency is a system package that you need when *installing* an R
package from source. A run-time dependency is a system package that you
need when *using* an R package. Most Linux distribution create (at
least) two packages for each software library: a runtime package and a
development package. For an R package that uses such a software library,
the runtime package is a run-time dependency and the development package
is a build-time dependency. However, pak does not currently know the
difference between build-time and run-time dependencies, and it will
install both types of dependencies, always. This means that pak usually
installs system packages that are not strictly necessary. These are
typically development packages of libraries, i.e. header files, and
typically do not cause any issues. If you are short on disk space, then
you can try removing them.

## How it works

pak uses the database of system requirements at
<https://github.com/rstudio/r-system-requirements>. It has its own copy
of the database embedded into the package, and it also tries to download
updated versions of the database from GitHub, if its current copy is
older than one day. You can explicitly update the database from GitHub
using the
[`sysreqs_db_update()`](https://pak.r-lib.org/dev/reference/sysreqs_db_update.md)
function.

For CRAN packages, it downloads the `SystemRequirements` fields from
`https://cran.r-pkg.org/metadata`, which is a database updated daily.
For Bioconductor packages, it downloads then from GitHub. (We are
planning on moving CRAN database to GitHub as well.)

For packages sources that require pak to obtain a package `DESCRIPTION`
file (e.g. `github::`, `git::`, etc.), pak obtains `SystemRequirements`
directly from the `DESCRIPTION` file.

Once having the `SystemRequirements` fields, pak matches them to the
database, to obtain the canonized list of system requirements.

Then pak queries the local platform, to see the exact system packages
needed. It also queries the installed system packages, to avoid trying
to install system packages that are already installed.

## Configuration

There are several pak configuration options you can use to adjust how
system requirements are handled. We will list some of them here, please
see the options with a `sysreqs` prefix in the `?pak-config` manual page
for a complete and current list.

- `sysreqs`: whether to install system requirements. The default is
  `TRUE` if the platform is supported and the user can install system
  packages, either because it is the superuser, or via `sudo`. If it is
  `FALSE` (or the user cannot install system packages), but the platform
  is supported, system requirements are printed, but not installed.

- `sysreqs_db_update`: whether to try to update the system requirements
  database from GitHub.

- `sysreqs_db_update_timeout`: timeout for the system requirements
  update from GitHub.

- `sysreqs_dry_run`: if `TRUE` then pak only prints the install
  commands, but does not actually run them.

- `sysreqs_platform`: the platform name to use for determining system
  requirements. Defaults to the current platform. If you are using a
  Linux distribution that is compatible with some distribution that pak
  supports, then you can set this option manually. E.g. Ubuntu-based
  distros can set it to `ubuntu-22.04`, or the appropriate Ubuntu
  version.

- `sysreqs_sudo`: whether to use `sudo` to install system packages. If
  this is not set, then pak tries to auto-detect if `sudo` is needed or
  not.

- `sysreqs_update`: whether to try to update system packages that are
  already installed. pak does not know which version of a system package
  is required, and it does not try to update system packages by default.
  If you think that you need newer system packages, then you can set
  this option to `TRUE`.

- `sysreqs_verbose`: whether to print the output of the system package
  installation commands. Useful for debugging, and it is `TRUE` by
  default in a CI environment.

## About other OSes

### Windows

While the system requirements database has some information about system
dependencies on Windows, pak does not use this information and it does
not try to install system software on Windows. CRAN, PPM and
Bioconductor have Windows binary packages available for the majority of
R packages they serve, and these packages practically always link to
system libraries statically, so they don’t need any external software.

If you wish to compile Windows packages from source, then you need to
install the appropriate version of Rtools, and possibly extra packages
using the `pacman` tool of Rtools4x.

Rtools42 and newer Rtools versions bundle lots of libraries, so most
likely no extra `pacman` packages are needed. Rtools40 has a leaner
default installation, and you’ll probably need to install packages
manually:
<https://github.com/r-windows/docs/blob/master/rtools40.md#readme>

We are planning on adding better Windows system software support to pak
in the future.

### macOS

pak does not currently have system requirement information for macOS.
macOS is similar to Windows, in that most repositories will serve
statically linked macOS binary packages that do not need system
software.

If you do need to compile packages from source, then you possibly need
to install some system libraries, either via Homebrew, or by downloading
CRAN’s static library builds from <https://mac.r-project.org/bin/>

We are planning on adding better macOS system software support to pak in
the future.
