# doc_config

    Code
      writeLines(doc_config())
    Output
      \itemize{\item \sQuote{build_vignettes}: (Env var: \code{PKG_BUILD_VIGNETTES}, option: \code{pkg.build_vignettes}.) Whether to build vignettes for package trees.
      This is only used if the package is obtained from a package tree,
      and not from a source (or binary) package archive. By default
      vignettes are not built in this case. If you set this to \code{TRUE},
      then you need to make sure that the vignette builder packages are
      available, as these are not installed by default currently.
      \item \sQuote{cache_dir}: (Env var: \code{PKG_CACHE_DIR}, option: \code{pkg.cache_dir}.) Directory to download the packages to. Defaults to a temporary
      directory within the R session temporary directory, see
      \code{\link[base:tempfile]{base::tempdir()}}.
      \item \sQuote{cran_mirror}: (Env var: \code{PKG_CRAN_MIRROR}, option: \code{pkg.cran_mirror}.) CRAN mirror to use. Defaults to the \code{repos} option
      (see \code{\link[base:options]{base::options()}}), if that's not set then
      \verb{https://cran.rstudio.com}. See also \code{\link[pak:repo_add]{pak::repo_add()}} and
      \code{\link[pak:repo_get]{pak::repo_get()}}
      \item \sQuote{include_linkingto}: (Env var: \code{PKG_INCLUDE_LINKINGTO}, option: \code{pkg.include_linkingto}.) Whether to always include \code{LinkingTo} dependencies in the solution
      of and installation, even if they are needed because the packages
      are installed from binaries. This is sometimes useful, see e.g.
      \url{https://github.com/r-lib/pak/issues/485} for an example use case.
      \item \sQuote{library}: (Env var: \code{PKG_LIBRARY}, option: \code{pkg.library}.) Package library to install packages to. It is also used for
      already installed packages when considering dependencies.
      \item \sQuote{metadata_cache_dir}: (Env var: \code{PKG_METADATA_CACHE_DIR}, option: \code{pkg.metadata_cache_dir}.) Location of metadata replica of
      \code{\link[pkgcache:cranlike_metadata_cache]{pkgcache::cranlike_metadata_cache}}. Defaults to a temporary
      directory within the R session temporary directory, see
      \code{\link[base:tempfile]{base::tempdir()}}.
      \item \sQuote{metadata_update_after}: (Env var: \code{PKG_METADATA_UPDATE_AFTER}, option: \code{pkg.metadata_update_after}.) A time interval as a \link{difftime} object. pak will update the
      metadata cache if it is older than this. The default is one day.
      The \code{PKG_METADATA_UPDATE_AFTER} environment variable may be set
      in seconds (\code{s} suffix), minutes (\code{m} suffix), hours (\code{h} suffix),
      or days (\code{d} suffix). E.g: \verb{1d} means one day.
      \item \sQuote{package_cache_dir}: (Env var: \code{PKG_PACKAGE_CACHE_DIR}, option: \code{pkg.package_cache_dir}.) Location of the package cache on the disk. See
      \code{\link[pak:cache]{pak::cache_summary()}}. Default is selected by pkgcache.
      \item \sQuote{platforms}: (Env var: \code{PKG_PLATFORMS}, option: \code{pkg.platforms}.) Character vector of platforms to \emph{download} or \emph{install} packages
      for. See \code{\link[pkgdepends:default_platforms]{pkgdepends::default_platforms()}} for possible platform
      names. Defaults to the platform of the current R session, plus
      \code{"source"}.
      \item \sQuote{r_versions}: (Env var: \code{PKG_R_VERSIONS}, option: \code{pkg.r_versions}.) Character vector, R versions to download or install
      packages for. It defaults to the current R version.
      \item \sQuote{sysreqs}: (Env var: \code{PKG_SYSREQS}, option: \code{pkg.sysreqs}.) Whether to automatically look up and install system requirements.
      If \code{TRUE}, then pkgdepends will try to install required
      system packages. If \code{FALSE}, then system requirements are still
      printed (including OS packages on supported platforms), but they
      are not installed.
      By default it is \code{TRUE} on supported platforms,
      if the current user is the root user or password-less \code{sudo} is
      configured for the current user.
      \item \sQuote{sysreqs_db_update}: (Env var: \code{PKG_SYSREQS_DB_UPDATE}, option: \code{pkg.sysreqs_db_update}.) Whether to try to update the system requirements database from
      GitHub. If the update fails, then the cached or the build-in
      database if used. Defaults to TRUE.
      \item \sQuote{sysreqs_db_update_timeout}: (Env var: \code{PKG_SYSREQS_DB_UPDATE_TIMEOUT}, option: \code{pkg.sysreqs_db_update_timeout}.) Timeout for the system requirements database update.
      Defaults to five seconds.
      \item \sQuote{sysreqs_dry_run}: (Env var: \code{PKG_SYSREQS_DRY_RUN}, option: \code{pkg.sysreqs_dry_run}.) If \code{TRUE}, then pak only prints the system commands to
      install system requirements, but does not execute them.
      \item \sQuote{sysreqs_platform}: (Env var: \code{PKG_SYSREQS_PLATFORM}, option: \code{pkg.sysreqs_platform}.) The platform to use for system requirements lookup. On Linux, where
      system requirements are currently supported, it must be a string
      containing the distribution name and release, separated by a dash.
      E.g.: \code{"ubuntu-22.04"}, or \code{"rhel-9"}.
      \item \sQuote{sysreqs_rspm_repo_id}: (Env var: \code{PKG_SYSREQS_RSPM_REPO_ID}, option: \code{pkg.sysreqs_rspm_repo_id}.) Posit Package Manager (formerly RStudio Package Manager) repository
      id to use for CRAN system requirements lookup. Defaults to the
      \code{RSPM_REPO_ID} environment variable, if set. If not set, then it
      defaults to \code{1}.
      \item \sQuote{sysreqs_rspm_url}: (Env var: \code{PKG_SYSREQS_RSPM_URL}, option: \code{pkg.sysreqs_rspm_url}.) Root URL of Posit Package Manager (formerly RStudio Package
      Manager) for system requirements lookup. By default the \code{RSPM_ROOT}
      environment variable is used, if set. If not set,
      it defaults to \verb{https://packagemanager.posit.co}.
      \item \sQuote{sysreqs_sudo}: (Env var: \code{PKG_SYSREQS_SUDO}, option: \code{pkg.sysreqs_sudo}.) Whether to use \code{sudo} to install system requirements,
      on Unix. By default it is \code{TRUE} on Linux if the effective user id
      of the current process is not the \code{root} user.
      \item \sQuote{sysreqs_update}: (Env var: \code{PKG_SYSREQS_UPDATE}, option: \code{pkg.sysreqs_update}.) Whether to try to update system packages that are already installed.
      It defaults to \code{TRUE} on CI systems: if the \code{CI} environment
      variable is set to \code{true}.
      \item \sQuote{sysreqs_verbose}: (Env var: \code{PKG_SYSREQS_VERBOSE}, option: \code{pkg.sysreqs_verbose}.) Whether to echo the output of system requirements installation.
      Defaults to \code{TRUE} if the \code{CI} environment variable is set.
      \item \sQuote{use_bioconductor}: (Env var: \code{PKG_USE_BIOCONDUCTOR}, option: \code{pkg.use_bioconductor}.) Whether to automatically use the Bioconductor repositories.
      Defaults to \code{TRUE}.
      \item \sQuote{windows_archs}: (Env var: \code{PKG_WINDOWS_ARCHS}, option: \code{pkg.windows_archs}.) Character scalar specifying which architectures
      to download/install for on Windows. Its possible values are:
      \itemize{
      \item \code{"prefer-x64"}: Generally prefer x64 binaries. If the current R
      session is \code{x64}, then we download/install x64 packages.
      (These packages might still be multi-architecture binaries!)
      If the current R session is \code{i386}, then we download/install
      packages for both architectures. This might mean compiling
      packages from source if the binary packages are for \code{x64} only,
      like the CRAN Windows binaries for R 4.2.x currently.
      \code{"prefer-x64"} is the default for R 4.2.0 and later.
      \item \code{"both"}: Always download/install packages for both \code{i386} and
      \code{x64} architectures. This might need compilation from source
      if the available binaries are for \code{x64} only, like the CRAN
      Windows binaries for R 4.2.x currently. \code{"both"} is the default
      for R 4.2.0 and earlier.
      }}

# include_docs

    Code
      writeLines(include_docs("pkgdepends", "docs/lib-status-return.rds"))
    Output
      It has always has columns:
      \itemize{
      \item \code{biocviews}: the corresponding field from \code{DESCRIPTION}, it must be
      present for all Bioconductor packages, other packages typically don't
      have it.
      \item \code{built}: the \code{Built} field from \code{DESCRIPTION}.
      \item \code{depends}, \code{suggests}, \code{Imports}, \code{linkingto}, \code{enhances}: the corresponding
      fields from the \code{DESCRIPTION} files.
      \item \code{deps}: A list or data frames, the dependencies of the package. It has
      columns: \code{ref}, \code{type} (dependency type in lowercase), \code{package}
      (dependent package, or \code{R}), \code{op} and \code{version}, for last two are for
      version requirement. \code{op} can be \code{>=}, \code{>}, \code{==} or \code{<=}, although the
      only the first one is common in practice.
      \item \code{library}: path to the package library containing the package.
      \item \code{license}: from \code{DESCRIPTION}.
      \item \code{md5sum}: from \code{DESCTIPTION}, typically \code{NA}, except on Windows.
      \item \code{needscompilation}: from \code{DESCRIPTION}, this column is logical.
      \item \code{package}: package name.
      \item \code{platform}: from the \code{Built} field in \code{DESCRIPTION}, the current platform
      if missing from \code{DESCRIPTION}.
      \item \code{priority}: from \code{DESCRIPTION}, usually \code{base}, \code{recommended}, or missing.
      \item \code{ref}: the corresponding \verb{installed::*} package reference.
      \item \code{repository}: from \code{DESCRIPTION}. For packages from a CRAN repository this
      is \code{CRAN}, some other repositories, e.g. R-universe adds the repository
      URL here.
      \item \code{repotype}: \code{cran}, \code{bioc} or missing.
      \item \code{rversion}: from the \code{Built} field. If no such field, then the current
      R version.
      \item \code{sysreqs}: the \code{SystemRequirements} field from \code{DESCRIPTION}.
      \item \code{title}: package title.
      \item \code{type}: always \code{installed}.
      \item \code{version}: package version (as string).
      }
      
      Most of these columns are unchanged from \code{DESCRIPTION}, but
      pak also adds a couple.
      \subsection{Notes:}{
      \itemize{
      \item In addition, it also has all \verb{remote*} and \verb{config/needs/*} entries from
      the \code{DESCRIPTION} files. (Case insensitive.)
      \item All columns are of type \code{character}, except for \code{needscompilation}, which
      is logical and \code{deps}, which is a list columns.
      \item If an entry is missing for a package, it is set to \code{NA}.
      \item Note that column names are lowercase, even if the corresponding entries
      are not in \code{DESCRIPTION}.
      \item The order of the columns is not deterministic, so don't assume any order.
      \item Additional columns might be present, these are internal for
      pak and should not be used in user code.
      }
      }

# man_config_link

    Code
      man_config_link("configuration option")
    Output
      [1] "\\link[=pak-config]{configuration option}"

