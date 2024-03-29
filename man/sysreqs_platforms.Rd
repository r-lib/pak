% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sysreqsdocs.R
\name{sysreqs_platforms}
\alias{sysreqs_platforms}
\title{List platforms with system requirements support}
\usage{
sysreqs_platforms()
}
\value{
Data frame with columns:
\itemize{
\item \code{name}: human readable OS name.
\item \code{os}: OS name, e.g. \code{linux}.
\item \code{distribution}: OS id, e.g. \code{ubuntu} or \code{redhat}.
\item \code{version}: distribution version. A star means that all versions are
supported, that are also supported by the vendor.
\item \code{update_command}: command to run to update the system package metadata.
\item \code{install_command}: command to run to install packages.
\item \code{query_command}: name of the tool to use to query system package
information.
}
}
\description{
List platforms with system requirements support
}
\examples{
\dontshow{if (Sys.getenv("IN_PKGDOWN") == "true") (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
sysreqs_platforms()
\dontshow{\}) # examplesIf}
}
\seealso{
Other system requirements functions: 
\code{\link{pkg_sysreqs}()},
\code{\link{sysreqs_check_installed}()},
\code{\link{sysreqs_db_list}()},
\code{\link{sysreqs_db_match}()},
\code{\link{sysreqs_db_update}()},
\code{\link{sysreqs_is_supported}()},
\code{\link{sysreqs_list_system_packages}()}
}
\concept{system requirements functions}
