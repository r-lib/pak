# Query system requirements

**\[deprecated\]**

Note that these functions are now *deprecated*, in favor of
[`pkg_sysreqs()`](https://pak.r-lib.org/reference/pkg_sysreqs.md) and
the `sysreqs_*` functions, which are more powerful, as they work for all
package sources (packages at Github, GitLab, URLs, etc.) and they have
more detailed output.

Instead of

    pak::pkg_system_requirement("curl")

call

    pak::pkg_sysreqs("curl")$install_scripts

and the equivalent of

    pak::local_system_requirements()

is

    pak::pkg_sysreqs("local::.", dependencies = TRUE)$install_script

## Usage

``` r
local_system_requirements(
  os = NULL,
  os_release = NULL,
  root = ".",
  execute = FALSE,
  sudo = execute,
  echo = FALSE
)

pkg_system_requirements(
  package,
  os = NULL,
  os_release = NULL,
  execute = FALSE,
  sudo = execute,
  echo = FALSE
)
```

## Arguments

- os, os_release:

  The operating system and operating system release version, e.g.
  "ubuntu", "centos", "redhat". See `supported_os_versions()` for all
  full list of supported operating systems.

  If `NULL`, the default, these will be looked up.

- root:

  Path to the package tree.

- execute, sudo:

  If `execute` is `TRUE`, pak will execute the system commands (if any).
  If `sudo` is `TRUE`, pak will prepend the commands with
  [sudo](https://en.wikipedia.org/wiki/Sudo).

- echo:

  If `echo` is `TRUE` and `execute` is `TRUE`, echo the command output.

- package:

  Package names to lookup system requirements for.

## Value

A character vector of commands needed to install the system requirements
for the package.

## Details

Returns a character vector of commands to run that will install system
requirements for the queried operating system.

`local_system_requirements()` queries system requirements for a dev
package (and its dependencies) given its `root` path.

`pkg_system_requirements()` queries system requirements for existing
packages (and their dependencies).

## Examples

``` r
if (FALSE) {
local_system_requirements("ubuntu", "20.04")
}
if (FALSE) {
pkg_system_requirements("pak", "ubuntu", "20.04")
pkg_system_requirements("pak", "redhat", "7")
pkg_system_requirements("config", "ubuntu", "20.04") # no sys reqs
pkg_system_requirements("curl", "ubuntu", "20.04")
pkg_system_requirements("git2r", "ubuntu", "20.04")
pkg_system_requirements(c("config", "git2r", "curl"), "ubuntu", "20.04")
# queried packages must exist
pkg_system_requirements("iDontExist", "ubuntu", "20.04")
pkg_system_requirements(c("curl", "iDontExist"), "ubuntu", "20.04")
}
```
