
# Introduction

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

# Requirements, supported platforms

Call `pak::sysreqs_platforms()` to list all platforms that support
system requirements:

``` r
pak::sysreqs_platforms()
```

<div class="asciicast" style="color: #172431;font-family: &#39;Fira Code&#39;,Monaco,Consolas,Menlo,&#39;Bitstream Vera Sans Mono&#39;,&#39;Powerline Symbols&#39;,monospace;line-height: 1.300000">

<pre>
## <span style="color: #999999;"># A data frame: 10 × 7</span>
##    name        os    distribution version update_command install_command
##    <span style="font-style: italic;color: #999999;">&lt;chr&gt;</span>       <span style="font-style: italic;color: #999999;">&lt;chr&gt;</span> <span style="font-style: italic;color: #999999;">&lt;chr&gt;</span>        <span style="font-style: italic;color: #999999;">&lt;chr&gt;</span>   <span style="font-style: italic;color: #999999;">&lt;chr&gt;</span>          <span style="font-style: italic;color: #999999;">&lt;chr&gt;</span>
## <span style="color: #c2c2c2;"> 1</span> Ubuntu Lin… linux ubuntu       *       apt-get -y up… apt-get -y ins…
## <span style="color: #c2c2c2;"> 2</span> Debian Lin… linux debian       *       apt-get -y up… apt-get -y ins…
## <span style="color: #c2c2c2;"> 3</span> CentOS Lin… linux centos       *       <span style="color: #DC322F;">NA</span>             yum install -y
## <span style="color: #c2c2c2;"> 4</span> Rocky Linux linux rockylinux   *       <span style="color: #DC322F;">NA</span>             dnf install -y
## <span style="color: #c2c2c2;"> 5</span> Red Hat En… linux redhat       6       <span style="color: #DC322F;">NA</span>             yum install -y
## <span style="color: #c2c2c2;"> 6</span> Red Hat En… linux redhat       7       <span style="color: #DC322F;">NA</span>             yum install -y
## <span style="color: #c2c2c2;"> 7</span> Red Hat En… linux redhat       *       <span style="color: #DC322F;">NA</span>             dnf install -y
## <span style="color: #c2c2c2;"> 8</span> Fedora Lin… linux fedora       *       <span style="color: #DC322F;">NA</span>             dnf install -y
## <span style="color: #c2c2c2;"> 9</span> openSUSE L… linux opensuse     *       <span style="color: #DC322F;">NA</span>             zypper --non-i…
## <span style="color: #c2c2c2;">10</span> SUSE Linux… linux sle          *       <span style="color: #DC322F;">NA</span>             zypper --non-i…
## <span style="color: #999999;"># ℹ 1 more variable: query_command &lt;chr&gt;</span>
</pre>

</div>

Call `pak::sysreqs_is_supported()` to see if your system is supported:

``` r
pak::sysreqs_is_supported()
```

<div class="asciicast" style="color: #172431;font-family: &#39;Fira Code&#39;,Monaco,Consolas,Menlo,&#39;Bitstream Vera Sans Mono&#39;,&#39;Powerline Symbols&#39;,monospace;line-height: 1.300000">

<pre>
## [1] TRUE
</pre>

</div>

This vignette was built on Ubuntu 22.04.2 LTS, which is a platform pak
does support. So in the following you will see the output of the code.

# R package installation

If you are using pak as a superuser, on a supported platform, then pak
will look up system requirements, and install the missing ones. Here is
an example:

``` r
pak::pkg_install("RPostgres")
```

<div class="asciicast" style="color: #172431;font-family: &#39;Fira Code&#39;,Monaco,Consolas,Menlo,&#39;Bitstream Vera Sans Mono&#39;,&#39;Powerline Symbols&#39;,monospace;line-height: 1.300000">

<pre>
## <span style="color: #859900;">✔</span> Loading metadata database ... done
##
## → Will <span style="font-style: italic;">install</span> 17 packages.
## → All 17 packages (0 B) are cached.
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">DBI</span>          1.1.3
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">RPostgres</span>    1.4.5  <span style="color: #525252;"> + </span><span style="color: #DC322F;">✖</span><span style="color: #2AA198;"> libpq-dev</span>
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">Rcpp</span>         1.0.10
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">bit</span>          4.0.5
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">bit64</span>        4.0.5
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">blob</span>         1.2.4
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">cli</span>          3.6.1
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">generics</span>     0.1.3
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">glue</span>         1.6.2
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">hms</span>          1.1.3
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">lifecycle</span>    1.0.3
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">lubridate</span>    1.9.2
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">pkgconfig</span>    2.0.3
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">rlang</span>        1.1.1
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">timechange</span>   0.2.0
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">vctrs</span>        0.6.2
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">withr</span>        2.5.0
## → Will <span style="font-style: italic;">install</span> 1 system package:
## <span style="color: #525252;">+ </span><span style="color: #2AA198;">libpq-dev</span>  <span style="color: #525252;">- </span><span style="color: #268BD2;">RPostgres</span>
## <span style="color: #2AA198;">ℹ</span> No downloads are needed, 17 pkgs are cached
## <span style="color: #2AA198;">ℹ</span> Installing system requirements
## <span style="color: #2AA198;">ℹ</span> Executing `sh -c apt-get -y update`
## <span style="color: #2AA198;">ℹ</span> Executing `sh -c apt-get -y install libpq-dev`
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">DBI</span> 1.1.3  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">RPostgres</span> 1.4.5  <span style="color: #a3a3a3;">(1.2s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">Rcpp</span> 1.0.10  <span style="color: #a3a3a3;">(1.2s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">bit</span> 4.0.5  <span style="color: #a3a3a3;">(1.2s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">bit64</span> 4.0.5  <span style="color: #a3a3a3;">(148ms)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">blob</span> 1.2.4  <span style="color: #a3a3a3;">(63ms)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">cli</span> 3.6.1  <span style="color: #a3a3a3;">(88ms)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">generics</span> 0.1.3  <span style="color: #a3a3a3;">(61ms)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">glue</span> 1.6.2  <span style="color: #a3a3a3;">(63ms)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">hms</span> 1.1.3  <span style="color: #a3a3a3;">(62ms)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">lifecycle</span> 1.0.3  <span style="color: #a3a3a3;">(61ms)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">lubridate</span> 1.9.2  <span style="color: #a3a3a3;">(87ms)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">pkgconfig</span> 2.0.3  <span style="color: #a3a3a3;">(62ms)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">rlang</span> 1.1.1  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">timechange</span> 0.2.0  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">vctrs</span> 0.6.2  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">withr</span> 2.5.0  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> 1 pkg + 16 deps: added 17 <span style="color: #b8b8b8;">[17.5s]</span>
</pre>

</div>

## Running R as a regular user

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

``` r
pak::pkg_install("RPostgres?source")
```

<div class="asciicast" style="color: #172431;font-family: &#39;Fira Code&#39;,Monaco,Consolas,Menlo,&#39;Bitstream Vera Sans Mono&#39;,&#39;Powerline Symbols&#39;,monospace;line-height: 1.300000">

<pre>
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">plogr</span>       0.2.0
## <span style="color: #DC322F;">✖</span> Missing 1 system package. You'll probably need to install it
## manually:
## <span style="color: #525252;">+ </span><span style="color: #2AA198;">libpq-dev</span>  <span style="color: #525252;">- </span><span style="color: #268BD2;">RPostgres</span>
## <span style="color: #2AA198;">ℹ</span> No downloads are needed, 2 pkgs (1.47 MB) are cached
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">plogr</span> 0.2.0  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #2AA198;">ℹ</span> Building <span style="color: #268BD2;">RPostgres</span> 1.4.5
## <span style="color: #DC322F;">✖</span> Failed to build <span style="color: #268BD2;">RPostgres</span> 1.4.5
## Registered S3 methods overwritten by 'callr':
##   method                    from
##   format.callr_status_error
##   print.callr_status_error
## <span style="font-weight: bold;color: #B58900;">Error</span>:
## <span style="color: #B58900;">!</span> error in pak subprocess
## <span style="font-weight: bold;">Caused by error</span> in `stop_task_build(state, worker)`:
## <span style="color: #B58900;">!</span> Failed to build source package 'RPostgres'
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
## <span style="font-weight: bold;">&lt;stdin&gt;:1:10: </span><span style="font-weight: bold;color: #DC322F;">fatal error: libpq-fe.h: No such file or directory</span>
## <span style="font-weight: bold;color: #DC322F;">compilation terminated.</span>
## <span style="font-weight: bold;color: #DC322F;">-----------------------------------------------------------------------</span>
## <span style="font-weight: bold;color: #DC322F;">ERROR: configuration failed for package ‘RPostgres’</span>
## <span style="font-weight: bold;color: #DC322F;">* removing ‘/tmp/RtmpsOXbPZ/pkg-lib4a492949a49e/RPostgres’</span>
## <span style="font-weight: bold;color: #DC322F;">---</span>
## <span style="font-weight: bold;color: #DC322F;">Backtrace:</span>
## <span style="font-weight: bold;color: #525252;">1. </span><span style="font-weight: bold;">pak::</span><span style="font-weight: bold;color: #2AA198;">pkg_install</span><span style="font-weight: bold;color: #B58900;">("RPostgres?source")</span>
## <span style="font-weight: bold;color: #525252;">2. </span><span style="font-weight: bold;">pak:::remote(function(...) get("pkg_install_do_plan", asNamespace("pa</span>
## <span style="font-weight: bold;">k")…</span><span style="font-weight: bold;color: #525252;"> at package.R:84:3</span>
## <span style="font-weight: bold;color: #525252;">3. </span><span style="font-weight: bold;">err</span><span style="font-weight: bold;color: #859900;">$</span><span style="font-weight: bold;color: #2AA198;">throw</span><span style="font-weight: bold;color: #B58900;">(</span><span style="font-weight: bold;">res</span><span style="font-weight: bold;color: #859900;">$</span><span style="font-weight: bold;">error</span><span style="font-weight: bold;color: #B58900;">)</span><span style="font-weight: bold;color: #525252;"> at subprocess.R:115:5</span>
## <span style="font-weight: bold;">---</span>
## <span style="font-weight: bold;">Subprocess backtrace:</span>
## <span style="font-weight: bold;color: #525252;"> 1. </span><span style="font-weight: bold;">base::withCallingHandlers(cli_message = function(msg) { …</span>
## <span style="font-weight: bold;color: #525252;"> 2. </span><span style="font-weight: bold;color: #2AA198;">get</span><span style="font-weight: bold;color: #B58900;">("pkg_install_do_plan"</span><span style="font-weight: bold;">, </span><span style="font-weight: bold;color: #2AA198;">asNamespace</span><span style="font-weight: bold;color: #268BD2;">(</span><span style="font-weight: bold;color: #B58900;">"pak"</span><span style="font-weight: bold;color: #268BD2;">)</span><span style="font-weight: bold;color: #B58900;">)(</span><span style="font-weight: bold;">...</span><span style="font-weight: bold;color: #B58900;">)</span>
## <span style="font-weight: bold;color: #525252;"> 3. </span><span style="font-weight: bold;">proposal</span><span style="font-weight: bold;color: #859900;">$</span><span style="font-weight: bold;color: #2AA198;">install</span><span style="font-weight: bold;color: #B58900;">()</span>
## <span style="font-weight: bold;color: #525252;"> 4. </span><span style="font-weight: bold;">pkgdepends::install_package_plan(plan, lib = private$library, num_wo</span>
## <span style="font-weight: bold;">rkers = nw…</span>
## <span style="font-weight: bold;color: #525252;"> 5. </span><span style="font-weight: bold;">base::withCallingHandlers({ …</span>
## <span style="font-weight: bold;color: #525252;"> 6. </span><span style="font-weight: bold;">pkgdepends:::</span><span style="font-weight: bold;color: #2AA198;">handle_events</span><span style="font-weight: bold;color: #B58900;">(</span><span style="font-weight: bold;">state, events</span><span style="font-weight: bold;color: #B58900;">)</span>
## <span style="font-weight: bold;color: #525252;"> 7. </span><span style="font-weight: bold;">pkgdepends:::</span><span style="font-weight: bold;color: #2AA198;">handle_event</span><span style="font-weight: bold;color: #B58900;">(</span><span style="font-weight: bold;">state, i</span><span style="font-weight: bold;color: #B58900;">)</span>
## <span style="font-weight: bold;color: #525252;"> 8. </span><span style="font-weight: bold;">pkgdepends:::</span><span style="font-weight: bold;color: #2AA198;">stop_task</span><span style="font-weight: bold;color: #B58900;">(</span><span style="font-weight: bold;">state, worker</span><span style="font-weight: bold;color: #B58900;">)</span>
## <span style="font-weight: bold;color: #525252;"> 9. </span><span style="font-weight: bold;">pkgdepends:::</span><span style="font-weight: bold;color: #2AA198;">stop_task_build</span><span style="font-weight: bold;color: #B58900;">(</span><span style="font-weight: bold;">state, worker</span><span style="font-weight: bold;color: #B58900;">)</span>
## <span style="font-weight: bold;color: #525252;">10. </span><span style="font-weight: bold;">base::throw(new_pkg_build_error("Failed to build source package {pkg</span>
## <span style="font-weight: bold;">}", …</span>                                                                  <span style="font-weight: bold;"> </span>
## <span style="font-weight: bold;color: #525252;">11. | base::signalCondition(cond)</span>                                      <span style="font-weight: bold;"> </span>
## <span style="font-weight: bold;color: #525252;">12. </span><span style="font-weight: bold;">global (function (e) …</span>                                             <span style="font-weight: bold;"> </span>
## <span style="font-weight: bold;">Execution halted</span>                                                       <span style="font-weight: bold;"> </span>
</pre>

</div>

On the other hand, if you are installing binary packages, e.g. from the
Posit Package Manager, then the installation typically succeeds, but
then loading the package fails:

``` r
pak::pkg_install("RPostgres")
library(RPostgres)
```

<div class="asciicast" style="color: #172431;font-family: &#39;Fira Code&#39;,Monaco,Consolas,Menlo,&#39;Bitstream Vera Sans Mono&#39;,&#39;Powerline Symbols&#39;,monospace;line-height: 1.300000">

<pre>
## → Will <span style="font-style: italic;">install</span> 17 packages.
## → All 17 packages (0 B) are cached.
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">DBI</span>          1.1.3
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">RPostgres</span>    1.4.5  <span style="color: #525252;"> + </span><span style="color: #DC322F;">✖</span><span style="color: #2AA198;"> libpq-dev</span>
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">Rcpp</span>         1.0.10
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">bit</span>          4.0.5
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">bit64</span>        4.0.5
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">blob</span>         1.2.4
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">cli</span>          3.6.1
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">generics</span>     0.1.3
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">glue</span>         1.6.2
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">hms</span>          1.1.3
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">lifecycle</span>    1.0.3
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">lubridate</span>    1.9.2
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">pkgconfig</span>    2.0.3
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">rlang</span>        1.1.1
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">timechange</span>   0.2.0
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">vctrs</span>        0.6.2
## <span style="color: #525252;">+ </span><span style="color: #268BD2;">withr</span>        2.5.0
## <span style="color: #DC322F;">✖</span> Missing 1 system package. You'll probably need to install it
## manually:
## <span style="color: #525252;">+ </span><span style="color: #2AA198;">libpq-dev</span>  <span style="color: #525252;">- </span><span style="color: #268BD2;">RPostgres</span>
## <span style="color: #2AA198;">ℹ</span> No downloads are needed, 17 pkgs are cached
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">DBI</span> 1.1.3  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">RPostgres</span> 1.4.5  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">Rcpp</span> 1.0.10  <span style="color: #a3a3a3;">(1.2s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">bit</span> 4.0.5  <span style="color: #a3a3a3;">(1.2s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">bit64</span> 4.0.5  <span style="color: #a3a3a3;">(144ms)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">blob</span> 1.2.4  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">cli</span> 3.6.1  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">generics</span> 0.1.3  <span style="color: #a3a3a3;">(90ms)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">glue</span> 1.6.2  <span style="color: #a3a3a3;">(87ms)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">hms</span> 1.1.3  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">lifecycle</span> 1.0.3  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">lubridate</span> 1.9.2  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">pkgconfig</span> 2.0.3  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">rlang</span> 1.1.1  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">timechange</span> 0.2.0  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">vctrs</span> 0.6.2  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">withr</span> 2.5.0  <span style="color: #a3a3a3;">(1.1s)</span>
## <span style="color: #859900;">✔</span> 1 pkg + 16 deps: added 17 <span style="color: #b8b8b8;">[11.6s]</span>
## Error: package or namespace load failed for ‘RPostgres’ in dyn.load(file
## , DLLpath = DLLpath, ...):
##  unable to load shared object '/tmp/RtmpWqZycA/lib/RPostgres/libs/RPostg
## res.so':
##   libpq.so.5: cannot open shared object file: No such file or directory
## Execution halted
</pre>

</div>

# Query system requirements without installation

If you only want to query system requirements, without installing any
packages, use the `pkg_sysreqs()` function. This is similar to
`pkg_deps()` but in addition to looking up package dependencies, it also
looks up system dependencies, and only reports the latter:

``` r
pak::pkg_sysreqs(c("curl", "xml2", "devtools", "CHRONOS"))
```

<div class="asciicast" style="color: #172431;font-family: &#39;Fira Code&#39;,Monaco,Consolas,Menlo,&#39;Bitstream Vera Sans Mono&#39;,&#39;Powerline Symbols&#39;,monospace;line-height: 1.300000">

<pre>
## <span style="color: #859900;">✔</span> Loading metadata database ... done
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
</pre>

</div>

See the manual of `pkg_sysreqs()` to see how to programmatically extract
information from its return value.

# Other queries

In addition to the automatic system package lookup and installation, pak
also has some other functions to help you with system dependencies. The
`sysreqs_db_list()` function lists all system requirements pak knows
about.

``` r
pak::sysreqs_db_list()
```

<div class="asciicast" style="color: #172431;font-family: &#39;Fira Code&#39;,Monaco,Consolas,Menlo,&#39;Bitstream Vera Sans Mono&#39;,&#39;Powerline Symbols&#39;,monospace;line-height: 1.300000">

<pre>
## <span style="color: #999999;"># A data frame: 106 × 5</span>
##    name       patterns  packages  pre_install post_install
##    <span style="font-style: italic;color: #999999;">&lt;chr&gt;</span>      <span style="font-style: italic;color: #999999;">&lt;list&gt;</span>    <span style="font-style: italic;color: #999999;">&lt;list&gt;</span>    <span style="font-style: italic;color: #999999;">&lt;list&gt;</span>      <span style="font-style: italic;color: #999999;">&lt;list&gt;</span>
## <span style="color: #c2c2c2;"> 1</span> QuantLib   <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;NULL&gt;</span>      <span style="color: #999999;">&lt;NULL&gt;</span>
## <span style="color: #c2c2c2;"> 2</span> apparmor   <span style="color: #999999;">&lt;chr [2]&gt;</span> <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;NULL&gt;</span>      <span style="color: #999999;">&lt;NULL&gt;</span>
## <span style="color: #c2c2c2;"> 3</span> atk        <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;NULL&gt;</span>      <span style="color: #999999;">&lt;NULL&gt;</span>
## <span style="color: #c2c2c2;"> 4</span> automake   <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;NULL&gt;</span>      <span style="color: #999999;">&lt;NULL&gt;</span>
## <span style="color: #c2c2c2;"> 5</span> berkeleydb <span style="color: #999999;">&lt;chr [2]&gt;</span> <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;NULL&gt;</span>      <span style="color: #999999;">&lt;NULL&gt;</span>
## <span style="color: #c2c2c2;"> 6</span> blender    <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;NULL&gt;</span>      <span style="color: #999999;">&lt;NULL&gt;</span>
## <span style="color: #c2c2c2;"> 7</span> bowtie2    <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;NULL&gt;</span>      <span style="color: #999999;">&lt;NULL&gt;</span>
## <span style="color: #c2c2c2;"> 8</span> bwidget    <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;NULL&gt;</span>      <span style="color: #999999;">&lt;NULL&gt;</span>
## <span style="color: #c2c2c2;"> 9</span> cairo      <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;NULL&gt;</span>      <span style="color: #999999;">&lt;NULL&gt;</span>
## <span style="color: #c2c2c2;">10</span> chrome     <span style="color: #999999;">&lt;chr [1]&gt;</span> <span style="color: #999999;">&lt;NULL&gt;</span>    <span style="color: #999999;">&lt;chr [3]&gt;</span>   <span style="color: #999999;">&lt;chr [1]&gt;</span>
## <span style="color: #999999;"># ℹ 96 more rows</span>
</pre>

</div>

`sysreqs_db_match()` manually matches `SystemRequirements` fields
againts these system requirements:

``` r
sq <- pak::sysreqs_db_match("Needs libcurl and also Java.")
sq
```

<div class="asciicast" style="color: #172431;font-family: &#39;Fira Code&#39;,Monaco,Consolas,Menlo,&#39;Bitstream Vera Sans Mono&#39;,&#39;Powerline Symbols&#39;,monospace;line-height: 1.300000">

<pre>
## [[1]]
## <span style="color: #999999;"># A data frame: 2 × 5</span>
##   spec                         sysreq  packages pre_install post_install
##   <span style="font-style: italic;color: #999999;">&lt;chr&gt;</span>                        <span style="font-style: italic;color: #999999;">&lt;chr&gt;</span>   <span style="font-style: italic;color: #999999;">&lt;list&gt;</span>   <span style="font-style: italic;color: #999999;">&lt;list&gt;</span>      <span style="font-style: italic;color: #999999;">&lt;list&gt;</span>
## <span style="color: #c2c2c2;">1</span> Needs libcurl and also Java. java    <span style="color: #999999;">&lt;chr&gt;</span>    <span style="color: #999999;">&lt;NULL&gt;</span>      <span style="color: #999999;">&lt;chr [1]&gt;</span>
## <span style="color: #c2c2c2;">2</span> Needs libcurl and also Java. libcurl <span style="color: #999999;">&lt;chr&gt;</span>    <span style="color: #999999;">&lt;NULL&gt;</span>      <span style="color: #999999;">&lt;NULL&gt;</span>
##
</pre>

</div>

``` r
sq[[1]]$packages
```

<div class="asciicast" style="color: #172431;font-family: &#39;Fira Code&#39;,Monaco,Consolas,Menlo,&#39;Bitstream Vera Sans Mono&#39;,&#39;Powerline Symbols&#39;,monospace;line-height: 1.300000">

<pre>
## [[1]]
## [1] "default-jdk"
##
## [[2]]
## [1] "libcurl4-openssl-dev"
##
</pre>

</div>

You can also use it to query system requirements for other platfosm:

``` r
sqrhel9 <- pak::sysreqs_db_match("Needs libcurl and also Java.", "redhat-9")
sqrhel9
```

<div class="asciicast" style="color: #172431;font-family: &#39;Fira Code&#39;,Monaco,Consolas,Menlo,&#39;Bitstream Vera Sans Mono&#39;,&#39;Powerline Symbols&#39;,monospace;line-height: 1.300000">

<pre>
## [[1]]
## <span style="color: #999999;"># A data frame: 2 × 5</span>
##   spec                         sysreq  packages pre_install post_install
##   <span style="font-style: italic;color: #999999;">&lt;chr&gt;</span>                        <span style="font-style: italic;color: #999999;">&lt;chr&gt;</span>   <span style="font-style: italic;color: #999999;">&lt;list&gt;</span>   <span style="font-style: italic;color: #999999;">&lt;list&gt;</span>      <span style="font-style: italic;color: #999999;">&lt;list&gt;</span>
## <span style="color: #c2c2c2;">1</span> Needs libcurl and also Java. java    <span style="color: #999999;">&lt;chr&gt;</span>    <span style="color: #999999;">&lt;NULL&gt;</span>      <span style="color: #999999;">&lt;chr [1]&gt;</span>
## <span style="color: #c2c2c2;">2</span> Needs libcurl and also Java. libcurl <span style="color: #999999;">&lt;chr&gt;</span>    <span style="color: #999999;">&lt;NULL&gt;</span>      <span style="color: #999999;">&lt;NULL&gt;</span>
##
</pre>

</div>

``` r
sqrhel9[[1]]$packages
```

<div class="asciicast" style="color: #172431;font-family: &#39;Fira Code&#39;,Monaco,Consolas,Menlo,&#39;Bitstream Vera Sans Mono&#39;,&#39;Powerline Symbols&#39;,monospace;line-height: 1.300000">

<pre>
## [[1]]
## [1] "java-11-openjdk-devel"
##
## [[2]]
## [1] "libcurl-devel"
##
</pre>

</div>

`sysreqs_list_system_packages()` is a cross-platform way of listing all
installed system packages and capabilities:

``` r
pak::sysreqs_list_system_packages()
```

<div class="asciicast" style="color: #172431;font-family: &#39;Fira Code&#39;,Monaco,Consolas,Menlo,&#39;Bitstream Vera Sans Mono&#39;,&#39;Powerline Symbols&#39;,monospace;line-height: 1.300000">

<pre>
## <span style="color: #999999;"># A data frame: 433 × 4</span>
##    status package         version         provides
##    <span style="font-style: italic;color: #999999;">&lt;chr&gt;</span>  <span style="font-style: italic;color: #999999;">&lt;chr&gt;</span>           <span style="font-style: italic;color: #999999;">&lt;chr&gt;</span>           <span style="font-style: italic;color: #999999;">&lt;list&gt;</span>
## <span style="color: #c2c2c2;"> 1</span> ii     adduser         3.118ubuntu5    <span style="color: #999999;">&lt;chr [0]&gt;</span>
## <span style="color: #c2c2c2;"> 2</span> ii     apt             2.4.8           <span style="color: #999999;">&lt;chr [1]&gt;</span>
## <span style="color: #c2c2c2;"> 3</span> ii     autoconf        2.71-2          <span style="color: #999999;">&lt;chr [0]&gt;</span>
## <span style="color: #c2c2c2;"> 4</span> ii     automake        1:1.16.5-1.3    <span style="color: #999999;">&lt;chr [2]&gt;</span>
## <span style="color: #c2c2c2;"> 5</span> ii     autotools-dev   20220109.1      <span style="color: #999999;">&lt;chr [0]&gt;</span>
## <span style="color: #c2c2c2;"> 6</span> ii     base-files      12ubuntu4.3     <span style="color: #999999;">&lt;chr [1]&gt;</span>
## <span style="color: #c2c2c2;"> 7</span> ii     base-passwd     3.5.52build1    <span style="color: #999999;">&lt;chr [0]&gt;</span>
## <span style="color: #c2c2c2;"> 8</span> ii     bash            5.1-6ubuntu1    <span style="color: #999999;">&lt;chr [0]&gt;</span>
## <span style="color: #c2c2c2;"> 9</span> ii     binutils        2.38-4ubuntu2.1 <span style="color: #999999;">&lt;chr [2]&gt;</span>
## <span style="color: #c2c2c2;">10</span> ii     binutils-common 2.38-4ubuntu2.1 <span style="color: #999999;">&lt;chr [0]&gt;</span>
## <span style="color: #999999;"># ℹ 423 more rows</span>
</pre>

</div>

`sysreqs_check_installed()` is a handy function that checks if all
system requirements are installed for some or all R packages that are
installed in your library:

``` r
pak::sysreqs_check_installed()
```

<div class="asciicast" style="color: #172431;font-family: &#39;Fira Code&#39;,Monaco,Consolas,Menlo,&#39;Bitstream Vera Sans Mono&#39;,&#39;Powerline Symbols&#39;,monospace;line-height: 1.300000">

<pre>
## system package       installed required by
## --------------       --        -----------
## git                  <span style="color: #859900;">✔</span>         gitcreds
## gsfonts              <span style="color: #859900;">✔</span>         magick
## imagemagick          <span style="color: #859900;">✔</span>         magick
## libarchive-dev       <span style="color: #859900;">✔</span>         archive
## libcurl4-openssl-dev <span style="color: #859900;">✔</span>         curl
## libfontconfig1-dev   <span style="color: #859900;">✔</span>         systemfonts
## libfreetype6-dev     <span style="color: #859900;">✔</span>         ragg, systemfonts, textshaping
## libfribidi-dev       <span style="color: #859900;">✔</span>         textshaping
## libharfbuzz-dev      <span style="color: #859900;">✔</span>         textshaping
## libicu-dev           <span style="color: #859900;">✔</span>         stringi
## libjpeg-dev          <span style="color: #859900;">✔</span>         ragg
## libmagick++-dev      <span style="color: #859900;">✔</span>         magick
## libnode-dev          <span style="color: #859900;">✔</span>         V8
## libpng-dev           <span style="color: #859900;">✔</span>         ragg
## libpq-dev            <span style="color: #DC322F;">✖</span>         RPostgres
## libssl-dev           <span style="color: #859900;">✔</span>         curl, openssl
## libtiff-dev          <span style="color: #859900;">✔</span>         ragg
## libxml2-dev          <span style="color: #859900;">✔</span>         xml2
## make                 <span style="color: #859900;">✔</span>         fs, sass
## pandoc               <span style="color: #859900;">✔</span>         knitr, rmarkdown
## zlib1g-dev           <span style="color: #859900;">✔</span>         data.table
</pre>

</div>

`sysreqs_fix_installed()` goes one step further and also tries to
install the missing system requirements.

# Build-time and run-time dependencies

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

# How it works

pak uses the database of system requirements at
<https://github.com/rstudio/r-system-requirements>. It has its own copy
of the database embedded into the package, and it also tries to download
updated versions of the database from GitHub, if its current copy is
older than one day. You can explicitly update the database from GitHub
using the `sysreqs_db_update()` function.

For CRAN packages, it downloads the `SystemRequirements` fields from
`https://cran.r-pkg.org/metadata`, which is a database updated daily.
For Bioconductor packages, it downloads then from GitHub. (We are
planning on moving CRAN database to GitHub as well.)

For packages sources that require pak to obtain a package `DESCRIPTION`
file (e.g. `github::`, `git::`, etc.), pak obtains `SystemRequirements`
directly from the `DESCRIPTION` file.

Once having the `SystemRequirements` fields, pak matches them to the
database, to obtain the cacnonized list of system requirements.

Then pak queries the local platform, to see the exact system packages
needed. It also queries the installed system packages, to avoid trying
to install system packages that are already installed.

# Configuration

There are several pak configuration options you can use to adjust how
system requirements are handled. We will list some of them here, please
see the options with a `sysreqs` prefix in the `?pak-config` manual page
for a complete and current list.

  - `sysreqs`: whether to install system requirements. The default is
    `TRUE` if the platform is supported and the user can install system
    packages, either because it is the superuser, or via `sudo`. If it
    is `FALSE` (or the user cannot install system packages), but the
    platform is supported, system requirements are printed, but not
    installed.
  - `sysreqs_db_update`: whether to try to update the system
    requirements database from GitHub.
  - `sysreqs_db_update_timeout`: timeout for the system requirements
    update from GitHub.
  - `sysreqs_dry_run`: if `TRUE` then pak only prints the install
    commands, but does not actually run them.
  - `sysreqs_platform`: the platform name to use for determining system
    requirements. Defaults to the current platform. If you are using a
    Linux distribution that is compatible with some distribution that
    pak supports, then you can set this option manually. E.g.
    Ubuntu-based distros can set it to `ubuntu-22.04`, or the
    appropriate Ubuntu version.
  - `sysreqs_sudo`: whether to use `sudo` to install system packages. If
    this is not set, then pak tries to auto-detect if `sudo` is needed
    or not.
  - `sysreqs_update`: whether to try to update system packages that are
    already installed. pak does not know which version of a system
    package is required, and it does not try to update system packages
    by default. If you think that you need newer system packages, then
    you can set this option to `TRUE`.
  - `sysreqs_verbose`: whether to print the output of the system package
    installation commands. Useful for debugging, and it is `TRUE` by
    default in a CI environment.

# About other OSes

## Windows

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

## macOS

pak does not currently have system requirement information for macOS.
macOS is similar to Windows, in that most repositories will serve
statically linked macOS binary packages that do not need system
software.

If you do need to compile packages from source, then you possibly need
to install some sytem libraries, either via Homebrew, or by downloading
CRAN’s static library builds from <https://mac.r-project.org/bin/>

We are planning on adding better macOS system software support to pak in
the future.
