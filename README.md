
# pak

> A Fresh Approach to R Package Installation

<!-- badges: start -->

![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg) [![](https://www.r-pkg.org/badges/version/pak)](https://cran.r-project.org/package=pak) [![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/pak)](https://www.r-pkg.org/pkg/pak) [![Codecov test coverage](https://codecov.io/gh/r-lib/pak/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/pak?branch=main) [![R-CMD-check](https://github.com/r-lib/pak/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/pak/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

pak installs R packages from CRAN, Bioconductor, GitHub, URLs, local files and directories. It is an alternative to `install.packages()` and `devtools::install_github()`. pak is fast, safe and convenient.

- <a href="#rocket-short-tour" id="toc-rocket-short-tour">:rocket: Short
  tour</a>
- <a href="#link-quick-links-start-here-if-in-doubt"
  id="toc-link-quick-links-start-here-if-in-doubt">:link: Quick links
  (start here if in doubt!)</a>
- <a href="#sparkles-features" id="toc-sparkles-features">:sparkles:
  Features</a>
- <a href="#arrow_down-installation"
  id="toc-arrow_down-installation">:arrow_down: Installation</a>
- <a href="#blue_book-license"
  id="toc-blue_book-license"><strong>:blue_book:</strong> License</a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

## :rocket: Short tour

#### Install or update packages from CRAN or Bioconductor

``` r
pak::pkg_install("tibble")
```

<div class="asciicast"
style="color: #172431;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000">

<pre>
#> <span style="color: #859900;">✔</span> Loading metadata database ... done                                            
#>                                                                                 
#> → Will <span style="font-style: italic;">install</span> 11 packages.                                                     
#> → All 11 packages (7.48 MB) are cached.                                         
#> <span style="color: #525252;">+ </span><span style="color: #268BD2;">cli</span>         3.4.1                                                             
#> <span style="color: #525252;">+ </span><span style="color: #268BD2;">fansi</span>       1.0.3                                                             
#> <span style="color: #525252;">+ </span><span style="color: #268BD2;">glue</span>        1.6.2                                                             
#> <span style="color: #525252;">+ </span><span style="color: #268BD2;">lifecycle</span>   1.0.3                                                             
#> <span style="color: #525252;">+ </span><span style="color: #268BD2;">magrittr</span>    2.0.3                                                             
#> <span style="color: #525252;">+ </span><span style="color: #268BD2;">pillar</span>      1.8.1                                                             
#> <span style="color: #525252;">+ </span><span style="color: #268BD2;">pkgconfig</span>   2.0.3                                                             
#> <span style="color: #525252;">+ </span><span style="color: #268BD2;">rlang</span>       1.0.6                                                             
#> <span style="color: #525252;">+ </span><span style="color: #268BD2;">tibble</span>      3.1.8                                                             
#> <span style="color: #525252;">+ </span><span style="color: #268BD2;">utf8</span>        1.2.2                                                             
#> <span style="color: #525252;">+ </span><span style="color: #268BD2;">vctrs</span>       0.5.1                                                             
#> <span style="color: #2AA198;">ℹ</span> No downloads are needed, 11 pkgs (7.48 MB) are cached                         
#> <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">cli</span> 3.4.1  <span style="color: #a3a3a3;">(68ms)</span>                                                   
#> <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">fansi</span> 1.0.3  <span style="color: #a3a3a3;">(78ms)</span>                                                 
#> <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">glue</span> 1.6.2  <span style="color: #a3a3a3;">(95ms)</span>                                                  
#> <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">lifecycle</span> 1.0.3  <span style="color: #a3a3a3;">(120ms)</span>                                            
#> <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">magrittr</span> 2.0.3  <span style="color: #a3a3a3;">(126ms)</span>                                             
#> <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">pkgconfig</span> 2.0.3  <span style="color: #a3a3a3;">(125ms)</span>                                            
#> <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">pillar</span> 1.8.1  <span style="color: #a3a3a3;">(154ms)</span>                                               
#> <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">rlang</span> 1.0.6  <span style="color: #a3a3a3;">(176ms)</span>                                                
#> <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">tibble</span> 3.1.8  <span style="color: #a3a3a3;">(77ms)</span>                                                
#> <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">utf8</span> 1.2.2  <span style="color: #a3a3a3;">(44ms)</span>                                                  
#> <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">vctrs</span> 0.5.1  <span style="color: #a3a3a3;">(35ms)</span>                                                 
#> <span style="color: #859900;">✔</span> 1 pkg + 10 deps: added 11 <span style="color: #b8b8b8;">[2.8s]</span>                                              
</pre>

</div>

#### Install packages from GitHub

``` r
pak::pkg_install("tidyverse/tibble")
```

<div class="asciicast"
style="color: #172431;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000">

<pre>
#>                                                                                 
#> → Will <span style="font-style: italic;">update</span> 2 packages.                                                       
#> → All 2 packages (0 B) are cached.                                              
#> <span style="color: #525252;">+ </span><span style="color: #268BD2;">tibble</span> 3.1.8 → 3.1.8<span style="font-weight: bold;">.9002</span> 👷🏾🔧 (GitHub: 37ec86a)                               
#> <span style="color: #525252;">+ </span><span style="color: #268BD2;">vctrs</span>  0.5.1 → 0.5.1<span style="font-weight: bold;">.9000</span> 👷🏼‍♂️🔧 (GitHub: 48794fd)                            
#> <span style="color: #2AA198;">ℹ</span> No downloads are needed, 2 pkgs are cached                                    
#> <span style="color: #2AA198;">ℹ</span> Packaging <span style="color: #268BD2;">vctrs</span> 0.5.1.9000                                                    
#> <span style="color: #859900;">✔</span> Packaged <span style="color: #268BD2;">vctrs</span> 0.5.1.9000 <span style="color: #a3a3a3;">(1.5s)</span>                                              
#> <span style="color: #2AA198;">ℹ</span> Building <span style="color: #268BD2;">vctrs</span> 0.5.1.9000                                                     
#> <span style="color: #859900;">✔</span> Built <span style="color: #268BD2;">vctrs</span> 0.5.1.9000 <span style="color: #a3a3a3;">(11s)</span>                                                  
#> <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">vctrs</span> 0.5.1.9000 (github::r-lib/vctrs@48794fd) <span style="color: #a3a3a3;">(36ms)</span>               
#> <span style="color: #2AA198;">ℹ</span> Packaging <span style="color: #268BD2;">tibble</span> 3.1.8.9002                                                   
#> <span style="color: #859900;">✔</span> Packaged <span style="color: #268BD2;">tibble</span> 3.1.8.9002 <span style="color: #a3a3a3;">(525ms)</span>                                            
#> <span style="color: #2AA198;">ℹ</span> Building <span style="color: #268BD2;">tibble</span> 3.1.8.9002                                                    
#> <span style="color: #859900;">✔</span> Built <span style="color: #268BD2;">tibble</span> 3.1.8.9002 <span style="color: #a3a3a3;">(3.1s)</span>                                                
#> <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">tibble</span> 3.1.8.9002 (github::tidyverse/tibble@37ec86a) <span style="color: #a3a3a3;">(34ms)</span>         
#> <span style="color: #859900;">✔</span> 1 pkg + 10 deps: kept 9, upd 2 <span style="color: #b8b8b8;">[18.9s]</span>                                        
</pre>

</div>

#### Look up dependencies

``` r
pak::pkg_deps_tree("tibble")
```

<div class="asciicast"
style="color: #172431;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000">

<pre>
#> <span style="font-weight: bold;font-style: italic;color: #2AA198;">tibble </span><span style="font-weight: bold;font-style: italic;color: #525252;">3.1.8</span> <span style="color: #859900;">✨</span>                                                                  
#> ├─fansi <span style="color: #525252;">1.0.3</span> <span style="color: #859900;">✨</span>                                                                 
#> ├─lifecycle <span style="color: #525252;">1.0.3</span> <span style="color: #859900;">✨</span>                                                             
#> │ ├─cli <span style="color: #525252;">3.4.1</span> <span style="color: #859900;">✨</span>                                                                 
#> │ ├─glue <span style="color: #525252;">1.6.2</span> <span style="color: #859900;">✨</span>                                                                
#> │ └─rlang <span style="color: #525252;">1.0.6</span> <span style="color: #859900;">✨</span>                                                               
#> ├─magrittr <span style="color: #525252;">2.0.3</span> <span style="color: #859900;">✨</span>                                                              
#> ├─pillar <span style="color: #525252;">1.8.1</span> <span style="color: #859900;">✨</span>                                                                
#> │ ├─cli                                                                         
#> │ ├─fansi                                                                       
#> │ ├─glue                                                                        
#> │ ├─lifecycle                                                                   
#> │ ├─rlang                                                                       
#> │ ├─utf8 <span style="color: #525252;">1.2.2</span> <span style="color: #859900;">✨</span>                                                                
#> │ └─vctrs <span style="color: #525252;">0.5.1</span> <span style="color: #859900;">✨</span>                                                               
#> │   ├─cli                                                                       
#> │   ├─glue                                                                      
#> │   ├─lifecycle                                                                 
#> │   └─rlang                                                                     
#> ├─pkgconfig <span style="color: #525252;">2.0.3</span> <span style="color: #859900;">✨</span>                                                             
#> ├─rlang                                                                         
#> └─vctrs                                                                         
#>                                                                                 
#> Key:  <span style="color: #859900;">✨</span> new                                                                     
</pre>

</div>

#### Explain dependencies

``` r
pak::pkg_deps_explain("tibble", "rlang")
```

<div class="asciicast"
style="color: #172431;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000">

<pre>
#> tibble -&gt; lifecycle -&gt; rlang                                                    
#> tibble -&gt; pillar -&gt; lifecycle -&gt; rlang                                          
#> tibble -&gt; pillar -&gt; rlang                                                       
#> tibble -&gt; pillar -&gt; vctrs -&gt; lifecycle -&gt; rlang                                 
#> tibble -&gt; pillar -&gt; vctrs -&gt; rlang                                              
#> tibble -&gt; rlang                                                                 
#> tibble -&gt; vctrs -&gt; lifecycle -&gt; rlang                                           
#> tibble -&gt; vctrs -&gt; rlang                                                        
</pre>

</div>

#### Install a local package and its dependencies

``` r
pak::local_install("cli")
```

<div class="asciicast"
style="color: #172431;font-family: 'Fira Code',Monaco,Consolas,Menlo,'Bitstream Vera Sans Mono','Powerline Symbols',monospace;line-height: 1.300000">

<pre>
#>                                                                                 
#> → Will <span style="font-style: italic;">update</span> 1 package.                                                        
#> → The package (0 B) is cached.                                                  
#> <span style="color: #525252;">+ </span><span style="color: #268BD2;">cli</span> 3.4.1 → 3.4.1 👷🏿🔧                                                         
#> <span style="color: #2AA198;">ℹ</span> No downloads are needed, 1 pkg is cached                                      
#> <span style="color: #859900;">✔</span> Got <span style="color: #268BD2;">cli</span> 3.4.1 (source) (96 B)                                                 
#> <span style="color: #2AA198;">ℹ</span> Packaging <span style="color: #268BD2;">cli</span> 3.4.1                                                           
#> <span style="color: #859900;">✔</span> Packaged <span style="color: #268BD2;">cli</span> 3.4.1 <span style="color: #a3a3a3;">(664ms)</span>                                                    
#> <span style="color: #2AA198;">ℹ</span> Building <span style="color: #268BD2;">cli</span> 3.4.1                                                            
#> <span style="color: #859900;">✔</span> Built <span style="color: #268BD2;">cli</span> 3.4.1 <span style="color: #a3a3a3;">(5s)</span>                                                          
#> <span style="color: #859900;">✔</span> Installed <span style="color: #268BD2;">cli</span> 3.4.1 (local) <span style="color: #a3a3a3;">(42ms)</span>                                            
#> <span style="color: #859900;">✔</span> 1 pkg: upd 1, dld 1 (NA B) <span style="color: #b8b8b8;">[6.4s]</span>                                             
</pre>

</div>

## :link: Quick links (start here if in doubt!)

### How do I … ?

Start with at [*Get
Started with pak*](https://pak.r-lib.org/dev/reference/get-started.html) to
solve specific issues.

### FAQ

Check out [list of frequently asked
questions](https://pak.r-lib.org/dev/reference/faq.html).

### Reference

[The complete reference of pak
functions](https://pak.r-lib.org/dev/reference/) is the most complete
source of information about pak.

### I have a(nother) question

Don’t hesitate to ask at the [RStudio Community
forum](https://community.rstudio.com/). Use the `pak` tag.

### I would like to report a bug

Head to the [pak issue tracker](https://github.com/r-lib/pak/issues).

## :sparkles: Features

:zap: Fast - parallel downloads and installation, caching, etc.

:safety_vest: Safe - dependency solver, system dependency solver, etc.

:convenience_store: Convenient - packages from multiple sources, time
travel, etc.

See the [complete list of awesome
features](https://pak.r-lib.org/dev/reference/features.html).

## [:arrow_down:](https://github.com/r-lib/rig#%EF%B8%8F--installation) Installation

### Pre-built binaries

Install a binary build of pak from our repository on GitHub:

``` r
install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))
```

This is supported for the following systems:

| OS                 | CPU     | R version         |
|--------------------|---------|-------------------|
| Linux              | x86_64  | R 3.4.0 - R-devel |
| Linux              | aarch64 | R 3.4.0 - R-devel |
| macOS High Sierra+ | x86_64  | R 3.4.0 - R-devel |
| macOS Big Sur+     | aarch64 | R 4.1.0 - R-devel |
| Windows            | x86_64  | R 3.4.0 - R-devel |

For macOS we only support the official CRAN R build. Other builds, e.g.
Homebrew R, are not supported.

### Install from CRAN

Install the released version of the package from CRAN as usual:

``` r
install.packages("pak")
```

This potentially needs a C compiler on platforms CRAN does not have
binaries packages for.

### Other platforms and nightly builds

See the [installation
page](https://pak.r-lib.org/dev/reference/install.html)!

## **:blue_book:** License

GPL-3 © RStudio
