
## 🚀 crayon is now superseded by the cli package. 🚀

> Please use [cli](https://github.com/r-lib/cli) for new projects.
>
> crayon is still supported and will receive important bug fixes,
> but no new features.

<h1 align="center">
    <br>
    <br>
    <img width="400" src="https://user-images.githubusercontent.com/660288/102484487-41cd2900-405e-11eb-87d4-65793ad9db6a.png" alt="crayon">
    <br>
    <br>
    <br>
</h1>

> Stylish terminal output in R

<!-- badges: start -->

[![Lifecycle: superseded](https://img.shields.io/badge/lifecycle-superseded-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#superseded)
[![R build status](https://github.com/r-lib/crayon/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/crayon/actions)
[![](https://www.r-pkg.org/badges/version/crayon)](https://r-pkg.org/pkg/crayon)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/crayon)](https://r-pkg.org/pkg/crayon)
[![Coverage Status](https://img.shields.io/codecov/c/github/r-lib/crayon/master.svg)](https://codecov.io/github/r-lib/crayon?branch=master)

<!-- badges: end -->

With crayon it is easy to add color to terminal output, create styles for notes, warnings, errors; and combine styles.

ANSI color support is automatically detected and used. Crayon was largely
inspired by [chalk](https://github.com/chalk/chalk).

## Installation

```r
install.packages("crayon")
```

## Styles

Crayon defines several styles that can be combined. Each style in the list
has a corresponding function with the same name.

### General styles

* `reset`
* `bold`
* `blurred` (usually called `dim`, renamed to avoid name clash)
* `italic` (not widely supported)
* `underline`
* `inverse`
* `hidden`
* `strikethrough` (not widely supported)

### Text colors

* `black`
* `red`
* `green`
* `yellow`
* `blue`
* `magenta`
* `cyan`
* `white`
* `silver` (usually called `gray`, renamed to avoid name clash)

### Background colors

* `bgBlack`
* `bgRed`
* `bgGreen`
* `bgYellow`
* `bgBlue`
* `bgMagenta`
* `bgCyan`
* `bgWhite`

### Screenshot on OSX

![](https://user-images.githubusercontent.com/660288/102484516-4d205480-405e-11eb-93fa-37a6cd6d4066.png)

## Usage

The styling functions take any number of character vectors as arguments,
and they concatenate and style them:

```r
library(crayon)
cat(blue("Hello", "world!\n"))
```

Crayon defines the `%+%` string concatenation operator to make it easy
to assemble strings with different styles.

```r
cat("... to highlight the " %+% red("search term") %+% " in a block of text\n")
```

Styles can be combined using the `$` operator:

```r
cat(yellow$bgMagenta$bold('Hello world!\n'))
```

Styles can also be nested, and then inner style takes precedence:

```r
cat(green(
  'I am a green line ' %+%
  blue$underline$bold('with a blue substring') %+%
  ' that becomes green again!\n'
))
```
  
It is easy to define your own themes:

```r
error <- red $ bold
warn <- magenta $ underline
note <- cyan
cat(error("Error: subscript out of bounds!\n"))
cat(warn("Warning: shorter argument was recycled.\n"))
cat(note("Note: no such directory.\n"))
```

## 256 colors

Most modern terminals support the ANSI standard for 256 colors,
and you can define new styles that make use of them. The `make_style`
function defines a new style. It can handle R's built in color names
(see the output of `colors()`) as well as RGB specifications via the
`rgb()` function. It automatically chooses the ANSI colors that
are closest to the specified R and RGB colors, and it also has
a fallback to terminals with 8 ANSI colors only.

```r
ivory <- make_style("ivory")
bgMaroon <- make_style("maroon", bg = TRUE)
fancy <- combine_styles(ivory, bgMaroon)
cat(fancy("This will have some fancy colors"), "\n")
```

![](https://user-images.githubusercontent.com/660288/102484539-53aecc00-405e-11eb-9f24-85b4c10b5e38.png)

## License

MIT @ Gábor Csárdi
