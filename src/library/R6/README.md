R6: Encapsulated object-oriented programming for R <img src='man/figures/logo.png' align="right" height="138.5" />
==================================================

  <!-- badges: start -->
  [![R-CMD-check](https://github.com/r-lib/R6/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/R6/actions)
  <!-- badges: end -->

R6 is an implemention of encapsulated object-oriented programming for R, and is a simpler, faster, lighter-weight alternative to R's built-in reference classes. This style of programming is also sometimes referred to as classical object-oriented programming.

Some features of R6:

* R6 objects have reference semantics.
* R6 cleanly supports inheritance across packages.
* R6 classes have public and private members.

In contrast to R's reference classes, R6 is not built on the S4 class system, so it does not require the *methods* package. Unlike reference classes, R6 classes can be cleanly inherited across different packages.

See the [Introduction](https://r6.r-lib.org/articles/Introduction.html) article for usage examples.


## Installation

To install R6 from CRAN:

```R
install.packages('R6')
```

To install the development version (requires the devtools package):

```R
devtools::install_github('r-lib/R6', build_vignettes = FALSE)
```


## Documentation

* [Introduction to R6](https://r6.r-lib.org/articles/Introduction.html)
* [Debugging methods in R6 objects](https://r6.r-lib.org/articles/Debugging.html)
* [Performance tests](https://r6.r-lib.org/articles/Performance.html) - Speed and memory comparisons of R6 classes and reference classes.
* [Portable R6 classes](https://r6.r-lib.org/articles/Portable.html) - Inheritance across different packages.


### Why R6?

Why the name R6? When R's reference classes were introduced, some users, following the names of R's existing class systems S3 and S4, called the new class system R5 in jest. Although reference classes are not actually called R5, the name of this package and its classes takes inspiration from that name.

The name R5 was also a code-name used for a different object system started by Simon Urbanek, meant to solve some issues with S4 relating to syntax and performance. However, the R5 branch was shelved after a little development, and it was never released.
