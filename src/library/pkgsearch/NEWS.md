# pkgsearch (development version)

* pkgsearch now uses the `timeout` option to set the limit for the total
  time of each HTTP request (#125, @gladkia).

# pkgsearch 3.1.3

* No user visible changes.

# pkgsearch 3.1.2

* `cran_new()` works again.

# pkgsearch 3.1.1

* pkgsearch gives nicer error messages now.

# pkgsearch 3.1.0

* pkgsearch functions return data frames now, instead of tibbles.
  The data frames have a `tbl` class, so they are still printed the
  same way as tibbles, as long as the pillar package is available.
  Otherwise they behave as data frames.

* New `cran_new()` function to query new packages on CRAN.

# pkgsearch 3.0.3

* Fix dependency handling in the add-in (@salim-b, #101)

* pkgsearch uses curl now for the HTTP calls, instead of httr, which makes
  it a bit more lightweight.

# pkgsearch 3.0.2

* The RStudio addin now gives a better error more missing dependencies
  (#84, @yonicd)

* `cran_package_history()` now errors for non-existing packages, instead
  of returning `NULL` or the data for another package (#88).

# pkgsearch 3.0.1

* The "My packages" and "Most depended upon" items now work properly
  in the RStudio addin (#77).

* The RStudio addin has a better window title when running in a
  browser (#79).

* The addin now does not crash RStudio when closing the window (#78).

# pkgsearch 3.0.0

* New RStudio addin to search for packages in a GUI:
  `pkg_search_addin()`.

* New `cran_package()`, `cran_packages()` and `cran_package_history()`
  functions to query metadata about certain packages.

* New `cran_events()` function to list recent CRAN events, new, updated
  or archived packages.

* New `cran_top_downloaded()` function to query packages with the most
  downloads.

* New `cran_trending()` function to return the trending CRAN packages.

* New function `advanced_search()` for more search flexibility.

# pkgsearch 2.0.1

* Fix a bug when a search hit does not have a 'downloads' field.
  (Because it is a brand new package.)

# pkgsearch 2.0.0

First release on CRAN.
