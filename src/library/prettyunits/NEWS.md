# prettyunits 1.2.0

* New `pretty_num()` function with all the [BIPM](https://www.bipm.org)
  agreed unit prefix (#26, @cregouby).

* New `pretty_round()` and `pretty_signif()` functions preserve the requested
  number of digits as character strings (#14, @billdenney).

* New `pretty_p_value()` function to convert p-values to character strings
  where small values are shown like "<0.0001" (#13, @billdenney).

* New `pretty_color()` functionm converts a color to a named color
  (#12, @billdenney).

# prettyunits 1.1.1

* Fix spurious zero fractions in `pretty_bytes()` when formatting
  vectors of sizes (#23).

# prettyunits 1.1.0

* `pretty_dt()`, `pretty_ms()` and `pretty_sec()` now handle `NA` values
  properly, and return `NA_character_` for them (#10, @petermeissner).

* `pretty_bytes()` now formats quantities just below the units better.
  E.g. 1MB - 1B is formatted as `"1 MB"` instead of `""1000 kB"` (#18).

* `pretty_bytes()` now has multiple styles. In particular, a fixed width
  style is useful for progress bars. Another style avoids the left-padding
  with spaces.

* The new low level `compute_bytes()` function can be used to create
  custom formatters for bytes.

# prettyunits 1.0.2

* `pretty_bytes()` always uses two fraction digits for non-integers.
  This looks nicer in a progress bar, as the width of string does not
  change so much.

# prettyunits 1.0.1

First version with a NEWS file.

* Get rid of `R CMD check` notes.

# prettyunits 1.0.0

Last version without a NEWS file.
