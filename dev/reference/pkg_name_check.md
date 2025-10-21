# Check if an R package name is available

Additionally, look up the candidate name in a number of dictionaries, to
make sure that it does not have a negative meaning.

## Usage

``` r
pkg_name_check(name, dictionaries = NULL)
```

## Arguments

- name:

  Package name candidate.

- dictionaries:

  Character vector, the dictionaries to query. Available dictionaries:
  \* `wikipedia` \* `wiktionary`, \* `sentiment`
  (<https://github.com/fnielsen/afinn>), \* `urban` (Urban Dictionary).
  If `NULL` (by default), the Urban Dictionary is omitted, as it is
  often offensive.

## Value

`pkg_name_check` object with a custom print method.

## Details

### Valid package name check

Check the validity of `name` as a package name. See 'Writing R
Extensions' for the allowed package names. Also checked against a list
of names that are known to cause problems.

### CRAN checks

Check `name` against the names of all past and current packages on CRAN,
including base and recommended packages.

### Bioconductor checks

Check `name` against all past and current Bioconductor packages.

### Profanity check

Check `name` with <https://www.purgomalum.com/service/containsprofanity>
to make sure it is not a profanity.

### Dictionaries

See the `dictionaries` argument.

## Examples

    pkg_name_check("sicily")

    #> ╔══════════════════════════════════════════════════════════════════════╗
    #> ║                            –*– sicily –*–                            ║
    #> ╚══════════════════════════════════════════════════════════════════════╝
    #> ┌──────────────────────────────────────────────────────────────────────┐
    #> │ v  valid name          v  CRAN               v  Bioconductor         │
    #> │ v  not a profanity                                                   │
    #> └──────────────────────────────────────────────────────────────────────┘
    #> ┌ Wikipedia ───────────────────────────────────────────────────────────┐
    #> │ Sicily Sicily (Italian: Sicilia [siˈtʃiːlja], Sicilian               │
    #> │ pronunciation: [sɪˈʃiːlja]) is the largest island in the             │
    #> │ Mediterranean Sea and one of the 20 regions of Italy. The Strait of  │
    #> │ Messina divides it from the region of Calabria in Southern Italy.    │
    #> │ It is one of the five Italian autonomous regions and is officially   │
    #> │ referred to as Regione Siciliana. The region has 5 million           │
    #> │ …                                                                    │
    #> └──────────────────────────────── https://en.wikipedia.org/wiki/Sicily ┘
    #> ┌ Wiktionary ──────────────────────────────────────────────────────────┐
    #> │ sicily No English definition found                                   │
    #> └────────────────────────────────────────────────────────────────────  ┘
    #> ┌──────────────────────────────────────────────────────────────────────┐
    #> │ Sentiment: 😐 (0)                                                    │
    #> └──────────────────────────────────────────────────────────────────────┘
