# pak SITuation REPort

It prints

- pak version,

- platform the package was built on, and the current platform,

- the current library path,

- versions of dependencies,

- whether dependencies can be loaded.

## Usage

``` r
pak_sitrep()
```

## Examples

    pak_sitrep()

    #>  compatible)
    #> - pak repository: - (local install?)
    #> * Optional packages installed:
    #> - pillar
    #> * Library path:
    #> - /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/RtmpRxl7Vu/fi
    #> le1868066d6ae59
    #> - /private/var/folders/ph/fpcmzfd16rgbbk8mxvy9m2_h0000gn/T/RtmpRxl7Vu/fi
    #> le186806281b34a
    #> - /Users/gaborcsardi/Library/R/arm64/4.3/library
    #> - /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library
    #> * Private library location:
    #> - /Users/gaborcsardi/Library/Caches/org.R-project.R/R/pak/lib/4.3/aarch6
    #> 4
    #> * Private library exists.
    #> * Private library is functional

## See also

Other pak housekeeping:
[`pak_cleanup()`](https://pak.r-lib.org/reference/pak_cleanup.md)
