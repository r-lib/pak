# vcapply

    Code
      vcapply(letters, function(x) 1L)
    Condition
      Error in `vapply()`:
      ! values must be type 'character',
       but FUN(X[[1]]) result is type 'integer'
    Code
      vcapply(1:5, function(x) c("foo", "bar"))
    Condition
      Error in `vapply()`:
      ! values must be length 1,
       but FUN(X[[1]]) result is length 2

# vlapply

    Code
      vlapply(letters, function(x) 1L)
    Condition
      Error in `vapply()`:
      ! values must be type 'logical',
       but FUN(X[[1]]) result is type 'integer'
    Code
      vlapply(1:5, function(x) c(TRUE, FALSE))
    Condition
      Error in `vapply()`:
      ! values must be length 1,
       but FUN(X[[1]]) result is length 2

# viapply

    Code
      viapply(letters, function(x) 1)
    Condition
      Error in `vapply()`:
      ! values must be type 'integer',
       but FUN(X[[1]]) result is type 'double'
    Code
      viapply(1:5, function(x) 1:2)
    Condition
      Error in `vapply()`:
      ! values must be length 1,
       but FUN(X[[1]]) result is length 2

# vdapply

    Code
      vdapply(letters, function(x) "boo")
    Condition
      Error in `vapply()`:
      ! values must be type 'double',
       but FUN(X[[1]]) result is type 'character'
    Code
      vdapply(1:5, function(x) 1:2 / 2)
    Condition
      Error in `vapply()`:
      ! values must be length 1,
       but FUN(X[[1]]) result is length 2

# cat0

    Code
      cat0(c("foo", "bar"), "foobar")
    Output
      foobarfoobar

# mkdirp

    Code
      mkdirp(c("foo", "bar"), "Created these")
    Message
      i Created these: 'foo' and 'bar'.

# base_packages

    Code
      base_packages()
    Output
       [1] "base"      "compiler"  "datasets"  "graphics"  "grDevices" "grid"     
       [7] "methods"   "parallel"  "splines"   "stats"     "stats4"    "tcltk"    
      [13] "tools"     "utils"    

