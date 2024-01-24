# print_install_details

    Code
      print_install_details(sol, local, character())
    Message
       
      > Will install 2 packages.
      > Will download 2 CRAN packages (<size>).
      + pkg1   1.0.0 [bld][dl] (<size>)
      + pkg2   1.0.0 [bld][dl] (<size>)

---

    Code
      print_install_details(sol, local, character())
    Message
       
      > Will update 1 package.
      > Will download 1 CRAN package (<size>).
      + pkg1 0.0.0 > 1.0.0 [bld][dl] (<size>)

---

    Code
      print_install_details(sol, local, character())
    Message
       
      > Will install 1 package.
      > The package (<size>) is cached.
      + pkg1   1.0.0 [bld]

---

    Code
      print_install_details(sol, local, character())
    Message
       
      > Will install 2 packages.
      > All 2 packages (<size>) are cached.
      + pkg1   1.0.0 [bld]
      + pkg2   1.0.0 [bld]

---

    Code
      print_install_details(sol, local, character())
    Message
       
      > Will install 1 package.
      > Will download 1 package with unknown size.
      + pkgu   1.0.0 [bld][dl]

---

    Code
      print_install_details(sol, local, character())
    Message
       
      > Will install 3 packages.
      > Will download 1 package (<size>), cached: 2 (<size>).
      + pkg1   1.0.0 [bld]
      + pkg2   1.0.0 [bld]
      + pkg3   1.0.0 [bld][dl] (<size>)

---

    Code
      print_install_details(sol, local, character())
    Message
       
      > Will install 4 packages.
      > Will download 1 CRAN package (<size>), cached: 2 (<size>).
      > Will download 1 package with unknown size.
      + pkg1   1.0.0 [bld]
      + pkg2   1.0.0 [bld]
      + pkg3   1.0.0 [bld][dl] (<size>)
      + pkgu   1.0.0 [bld][dl]

# get_answer

    Code
      res <- get_answer(c("this", "bar"))
    Output
      ? Your choice [this]: foo
      ? Your choice [this]: bar

# offer_restart

    Code
      offer_restart()
    Message
      
      ! pak had to unload some packages before installation, and the
        current R session may be unstable. It is best to restart R now.
      

---

    Code
      offer_restart()
    Message
      
      ! pak had to unload some packages before installation, and the
        current R session may be unstable. It is best to restart R now.
      
        1. Restart R without saving data.
        2. Save data to `.RData` and restart R.
        3. Do not restart R.
      
    Output
      [1] "restart"

---

    Code
      offer_restart()
    Message
      
      ! pak had to unload some packages before installation, and the
        current R session may be unstable. It is best to restart R now.
      
        1. Restart R without saving data.
        2. Save data to `.RData` and restart R.
        3. Do not restart R.
      
      Saving workspace to .RData...
    Output
      [1] "save-restart"

---

    Code
      expect_equal(offer_restart(), "OK")
    Message
      
      ! pak had to unload some packages before installation, and the
        current R session may be unstable. It is best to restart R now.
      
        1. Restart R without saving data.
        2. Save data to `.RData` and restart R.
        3. Do not restart R.
      

