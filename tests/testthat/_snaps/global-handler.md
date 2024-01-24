# handle_package_not_found

    Code
      handle_package_not_found(err = list(package = "foo", lib.loc = "/lib"))
    Message
      
      x Failed to load package foo. Do you want to install it into the default
      library at '/lib'?
      
        1. Yes, install it.
        2. No, stop now, and I'll handle it myself.
        
    Output
      
      NULL

---

    Code
      handle_package_not_found(err = list(package = "foo", lib.loc = "/lib"))
    Message
      
      x Failed to load package foo. Do you want to install it into the default
      library at '/lib'?
      
        1. Yes, install it.
        2. No, stop now, and I'll handle it myself.
        
    Output
      
    Message
      -- start installation ----------------------------------------------------------
      Installing...
      -- end installation ------------------------------------------------------------
      

---

    Code
      handle_package_not_found(err = list(package = "foo", lib.loc = "/lib"))
    Message
      
      x Failed to load package foo. Do you want to install it into the default
      library at '/lib'?
      
        1. Yes, install it, and continue the original computation.
        2. No, stop now, and I'll handle it myself.
        
    Output
      
    Message
      -- start installation ----------------------------------------------------------
      Installing...
      -- end installation ------------------------------------------------------------
      

