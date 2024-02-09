# extra_paalkages

    Code
      extra_packages()
    Output
      [1] "pillar"

# pak_install_extra

    Code
      pak_install_extra()
    Message
      i installing extra package: `pillar`.

# load_extra

    Code
      load_extra("foobar")
    Message
      ! Optional package `foobar` is not available for pak.
        Use `pak::pak_install_extra()` to install optional packages.
        Use `options(pak.no_extra_messages = TRUE)` to suppress this message.

---

    Code
      load_extra("foobar")

---

    Code
      load_extra("foobar")
    Output
      NULL

