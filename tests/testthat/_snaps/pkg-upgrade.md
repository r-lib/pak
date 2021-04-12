# pkg_upgrade [plain]

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      ! Cannot find source of foo, trying to upgrade it from the configured
        repositories.

---

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      i Updating CRAN package foo.

---

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      i Updating Bioconductor package foo.

---

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      i Updating package foo from repo/foo@master.

# pkg_upgrade [ansi]

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      [33m![39m Cannot find source of [34m[34mfoo[34m[39m, trying to upgrade it from the configured
        [33m[39m[34m[34m[34m[39mrepositories.

---

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      [36mi[39m Updating [3m[3mCRAN[3m[23m package [34m[34mfoo[34m[39m.

---

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      [36mi[39m Updating [3m[3mBioconductor[3m[23m package [34m[34mfoo[34m[39m.

---

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      [36mi[39m Updating package [34m[34mfoo[34m[39m from [34m[34mrepo/foo@master[34m[39m.

# pkg_upgrade [unicode]

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      ! Cannot find source of foo, trying to upgrade it from the configured
        repositories.

---

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      ℹ Updating CRAN package foo.

---

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      ℹ Updating Bioconductor package foo.

---

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      ℹ Updating package foo from repo/foo@master.

# pkg_upgrade [fancy]

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      [33m![39m Cannot find source of [34m[34mfoo[34m[39m, trying to upgrade it from the configured
        [33m[39m[34m[34m[34m[39mrepositories.

---

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      [36mℹ[39m Updating [3m[3mCRAN[3m[23m package [34m[34mfoo[34m[39m.

---

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      [36mℹ[39m Updating [3m[3mBioconductor[3m[23m package [34m[34mfoo[34m[39m.

---

    Code
      pkg_upgrade("foo")
    Message <cliMessage>
      [36mℹ[39m Updating package [34m[34mfoo[34m[39m from [34m[34mrepo/foo@master[34m[39m.

