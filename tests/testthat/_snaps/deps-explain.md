# pkg_deps_explain

    Code
      pkg_deps_explain("pkg3", "pkg1")
    Output
      pkg3 -> pkg2 -> pkg1

---

    Code
      pkg_deps_explain("pkg1", "pkg3")
    Output
      x pkg3

