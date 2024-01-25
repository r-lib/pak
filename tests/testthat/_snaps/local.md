# local_deps & co

    Code
      local_deps(file.path(dld, "pkg4"))$package
    Output
      [1] "pkg4" "pkg1" "pkg2"

---

    Code
      local_dev_deps(file.path(dld, "pkg4"))$package
    Output
      [1] "pkg4" "pkg1" "pkg2" "pkg3"

---

    Code
      local_deps_tree(file.path(dld, "pkg4"))
    Output
      local::<tempdir>/<tempfile>
      \-pkg2 1.0.0 [new][bld][dl] (<size>)
        \-pkg1 1.0.0 [new][bld][dl] (<size>)
      
      Key:  [new] new | [dl] download | [bld] build

---

    Code
      local_dev_deps_tree(file.path(dld, "pkg4"))
    Output
      local::<tempdir>/<tempfile>
      +-pkg2 1.0.0 [new][bld][dl] (<size>)
      | \-pkg1 1.0.0 [new][bld][dl] (<size>)
      \-pkg3 1.0.0 [new][bld][dl] (<size>)
        \-pkg2
      
      Key:  [new] new | [dl] download | [bld] build

---

    Code
      local_deps_explain("pkg1", file.path(dld, "pkg4"))
    Output
      pkg4 -> pkg2 -> pkg1

---

    Code
      local_dev_deps_explain("pkg3", file.path(dld, "pkg4"))
    Output
      pkg4 -> pkg3

