version_satisfies <- function(ver, op, cond) {
  ver <- package_version(ver)
  switch(
    op,
    "<" = ver < cond,
    "<=" = ver <= cond,
    "==" = ver == cond,
    ">=" = ver >= cond,
    ">" = ver > cond,
    "!=" = ver != cond
  )
}
