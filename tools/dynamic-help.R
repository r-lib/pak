
cat(
  sep = "\n",
  "\\renewcommand{\\eval}{\\Sexpr[stage=render,results=rd]{#1}}",
  "\\renewcommand{\\evalatinstall}{\\Sexpr[stage=install,results=rd]{#1}}",
  # The 'top' variants do not work in R < 4.0.0, because a top-level \Sexpr
  # is not allowed, even if it is put in by an Rd macro. Once we do not
  # support R 3.6.x, we can use the 'top' variants.
  # See also the comments in the include_docs() function.
  "\\renewcommand{\\evaltop}{\\Sexpr[stage=render,results=rd]{#1}}",
  "\\renewcommand{\\evalatinstalltop}{\\Sexpr[stage=install,results=rd]{#1}}",
  file = "man/macros/eval2.Rd"
)
