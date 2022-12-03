
cat(
  sep = "\n",
  "\\renewcommand{\\eval}{\\Sexpr[stage=render,results=rd]{#1}}",
  "\\renewcommand{\\evalatinstall}{\\Sexpr[stage=install,results=rd]{#1}}",
  file = "man/macros/eval2.Rd"
)
