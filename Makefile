
README.md: README.Rmd
	R -q -e 'rmarkdown::render("README.Rmd")'
