
all: install-2.svg

%.svg: %.asc.gz
	gzcat $< | svg-term --window | \
	sed 's/font-family=\"[^"]*"/font-family="Menlo,'\''Lucida\ Console'\'',monospace"/g' \
	> $@

.PHONY: all
