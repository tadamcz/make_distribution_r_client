# Makefile for R Package Development
PACKAGE_NAME = makedistribution

.PHONY: all build check install document clean

all: build check install document

build:
	R CMD build --no-resave-data .

check:
	R CMD build --no-resave-data .
	R CMD check --as-cran $(PACKAGE_NAME)_*.tar.gz

install:
	R CMD INSTALL $(PACKAGE_NAME)_*.tar.gz

document:
	R -e 'devtools::document()'

clean:
	rm -f $(PACKAGE_NAME)_*.tar.gz
	rm -rf $(PACKAGE_NAME).Rcheck
