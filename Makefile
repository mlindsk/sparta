PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

build:
	cd ..;\
	R CMD build $(PKGSRC)

build_fast:
	cd ..;\
	R CMD build --no-manual --no-build-vignettes $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

install_fast: build_fast
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

compile:
	Rscript -e "Rcpp::compileAttributes()"

doc:
	Rscript -e "devtools::document()"

check_rhub: 
	Rscript -e \
	"rhub::check_for_cran(); \
	rhub::check_on_solaris(); \
	rhub::check_on_debian(); \
	rhub::check_on_macos()"
test:
	Rscript -e "devtools::load_all(); tinytest::test_all('.', color = TRUE)"

readme:
	Rscript -e "rmarkdown::render('README.Rmd')"; \
	rm README.html

clean:
	rm -f README.html
	cd src/ && rm -f *.o && rm -f *.so
