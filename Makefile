.PHONY: compile doc check install build_site run_main test clean

compile:
	Rscript -e "Rcpp::compileAttributes()"

doc:
	Rscript -e "devtools::document()"

check: 
	Rscript -e "devtools::check()"

rhub_windows: 
	Rscript -e "rhub::check_on_windows()"

rhub_linux: 
	Rscript -e "rhub::check_on_linux()"

rhub_solaris: 
	Rscript -e "rhub::check_on_solaris()"

check_fast: 
	Rscript -e "devtools::check(build_args = c('--no-build-vignettes'), args = c('--no-build-vignettes'))"

install:
	Rscript -e "devtools::install()"

test:
	Rscript -e "devtools::load_all(); tinytest::test_all('.', color = TRUE)"

readme:
	Rscript -e "rmarkdown::render('README.Rmd')"
