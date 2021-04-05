sparta: Sparse Tables
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R build
status](https://github.com/mlindsk/sparta/workflows/R-CMD-check/badge.svg)](https://github.com/mlindsk/sparta/actions)
[![](https://www.r-pkg.org/badges/version/sparta?color=green)](https://cran.r-project.org/package=sparta)
<!-- badges: end -->

## About

The `sparta` package implements new methods (multiplication,
marginalization etc.) for dealing with conditional probability tables
(CPTs) and are especially useful when the CPTs are sparse.

## Installation

  - Current stable release from CRAN:

<!-- end list -->

``` r
install.packages("sparta")
```

  - Current development version:

<!-- end list -->

``` r
devtools::install_github("mlindsk/sparta", build_vignettes = FALSE)
```

## See Also

The `jti`, <https://github.com/mlindsk/jti>, package (on CRAN)
implements belief propagation via the Junction Tree Algorithm which
relies heavily on `sparta` tables.
