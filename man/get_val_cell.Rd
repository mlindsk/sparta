% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_sparta.R
\name{get_val}
\alias{get_val}
\alias{get_val.sparta}
\alias{get_cell_name}
\alias{get_cell_name.sparta}
\title{Get value or cell name}
\usage{
get_val(x, y)

\method{get_val}{sparta}(x, y)

get_cell_name(x, y)

\method{get_cell_name}{sparta}(x, y)
}
\arguments{
\item{x}{sparta}

\item{y}{named character vector or vector of cell indices}
}
\description{
Find the value or the name of a cell
}
\examples{
x <- array(
  c(1,0,0,2,3,4,0,0),
  dim = c(2,2,2),
  dimnames = list(
    a = c("a1", "a2"),
    b = c("b1", "b2"),
    c = c("c1", "c2")
  )
)

sx <- as_sparta(x)
get_val(sx, c(a = "a2", b = "b1", c = "c2"))
get_cell_name(sx, sx[, 4])
}
