## ---------------------------------------------------------
##                NON-EXPORTED HELPERS
## ---------------------------------------------------------

## MAPS
.map_chr <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = character(1), ...)
.map_int <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = integer(1), ...)
.map_dbl <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = numeric(1), ...)
.map_lgl <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = logical(1), ...)
.map_lst <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = list(), ...)

## SETS
## eq_empt_chr <- function(x) !identical(x, character(0))
## eq_empt_num <- function(x) !identical(x, numeric(0))
## eq_empt_int <- function(x) !identical(x, integer(0))
## eq_empt_lst <- function(x) !identical(x, list())
## eq_null     <- function(x) !is.null(x)
## '%ni%'       <- Negate('%in%')

## ASSERTERS
is_named_list <- function(x) {
  if (is.null(names(x))) return(FALSE)
  if ("" %in% names(x)) {
    return(FALSE) 
  } else {
    return(TRUE)
  }
}
