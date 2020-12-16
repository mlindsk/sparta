# ---------------------------------------------------------
#                NON-EXPORTED HELPERS
# ---------------------------------------------------------

# MAPS
# ----
.map_chr <- function(x, fun, ...) {
  vapply(X = x, FUN = fun, FUN.VALUE = character(1), ...)
}

.map_int <- function(x, fun, ...) {
  vapply(X = x, FUN = fun, FUN.VALUE = integer(1), ...)
}

.map_dbl <- function(x, fun, ...) {
  vapply(X = x, FUN = fun, FUN.VALUE = double(1), ...)
}

# SETS
# ----
eq_empt_chr <- function(x) identical(x, character(0))
'%ni%' <- Negate('%in%')

# ASSERTERS
# ---------
is_scalar <- function(x) {
  is.atomic(x) && length(x) == 1L && (class(x) == "numeric" || class(x) == "integer")
}

is_named_list <- function(x) {
  if (is.null(names(x))) return(FALSE)
  if ("" %in% names(x)) {
    return(FALSE) 
  } else {
    return(TRUE)
  }
}
