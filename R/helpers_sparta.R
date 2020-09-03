#' Construct sparta object
#'
#' Helper function to construct a sparta object with given values and dim names
#' 
#' @param x matrix where columns represents cells in an array-like object
#' @param vals vector of values corresponding to x
#' @param dim_names a named list
#' @return A sparta object
#' @export
sparta_struct <- function(x, vals, dim_names) {

  cond <- inherits(x, "matrix") &&
    inherits(vals, "numeric")   &&
    ncol(x) == length(vals)     &&
    length(dim_names) == nrow(x)
  
  stopifnot(cond)
  
  storage.mode(x) <- "integer"
  structure(
    x,
    vals = vals,
    dim_names = dim_names,
    class = c("sparta", "matrix")
  )
}


#' Find value 
#'
#' Find the value corresponding the the configuration in y
#'
#' @param x sparta
#' @param y named character vector
#' @examples
#' x <- array(
#'   c(1,0,0,2,3,4,0,0),
#'   dim = c(2,2,2),
#'   dimnames = list(
#'     a = c("a1", "a2"),
#'     b = c("b1", "b2"),
#'     c = c("c1", "c2")
#'   )
#' )
#'
#' sx <- as_sparta(x)
#' get_val(sx, c(a = "a2", b = "b1", c = "c2"))
#'
#' @export
get_val <- function(x, y) UseMethod("get_val")

#' @rdname get_val
#' @export
get_val.sparta <- function(x, y) {
  if (is.null(names(y)) && length(y) != attr(x, "dim_names")) {
    stop("y must have names corresponding to the respective variables", call. = TRUE)
  }
  # TODO: Check if y is allowed by matching against attr(x, "dimnames")?
  x_dim_names <- attr(x, "dim_names")
  y         <- y[names(x_dim_names)]
  idx       <- mapply(match, y, x_dim_names)
  idx_str   <- paste0(idx, collapse = "")
  idx_x     <- apply(x, 2L, paste0, collapse = "")
  which_idx <- match(idx_str, idx_x)
  if (is.na(which_idx)) {
     return(0L) 
  } else {
    return(attr(x, "vals")[which_idx])
  }
}

#' Normalize

#' @param x sparta
#' @return A sparta object
#' @examples
#'
#' x <- array(
#'   c(1,0,0,2,3,4,0,0),
#'   dim = c(2,2,2),
#'   dimnames = list(
#'     a = c("a1", "a2"),
#'     b = c("b1", "b2"),
#'     c = c("c1", "c2")
#'   )
#' )
#'
#' sx <- as_sparta(x)
#' normalize(sx)
#' @export
normalize <- function(x) UseMethod("normalize")

#' @rdname normalize
#' @export
normalize.sparta <- function(x) {
  attr(x, "vals") <- attr(x, "vals") / sum(attr(x, "vals"))
  x
}

#' Vector-like operations on sparta objects

#' @param x sparta
#' @param ... For S3 compatability.

#' @rdname vec-ops
#' @export
sum.sparta <- function(x, ...) {
  sum(attr(x, "vals"))
}

#' @rdname vec-ops
#' @export
max.sparta <- function(x, ...) {
  max(attr(x, "vals"))
}

#' @rdname vec-ops
#' @export
min.sparta <- function(x, ...) {
  min(attr(x, "vals"))
}

#' @rdname vec-ops
#' @export
which_min_cell <- function(x) UseMethod("which_min")

#' @rdname vec-ops
#' @export
which_min_cell.sparta <- function(x) {
  idx <- x[, which.min(attr(x, "vals"))]
  mapply(
    function(a, b) a[b],
    attr(x, "dim_names"),
    idx
  )
}

#' @rdname vec-ops
#' @export
which_min_idx <- function(x) UseMethod("which_min_idx")

#' @rdname vec-ops
#' @export
which_min_idx.sparta <- function(x) {
  which.min(attr(x, "vals"))
}

#' @rdname vec-ops
#' @export
which_max_cell <- function(x) UseMethod("which_max_cell")

#' @rdname vec-ops
#' @export
which_max_cell.sparta <- function(x) {
  idx <- x[, which.max(attr(x, "vals"))]
  mapply(
    function(a, b) a[b],
    attr(x, "dim_names"),
    idx
  )
}

#' @rdname vec-ops
#' @export
which_max_idx <- function(x) UseMethod("which_max_idx")

#' @rdname vec-ops
#' @export
which_max_idx.sparta <- function(x) {
  which.max(attr(x, "vals"))
}
