merge <- function(x, y, mult = TRUE) {
  is_y_scalar <- is_scalar(y)
  
  if (!inherits(y, "sparta") && !is_y_scalar) {
    stop("y must be of class 'sparta' or a scalar", call. = FALSE)
  }

  is_x_unity  <- inherits(x, "sparta_unity")
  
  if (is_y_scalar) {
    if (is_x_unity) {
      attr(x, "rank") <- attr(x, "rank") * y
      return(x)
    }
    attr(x, "vals") <- if (mult) attr(x, "vals") * y else attr(x, "vals") / y
    return(x)
  }

  is_y_unity <- inherits(y, "sparta_unity")

  dn1 <- attr(x, "dim_names")
  dn2 <- attr(y, "dim_names")
  dn  <- c(dn1, dn2[setdiff(names(dn2), names(dn1))])

  if (is_x_unity && is_y_unity) {
    return(sparta_unity_struct(dn, attr(x, "rank") * attr(y, "rank")))
  }
  
  m <- if (is_x_unity || is_y_unity) {
    if (is_y_unity && !is_x_unity) {
      ydim <- .map_int(dim_names(y), length)
      merge_unity_(x, vals(x), names(x), names(y), ydim, attr(y, "rank"))
    } else if (!is_y_unity && is_x_unity) {
      xdim <-.map_int(dim_names(x), length)
      merge_unity_(y, vals(y), names(y), names(x), xdim, attr(x, "rank"), ifelse(mult, FALSE, TRUE))
    }
  } else {
    merge_(x, y, vals(x), vals(y), names(x), names(y), ifelse(mult, "*", "/"))
  }

  sparta_struct(m[[1]], m[[2]], dn[m[[3]]])
}


#' @title Multiplication and division of sparse tables
#' @param x sparta object or scalar
#' @param y sparta object or scalar
#' @return A sparta object or a scalar
#' @examples
#'
#' # ----------
#' # Example 1)
#' # ----------
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
#' y <- array(
#'   c(1,3,0,2,4,2,7,0,
#'     1,8,0,1,6,2,1,0,
#'     1,5,0,3,2,9,1,0),
#'   dim = c(2,2,2, 3),
#'   dimnames = list(
#'     b = c("b1", "b2"),
#'     d = c("d1", "d2"),
#'     a = c("a1", "a2"),
#'     e = c("e1", "e2", "e3")
#'   )
#' )
#' 
#' sx <- as_sparta(x)
#' sy <- as_sparta(y)
#'
#' dim_names(sx)
#' dim_names(sy)
#'
#' mult(sx, sy)
#' div(sy, sx)
#'
#' # ----------
#' # Example 2)
#' # ----------
#'
#' d1   <- mtcars[, c("cyl", "vs", "am")]
#' d1[] <- lapply(d1, as.character)
#' d2   <- mtcars[, c("am", "gear", "carb")]
#' d2[] <- lapply(d2, as.character)
#' ds1  <- as_sparta(d1)
#' ds2  <- as_sparta(d2)
#'
#' mult(ds1, ds2)
#' div(ds1, ds2)
#'
#' # ----------
#' # Example 3)
#' # ----------
#'
#' su <- sparta_unity_struct(dim_names(sy), rank = 1)
#' mult(sx, su)
#' div(su, sx)
#'
#' # ----------
#' # Example 4)
#' # ----------
#' so <- sparta_ones(dim_names(sx))
#' mult(so, 2)
#' div(so, -2)

#' @rdname merge
#' @export
mult <- function(x, y) UseMethod("mult")

#' @rdname merge
#' @export
mult.sparta <- function(x, y) merge(x, y)

# #' @rdname merge
# #' @export
# mult.double <- function(x, y) {
#   if (!is_scalar(x)) stop("x must be of class 'sparta' or a scalar", call. = FALSE)
#   if (!inherits(y, "sparta")) {
#     stop("y must be a 'sparta' object when x is a scalar", call. = FALSE)
#   }
#   merge(y, x)
# }

#' @rdname merge
#' @export
mult.numeric <- function(x, y) {
  if (!is_scalar(x)) stop("x must be of class 'sparta' or a scalar", call. = FALSE)
  if (is_scalar(y)) return(x * y)
  if (inherits(y, "sparta")) {
    merge(y, x)
  }

}

#' @rdname merge
#' @export
div <- function(x, y) UseMethod("div")

#' @rdname merge
#' @export
div.sparta <- function(x, y) merge(x, y, FALSE)

# #' @rdname merge
# #' @export
# div.double <- function(x, y) {
#   if (!is_scalar(x)) stop("x must be of class 'sparta' or a scalar", call. = FALSE)
#   if (!inherits(y, "sparta")) {
#     stop("y must be a 'sparta' object when x is a scalar", call. = FALSE)
#   }
#   merge(y, x, FALSE)
# }

#' @rdname merge
#' @export
div.numeric <- function(x, y) {
  if (!is_scalar(x)) stop("x must be of class 'sparta' or a scalar", call. = FALSE)
  if (is_scalar(y)) return(x / y)
  if (inherits(y, "sparta")) {
    merge(y, x, FALSE)
  }
}


#' Marginalization of sparse tables
#'
#' Marginalize a sparse table given a vector of variables to marginalize out
#' 
#' @param x sparta object
#' @param y character vector of the variables to marginalize out
#' @param flow either "sum" or "max"
#' @return A sparta object (or scalar if all variables are summed out)
#' @examples
#'
#' x <- array(
#'  c(1,0,0,2,3,4,0,0),
#'  dim = c(2,2,2),
#'  dimnames = list(
#'    a = c("a1", "a2"),
#'    b = c("b1", "b2"),
#'    c = c("c1", "c2")
#'  )
#' )
#'
#' sx <- as_sparta(x)
#' marg(sx, c("c"))
#'
#' su <- sparta_unity_struct(dim_names(sx), rank = 1.5)
#' marg(su, c("a", "b"))
#' @export
marg <- function(x, y, flow = "sum") UseMethod("marg")

#' @rdname marg
#' @export
marg.sparta <- function(x, y, flow = "sum") {

  if (inherits(x, "sparta_unity")) {
    dny <- dim_names(x)[y]
    attr(x, "rank") <- attr(x, "rank") * prod(.map_dbl(dny, length))
    return(x)
  }

  if (flow %ni% c("sum", "max")) {
    stop("flow must be either 'sum' or 'max'", call. = FALSE)
  }
  
  dnx    <- dim_names(x)
  xnames <- names(dnx)

  if (setequal(xnames, y)) {
    return(sum(x))
  }

  m <- if (flow == "sum") {
    marginalize_sum_2(x, vals(x), xnames, y)
  } else {
    marginalize_max_(x, vals(x), xnames, y)
  }

  sparta_struct(
    m[[1]],
    m[[2]],
    dnx[setdiff(xnames, y)]
  )
}


#' Slice
#'
#' Find the slice of a sparse table
#' 
#' @param x sparta object
#' @param s a slice in form of a named character vector
#' @param drop Logical. If \code{TRUE}, the variables in \code{s} are removed
#' @return A sparta object
#' @examples
#'
#' x <- array(
#'  c(1,0,0,2,3,4,0,0),
#'  dim = c(2,2,2),
#'  dimnames = list(
#'    a = c("a1", "a2"),
#'    b = c("b1", "b2"),
#'    c = c("c1", "c2")
#'  )
#' )
#'
#' sx <- as_sparta(x)
#' 
#' # conditional probability table p(b,c|a)
#' sx <- as_cpt(sx, "a")
#'
#' # the probability distriubtion when 'a' is 'a2' 
#' sxa2 <- slice(sx, c(a = "a2"))
#' get_val(sxa2, c(a = "a2", b = "b1", c = "c2"))
#'
#' sxa2_drop <- slice(sx, c(a = "a2"), drop = TRUE)
#' get_val(sxa2_drop, c(b = "b1", c = "c2"))
#'
#' @export
slice <- function(x, s, drop = FALSE) UseMethod("slice")

#' @rdname slice
#' @export
slice.sparta <- function(x, s, drop = FALSE) {

  # TODO:
  # let s be a list, so we can slice on arbitrary
  # many levels for each variable in s
  
  if (inherits(x, "sparta_unity")) {
    stop("a sparta_unity cannot be sliced. If needed, use 'sparta_ones'.", call. = FALSE)
  }

  if (!all(names(s) %in% names(x))) {
    stop("some names in s are not in x. see `dim_names(x)`", call. = FALSE)
  }

  sp <- slice_(x, vals(x), dim_names(x), names(s), s)

  if (drop) {
    dn_new <- dim_names(x)[setdiff(names(x), names(s))]
    idx_s <- match(names(s), names(x))
    return(sparta_struct(sp[[1]][-idx_s, , drop = FALSE], sp[[2]], dn_new))
  }
  
  sparta_struct(sp[[1]], sp[[2]], dim_names(x))
}
