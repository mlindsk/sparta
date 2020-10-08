#' @title Multiplication and division of sparse tables
#' @param x sparta object
#' @param y sparta object or scalar
#' @return A sparta object
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
#' # Useful in connection to the Junction Tree Algorithm
#' # where all clique potentials must be intialized
#' # as the identity table
#' 
#' su <- sparta_unity_struct(dim_names(sy))
#' mult(sx, su)
#' div(su, sx)
#'
#' # ----------
#' # Example 4)
#' # ----------
#' so <- sparta_ones(dim_names(sx))
#' mult(so, 2)
#' div(so, -2)
#' 
#' 
#' @rdname merge
#' @export
mult <- function(x, y) UseMethod("mult")

#' @rdname merge
#' @export
mult.sparta <- function(x, y) {

  is_y_scalar <- is_scalar(y)
  
  if (!inherits(y, "sparta") && !is_y_scalar) {
    stop("y must be of class 'sparta' or a scalar", call. = FALSE)
  }

  is_x_unity <- inherits(x, "sparta_unity")
  
  if (is_y_scalar) {
    if (is_x_unity) {
      stop("A sparta_unity can not be multiplied with a scalar", call. = FALSE)
    }
    attr(x, "vals") <- attr(x, "vals") * y
    return(x)
  }
  
  if (!inherits(y, "sparta")) stop("y is not of class 'sparta'", call. = FALSE)

  is_y_unity <- inherits(y, "sparta_unity")
  dn1 <- attr(x, "dim_names")
  dn2 <- attr(y, "dim_names")

  if (is_x_unity && is_y_unity) {
    return(sparta_unity_struct(c(dn1, dn2[setdiff(names(dn2), names(dn1))])))
  }
  
  m <- if (is_x_unity || is_y_unity) {
    if (is_y_unity && !is_x_unity) {
      merge_unity_(x, vals(x), names(x), names(y), .map_int(dim_names(y), length))
    } else if (!is_y_unity && is_x_unity) {
      # The dimnames must be swapped
      dn  <- dn1
      dn1 <- dn2
      dn2 <- dn
      merge_unity_(y, vals(y), names(y), names(x), .map_int(dim_names(x), length))
    } 
  } else {
    merge_(x, y, vals(x), vals(y), names(x), names(y))
  }
  
  sparta_struct(m[[1]], m[[2]], c(dn1, dn2[setdiff(names(dn2), names(dn1))]))
}


#' @rdname merge
#' @export
div <- function(x, y) UseMethod("div")

#' @rdname merge
#' @export
div.sparta <- function(x, y) {

  is_y_scalar <- is_scalar(y)
  
  if (!inherits(y, "sparta") && !is_y_scalar) {
    stop("y must be of class 'sparta' or a scalar", call. = FALSE)
  }

  is_x_unity  <- inherits(x, "sparta_unity")
  
  if (is_y_scalar) {
    if (is_x_unity) {
      stop("A sparta_unity can not be divided with a scalar", call. = FALSE)
    }
    attr(x, "vals") <- attr(x, "vals") / y
    return(x)
  }

  is_y_unity <- inherits(y, "sparta_unity")
  dn1 <- attr(x, "dim_names")
  dn2 <- attr(y, "dim_names")

  if (is_x_unity && is_y_unity) {
    return(sparta_unity_struct(c(dn1, dn2[setdiff(names(dn2), names(dn1))])))
  }
  
  m <- if (is_x_unity || is_y_unity) {
    if (is_y_unity && !is_x_unity) {
      merge_unity_(x, vals(x), names(x), names(y), .map_int(dim_names(y), length))
    } else if (!is_y_unity && is_x_unity) {
      dn  <- dn1
      dn1 <- dn2
      dn2 <- dn
      merge_unity_(y, vals(y), names(y), names(x), .map_int(dim_names(x), length), TRUE)
    }
  } else {
    merge_(x, y, vals(x), vals(y), names(x), names(y), "/")
  }

  sparta_struct(m[[1]], m[[2]], c(dn1, dn2[setdiff(names(dn2), names(dn1))]))
}


#' Marginalization of sparse tables
#'
#' Marginalize a sparse table given a vector of variables to marginalize out
#' 
#' @param x sparta object
#' @param y character vector of the variables to marginalize out
#' @param flow either "sum" or "max"
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
#' marg(sx, c("b"))
#' @export
marg <- function(x, y, flow = "sum") UseMethod("marg")

#' @rdname marg
#' @export
marg.sparta <- function(x, y, flow = "sum") {

  if (inherits(x, "sparta_unity")) {
    stop("A sparta_unity can not be marginalized. If needed, use 'sparta_ones'")
  }
  
  dnx    <- attr(x, "dim_names")
  xnames <- names(dnx)

  if (setequal(xnames, y)) {
    return(sum(x))
  }

  if (flow %ni% c("sum", "max")) {
    stop("flow must be either 'sum' or 'max'", call. = FALSE)
  }

  m <- marginalize_(
    x,
    attr(x, "vals"),
    xnames,
    y,
    ifelse(flow == "sum", TRUE, FALSE)
  )

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
#' @export
slice <- function(x, s) UseMethod("slice")

#' @rdname slice
#' @export
slice.sparta <- function(x, s) {
  if (inherits(x, "sparta_unity")) {
    stop("A sparta_unity cannot be sliced. If needed, use 'sparta_ones'.")
  }
  if (is.null(names(s))) {
    stop("s must be a named character vector", call. = FALSE)
  }
  sp <- slice_(x, vals(x), dim_names(x), names(s), s)
  sparta_struct(sp[[1]], sp[[2]], dim_names(x))
}
