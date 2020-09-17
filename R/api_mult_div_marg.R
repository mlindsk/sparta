#' @title Multiplication and division of sparse tables
#' @param x sparta object
#' @param y sparta object
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
#' # Useful in connection with the junction tree algorithm where
#' # some clique potentials/tables may be initialized as the identity table
#' 
#' su <- sparta_unity_struct(dim_names(sy))
#' mult(sx, su)
#' div(su, sx)
#' 
#' @rdname merge
#' @export
mult <- function(x, y) UseMethod("mult")

#' @rdname merge
#' @export
mult.sparta <- function(x, y) {

  if (!inherits(y, "sparta")) stop("y is not of class 'sparta'", call. = FALSE)
  is_x_unity <- inherits(x, "sparta_unity")
  is_y_unity <- inherits(y, "sparta_unity")

  dn1 <- attr(x, "dim_names")
  dn2 <- attr(y, "dim_names")
  
  m <- if (is_x_unity || is_y_unity) {
    if (is_y_unity && !is_x_unity) {
      merge_unity_(x, vals(x), names(x), names(y), .map_int(dim_names(y), length))
    } else if (!is_y_unity && is_x_unity) {
      # The dimnames must be swapped
      dn  <- dn1
      dn1 <- dn2
      dn2 <- dn
      merge_unity_(y, vals(y), names(y), names(x), .map_int(dim_names(x), length))
    } else {
      stop("Not yet implemented for two sparta_unity tables.")
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

  if (!inherits(y, "sparta")) stop("y is not of class 'sparta'", call. = FALSE)
  is_x_unity <- inherits(x, "sparta_unity")
  is_y_unity <- inherits(y, "sparta_unity")

  dn1 <- attr(x, "dim_names")
  dn2 <- attr(y, "dim_names")
  
  m <- if (is_x_unity || is_y_unity) {
    if (is_y_unity && !is_x_unity) {
      merge_unity_(x, vals(x), names(x), names(y), .map_int(dim_names(y), length))
    } else if (!is_y_unity && is_x_unity) {
      dn  <- dn1
      dn1 <- dn2
      dn2 <- dn
      merge_unity_(y, vals(y), names(y), names(x), .map_int(dim_names(x), length), TRUE)
    } else {
      stop("Not yet implemented for two sparta_unity tables.")
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

  # TODO: Marginalize a sparta_unity object - do we need it ?
  
  dnx    <- attr(x, "dim_names")
  xnames <- names(dnx)

  if (setequal(xnames, y)) {
    stop("You can not marginalize all variables out. ",
      "It is ofcourse theoretically valid and would produce a '1L'.",
      call. = FALSE)
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
