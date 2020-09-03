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
#' @rdname merge
#' @export
mult <- function(x, y) UseMethod("mult")

#' @rdname merge
#' @export
mult.sparta <- function(x, y) {
  if (!inherits(y, "sparta")) stop("y is not of class 'sparta'", call. = FALSE)
  dnx <- attr(x, "dim_names")
  dny <- attr(y, "dim_names")
  m <- merge_(
    x,
    y,
    attr(x, "vals"),
    attr(y, "vals"),
    names(attr(x, "dim_names")),
    names(attr(y, "dim_names")),
    "*"
  )
  browser()
  sparta_struct(
    m[[1]],
    m[[2]],
    c(dnx, dny[setdiff(names(dny), names(dnx))])
  )
}

#' @rdname merge
#' @export
div <- function(x, y) UseMethod("div")

#' @rdname merge
#' @export
div.sparta <- function(x, y) {
  if (!inherits(y, "sparta")) stop("y is not of class 'sparta'", call. = FALSE)
  dnx <- attr(x, "dim_names")
  dny <- attr(y, "dim_names")
  m <- merge_(
    x,
    y,
    attr(x, "vals"),
    attr(y, "vals"),
    names(attr(x, "dim_names")),
    names(attr(y, "dim_names")),
    "/"
  )
  sparta_struct(
    m[[1]],
    m[[2]],
    c(dnx, dny[setdiff(names(dny), names(dnx))])
  )
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
    ifelse(flow == "sum", TRUE, FALSE) # true is "sum" and false is "max"
  )

  sparta_struct(
    m[[1]],
    m[[2]],
    dnx[setdiff(xnames, y)]
  )
}
