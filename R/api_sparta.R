merge <- function(x, y, mult = TRUE) {
  is_y_scalar <- is_scalar(y)
  
  if (!inherits(y, "sparta") && !is_y_scalar) {
    stop("y must be of class 'sparta' or a scalar", call. = FALSE)
  }

  is_x_unity  <- inherits(x, "sparta_unity")
  
  if (is_y_scalar) {
    if (is_x_unity) {
      attr(x, "vals") <- attr(x, "vals") * y
      return(x)
    }
    attr(x, "vals") <- if (mult) attr(x, "vals") * y else attr(x, "vals") / y
    return(x)
  }

  is_y_unity <- inherits(y, "sparta_unity")
  dn1        <- attr(x, "dim_names")
  dn2        <- attr(y, "dim_names")
  dn         <- c(dn1, dn2[setdiff(names(dn2), names(dn1))])

  if (is_x_unity && is_y_unity) {
    return(sparta_unity_struct(dn, if (mult) attr(x, "vals") * attr(y, "vals") else attr(x, "vals") / attr(y, "vals")))
  }

  x_in_y <- all(names(x) %in% names(y))
  y_in_x <- all(names(y) %in% names(x))

  # TODO: Implement merge_unity_subset_
  # easy: sparta::mult(tab, sparta_rank(sub_tab))
  
  m <- if (is_x_unity || is_y_unity) {
    if (is_y_unity && !is_x_unity) {
      ydim <- .map_int(dim_names(y), length)
      merge_unity_(x, vals(x), names(x), names(y), ydim, attr(y, "vals"))
    } else if (!is_y_unity && is_x_unity) {
      xdim <-.map_int(dim_names(x), length)
      merge_unity_(y, vals(y), names(y), names(x), xdim, attr(x, "vals"), ifelse(mult, FALSE, TRUE))
    }
  } else if (x_in_y) {
    merge_subset_(x, y, vals(x), vals(y), names(x), names(y), ifelse(mult, "*", "/"))
  } else if (y_in_x){
    merge_subset_(y, x, vals(y), vals(x), names(y), names(x), ifelse(mult, "*", "/"))
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
#' sparsity(sx)
#' table_size(sx)
#' dim_names(sx)
#' names(sx)
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
#'
#' # ----------
#' # Example 3)
#' # ----------
#'
#' su <- sparta_unity_struct(dim_names(sy), rank = 3.1415)
#' sparta_rank(su)
#' sum(su)
#' sun <- normalize(su)
#' sun
#' sum(sun)
#'
#' mult(sx, sun)
#' 
#' # ----------
#' # Example 4)
#' # ----------
#' so <- sparta_ones(dim_names(sx))
#' mult(so, 2)

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
#' su <- sparta_unity_struct(dim_names(sx), rank = 3.14)
#' marg(su, c("a", "b"))
#' @export
marg <- function(x, y, flow = "sum") UseMethod("marg")

#' @rdname marg
#' @export
marg.sparta <- function(x, y, flow = "sum") {

  if (eq_empt_chr(y)) return(x)
  
  if (inherits(x, "sparta_unity")) {
    dny <- dim_names(x)[y]
    attr(x, "vals") <- attr(x, "vals") * prod(.map_dbl(dny, length))

    attr(x, "dim_names") <- dim_names(x)[setdiff(names(x), names(dny))]
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
#' u <- sparta_unity_struct(dim_names(sx))
#' slice(u, c(a = "a1"), drop = TRUE)
#' 
#' @export
slice <- function(x, s, drop = FALSE) UseMethod("slice")

#' @rdname slice
#' @export
slice.sparta <- function(x, s, drop = FALSE) {

  if (length(s) == length(names(x)) && drop == TRUE) {
    stop(
      "cannot slice and drop all variables.",
      " At least one variable must remain when drop = TRUE."
    )
  }
  
  if (!is_non_empty_vector_chr(s)) {
    stop("s must be a character vector of length > 0")
  }
  
  if (!all(names(s) %in% names(x))) {
    stop("some names in s are not in x. see `dim_names(x)`", call. = FALSE)
  }
  
  if (inherits(x, "sparta_unity")) {
    if (drop == TRUE) {
      dn_new <- dim_names(x)[setdiff(names(x), names(s))]
      return(sparta_unity_struct(dn_new, rank = attr(x, "vals")))
    } else {
      stop(
        "A sparta_unity cannot be sliced ",
        "unless drop = TRUE. If needed, use 'sparta_ones'.", call. = FALSE)      
    }
  }

  # IDEA:
  # let s be a list, so we can slice on a range of levels
  # i.e. list(a = c("a1", "a5"), b = c("b1"))
  # will retain all levels of a that have "a1" and "a5"
  sp <- slice_(x, vals(x), dim_names(x), names(s), s)

  if (drop) {
    dn_new <- dim_names(x)[setdiff(names(x), names(s))]
    idx_s <- match(names(s), names(x))
    return(sparta_struct(sp[[1]][-idx_s, , drop = FALSE], sp[[2]], dn_new))
  }
  
  sparta_struct(sp[[1]], sp[[2]], dim_names(x))
}
