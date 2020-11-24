#' As sparse table
#'
#' Turn an array-like object or a data.frame into a sparse representation
#'
#' @param x array-like object or a data.frame
#' @return A sparta object
#' @seealso \code{\link{as_array}}
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
#' as_sparta(x)
#'
#' # ----------
#' # Example 2)
#' # ----------
#' 
#' y   <- mtcars[, c("gear", "carb")]
#' y[] <- lapply(y, as.character)
#' as_sparta(y)
#' 
#' @rdname as_sparta
#' @export
as_sparta <- function(x) UseMethod("as_sparta")

#' @rdname as_sparta
#' @export
as_sparta.array <- function(x) {
  if (!is_named_list(dimnames(x))) stop("some dimensions are not named properly")
  dim  <- .map_int(dimnames(x), function(z) length(z))
  sp <- as_sparta_(x, dim)
  sparta_struct(sp[[1]], sp[[2]], dimnames(x))  
}

#' @rdname as_sparta
#' @export
as_sparta.matrix <- as_sparta.array

#' @rdname as_sparta
#' @export
as_sparta.table <- as_sparta.array

#' @rdname as_sparta
#' @export
as_sparta.sparta <- function(x) x

#' @rdname as_sparta
#' @export
as_sparta.data.frame <- function(x) {
  if (!all(lapply(x, class) == "character")) {
     stop("all varibles must be of class 'character'", call. = FALSE)
  }
  as_sparta(table(x, dnn = colnames(x)))
}

#' As array
#'
#' Turn a sparse table into an array
#'
#' @param x sparta object
#' @return An array
#' @seealso \code{\link{as_array}}
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
#' as_array(as_sparta(x))
#' @export
as_array <- function(x) UseMethod("as_array")

#' @rdname as_array
#' @export
as_array.sparta <- function(x) {
  dim_ <- .map_int(attr(x, "dim_names"), function(z) length(z))
  arr  <- array(0L, dim = dim_, dimnames = attr(x, "dim_names"))
  for (j in 1:ncol(x)) {
    colj <- matrix(x[, j], nrow = 1L)
    arr[colj] <- attr(x, "vals")[j]
  }
  arr
}


#' As cpt
#'
#' Turn a sparta into a conditional probability table
#'
#' @param x sparta object
#' @param y the conditioning variables
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
#'
#' # A joint probability table p(a, b, c)
#' as_cpt(sx, character(0))
#' # the same as normalize
#' normalize(sx)
#'
#' # A conditional probability table p(a, c | b)
#' pacb <- as_cpt(sx, "b")
#'
#' # The probability distribution when b = b1
#' slice(pacb, c(b = "b1"))
#' 
#' @export
as_cpt <- function(x, y) UseMethod("as_cpt")


#' @rdname as_cpt
#' @export
as_cpt.sparta <- function(x, y) {

  if(inherits(x, "sparta_unity")) {
    x <- sparta_ones(dim_names(x))
  }
  
  if (!inherits(y, "character")) stop("y must be a character")
  if (eq_empt_chr(y)) return(normalize(x))
  
  cpt <- as_cpt_(x,
    attr(x, "vals"),
    names(attr(x, "dim_names")),
    y
  )

  sparta_struct(cpt[[1]], cpt[[2]], attr(x, "dim_names"))
}
