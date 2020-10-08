#' Classes that can be converted to sparta
#'
#' A non-argument function, that outputs the classes
#' that can be converted to a sparta object
#' 
#' @export
allowed_class_to_sparta <- function() {
  c(.map_chr(utils::methods("as_sparta"), function(generic) sub("as_sparta.", "", generic)))  
}

#' Sparta Ones
#'
#' Construct a sparta object filled with ones
#'
#' @param dim_names A named list of discrete levels
#' @return A sparta object
#' @examples
#' sparta_ones(list(a = c("a1", "a2"), b = c("b1", "b2")))
#' @export
sparta_ones <- function(dim_names) {
  dim_ <- .map_int(dim_names, length)
  utab <- array(1L, dim_, dim_names)
  as_sparta(utab)
}

#' Sparse unity table
#'
#' Construct a sparse table of ones
#'
#' @param dim_names A named list of discrete levels
#' @return A sparta object
#' @examples
#' sparta_unity_struct(list(a = c("a1", "a2"), b = c("b1", "b2")))
#' @export
sparta_unity_struct <- function(dim_names) {
  structure(
    matrix(nrow = 0L, ncol = 0L),
    vals = vector("numeric", length = 0L),
    dim_names = dim_names,
    class = c("sparta_unity", "sparta", "matrix")
  )
}

#' Construct sparta object
#'
#' Helper function to construct a sparta object with given values and dim names
#' 
#' @param x matrix where columns represents cells in an array-like object
#' @param vals vector of values corresponding to x
#' @param dim_names a named list
#' @return A sparta object
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
#' sparta_struct(sx, vals(sx), dim_names(sx))
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


#' Get value 
#'
#' Find the value corresponding to the configuration in y
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
  
  if (is.null(names(y)) || (length(y) != length(attr(x, "dim_names")))) {
    stop("y must have names corresponding to the respective variables", call. = TRUE)
  }
  
  x_dim_names <- attr(x, "dim_names")
  y         <- y[names(x_dim_names)]
  idx       <- mapply(match, y, x_dim_names)
  
  if (anyNA(idx)) stop("some values of y are not valid.", call. = FALSE)
  if (inherits(x, "sparta_unity")) return(1L)
  
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
  if (inherits(x, "sparta_unity")) {
    stop("a sparta_unity cannot be normalize. see 'sparta_ones'")
  }
  attr(x, "vals") <- attr(x, "vals") / sum(attr(x, "vals"))
  x
}

#' Sparta getters
#'
#' Getter methods for sparta objects
#' 
#' @param x sparta object

#' @rdname getter
#' @export
vals <- function(x) UseMethod("vals")

#' @rdname getter
#' @export
vals.sparta <- function(x) attr(x, "vals")

#' @rdname getter
#' @export
dim_names <- function(x) UseMethod("dim_names")

#' @rdname getter
#' @export
dim_names.sparta <- function(x) attr(x, "dim_names")

#' @rdname getter
#' @export
names.sparta <- function(x) names(attr(x, "dim_names"))


#' Vector-like operations on sparta objects

#' @param x sparta
#' @param ... For S3 compatability.

#' @rdname vec-ops
#' @export
sum.sparta <- function(x, ...) {
  if (inherits(x, "sparta_unity")) {
    ncells <- .map_int(dim_names(x), length)
    return(prod(ncells))
  }
  sum(attr(x, "vals"))
}

#' @rdname vec-ops
#' @export
max.sparta <- function(x, ...) {
  if (inherits(x, "sparta_unity")) return(1L)
  max(attr(x, "vals"))
}

#' @rdname vec-ops
#' @export
min.sparta <- function(x, ...) {
  if (inherits(x, "sparta_unity")) return(1L)
  min(attr(x, "vals"))
}

#' @rdname vec-ops
#' @export
which_min_cell <- function(x) UseMethod("which_min")

#' @rdname vec-ops
#' @export
which_min_cell.sparta <- function(x) {
  if (inherits(x, "sparta_unity")) {
    return(.map_chr(dim_names(x), function(z) z[1]))
  }
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
  if (inherits(x, "sparta_unity")) return(1L)
  which.min(attr(x, "vals"))
}

#' @rdname vec-ops
#' @export
which_max_cell <- function(x) UseMethod("which_max_cell")

#' @rdname vec-ops
#' @export
which_max_cell.sparta <- function(x) {
  if (inherits(x, "sparta_unity")) {
    return(.map_chr(dim_names(x), function(z) z[1]))
  }
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
  if (inherits(x, "sparta_unity")) return(1L)
  which.max(attr(x, "vals"))
}

#' Print
#'
#' Print method for sparta objects
#' 
#' @param x sparta object
#' @param ... For S3 compatability. Not used.
#' @export
print.sparta <- function(x, ...) {
  if (inherits(x, "sparta_unity")) {
    cat(" <sparta_unity>\n")
  } else {
    cat(" <cells>")
    prmatrix(
      x,
      rowlab = names(attr(x, "dim_names")),
      collab = rep("", ncol(x))
    )
    cat("\n <vals>")
    prmatrix(
      matrix(attr(x, "vals"), nrow = 1L),
      rowlab = "",
      collab = rep("", length(attr(x, "vals")))
    )
  }
  cat("\n <dim_names>\n")
  dn  <- attr(x, "dim_names")
  ndn <- names(dn)
  for (k in 1:length(dn)) {
    dn_k <- paste(ndn[k], ": ", paste(dn[[k]], collapse = ", "), sep = "")
    cat(dn_k, "\n")
  }
}
