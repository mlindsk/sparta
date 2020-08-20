.set_as_sparr <- function(m, dim_names_x, dim_names_y) {
  sparr <- structure(
    m[[1]],
    dim_names = c(
      dim_names_x,
      dim_names_y[setdiff(names(dim_names_y), names(dim_names_x))]),
    class = c("sparr", class(m[[1]]))
  )
  storage.mode(sparr) = "integer"
  attr(sparr, "vals") <- m[[2]]
  sparr
}

merge.sparr <- function(x, y, op = "*") {
  # TODO: Implement the "op" argument in merge_
  if (!inherits(y, "sparr")) stop("y is not of class 'sparr'", call. = FALSE)
  m <- merge_(
    x,
    y,
    attr(x, "vals"),
    attr(y, "vals"),
    names(attr(x, "dim_names")),
    names(attr(y, "dim_names"))
  )
  .set_as_sparr(m, attr(x, "dim_names"), attr(y, "dim_names"))
}

marginalize <- function(x, y) UseMethod("marginalize")

marginalize.sparr <- function(x, y) {
  xnames <- names(attr(x, "dim_names"))
  m <- marginalize_(
    x,
    attr(x, "vals"),
    xnames,
    y
  )
  .set_as_sparr(m, attr(x, "dim_names")[setdiff(xnames, y)], character(0))
}
