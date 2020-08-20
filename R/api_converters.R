as_sparr <- function(x) UseMethod(x)

as_sparr.array <- function(x) {
  # TODO: Convert to cpp: as_sparse_array_.cpp
  
  # x: array-like type. See jti:::.allowed_cpt_classes() (minus sptable)

  # TODO: Make a getter `[` for `sparse` object. Otherwise, the attributes
  # vanishes when `[` is dispatched on to the matrix class

  # TODO: Handle one-dimensional arrays (vectors): jti::asia2[[1]]
  ## if (length(dimnames(x)) == 1L) {
  ##   stop("one-dimensional arrays cannot be converted at the moment")
  ## }
  
  if (!is_named_list(dimnames(x))) stop("some dimensions are not named properly")

  dims  <- .map_int(dimnames(x), function(z) length(z))
  ndims <- length(dims)
  nvals <- length(x)
  non_zeroes <- which(x != 0L)
  
  sparr <- matrix(
    nrow = length(dims) + 1L, # 1L is the value row
    ncol = length(non_zeroes)
  )

  cell <- rep(1, ndims)
  non_zero_counter <- 1L
  
  for (i in 1:nvals) {
    ival <- x[matrix(cell, nrow = 1L)]
    if (ival != 0L) {
      sparr[, non_zero_counter] <- c(cell, ival)
      non_zero_counter <- non_zero_counter + 1L
    }
    # TODO: Remove dependence when as_sparse is converted to cpp
    cell <- gRbase::next_cell(cell, dims)
  }

  vals <- sparr[nrow(sparr), ]
  
  sparr <- structure(
    sparr[-nrow(sparr), , drop = FALSE],
    dim_names = dimnames(x),
    class = c("sparr", class(sparr))
  )

  storage.mode(sparr) = "integer"
  attr(sparr, "vals") <- vals
  sparr
}


as_sparr.data.frame <- function(x) {
  # Convert to as_sparse_data_frame_.cpp
  arr <- array(0L,
    dim =  .map_int(x, function(z) length(unique(z))),
    dimnames = lapply(x, unique)
  )
  
  for (k in 1:nrow(x)) {
    idx <- as.matrix(x[k, ], nrow = 1L)
    arr[idx] <- arr[idx] + 1L
  }
  
  as_sparr.array(arr)
}

## x <- as_sparr.data.frame(jti::asia[, 1:7])
## y <- as_sparr.data.frame(jti::asia[, 2:7])

## merge(x, y)
## marginalize(x, c("A", "L", "T"))
