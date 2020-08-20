## Rcpp::sourceCpp("../src/marginalize.cpp")

## x <- array(
##   c(1,0,0,2,3,4,0,0, 1,0,0,2,3,4,0,0),
##   dim = c(2,2,2,2),
##   dimnames = list(
##     a = c("a1", "a2"),
##     b = c("b1", "b2"),
##     c = c("c1", "c2"),
##     d = c("d1", "d2")
##   )
## )

## y <- array(
##   c(1,3,0,1,2),
##   dim = c(2, 3),
##   dimnames = list(
##     d = c("d1", "d2"),
##     e = c("e1", "e2", "e3")
##   )
## )

## sx <- as_sparse(x)
## sy <- as_sparse(y)

## merge(sx, sy)
## marginalize(sx, c("a",  "b", "c"))



## x <- array(
##   c(1,0,0,2,3,4,0,0),
##   dim = c(2,2,2),
##   dimnames = list(
##     a = c("a1", "a2"),
##     b = c("b1", "b2"),
##     c = c("c1", "c2")
##   )
## )

## y <- array(
##   c(1,3,0,1,2,2,1,0,
##     1,3,0,1,2,2,1,0,
##     1,3,0,1,2,2,1,0),
##   dim = c(2,2,2, 3),
##   dimnames = list(
##     b = c("b1", "b2"),
##     d = c("d1", "d2"),
##     a = c("a1", "a2"),
##     e = c("e1", "e2", "e3")
##   )
## )

## sx <- as_sparse(x)
## sy <- as_sparse(y)

## microbenchmark::microbenchmark(
##   gRbase::tabMult(x,y),
##   merge(sx, sy),
##   times = 2000
## )

## ## # Truely sparse

## ndims <- 7L
## nlvls <- 3L
## dims  <- rep(nlvls, ndims)
## n     <- prod(dims)
## sparsity <- 0.9

## A <- array(
##   sample(c(0:1), n, TRUE, c(sparsity, 1 - sparsity)),
##   dim = dims,
##   dimnames = structure(
##     replicate(ndims, letters[1:nlvls], FALSE),
##     names = LETTERS[1:ndims]
##   )
## )

## offset <- 4L

## B <- array(
##   sample(c(0:1), n, TRUE, c(sparsity, 1 - sparsity)),
##   dim = dims,
##   dimnames = structure(
##     replicate(ndims, letters[1:nlvls], FALSE),
##     names = LETTERS[offset:(ndims + offset -1L)]
##   )
## )

## As <- as_sparse(A)
## Bs <- as_sparse(B)

## microbenchmark(
##   gRbase::tabMult(A,B),
##   merge(As, Bs),
##   times = 1000
## )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      ROBIN HOOD
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## microbenchmark::microbenchmark(
##   stdumap(100000000),
##   rhumap(100000000),
##   times = 3
## )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               Adding + and - to merge_
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                    next_cell
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Rcpp::sourceCpp("../src/misc_utils.cpp")
## cell <- c(0, 2, 1)
## dim <- c(2,2,2)
## next_cell_(cell, dim, 0L) # 1L is the C++ index for the second element
