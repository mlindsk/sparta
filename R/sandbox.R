## Rcpp::sourceCpp("src/marginalize.cpp")

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

## sx <- as_sparta(x)
## marginalize(sx, "a", FALSE)

## sy <- as_sparta(y)

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        SPARSE EXAMPLES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ndims <- 7L
## nlvls <- 3L
## dims  <- rep(nlvls, ndims)
## n     <- prod(dims)
## sparsity <- 0.9

## x <- array(
##   sample(c(0:1), n, TRUE, c(sparsity, 1 - sparsity)),
##   dim = dims,
##   dimnames = structure(
##     replicate(ndims, letters[1:nlvls], FALSE),
##     names = LETTERS[1:ndims]
##   )
## )

## offset <- 4L

## y <- array(
##   sample(c(0:1), n, TRUE, c(sparsity, 1 - sparsity)),
##   dim = dims,
##   dimnames = structure(
##     replicate(ndims, letters[1:nlvls], FALSE),
##     names = LETTERS[offset:(ndims + offset -1L)]
##   )
## )

## sx <- as_sparta(x)
## sy <- as_sparta(y)

# rm(x,y)

## dimx <- attr(sx, "dim_names")
## dimy <- attr(sy, "dim_names")

## dims <- .map_int(
##   c(
##   dimx,
##   dimy[setdiff(names(dimy), names(dimx))]
##   ),
##   length
## )


## microbenchmark::microbenchmark(
##   gRbase::tabAdd(x,y),
##   merge(sx, sy),
##   merge2_(
##     sx,
##     sy,
##     attr(sx, "vals"),
##     attr(sy, "vals"),
##     names(dimx),
##     names(dimy),
##     dims
##   ) ,
##   times = 1
## )

## microbenchmark::microbenchmark(
##   gRbase::tabMult(x,y),
##   merge(sx, sy),
##   times = 5
## )


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
##   c(0,0,0,2,4,0,3,5,rep(0,8)),
##   dim = c(2, 2, 2, 2),
##   dimnames = list(
##     e = c("e1", "e2"),
##     f = c("f1", "f2"),
##     c = c("c1", "c2"),
##     d = c("d1", "d2")
##   )
## )

## sx <- as_sparta(x)
## sy <- as_sparta(y)

## dimx <- attr(sx, "dim_names")
## dimy <- attr(sy, "dim_names")

## dims <- .map_int(
##   c(
##   dimx,
##   dimy[setdiff(names(dimy), names(dimx))]
##   ),
##   length
## )

## merge3_(
##   sx,
##   sy,
##   attr(sx, "vals"),
##   attr(sy, "vals"),
##   names(dimx),
##   names(dimy),
##   dims
## )


## microbenchmark::microbenchmark(
##   a <- merge2_(
##     sx,
##     sy,
##     attr(sx, "vals"),
##     attr(sy, "vals"),
##     names(dimx),
##     names(dimy),
##     dims
##   ),
##   b <- gRbase::tabAdd(x,y),
##   times = 1
## )




## size_mb(b)
## size_mb(a[[1]])

## length((a[[2]])

## length(which(gRbase::tabAdd(x, y) != 0))

## a <- gRbase::tabAdd(x, y)
## y <- merge3(sx, sy, dims)
## names(attr(y, "dim_names"))
## a[matrix(y[, 6L][match(names(dimnames(a)), names(attr(y, "dim_names")))], nrow = 1L)]
## merge3 <- function(x, y, dims, op = "*") {
##   if (!inherits(y, "sparta")) stop("y is not of class 'sparta'", call. = FALSE)
##   m <- merge3_(
##     x,
##     y,
##     attr(x, "vals"),
##     attr(y, "vals"),
##     names(attr(x, "dim_names")),
##     names(attr(y, "dim_names")),
##     dims
##   )
##   .set_as_sparta(m, attr(x, "dim_names"), attr(y, "dim_names"))
## }

## microbenchmark::microbenchmark(
##   merge3(sx, sy, dims),
##   gRbase::tabAdd(x,y),
##   times = 50
## )


## microbenchmark::microbenchmark(
##   a <- merge.sparta(sx, sy),
##   b <- gRbase::tabMult(x, y),
##   times = 5
## ) 

## size_mb(b)

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

## as_sparta(x)
## dims <- .map_int(dimnames(x), length)

## microbenchmark::microbenchmark(
##   merge(sx, sy, "/"),
##   gRbase::tabDiv(x,y),
##   times = 200
## )

## size_mb <- function(x) {
##   format(object.size(x), units = "Mb", standard = "auto", digits = 1L)
## }

## a <- merge(sx, sy)
## rm(a)

## yn <- c("yes", "no")

## x    <- matrix(c(2,2,2,1,1,1,1,2,1,1,1,2), nrow = 3)
## xval <- c(1, 1, 1, 1)
## xdim <-   list(either = yn, lung = yn, tub = yn)

## par <- sparta_struct(
##   x,
##   xval,
##   xdim
## )

## y    <- matrix(c(1,2,2,2,1,1,2,1), nrow = 2)
## yval <- c(0.945, 0.945, 0.055, 0.055)
## ydim <- list(lung = yn, either = yn)

## msg <- sparta_struct(
##   y,
##   yval,
##   ydim
## )

## mult(par, msg)

## Rcpp::sourceCpp("../src/merge.cpp")

## a <- merge_(
##   x,
##   y,
##   xval,
##   yval,
##   names(xdim),
##   names(ydim)
## )



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        Browse[3]> message_k_names
## [1] "smoke"


## Browse[3]> jt$charge$C[[C_lvs_k_name]]
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
## [1,]    1    1    1    1    2    2    2    2
## [2,]    1    1    2    2    1    1    2    2
## [3,]    2    1    2    1    2    1    2    1
## attr(,"dim_names")
## attr(,"dim_names")$smoke
## [1] "yes" "no" 

## attr(,"dim_names")$lung
## [1] "yes" "no" 

## attr(,"dim_names")$either
## [1] "yes" "no" 

## attr(,"class")
## [1] "sparta" "matrix"
## attr(,"vals")
## [1] 0.050 0.050 0.450 0.450 0.005 0.005 0.495 0.495
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## x <- c(
##   1,1,2,
##   1,1,1,
##   1,2,2,
##   1,2,1,
##   2,1,2,
##   2,1,1,
##   2,2,2,
##   2,2,1
## )

## x  <- matrix(x, nrow = 3)
## v  <- c(0.050, 0.050, 0.450, 0.450, 0.005, 0.005, 0.495, 0.495)
## dn <- list(smoke = c("yes", "no"), lung = c("yes", "no"), either = c("yes", "no"))
## px <- sparta_struct(x, v, dn)

## Rcpp::sourceCpp("../src/marginalize.cpp")

## marginalize_(
##   x,
##   v,
##   names(dn),
##   "smoke"
## )

## marg(px, "smoke")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                 MUNIN TABLE 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## f <- "/home/mads/Documents/phd/software/sparta/inst/extdat/arr.Rds"
## a <- readRDS(f)
## sp   <- a[[1]]
## vals <- attr(sp, "vals")
## dn   <- attr(sp, "dim_names")
## msg  <- a[[2]]

## m <- marginalize_(
##   sp,
##   vals,
##   names(dn),
##   msg
## )

## Rcpp::sourceCpp("../src/marginalize.cpp")
