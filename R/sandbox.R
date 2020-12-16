## Rcpp::sourceCpp("../src/slice.cpp")

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

## sx <- as_sparta(x)
## sy <- as_sparta(y)

## slice_(sy, vals(sy), dim_names(sy), c("a", "e"), c("a2", "e3"))
## slice(sx, c(a = "a1", b = "b1"))

## slice <- function(x, s) UseMethod("slice")

## slice.sparta <- function(x, s) {
##   if (inherits(x, "sparta_unity")) {
##     stop("A sparta_unity cannot be sliced. Try using 'sparta_ones'.")
##   }
##   if (is.null(names(s))) {
##     stop("s must be a named character vector", call. = FALSE)
##   }
##   sp <- slice_(x, vals(x), dim_names(x), names(s), s)
##   sparta_struct(sp[[1]], sp[[2]], dim_names(x))
## }

## microbenchmark::microbenchmark(
##   merge_(
##     sx,
##     sy,
##     vals(sx),
##     vals(sy),
##     names(sx),
##     names(sy)
##   ),
##   merge2_(
##     sx,
##     sy,
##     vals(sx),
##     vals(sy),
##     names(sx),
##     names(sy)
##   ),
##   times = 10
## )


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

## ndims <- 3L
## nlvls <- 2L
## dims  <- rep(nlvls, ndims)
## n     <- prod(dims)
## sparsity <- 0.7
## x <- array(
##   sample(c(0:1), n, TRUE, c(sparsity, 1 - sparsity)),
##   dim = dims,
##   dimnames = structure(
##     replicate(ndims, letters[1:nlvls], FALSE),
##     names = LETTERS[1:ndims]
##   )
## )

# https://notast.netlify.app/post/r-you-ready-for-python-gentle-introduction-to-reticulate-package/
# https://rstudio.github.io/reticulate/articles/calling_python.html


## library(reticulate)
## gum <- import("pyAgrum", convert = FALSE)

## bn = gum$BayesNet('test_potentials')

## for (name_ in c("a", "b", "c", "d", "e")) {
##   bn$add(name_, 2L)
## }

## for (e in list(c("b", "a"), c("c", "a"), c("a", "d"), c("e", "d"))) {
##   bn$addArc(e[1], e[2])
## }

## bn$cpt("a")$fillWith(as.integer(1:2^3))
## bn$cpt("d")$fillWith(as.integer(1:2^3))

## py_run_string("def mult (x, y):
##           return(x * y)")

## py$mult(bn$cpt("a"), bn$cpt("d"))

## construct_R_potential <- function(ndims, nlvls, sparsity) {
##   dims <- rep(nlvls, ndims)
##   n <- prod(dims)
##   array(
##     sample(c(0:1), n, TRUE, c(sparsity, 1 - sparsity)),
##     dim = dims,
##     dimnames = structure(
##       replicate(ndims, letters[1:nlvls], FALSE),
##       names = LETTERS[1:ndims]
##     )
##   )  
## }



## offset <- 4L

## y <- array(
##   sample(c(0:1), n, TRUE, c(sparsity, 1 - sparsity)),
##   dim = dims,
##   dimnames = structure(
##     replicate(ndims, letters[1:nlvls], FALSE),
##     names = LETTERS[offset:(ndims + offset -1L)]
##   )
## )

## y <- array(
##   sample(c(0:1), n, TRUE, c(sparsity, 1 - sparsity)),
##   dim = c(2, 2, 2, 2),
##   dimnames = structure(
##     replicate(4, c("a", "b"), FALSE),
##     names = c("A", "B", "Q", "Z")
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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   UNIT TABLE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

## sx   <- as_sparta(x)

## sy <- sparta_unity_struct(
##   list(
##     c = c("c1", "c2"),
##     a = c("a1", "a2"),
##     e = c("e1", "e2"),
##     f = c("f1", "f2")
##   )
## )

## sy <- sparta_unity_struct(
##   list(
##     l = c("e1", "e2"),
##     s = c("f1", "f2"),
##     e = c("f1", "f2"),
##     r = c("f1", "f2"),
##     q = c("f1", "f2")
##   )
## )

## dimy <- .map_int(dim_names(sy), length)

## merge_unity_(
##   sx,
##   vals(sx),
##   names(sx),
##   names(sy),
##   dimy,
##   '/',
##   FALSE # only a problem for '/'!
## )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                       LINKS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## l <- readRDS("../../../../sandbox/r/link/link.rds")

## cpts <- lapply(l, function(x) {
##   xp <- x$prob
##   class(xp) <- c("array", class(xp))
##   if (length(dim(xp)) == 1L) {
##     xn <- structure(list(dimnames(xp)[[1]]), names = x$node)
##     dimnames(xp) <- xn
##   }
##   if (!sparta:::is_named_list(dimnames(xp))) browser()
##   xp
## })

## cl <- jti::cpt_list(cpts)
## cp <- jti::compile(cl, save_graph = TRUE)
## g  <- jti::dag(cp)
## plot(g)

## Rcpp::sourceCpp("../src/merge.cpp")

## mult(cp$charge$C[[59]], cp$charge$C[[60]]) #cp$charge$C[[60]])

## plot(g, vertex.size = 0, vertex.label = NA)
## mg <- moralize(g)
## plot(mg, vertex.size = 0, vertex.label = NA)
## mtg <- triangulate(mg)
## plot(mtg, vertex.size = 0, vertex.label = NA)

## attr(cp, "cliques")
## .map_int(cp$cliques, length)
## cp$cliques$C55

## .map_int(cp$charge$C, function(x) length(names(x)))
# sparta::mult(cp$charge$C[[59]], cp$charge$C[[60]])
## cp$charge$C[[60]]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                 XPTR    project
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## // [[Rcpp::export]]
## SEXP make_xptr() {
##   arma::Mat<unsigned short> A(1000, 1000000);
##   // arma::Mat<short>* ptr = new arma::Mat<short>;
##   // (*ptr) = A;
##   arma::Mat<unsigned short>* ptr(new arma::Mat<unsigned short>(A));
##   std::cout << sizeof(*ptr) << "\n";
##   XPtr<arma::Mat<unsigned short>> p(ptr, true);
##   return p;
## }

## size_mb <- function(x) {
##   format(object.size(x), units = "Mb", standard = "auto", digits = 1L)
## }

## pryr::mem_used()

## A <- matrix(1L, nrow  = 1000L, ncol = 100000)
## size_mb(A)      # 3.814G

# Initial: 1.04G / 7.45G
# a <- make_xptr() # 1.87G -> 2.91G

# Initial: 1.07G / 7.45G
# a <- make_xptr() # 1.87G -> 2.91G // using short!

# Initial: 1.08G / 7.45G
# a <- make_xptr() # 3.73G -> 4.81G // using int!


# Initial: 1.08G / 7.45G
# a <- make_xptr() # 1.85G -> 2.93G // using unsigned short!

## A <- matrix(1L, nrow = 1000L, ncol = 1000000L) # 3.8G
## rm(A)
## size_mb(A)

## x1   <- new.env()
## a <- create_xptr(1000L, 100000L)
## fill_xptr(a, 1L)
## g <- return_val(a) # 381.5MB
## release_me(a)
## rm(a)

## f <- function(e) release_me(e$b)

## x1   <- new.env()
## x1$b <- create_xptr2(1000L, 1000000L) # 3.61- 1.75 = 1.86G
## reg.finalizer(x1, f)
## rm(x1)

## x2   <- new.env()
## x2$b <- create_xptr2(1000L, 1000000L) # 2.24G
## reg.finalizer(x2, f)
## rm(x2)

## x3   <- new.env()
## x3$b <- create_xptr2(1000L, 1000000L) # 2.24G
## reg.finalizer(x3, f)
## rm(x3)

## x4   <- new.env()
## x4$b <- create_xptr2(1000L, 1000000L) # 2.24G
## reg.finalizer(x4, f)
## rm(x4)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## gcinfo(TRUE)
## pryr::mem_used()

## make_ <- function() {
##   e <- new.env(size = 3e8); rm(e)
##   e <- new.env()
##   e$xptr <- create_xptr2(1000L, 1e6)
##   reg.finalizer(e, function(e) release_me(e$xptr))
##   e
## }

## f <- function(e) release_me(e)

## a <- create_xptr2(1000L, 1e6L)
## reg.finalizer(a, f)
## rm(a)
## gc()

## b <- make_()
## rm(b)

## c <- make_()
## rm(c)

## d <- make_()
## rm(d)

## create_xptr2(1000L, 1e6)

## e <- new.env(size = 1e8)
## r <- new.env(size = 3e8)
## pryr::mem_used()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#               as_sparta.data.frame 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Rcpp::sourceCpp("../src/converters.cpp")
## A <- jti::asia


## microbenchmark::microbenchmark(
##   as_sparta__(A, structure(lapply(A, unique), names = colnames(A))),
##   table(A)
## )

## microbenchmark::microbenchmark(
##   as_sparta__(d, structure(lapply(d, unique), names = colnames(d))),
##   table(d),
##   times = 5
## )


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  FIXING SLICE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Rcpp::sourceCpp("../src/slice.cpp")

## bnlearn_to_cpts <- function(l) {
##   cpts <- lapply(l, function(x) {
##     xp <- x$prob
##     # Make as_sparta.table instead of this hack
##     class(xp) <- c("array", class(xp))
##     if (length(dim(xp)) == 1L) {
##       xn <- structure(list(dimnames(xp)[[1]]), names = x$node)
##       dimnames(xp) <- xn
##     }
##     xp
##   })
## }

## nodes <- c(
##   "meldug_1",
##   "meldug_2",
##   "middel_1",
##   "lai_0",
##   "lai_1",
##   "nedboer_1",
##   "mikro_1",
##   "temp_1",
##   "foto_1",
##   "straaling_1",
##   "dm_1",
##   "dm_2", # From here it fucks up!
##   "foto_2",
##   "straaling_2",
##   "temp_2",
##   "lai_2"
## )

## l    <- readRDS("../../../../sandbox/r/bns/mildew.rds")
## cpts <- bnlearn_to_cpts(l)
## cpts <- cpts[nodes]
## cl   <- jti::cpt_list(cpts)



## s <- c(foto_2 = "0_55_kg_m2", dm_1 = "0_16_kg_m2") # 12 - 9
## s <- s[2:1]
## slice_(cl$dm_2, vals(cl$dm_2), dim_names(cl$dm_2), names(s), s)


## gRbase::tabSlice(
##   cpts$dm_2,
##   slice = list(foto_2 = "0_55_kg_m2", dm_1 = "0_16_kg_m2")
## )[21:26]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## x <- readRDS("/home/mads/Documents/phd/software/sparta/inst/sp_pot_par_k.rds")
## y <- readRDS("/home/mads/Documents/phd/software/sparta/inst/message_k.rds")
## z <- readRDS("/home/mads/Documents/phd/software/sparta/inst/gr_pot_par_k.rds")

## nz <- names(z)
## a <- apply(z, 2L, function(j) {
##   cell <- structure(get_cell_name(z, j), names = nz)
##   val  <- get_val(z, cell)
##   cell <- c(cell, val = val)
##   list(cell, "-")
## })

## d <- lapply(a, function(x) {
##   d <- as.data.frame(matrix(x[[1]], 1))
##   colnames(d) <- c(nz, "val")
##   d
## })
## d <- do.call(rbind, d)


## m <- merge_(x, y, vals(x), vals(y), names(x), names(y))
## dn <- dim_names(z)
## dn <- dn[c(3,2,1,5,4)]


## dn1 <- attr(x, "dim_names")
## dn2 <- attr(y, "dim_names")
## c(dn1, dn2[setdiff(names(dn2), names(dn1))])

## e <- sparta_struct(m[[1]], m[[2]], dn)
## dim_names(e)
## apply(e, 2L, function(j) {
##   cell <- get_cell_name(e, j)
## })
## equiv(e, z)
