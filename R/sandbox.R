# microbenchmark::microbenchmark(
#   m3 <- gRbase::tabMarg(x, c("D", "E", "F", "G", "H")),
#   times = 1
# )


## Rcpp::sourceCpp("../src/marginalize.cpp")

# x <- array(
#   c(1,0,0,2,3,4,0,0),
#   dim = c(2,2,2),
#   dimnames = list(
#     a = c("a1", "a2"),
#     b = c("b1", "b2"),
#     c = c("c1", "c2")
#   )
# )

# y <- array(
#   c(1,3,0,1,2,2,1,0,
#     1,3,0,1,2,2,1,0,
#     1,3,0,1,2,2,1,0),
#   dim = c(2,2,2, 3),
#   dimnames = list(
#     b = c("b1", "b2"),
#     d = c("d1", "d2"),
#     a = c("a1", "a2"),
#     e = c("e1", "e2", "e3")
#   )
# )

# sx <- as_sparta(x)
# sy <- as_sparta(y)




# microbenchmark::microbenchmark(
#   marginalize_sum_2(sx, vals(sx), names(sx), c("A", "C", "E")),
#   marginalize_sum_(sx, vals(sx), names(sx), c("A", "C", "E")),
#   times = 2
# )


# microbenchmark::microbenchmark(
#   marginalize_sum_2(sx, vals(sx), names(sx), c("A", "C", "E")),
#   times = 1
# )

# microbenchmark::microbenchmark(
#   marginalize_sum_(sx, vals(sx), names(sx), c("A", "C", "E")),
#   times = 1
# )

# size_mb <- function(x) {
#   format(object.size(x), units = "Mb", standard = "auto", digits = 1L)
# }


## slice_(sy, vals(sy), dim_names(sy), c("a", "e"), c("a2", "e3"))
## slice(sx, c(a = "a1", b = "b1"))


## marg(sx, c("a",  "b", "c"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        SPARSE EXAMPLES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rcpp::sourceCpp("../src/merge.cpp")

# ndims <- 8L
# nlvls <- 9L

# dims  <- rep(nlvls, ndims)
# n     <- prod(dims)
# sparsity <- 0.35

# x <- array(
#   sample(c(0:1), n, TRUE, c(sparsity, 1 - sparsity)),
#   dim = dims,
#   dimnames = structure(
#     replicate(ndims, letters[1:nlvls], FALSE),
#     names = LETTERS[1:ndims]
#   )
# )

# sx <- as_sparta(x)

# y <- array(
#   sample(c(0:1), n, TRUE, c(sparsity, 1 - sparsity)),
#   dim = dims,
#   dimnames = structure(
#     replicate(ndims, letters[1:nlvls], FALSE),
#     names = LETTERS[3:(ndims+2)]
#   )
# )

# sy <- as_sparta(y)
# sx <- marg(sx, setdiff(names(sx), names(sx)[which(names(sx) %in% names(sy))]))

# rm(x, y)
# gc()
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                 NEW MERGE 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rcpp::sourceCpp("../src/merge.cpp")

# x <- array(
#   c(4,7,0,9),
#   dim = c(2,2),
#   dimnames = list(
#     b = c("b1", "b2"),
#     c = c("c1", "c2")
#   )
# )

# y <- array(
#   c(3,0,0,6,7,8,9,0),
#   dim = c(2,2,2),
#   dimnames = list(
#     a = c("a1", "a2"),
#     b = c("b1", "b2"),
#     c = c("c1", "c2")
#   )
# )

# sx <- as_sparta(x)
# sy <- as_sparta(y)

# microbenchmark::microbenchmark(
#   merge_(
#     sx,
#     sy,
#     vals(sx),
#     vals(sy),
#     names(sx),
#     names(sy)
#   ),
#   merge_subset_(
#     sx,
#     sy,
#     vals(sx),
#     vals(sy),
#     names(sx),
#     names(sy)
#   ),
#   times = 1000
# )

# m <- merge_(
#   sx,
#   sy,
#   vals(sx),
#   vals(sy),
#   names(sx),
#   names(sy)
# )

# ncol(m[[1]])
# object.size(m)
# rm(m); gc()

# # 2.85 -> 
# m1 <- merge_subset_(
#   sx,
#   sy,
#   vals(sx),
#   vals(sy),
#   names(sx),
#   names(sy)
# )

# ncol(m1[[1]]) # 27981238

# object.size(m1) # 1119136624 bytes
# rm(m1); gc()



# merge_(
#   sx,
#   sy,
#   vals(sx),
#   vals(sy),
#   names(sx),
#   names(sy)
# )

# merge_subset_(
#   sx,
#   sy,
#   vals(sx),
#   vals(sy),
#   names(sx),
#   names(sy)
# )




