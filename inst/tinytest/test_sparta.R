x <- array(
  c(1,0,0,2,3,4,0,0),
  dim = c(2,2,2),
  dimnames = list(
    a = c("a1", "a2"),
    b = c("b1", "b2"),
    c = c("c1", "c2")
  )
)

y <- array(
  c(1,3,0,1,2,2,1,0,
    1,3,0,1,2,2,1,0,
    1,3,0,1,2,2,1,0),
  dim = c(2,2,2, 3),
  dimnames = list(
    b = c("b1", "b2"),
    d = c("d1", "d2"),
    a = c("a1", "a2"),
    e = c("e1", "e2", "e3")
  )
)

sx <- as_sparta(x)
sy <- as_sparta(y)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# merging a table with itself
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
msx <- mult(sx, sx)
msy <- mult(sy, sy)
expect_equal(ncol(msx), ncol(sx))
expect_equal(ncol(msy), ncol(sy))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# test that multiplication is commutative
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
msxy1 <- mult(sx, sy)
msxy2 <- mult(sy, sx)
cell  <- c(a = "a2", b = "b1", c = "c2", d = "d1", e = "e1")
expect_equal(
  get_val(msxy1, cell),
  get_val(msxy2, cell)  
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# test that self-division results in ones
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dsxy1 <- div(sx, sx)
dsxy2 <- div(sy, sy)

expect_equal(
  sum(dsxy1),
  ncol(dsxy1)  
)

expect_equal(
  sum(dsxy2),
  ncol(dsxy2)
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# marginalize
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
expect_equal(
  marg(msxy1, c("b", "c", "d", "e")),
  marg(msxy2, c("b", "c", "d", "e"))  
)

msxy1_arr <- as_array(msxy1)

expect_equal(
  get_val(marg(msxy1, c("b", "c", "d", "e")), c(a = "a1")),
  unname(apply(msxy1_arr, 1L, sum)["a1"])
)

# z is not a variable in msxy1
expect_error(
  marg(msxy1, c("z"))
)

# marginalizing out all vaiables
expect_equal(
  marg(msxy1, c("a", "b", "c", "d", "e")),
  sum(msxy1)
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# conditional probability tables
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 3 configurations of a and b (1, 2) is missing
expect_identical(
  sum(as_cpt(msxy1, c("a", "b"))),
  3
)

# normalizing
expect_identical(
  sum(as_cpt(msxy1, character(0))),
  sum(normalize(msxy1)),
  1
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# test that null-table times anything is null
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
snull <- structure(
    matrix(nrow = 0L, ncol = 0L),
    vals = vector("numeric", length = 0L),
    dim_names = dim_names(sy),
    class = c("sparta", "matrix")
)

expect_equal(
  ncol(mult(sx, snull)),
  0L
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  slicing on a cell that was never observed
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
expect_error(
  slice(sx, c(b = "b2", c = "c2"))
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  multiplication with a scalar 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
expect_identical(
  mult(2, sx),
  mult(sx, 2)
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  multiplication and division of two unities
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
r1 <- 0.3
r2 <- 0.6
u1 <- sparta_unity_struct(dim_names(sx), r1)
u2 <- sparta_unity_struct(dim_names(sy), r2)
dn1 <- attr(u1, "dim_names")
dn2 <- attr(u2, "dim_names")
dn  <- c(dn1, dn2[setdiff(names(dn2), names(dn1))])

expect_identical(
  sparta_unity_struct(dn, r1*r2),
  mult(u1,u2)
)

expect_identical(
  sparta_unity_struct(dn, r1/r2),
  div(u1, u2)
)

# reverting u1 and u2 permnutes the dim names
expect_equal(
  list(dim_names(sparta_unity_struct(dn, r2/r1)), sparta_rank(sparta_unity_struct(dn, r2/r1))),
  list(dim_names(div(u2, u1))[names(sparta_unity_struct(dn, r2/r1))], sparta_rank(div(u2, u1)))
)
