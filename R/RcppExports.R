# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

as_sparta_ <- function(arr, dim) {
    .Call(`_sparta_as_sparta_`, arr, dim)
}

as_cpt_ <- function(x, xval, xvar, y) {
    .Call(`_sparta_as_cpt_`, x, xval, xvar, y)
}

marginalize_sum_ <- function(x, xval, xvar, y) {
    .Call(`_sparta_marginalize_sum_`, x, xval, xvar, y)
}

marginalize_sum_2 <- function(x, xval, xvar, y) {
    .Call(`_sparta_marginalize_sum_2`, x, xval, xvar, y)
}

marginalize_max_ <- function(x, xval, xvar, y) {
    .Call(`_sparta_marginalize_max_`, x, xval, xvar, y)
}

merge_ <- function(x, y, xval, yval, xvar, yvar, op = "*") {
    .Call(`_sparta_merge_`, x, y, xval, yval, xvar, yvar, op)
}

merge_unity_ <- function(x, xval, xvar, yvar, ydim, rank = 1, reciprocal = FALSE) {
    .Call(`_sparta_merge_unity_`, x, xval, xvar, yvar, ydim, rank, reciprocal)
}

merge_subset_ <- function(x, y, xval, yval, xvar, yvar, op = "*") {
    .Call(`_sparta_merge_subset_`, x, y, xval, yval, xvar, yvar, op)
}

slice_ <- function(x, xval, dim_names, slice_names, slice_cell) {
    .Call(`_sparta_slice_`, x, xval, dim_names, slice_names, slice_cell)
}

