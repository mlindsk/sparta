#include "sparta_types.h"
#include "set_ops.h"
#include "misc_utils.h"


// [[Rcpp::export]]
Rcpp::List slice_(
  arma::Mat<short>& x,
  vec_dbl& xval,
  Rcpp::List dim_names,
  vec_str slice_names,
  vec_str slice_cell
  )
{

  vec_str xvar = dim_names.names(); // These two from a named vector on the R side
  int ns = slice_names.size();

  // Which rows correspond to the slice
  vec_int row_idx_slice(ns);
  for (int i = 0; i < ns; i++) {
    std::string s = slice_names[i];
    auto it = std::find(xvar.begin(), xvar.end(), s);
    if (it != xvar.end()) {
      int xvar_idx = std::distance(xvar.begin(), it);
      row_idx_slice[i] = xvar_idx;
    }
  }

  // Find the slicing cell
  vec_int slicing_cell_idx(ns);

  for (int i = 0; i < ns; i++) {
    int s_idx = row_idx_slice[i];
    vec_str dim_names_i = dim_names[s_idx];
    auto it = std::find(dim_names_i.begin(), dim_names_i.end(), slice_cell[i]);
    int d = std::distance(dim_names_i.begin(), it);
    slicing_cell_idx[i] = d + 1;
  }

  std::string slicing_cell_idx_pasted = std::to_string(slicing_cell_idx[0]);
  slicing_cell_idx_pasted = std::accumulate(
    std::next(slicing_cell_idx.begin()),
    slicing_cell_idx.end(),
    slicing_cell_idx_pasted,
    [](std::string i, int j) -> std::string {return i + ":" + std::to_string(j);}
    );  

  arma::uvec arma_rowidx_slice = conv_to<arma::uvec>::from(row_idx_slice);
  arma::Mat<short> x_sub_slice = x.rows(arma_rowidx_slice);
  umap_str_int x_slice_idx_map = paste_cols(x_sub_slice);

  if (x_slice_idx_map.find(slicing_cell_idx_pasted) == x_slice_idx_map.end()) {
    Rcpp::stop("cannot slice on a cell that is not present");
    // This would lead to a zero probability any way!
    // return Rcpp::List::create(x, xval);
  }
  
  vec_int keep_cols = x_slice_idx_map[slicing_cell_idx_pasted];  
  arma::uvec keep_cols_arma = conv_to<arma::uvec>::from(keep_cols);

  arma::Mat<short> out_mat = x.cols(keep_cols_arma);
  vec_dbl out_val = std_sub_vec(xval, keep_cols);
  
  return Rcpp::List::create(out_mat, out_val);
}
