#include "sparta_types.h"
#include "misc_utils.h"
#include "set_ops.h"

// [[Rcpp::export]]
Rcpp::List as_sparta_(const vec_dbl& arr, const vec_int& dim) {

  int n_vals = arr.size();
  int n_rows = dim.size();
  int n_nonzeroes = std::count_if(
    arr.begin(),
    arr.end(),
    [](double i) -> bool {return i != 0;});
  
  arma::Mat<short> sptab(n_rows, n_nonzeroes);
  vec_dbl vals(n_nonzeroes);

  arma::Col<short> cell(n_rows);
  cell.ones();

  int non_zero_counter = 0;

  for (int i = 0; i < n_vals; i++) {
    double ival = arr[i];
    if (ival != 0) {
      sptab.col(non_zero_counter) = cell;
      vals[non_zero_counter] = ival;
      non_zero_counter += 1;
    }
    cell = next_cell_(cell, dim);
  }
  
  return Rcpp::List::create(sptab, vals);
}

// [[Rcpp::export]]
Rcpp::List as_cpt_(arma::Mat<short>& x,
		   vec_dbl& xval,
		   vec_str& xvar,
		   vec_str& y
		   )
{

  bool y_in_xvar = set_issubeq(y, xvar);
  if (!y_in_xvar) Rcpp::stop("some variables in y are not present in x");
  
  vec_int rowidx_y(y.size());
  
  for (int i = 0; i < y.size(); i++) {
    auto ity = std::find(xvar.begin(), xvar.end(), y[i]);
    int idxy = std::distance(xvar.begin(), ity);
    rowidx_y[i] = idxy;
  }
  
  arma::uvec arma_rowidx_y = conv_to<arma::uvec>::from(rowidx_y);
  arma::Mat<short> y_sub = x.rows(arma_rowidx_y);

  umap_str_int y_idx_map = paste_cols(y_sub);
  
  for (auto & e : y_idx_map) {
    vec_int idx_e = e.second;
    int ne = idx_e.size();
    double sum_e = 0;
    
    for (int i = 0; i < ne; i++) {
      sum_e += xval[idx_e[i]];
    }

    for (int i = 0; i < ne; i++) {
      xval[idx_e[i]] = xval[idx_e[i]] / sum_e;
    }    
  }
  
  return Rcpp::List::create(x, xval);
}
