#include "sparr_types.h"
#include "set_ops.h"
#include "misc_utils.h"

// [[Rcpp::export]]
Rcpp::List marginalize_(
  arma::Mat<short>& x,
  std::vector<double>& xval,
  std::vector<std::string> xvar,
  std::vector<std::string> y // The variables to marginalze out
  )
{

  bool y_in_xvar = set_issubeq(y, xvar);
  if (!y_in_xvar) Rcpp::stop("some variables in y are not present in x");
  
  int n_col_x = xval.size();
  int n_xvar  = xvar.size();
  int n_y     = y.size();
  std::vector<std::string> xvar_without_y = set_diff(xvar, y);
  int n_xvar_without_y  = n_xvar - n_y; // TODO: Test for y.size() == 0 and just return x then

  std::vector<int> rowidx_xvar_without_y(n_xvar_without_y);

  for (int i = 0; i < n_xvar; i++) {
    auto itx = std::find(xvar.begin(), xvar.end(), xvar_without_y[i]);
    int idxx = std::distance(xvar.begin(), itx);
    rowidx_xvar_without_y[i] = idxx;
  }
  arma::uvec arma_rowidx_xvar_without_y = conv_to<arma::uvec>::from(rowidx_xvar_without_y);
  arma::Mat<short> x_without_y = x.rows(arma_rowidx_xvar_without_y);
  std::unordered_map<std::string, std::vector<int>> x_without_y_idx_info = paste_cols(x_without_y);

  std::size_t n_cols_out = x_without_y_idx_info.size();
  arma::Mat<short> out_mat(n_xvar_without_y, n_cols_out);
  std::vector<double> out_val(n_cols_out);
  std::size_t counter = 0;

  for (auto & e : x_without_y_idx_info) {
    std::vector<int> v = e.second;
    out_mat.col(counter) = x_without_y.col(v[0]);

    double v_idx_sum = 0;
    for (int k = 0; k < v.size(); k++) {
      v_idx_sum += xval[v[k]];
    }
    
    out_val[counter] = v_idx_sum;
    counter += 1;
  }
  
  return Rcpp::List::create(out_mat, out_val);
}
