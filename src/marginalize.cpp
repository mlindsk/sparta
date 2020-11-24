#include "sparta_types.h"
#include "set_ops.h"
#include "misc_utils.h"

// [[Rcpp::export]]
Rcpp::List marginalize_sum_(
  arma::Mat<short>& x,
  vec_dbl& xval,
  vec_str xvar,
  vec_str y       // The variables to marginalze out
  )
{

  bool y_in_xvar = set_issubeq(y, xvar);
  if (!y_in_xvar) Rcpp::stop("some variables in y are not present in x");
  
  int n_col_x = xval.size();
  int n_xvar  = xvar.size();
  int n_y     = y.size();
  vec_str xvar_without_y = set_diff(xvar, y);
  int n_xvar_without_y   = n_xvar - n_y;
  vec_int rowidx_xvar_without_y(n_xvar_without_y);

  for (int i = 0; i < n_xvar_without_y; i++) {
    auto itx = std::find(xvar.begin(), xvar.end(), xvar_without_y[i]);
    int idxx = std::distance(xvar.begin(), itx);
    rowidx_xvar_without_y[i] = idxx;
  }
    
  // It is important to sort, such that the names correspond to the correct rows!
  std::sort(rowidx_xvar_without_y.begin(), rowidx_xvar_without_y.end());
  
  arma::uvec arma_rowidx_xvar_without_y = conv_to<arma::uvec>::from(rowidx_xvar_without_y);
  arma::Mat<short> x_without_y = x.rows(arma_rowidx_xvar_without_y);
  umap_str_int x_without_y_idx_info = paste_cols(x_without_y);

  std::size_t n_cols_out = x_without_y_idx_info.size();
  arma::Mat<short> out_mat(n_xvar_without_y, n_cols_out);
  vec_dbl out_val(n_cols_out);
  std::size_t counter = 0;

  for (auto & e : x_without_y_idx_info) {
    vec_int v = e.second;
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



// [[Rcpp::export]]
Rcpp::List marginalize_max_(
  arma::Mat<short>& x,
  vec_dbl& xval,
  vec_str xvar,
  vec_str y       // The variables to marginalze out
  )
{

  bool y_in_xvar = set_issubeq(y, xvar);
  if (!y_in_xvar) Rcpp::stop("some variables in y are not present in x");
  
  int n_col_x = xval.size();
  int n_xvar  = xvar.size();
  int n_y     = y.size();
  vec_str xvar_without_y = set_diff(xvar, y);
  int n_xvar_without_y   = n_xvar - n_y;
  vec_int rowidx_xvar_without_y(n_xvar_without_y);

  for (int i = 0; i < n_xvar_without_y; i++) {
    auto itx = std::find(xvar.begin(), xvar.end(), xvar_without_y[i]);
    int idxx = std::distance(xvar.begin(), itx);
    rowidx_xvar_without_y[i] = idxx;
  }
    
  // It is important to sort, such that the names correspond to the correct rows!
  std::sort(rowidx_xvar_without_y.begin(), rowidx_xvar_without_y.end());
  
  arma::uvec arma_rowidx_xvar_without_y = conv_to<arma::uvec>::from(rowidx_xvar_without_y);
  arma::Mat<short> x_without_y = x.rows(arma_rowidx_xvar_without_y);
  umap_str_int x_without_y_idx_info = paste_cols(x_without_y);

  std::size_t n_cols_out = x_without_y_idx_info.size();
  arma::Mat<short> out_mat(n_xvar_without_y, n_cols_out);
  vec_dbl out_val(n_cols_out);
  std::size_t counter = 0;

  for (auto & e : x_without_y_idx_info) {
    vec_int v = e.second;
    out_mat.col(counter) = x_without_y.col(v[0]);

    double v_idx_max = R_NegInf;
    for (int k = 0; k < v.size(); k++) {
      double xvalk = xval[v[k]];
      if (xvalk > v_idx_max) {
	v_idx_max = xvalk;  
      }
      out_val[counter] = v_idx_max;
    }
    counter += 1;
  }
  
  return Rcpp::List::create(out_mat, out_val);
}



// ** OLD APPROACH ** //

// // [[Rcpp::export]]
// Rcpp::List marginalize_(
//   arma::Mat<short>& x,
//   vec_dbl& xval,
//   vec_str xvar,
//   vec_str y,       // The variables to marginalze out
//   bool flow = true // true = sum flow and false =  max flow
//   )
// {

//   bool y_in_xvar = set_issubeq(y, xvar);
//   if (!y_in_xvar) Rcpp::stop("some variables in y are not present in x");
  
//   int n_col_x = xval.size();
//   int n_xvar  = xvar.size();
//   int n_y     = y.size();
//   vec_str xvar_without_y = set_diff(xvar, y);
//   int n_xvar_without_y   = n_xvar - n_y;
//   vec_int rowidx_xvar_without_y(n_xvar_without_y);

//   for (int i = 0; i < n_xvar_without_y; i++) {
//     auto itx = std::find(xvar.begin(), xvar.end(), xvar_without_y[i]);
//     int idxx = std::distance(xvar.begin(), itx);
//     rowidx_xvar_without_y[i] = idxx;
//   }
    
//   // It is important to sort, such that the names correspond to the correct rows!
//   std::sort(rowidx_xvar_without_y.begin(), rowidx_xvar_without_y.end());
  
//   arma::uvec arma_rowidx_xvar_without_y = conv_to<arma::uvec>::from(rowidx_xvar_without_y);
//   arma::Mat<short> x_without_y = x.rows(arma_rowidx_xvar_without_y);
//   umap_str_int x_without_y_idx_info = paste_cols(x_without_y);

//   std::size_t n_cols_out = x_without_y_idx_info.size();
//   arma::Mat<short> out_mat(n_xvar_without_y, n_cols_out);
//   vec_dbl out_val(n_cols_out);
//   std::size_t counter = 0;

//   for (auto & e : x_without_y_idx_info) {
//     vec_int v = e.second;
//     out_mat.col(counter) = x_without_y.col(v[0]);

//     if (flow) { // sum
      
//       double v_idx_sum = 0;
//       for (int k = 0; k < v.size(); k++) {
//   	v_idx_sum += xval[v[k]];
//       }
//       out_val[counter] = v_idx_sum;
      
//     } else {    // max

//       double v_idx_max = R_NegInf;
//       for (int k = 0; k < v.size(); k++) {
//   	double xvalk = xval[v[k]];
//   	if (xvalk > v_idx_max) {
//   	  v_idx_max = xvalk;  
//   	}
//   	out_val[counter] = v_idx_max;
//       }
//     }
//     counter += 1;
//   }
  
//   return Rcpp::List::create(out_mat, out_val);
// }
