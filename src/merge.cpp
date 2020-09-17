#include "sparta_types.h"
#include "set_ops.h"
#include "misc_utils.h"


Rcpp::List merge_disjoint_(
  arma::Mat<short>& x,
  arma::Mat<short>& y,
  vec_dbl& xval,
  vec_dbl& yval,
  vec_str xvar,
  vec_str yvar,
  std::string op = "*"
  )
{

  // TODO: Let the following be a macro definition?
  int n_col_x = xval.size();
  int n_col_y = yval.size();
  int n_xvar  = xvar.size();
  int n_yvar  = yvar.size();
  int n_joint_variables = n_xvar + n_yvar;

  std::size_t n_col_out = n_col_x * n_col_y;
  arma::Mat<short> out_mat(n_joint_variables, n_col_out);
  vec_dbl out_val(n_col_out);

  std::size_t counter = 0;
  for (std::size_t i = 0; i < n_col_x; i++) {
    for (std::size_t j = 0; j < n_col_y; j++) {

      int counter_ij = 0;
      arma::Col<short> v(n_joint_variables);
      arma::Col<short> xcoli = x.col(i);
      arma::Col<short> ycolj = y.col(j);

      for (int k = 0; k < n_xvar; k++) {
    	v[counter_ij] = xcoli[k];
    	counter_ij += 1;
      }
	  
      for (int k = 0; k < n_yvar; k++) {
    	v[counter_ij] = ycolj[k];
    	counter_ij += 1;
      }

      double val_ij = op == "*" ? xval[i] * yval[j] : xval[i] / yval[j];
      out_val[counter] = val_ij;
      out_mat.col(counter) = v;
      counter += 1;
    }
  }

  return Rcpp::List::create(out_mat, out_val);
}


// [[Rcpp::export]]
Rcpp::List merge_(
  arma::Mat<short>& x,
  arma::Mat<short>& y,
  vec_dbl& xval,
  vec_dbl& yval,
  vec_str xvar,
  vec_str yvar,
  std::string op = "*"
  )
{

  // NOTE: The order of the variables in the final output is (xvar, yvar\xvar)
  
  vec_str names_sep = set_intersect(xvar, yvar);
  if (names_sep.size() == 0) return merge_disjoint_(x, y, xval, yval, xvar, yvar); //
  vec_str names_res = set_diff(yvar, names_sep);

  int n_sep  = names_sep.size();
  int n_res  = names_res.size();
  int n_xvar = xvar.size();
  int n_yvar = yvar.size();
  int n_joint_variables = n_xvar + n_res;

  vec_int rowidx_sep_x(n_sep);
  vec_int rowidx_sep_y(n_sep);
  
  for (int i = 0; i < n_sep; i++) {
    auto itx = std::find(xvar.begin(), xvar.end(), names_sep[i]);
    auto ity = std::find(yvar.begin(), yvar.end(), names_sep[i]);

    int idxx = std::distance(xvar.begin(), itx);
    int idxy = std::distance(yvar.begin(), ity);

    rowidx_sep_x[i] = idxx;
    rowidx_sep_y[i] = idxy;
  }

  vec_int rowidx_res_y(n_res);
  for (int i = 0; i < n_res; i++) {
    auto it = std::find(yvar.begin(), yvar.end(), names_res[i]);
    int idx = std::distance(yvar.begin(), it);
    rowidx_res_y[i] = idx;
  }

  arma::uvec arma_rowidx_sep_x = conv_to<arma::uvec>::from(rowidx_sep_x);
  arma::uvec arma_rowidx_sep_y = conv_to<arma::uvec>::from(rowidx_sep_y);
  arma::uvec arma_rowidx_res_y = conv_to<arma::uvec>::from(rowidx_res_y);
  
  arma::Mat<short> x_sub_sep = x.rows(arma_rowidx_sep_x);
  arma::Mat<short> y_sub_sep = y.rows(arma_rowidx_sep_y);

  umap_str_int x_sep_idx_map = paste_cols(x_sub_sep);
  umap_str_int y_sep_idx_map = paste_cols(y_sub_sep);
  
  // Find the total number of columns in the final sparse matrix
  std::size_t n_cols_out = 0;
  
  for (auto & e : x_sep_idx_map) {
    auto key = e.first;
    auto key_in_y_sep = y_sep_idx_map.find(key);
    if (key_in_y_sep != y_sep_idx_map.end()) {
      n_cols_out +=e.second.size() * y_sep_idx_map[key].size();
    }
  }

  arma::Mat<short> out_mat(n_joint_variables, n_cols_out);
  vec_dbl out_val(n_cols_out);
  
  std::size_t counter = 0;

  for (auto & e : x_sep_idx_map) {
    auto key = e.first;
    auto key_in_y_sep = y_sep_idx_map.find(key);
    
    if (key_in_y_sep != y_sep_idx_map.end()) {

      // The two vectors of column indices for which we must multiply all combinations
      vec_int& a = e.second;
      vec_int& b = y_sep_idx_map[key];

      for (auto & i : a) {
  	for (auto & j : b) {
  	  int counter_ij = 0;
  	  arma::Col<short> v(n_joint_variables);
  	  arma::Col<short> xcoli= x.col(i);
  	  arma::Col<short> rescoli = y.col(j);
  	  rescoli = rescoli(arma_rowidx_res_y); // extract the residual part now!

  	  for (int i = 0; i < n_xvar; i++) {
  	    v[counter_ij] = xcoli[i];
  	    counter_ij += 1;
  	  }
	  
  	  for (int i = 0; i < n_res; i++) {
  	    v[counter_ij] = rescoli[i];
  	    counter_ij += 1;
  	  }

  	  double val_ij = op == "*" ? xval[i] * yval[j] : xval[i] / yval[j];
  	  out_val[counter] = val_ij;
  	  out_mat.col(counter) = v;
  	  counter += 1;
  	}
      }
    }
  }
  return Rcpp::List::create(out_mat, out_val);
}


// [[Rcpp::export]]
Rcpp::List merge_unity_(
  arma::Mat<short>& x,
  vec_dbl& xval,
  vec_str xvar,
  vec_str yvar,
  vec_int ydim, 
  bool reciprocal = false
  )
{
  
  vec_str names_sep = set_intersect(xvar, yvar);
  // if (names_sep.size() == 0) return merge_unity_disjoint_(...); //
  vec_str names_res = set_diff(yvar, names_sep);

  int n_res  = names_res.size();
  int n_xvar = xvar.size();
  int n_xval = xval.size();
  int n_joint_variables = n_xvar + n_res;
  
  vec_int rowidx_res_y(n_res);
  for (int i = 0; i < n_res; i++) {
    auto it = std::find(yvar.begin(), yvar.end(), names_res[i]);
    int idx = std::distance(yvar.begin(), it);
    rowidx_res_y[i] = idx;
  }
  
  vec_int ydim_res = std_sub_vec(ydim, rowidx_res_y);
  std::vector<arma::Col<short>> all_res_y_cells = all_cells_(ydim_res);
  int n_all_res_y_cells = all_res_y_cells.size();

  std::size_t n_cols_out =  n_xval * n_all_res_y_cells;
  arma::Mat<short> out_mat(n_joint_variables, n_cols_out);
  vec_dbl out_val(n_cols_out);

  std::size_t counter = 0;
  for (int i = 0; i < n_xval; i++) {

    for (int j = 0; j < n_all_res_y_cells; j++) {

      arma::Col<short> v(n_joint_variables);
      arma::Col<short> xcoli= x.col(i);
      arma::Col<short> res_cell = all_res_y_cells[j];

      int counter_ij = 0;
      
      for (int k = 0; k < n_xvar; k++) {
  	v[counter_ij] = xcoli[k];
	counter_ij ++;
      }
      
      for (int k = 0; k < n_res; k++) {
  	v[counter_ij] = res_cell[k];
	counter_ij ++;
      }

      out_mat.col(counter) = v;
      out_val[counter] = reciprocal ? 1 / xval[i] : xval[i];
      counter ++;
    }
  }
  
  return Rcpp::List::create(out_mat, out_val);
}
