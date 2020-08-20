#include "sparr_types.h"
#include "set_ops.h"
#include "misc_utils.h"

// [[Rcpp::export]]
Rcpp::List merge_disjoint_(
  arma::Mat<short>& x,
  arma::Mat<short>& y,
  std::vector<double>& xval,
  std::vector<double>& yval,
  std::vector<std::string> xvar,
  std::vector<std::string> yvar
  )
{

  int n_col_x = xval.size();
  int n_col_y = yval.size();
  int n_xvar  = xvar.size();
  int n_yvar  = yvar.size();
  int n_joint_variables = n_xvar + n_yvar;

  std::size_t n_col_out = n_col_x * n_col_y;
  arma::Mat<short> out_mat(n_joint_variables, n_col_out);
  std::vector<double> out_val(n_col_out);

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

      out_mat.col(counter) = v;
      out_val[counter] = xval[i] * yval[j];
      counter += 1;
    }
  }

  return Rcpp::List::create(out_mat, out_val);
}


// [[Rcpp::export]]
Rcpp::List merge_(
  arma::Mat<short>& x,
  arma::Mat<short>& y,
  std::vector<double>& xval,
  std::vector<double>& yval,
  std::vector<std::string> xvar,
  std::vector<std::string> yvar,
  std::string op = "*"
  )
{

  std::vector<std::string> ops{"*", "/"}; //, '+', '-'};
  bool op_valid = set_in(op, ops);
  if (!op_valid) Rcpp::stop("The operator supplied is not valid. Use one of: '*', '/'"); // , '+', '-'.")
  
  // TODO: check if x.isempty() || y.isempty() ?
  
  std::vector<std::string> names_sep = set_intersect(xvar, yvar);
  if (names_sep.size() == 0) return merge_disjoint_(x, y, xval, yval, xvar, yvar); //
  std::vector<std::string> names_res = set_diff(yvar, names_sep);

  int n_sep  = names_sep.size();
  // TODO: what if n_sep = 0? My best guess is, that the procedure will fail! Fix!
  if (n_sep == xvar.size()) Rcpp::stop("The procedure is not yet implemented for identical tables");
  int n_res  = names_res.size();
  int n_xvar = xvar.size();
  int n_yvar = yvar.size();
  int n_joint_variables = n_xvar + n_res;

  std::vector<int> rowidx_sep_x(n_sep);
  std::vector<int> rowidx_sep_y(n_sep);
  
  for (int i = 0; i < n_sep; i++) {
    auto itx = std::find(xvar.begin(), xvar.end(), names_sep[i]);
    auto ity = std::find(yvar.begin(), yvar.end(), names_sep[i]);

    int idxx = std::distance(xvar.begin(), itx);
    int idxy = std::distance(yvar.begin(), ity);

    rowidx_sep_x[i] = idxx;
    rowidx_sep_y[i] = idxy;
  }

  std::vector<int> rowidx_res_y(n_res);
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

  std::unordered_map<std::string, std::vector<int>> x_sep_idx_vec = paste_cols(x_sub_sep);
  std::unordered_map<std::string, std::vector<int>> y_sep_idx_vec = paste_cols(y_sub_sep);
    
  // Find the total number of columns in the final sparse matrix
  // - if op in (+, -) we must work a little more. See below.
  std::size_t n_cols_out = 0;
  for (auto & e : x_sep_idx_vec) {
    auto key = e.first;
    auto key_in_y_sep = y_sep_idx_vec.find(key);
    if (key_in_y_sep != y_sep_idx_vec.end()) {
      n_cols_out +=e.second.size() * y_sep_idx_vec[key].size();
    }
  }

  arma::Mat<short>    out_mat(n_joint_variables, n_cols_out);
  std::vector<double> out_val(n_cols_out);

  // TODO: Find the union seps!
  // input a reference object to paste_cols that keeps all the
  // names in std::set and take the union of these!
  
  std::size_t counter = 0;

  // TODO: Make an if (op in c('*', '/')) else {}
  for (auto & e : x_sep_idx_vec) {
    auto key = e.first;
    auto key_in_y_sep = y_sep_idx_vec.find(key);

    // TODO: Could we implement the add here!
    // - since if (!key_in_y_sep) we know we must add something more!
    // - what we miss is then the other way around: if (!key_in_x_sep)
    // NOTE: HEY! WHAT IF WE JUST FOUND THE UNION AND LOOP OVER THOSE!!!
    // Then we must ask if (!key_in_y_sep && !key_in_x_sep)
    // BUT! Make all this completely separate and keep the optimized version
    // for '*' and '/' !
    
    if (key_in_y_sep != y_sep_idx_vec.end()) {

      // The two vectors of column indices for which we must multiply all combinations
      std::vector<int>& a = e.second;
      std::vector<int>& b = y_sep_idx_vec[key];

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

	  out_val[counter] = xval[i] * yval[j];
	  out_mat.col(counter) = v;
	  counter += 1;
	}
      }
    }
  }

  /*----------------------------------------------------------*
                   EXAMPLE: Add X and Y
		   ---------------------
  
             X                 Y
  
       o         o o o         o
    a  1 2 1 2 1 2 1 2    c  1 2 1 2
    b  1 1 2 2 1 1 2 2    d  1 1 2 2
    c  1 1 1 1 2 2 2 2 
       ---------------       -------
   col   1 2 3 4             1   2 3
   
   Note: 'o' := not part of the sparse array
   
   We need to identify (a,b,c) = (1,1,1) even though
   not present in X, since we have to add this entry
   with (c,d) = (1, 1) and (c,d) = (1, 2).

   Stated differently: For all entries in Y, we must find the
   associated entries which _are not_ in the sparse X

   Let (c, d) in {(1,1), (1,2)}, then sep = (c) = (1). The strategy is then
   to make the submatrix

          X           
               
    a   2 1 2
    b   1 2 2
    c   1 1 1
        -----
   col  1 2 3

   Then:
     1) construct the unordered_set, say z = {'221', '121', '221'}.
     2) define the cell = c(1,1,1)
     3) determine if cell_glued = '111' is in z
        - if yes, continue
	- else
	   make a new entry in the final sparse matrix (a, b, c, d) = (1,1,1,1) (if (c,d) = (1,1))
	   then call next_cell_(cell, dims, 2) where '2' refers to that 'c' (third element)
            _must not_ be altered (it is fixed)
   
   * ---------------------------------------------------------*/
    
  return Rcpp::List::create(out_mat, out_val);
}
