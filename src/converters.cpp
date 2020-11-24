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
		   vec_str& y // The conditioning variables
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


vec_str set_unique(vec_str v) {
  std::set<std::string> s;
  unsigned size = v.size();
  for (unsigned i = 0; i < size; ++i) s.insert(v[i]);
  v.assign(s.begin(), s.end());
  return v;
}

// arma::Col<short> row_to_cell(vec_str row, Rcpp::List& dn) {
  
// }


// https://stackoverflow.com/questions/10405030/c-unordered-map-fail-when-used-with-a-vector-as-key
// struct vector_hasher {
//     int operator()(const vec_short &V) const {
//         int hash = V.size();
//         for(auto &i : V) {
//             hash ^= i + 0x9e3779b9 + (hash << 6) + (hash >> 2);
//         }
//         return hash;
//     }
// };

// // [[Rcpp::export]]
// Rcpp::List as_sparta__(std::vector<vec_str> A, Rcpp::List dim_names) {

//   vec_str vars = dim_names.attr("names");
//   int nvars    = vars.size();
//   int nrow     = A[0].size();
//   int ncol     = A.size();
  
//   // Rows to cells
//   std::unordered_map<vec_short, double, vector_hasher> cell_map;
  
//   for (int j = 0; j < nrow; j++) {

//     vec_str row_j(nrow);
//     for (int i = 0; i < ncol; i++) {
//       row_j[i] = A[i][j];
//     }

//     vec_short cell(ncol);
  
//     for (int i = 0; i < ncol; i++) {
//       std::string row_ji = row_j[i];
//       vec_str dnj = dim_names[i];
//       auto it = std::find(dnj.begin(), dnj.end(), row_ji);
//       short val = std::distance(dnj.begin(), it);
//       cell[i] = val + 1;
//     }

//     // std::cout << "\n";
//     // for (auto & e : cell) {
//     //   std::cout << e << ", ";
//     // }
    
//     if (cell_map.find(cell) == cell_map.end()) {
//       cell_map[cell] = 1;
//     } else {
//       cell_map[cell] += 1;
//     }
    
//   }  


//   arma::Mat<short> cell_mat(nvars, cell_map.size());
//   vec_dbl cell_val(cell_map.size());

//   int counter = 0;
//   for (auto & e : cell_map) {
//     arma::Col<short> c = conv_to<arma::Col<short>>::from(e.first);
//     double v = e.second;
//     cell_mat.col(counter) = c;
//     cell_val[counter] = v;
//     counter += 1;
//   }
    
//   return Rcpp::List::create(cell_mat, cell_val); 
// }
