#include "sparr_types.h"
#include "set_ops.h"

// [[Rcpp::export]]
std::unordered_map<std::string, std::vector<int>> paste_cols(arma::Mat<short>& A) {

  int N = A.n_cols;
  std::unordered_map<std::string, std::vector<int>> out(N);

  for (int i = 0; i < N; i++) {
    arma::Col<short> v = A.col(i);
    std::string s = std::to_string(v[0]);
    s = std::accumulate(
      std::next(v.begin()),
      v.end(),
      s,
      [](std::string i, int j) -> std::string {return i + std::to_string(j);}
      );
    out[s].push_back(i);
  }
  
  return out;
}

//[[Rcpp::export]]
std::vector<int> next_cell_(
  std::vector<int>& cell,
  std::vector<int>& dim,
  std::vector<int>& fixed_indices // NOTE: Remember to subtract one when inputting an R index!
  ) {

  std::vector<int> out_cell = cell;
  int ndim = dim.size();
  
  for (int j = 0; j < ndim; j++) {

    bool j_in_fixed_indices = set_int(j, fixed_indices);
    if (j_in_fixed_indices) continue;

    if (out_cell[j] < dim[j]) {
      out_cell[j] = out_cell[j] + 1;
      break;
    } else {
      out_cell[j] = 1;
    }
    
  }
  return out_cell;
}
