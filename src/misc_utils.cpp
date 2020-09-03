#include "misc_utils.h"
#include "set_ops.h"

umap_str_int paste_cols(arma::Mat<short>& A) {
  int N = A.n_cols;
  umap_str_int out(N);
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

arma::Col<short> next_cell_(arma::Col<short> cell, const vec_int& dim) {
  int ndim = dim.size();
  for (int j = 0; j < ndim; j++) {
    if (cell[j] < dim[j]) {
      cell[j] = cell[j] + 1;
      break;
    } else {
      cell[j] = 1;
    }
  }
  return cell;
}
