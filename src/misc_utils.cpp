#include "misc_utils.h"
#include "set_ops.h"

vec_int std_sub_vec(vec_int& v, vec_int& indices) {
  int ni = indices.size();
  vec_int sub_vec(ni);
  for (int i = 0; i < ni; i++) {
    sub_vec[i] = v[indices[i]];
  }
  return sub_vec;
}

vec_dbl std_sub_vec(vec_dbl& v, vec_int& indices) {
  int ni = indices.size();
  vec_dbl sub_vec(ni);
  for (int i = 0; i < ni; i++) {
    sub_vec[i] = v[indices[i]];
  }
  return sub_vec;
}


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

std::vector<arma::Col<short>> all_cells_(const vec_int& dim) {
  
  int nout = 1;
  for (auto & d : dim) {
    nout *= d;
  }

  std::vector<arma::Col<short>> v(nout);
  int ndim = dim.size();
  arma::Col<short> cell = arma::ones<arma::Col<short>>(ndim);
  v[0] = cell; 

  for (int i = 1; i < nout; i++) {
    for (int j = 0; j < ndim; j++) {
      if (cell[j] < dim[j]) {
	cell[j] = cell[j] + 1;
	v[i] = cell;
	break;
      } else {
	cell[j] = 1;
      }
    }    
  }

  return v;
}
