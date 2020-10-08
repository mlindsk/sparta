#ifndef MISCUTILS_H
#define MISCUTILS_H

#include "sparta_types.h"

vec_int std_sub_vec(vec_int& v, vec_int& indices);
vec_dbl std_sub_vec(vec_dbl& v, vec_int& indices);
umap_str_int paste_cols(arma::Mat<short>& A);
arma::Col<short> next_cell_(arma::Col<short> cell, const vec_int& dim);
std::vector<arma::Col<short>> all_cells_(const vec_int& dim);

#endif
