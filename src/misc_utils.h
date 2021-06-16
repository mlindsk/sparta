#ifndef MISCUTILS_H
#define MISCUTILS_H

#include "sparta_types.h"

vec_int std_sub_vec(vec_int& v, vec_int& indices);
vec_dbl std_sub_vec(vec_dbl& v, vec_int& indices);
umap_str_int paste_cols(arma::Mat<short>& A);
umap_str_int paste_cols_from_index(arma::Mat<short>& A, arma::uvec& sep_idx);
umap_str_pair paste_marg(arma::Mat<short>& A, arma::uvec& row_idx, vec_dbl& xval);
arma::Col<short> next_cell_(arma::Col<short> cell, const vec_int& dim);
std::vector<arma::Col<short>> all_cells_(const vec_int& dim);

#endif
