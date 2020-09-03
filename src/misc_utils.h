#ifndef MISCUTILS_H
#define MISCUTILS_H

#include "sparta_types.h"

umap_str_int paste_cols(arma::Mat<short>& A);
arma::Col<short> next_cell_(arma::Col<short> cell, const vec_int& dim);

#endif
