#ifndef MISCUTILS_H
#define MISCUTILS_H

#include "sparr_types.h"

std::unordered_map<std::string, std::vector<int>> paste_cols(arma::Mat<short>& A);
std::vector<int> next_cell_(std::vector<int>& cell, std::vector<int>& dim, std::vector<int>& fixed_indices);

#endif
