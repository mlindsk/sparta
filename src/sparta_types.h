// #define ARMA_64BIT_WORD 1
#include <RcppArmadillo.h>
#include <numeric>       // For: transform and accumulate
#include <vector>        // For: 
#include <string>        // For:
#include <algorithm>     // For: sort, std set operations, iota, min/max etc., 
#include <unordered_map> // For: paste_col
// #include <set>           // For: unique elements
#include <cstddef>       // For: std::size_t

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]

using namespace arma;
using namespace Rcpp;

using vec_str = std::vector<std::string>;
using vec_int = std::vector<int>;
using vec_dbl = std::vector<double>;
using vec_short = std::vector<short>;
using umap_str_int = std::unordered_map<std::string, vec_int>;
