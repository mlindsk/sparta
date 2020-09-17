#include <RcppArmadillo.h>
#include <numeric>       // For: transform and accumulate
#include <vector>        // For: 
#include <string>        // For:
#include <algorithm>     // For: sort, std set operations, iota, min/max etc., 
#include <unordered_map> // For: paste_col
#include <unordered_set> // For: paste_col_with_sep_intersect
#include <cstddef>       // For: std::size_t

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]

using namespace arma;
using namespace Rcpp;

using vec_str = std::vector<std::string>;
using vec_int = std::vector<int>;
using vec_dbl = std::vector<double>;
using umap_str_int = std::unordered_map<std::string, vec_int>;
using uset_str = std::unordered_set<std::string>;


