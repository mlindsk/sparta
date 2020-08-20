#include <RcppArmadillo.h>
#include <numeric>       // For: 
#include <vector>        // For: 
#include <string>        // For:
#include <algorithm>     // For: sort, std set operations, iota, min/max etc., 
#include <unordered_map> // For: paste_col
#include <cstddef>       // For: std::size_t

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]

using namespace arma;
using namespace Rcpp;

using vs  = std::vector<std::string>;
using vd  = std::vector<double>;


