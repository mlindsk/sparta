#include "set_ops.h"

// [[Rcpp::export]]
vs set_intersect(vs v1, vs v2) {
  vs v;
  std::sort(v1.begin(), v1.end());
  std::sort(v2.begin(), v2.end());
  std::set_intersection(v1.begin(),v1.end(),
			v2.begin(),v2.end(),
			back_inserter(v));
  return v;
}

// [[Rcpp::export]]
vs set_diff(vs v1, vs v2) {
  vs v;
  std::sort(v1.begin(), v1.end());
  std::sort(v2.begin(), v2.end());
  std::set_difference(v1.begin(),v1.end(),
		      v2.begin(),v2.end(),
		      back_inserter(v));
  return v;
}

// [[Rcpp::export]]
bool set_issubeq(vs& a, vs& b) {
  for (auto const& av : a){
    if (std::find(b.begin(), b.end(), av) == b.end())
      return false;
  }
  return true;
}

// [[Rcpp::export]]
bool set_in(std::string& a, vs& b) {
  std::vector<std::string>::iterator it = std::find(b.begin(), b.end(), a);
  return it != b.end();
}

// [[Rcpp::export]]
bool set_int(int a, std::vector<int>& b) {
  std::vector<int>::iterator it = std::find(b.begin(), b.end(), a);
  return it != b.end();
}
