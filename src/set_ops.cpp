#include "set_ops.h"

vec_str set_intersect(vec_str v1, vec_str v2) {
  vec_str v;
  std::sort(v1.begin(), v1.end());
  std::sort(v2.begin(), v2.end());
  std::set_intersection(v1.begin(),v1.end(),
			v2.begin(),v2.end(),
			back_inserter(v));
  return v;
}

vec_str set_diff(vec_str v1, vec_str v2) {
  vec_str v;
  std::sort(v1.begin(), v1.end());
  std::sort(v2.begin(), v2.end());
  std::set_difference(v1.begin(),v1.end(),
		      v2.begin(),v2.end(),
		      back_inserter(v));
  return v;
}

vec_int set_diff_no_sort_on_first_arg(vec_int v1, vec_int v2) {
  vec_int v;
  std::sort(v2.begin(), v2.end());
  std::set_difference(v1.begin(),v1.end(),
		      v2.begin(),v2.end(),
		      back_inserter(v));
  return v;
}


bool set_issubeq(vec_str& a, vec_str& b) {
  for (auto const& av : a){
    if (std::find(b.begin(), b.end(), av) == b.end())
      return false;
  }
  return true;
}

bool set_in(std::string& a, vec_str& b) {
  std::vector<std::string>::iterator it = std::find(b.begin(), b.end(), a);
  return it != b.end();
}

// // [[Rcpp::export]]
// bool set_int(int a, std::vector<int>& b) {
//   std::vector<int>::iterator it = std::find(b.begin(), b.end(), a);
//   return it != b.end();
// }
