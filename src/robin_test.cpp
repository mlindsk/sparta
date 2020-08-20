// #include "sparr_types.h"
// #include "robin_hood/robin_hood.h"

// // [[Rcpp::export]]
// int rhumap(int n) {

//   robin_hood::unordered_map<int, int> x(n);
//   for (int i = 0; i < n; i++) {
//     x[i] = i + 1; 
//   }

//   for (int i = 0; i < n; i++) {
//     x[i] += 1;
//   }
  
//   // for (auto & e : rhm) {
//   //   std::cout << e.first << " : " << e.second << "\n";
//   // }
//   return 0;
// }

// // [[Rcpp::export]]
// int stdumap(int n) {

//   std::unordered_map<int, int> x(n);
//   for (int i = 0; i < n; i++) {
//     x[i] = i + 1; 
//   }

//   for (int i = 0; i < n; i++) {
//     x[i] += 1;
//   }
  
//   // for (auto & e : rhm) {
//   //   std::cout << e.first << " : " << e.second << "\n";
//   // }
  
//   return 0;
// }
