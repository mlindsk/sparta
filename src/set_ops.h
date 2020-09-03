#ifndef SETOPS_H
#define SETOPS_H

#include "sparta_types.h"

vec_str   set_intersect(vec_str v1, vec_str v2);
vec_str   set_diff(vec_str v1, vec_str v2);
bool set_issubeq(vec_str& a, vec_str& b);
bool set_in(std::string& a, vec_str& b);
// bool set_int(int a, std::vector<int>& b);

#endif
