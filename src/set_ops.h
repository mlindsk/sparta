#ifndef SETOPS_H
#define SETOPS_H

#include "sparr_types.h"

using vs = std::vector<std::string>;

vs   set_intersect(vs v1, vs v2);
vs   set_diff(vs v1, vs v2);
bool set_issubeq(vs& a, vs& b);
bool set_in(std::string& a, vs& b);
bool set_int(int a, std::vector<int>& b);

#endif
