// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "sparta_types.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// as_sparta_
Rcpp::List as_sparta_(const vec_dbl& arr, const vec_int& dim);
RcppExport SEXP _sparta_as_sparta_(SEXP arrSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const vec_dbl& >::type arr(arrSEXP);
    Rcpp::traits::input_parameter< const vec_int& >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(as_sparta_(arr, dim));
    return rcpp_result_gen;
END_RCPP
}
// as_cpt_
Rcpp::List as_cpt_(arma::Mat<short>& x, vec_dbl& xval, vec_str& xvar, vec_str& y);
RcppExport SEXP _sparta_as_cpt_(SEXP xSEXP, SEXP xvalSEXP, SEXP xvarSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::Mat<short>& >::type x(xSEXP);
    Rcpp::traits::input_parameter< vec_dbl& >::type xval(xvalSEXP);
    Rcpp::traits::input_parameter< vec_str& >::type xvar(xvarSEXP);
    Rcpp::traits::input_parameter< vec_str& >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(as_cpt_(x, xval, xvar, y));
    return rcpp_result_gen;
END_RCPP
}
// marginalize_
Rcpp::List marginalize_(arma::Mat<short>& x, vec_dbl& xval, vec_str xvar, vec_str y, bool flow);
RcppExport SEXP _sparta_marginalize_(SEXP xSEXP, SEXP xvalSEXP, SEXP xvarSEXP, SEXP ySEXP, SEXP flowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::Mat<short>& >::type x(xSEXP);
    Rcpp::traits::input_parameter< vec_dbl& >::type xval(xvalSEXP);
    Rcpp::traits::input_parameter< vec_str >::type xvar(xvarSEXP);
    Rcpp::traits::input_parameter< vec_str >::type y(ySEXP);
    Rcpp::traits::input_parameter< bool >::type flow(flowSEXP);
    rcpp_result_gen = Rcpp::wrap(marginalize_(x, xval, xvar, y, flow));
    return rcpp_result_gen;
END_RCPP
}
// merge_
Rcpp::List merge_(arma::Mat<short>& x, arma::Mat<short>& y, vec_dbl& xval, vec_dbl& yval, vec_str xvar, vec_str yvar, std::string op);
RcppExport SEXP _sparta_merge_(SEXP xSEXP, SEXP ySEXP, SEXP xvalSEXP, SEXP yvalSEXP, SEXP xvarSEXP, SEXP yvarSEXP, SEXP opSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::Mat<short>& >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::Mat<short>& >::type y(ySEXP);
    Rcpp::traits::input_parameter< vec_dbl& >::type xval(xvalSEXP);
    Rcpp::traits::input_parameter< vec_dbl& >::type yval(yvalSEXP);
    Rcpp::traits::input_parameter< vec_str >::type xvar(xvarSEXP);
    Rcpp::traits::input_parameter< vec_str >::type yvar(yvarSEXP);
    Rcpp::traits::input_parameter< std::string >::type op(opSEXP);
    rcpp_result_gen = Rcpp::wrap(merge_(x, y, xval, yval, xvar, yvar, op));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_sparta_as_sparta_", (DL_FUNC) &_sparta_as_sparta_, 2},
    {"_sparta_as_cpt_", (DL_FUNC) &_sparta_as_cpt_, 4},
    {"_sparta_marginalize_", (DL_FUNC) &_sparta_marginalize_, 5},
    {"_sparta_merge_", (DL_FUNC) &_sparta_merge_, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_sparta(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
