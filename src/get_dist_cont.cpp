#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

NumericVector get_dist_cont(NumericVector x, NumericMatrix y) {
	int nrow = y.nrow(), ncol = y.ncol();
	NumericVector out(nrow);
	
	for (int i = 0; i < nrow; ++i) {
		double total = 0;
		for (int j = 0; j < ncol; ++j) {
			total += (x[j]-y(i, j))*(x[j]-y(i, j));
		}
		out[i] = total;
	}
	return out;
}