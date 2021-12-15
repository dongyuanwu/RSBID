#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

NumericVector get_dist_cat(CharacterVector x, CharacterMatrix y) {
	int nrow = y.nrow(), ncol = y.ncol();
	NumericVector out(nrow);
	
	for (int i = 0; i < nrow; ++i) {
		double total = 0;
		for (int j = 0; j < ncol; ++j) {
			if (x[j] != y(i, j)) {
				total += 1;
			}
		}
		out[i] = total;
	}
	return out;
}