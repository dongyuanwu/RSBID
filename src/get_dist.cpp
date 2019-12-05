#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double get_dist(NumericVector x, NumericVector y) {
	int n = x.size();
	double total = 0;

	for(int i = 0; i < n; ++i) {
		total += (x[i]-y[i])*(x[i]-y[i]);
	}
	return total;
}