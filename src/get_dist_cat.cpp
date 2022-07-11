#include "cpp11.hpp"

using namespace cpp11;

[[cpp11::register]]
doubles get_dist_cat(strings x, strings_matrix<> y) {
	int nrow = y.nrow(), ncol = y.ncol();
	writable::doubles out(nrow);
	
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