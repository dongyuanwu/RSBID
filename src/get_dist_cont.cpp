#include "cpp11.hpp"

using namespace cpp11;

[[cpp11::register]]
doubles get_dist_cont(doubles x, doubles_matrix<> y) {
	int nrow = y.nrow(), ncol = y.ncol();
	writable::doubles out(nrow);
	
	for (int i = 0; i < nrow; ++i) {
		double total = 0;
		for (int j = 0; j < ncol; ++j) {
			total += (x[j]-y(i, j))*(x[j]-y(i, j));
		}
		out[i] = total;
	}
	return out;
}