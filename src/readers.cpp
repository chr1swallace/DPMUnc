#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//' @export
// @param X matrix of clustering solutions, rows = each solution, cols = each item to be clustered
// @param values a vector containing all the unique values in X
// [[Rcpp::export]]
arma::mat calc_psm(arma::mat X, arma::vec values) {
  arma::mat PSM(X.n_cols, X.n_cols);
  for (int i=0; (unsigned) i < values.n_elem; ++i) {
    arma::uvec idx = find(X == values(i)); // Substitute == with >, <, >=, <=, !=
    arma::mat isequal = arma::zeros(X.n_rows,X.n_cols);
    isequal.elem(idx).fill(1.0);        // Retrieve elements from positional index
    PSM = PSM + isequal.t() * isequal;
  }
  return PSM;
}

