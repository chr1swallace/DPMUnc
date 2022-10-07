#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//' @export
// [[Rcpp::export]]
arma::mat calc_psm(arma::mat X, arma::vec values) {
  arma::mat PSM(X.n_cols, X.n_cols);
  for (int i=0; i < values.n_elem; ++i) {
    arma::uvec idx = find(X == values(i)); // Substitute == with >, <, >=, <=, !=
    arma::mat isequal = arma::zeros(X.n_rows,X.n_cols);
    isequal.elem(idx).fill(1.0);        // Retrieve elements from positional index
    PSM = PSM + isequal.t() * isequal;
  }
  return PSM;
}

