# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

runDPMUnc <- function(observedData, observedVars, totalIterations, thinningFreq, quiet, saveClusterParams, saveLatentObs, outputDir, clusterAllocations, kappa0, alpha0, beta0) {
    invisible(.Call('_DPMUnc_runDPMUnc', PACKAGE = 'DPMUnc', observedData, observedVars, totalIterations, thinningFreq, quiet, saveClusterParams, saveLatentObs, outputDir, clusterAllocations, kappa0, alpha0, beta0))
}

resumeDPMUnc <- function(observedData, observedVars, remainingIterations, thinningFreq, quiet, saveClusterParams, saveLatentObs, outputDir, clusterAllocations, latentObservations, alpha_concentration, kappa0, alpha0, beta0) {
    invisible(.Call('_DPMUnc_resumeDPMUnc', PACKAGE = 'DPMUnc', observedData, observedVars, remainingIterations, thinningFreq, quiet, saveClusterParams, saveLatentObs, outputDir, clusterAllocations, latentObservations, alpha_concentration, kappa0, alpha0, beta0))
}

#' @export
NULL

calc_psm <- function(X, values) {
    .Call('_DPMUnc_calc_psm', PACKAGE = 'DPMUnc', X, values)
}

