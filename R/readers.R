utils::globalVariables(".")

## calc_psmR=function(x) {
##   unq=unique(as.vector(x)) %>% setdiff(.,NA)
##   ## print(unq)
##   m=matrix(0,ncol(x),ncol(x))
##   for(k in unq) {
##     cat(".")
##     xk=matrix(as.numeric(x==k),nrow(x),ncol(x))
##     m=m + crossprod(xk) #t(xk) %*% xk
##   }
##   psm=m/nrow(x)
##   ## rownames(psm)=rownames(x)
##   psm
## }
calc_psmC=function(x) {
  unq=unique(as.vector(x)) %>% setdiff(.,NA)
  calc_psm(x,unq)/nrow(x)
}

##' calculate a PSM from a DPMUnc run
##'
##' uses a different formulation to calculate PSM to mcclust's comp.psm,
##' iterating over the distinct cluster labels instead of the observations,
##' which should be faster when the maximum number of clusters is not large
##'
##' @title Posterior Similarity Matrix
##' @param saveFileDir directory containing result of a DPMUnc run
##' @param trim proportion of initial samples to discard (burn in)
##' @return matrix with entries in \[0,1\] indicating the proportion of times each
##'   pair of observations were in the same cluster
##' @export
##' @author Chris Wallace
get_psm=function(saveFileDir, trim=.5) {
  if(!file.exists(saveFileDir))
    stop("saveFileDir not found: ",saveFileDir)
  if(!length(list.files(saveFileDir)))
    stop("saveFileDir empty: ",saveFileDir)
  ## is this a single dir or a dir of seeds?
  if(is_seed_dir(saveFileDir)) {
    allocs= lapply(list.files(saveFileDir, full.names=TRUE), function(f) {
      read.table(file.path(f,"clusterAllocations.csv"),sep=",") %>%
        as.matrix() %>%
        tail(., floor((1-trim) * nrow(.)))
    }) %>%
      do.call("rbind",.)
  } else {
    allocs=read.table(file.path(saveFileDir,"clusterAllocations.csv"),sep=",") %>%
      as.matrix() %>%
      tail(., floor((1-trim) * nrow(.)))
  }
  psm=calc_psmC(allocs)
}

##' Reads in samples and stores as an mcmc.list compatible with the coda library.
##'
##' This means output can be processed using using any coda function
##' The function ‘mcmc’ is used to create a Markov Chain Monte Carlo
##' object.  The input data are taken to be a vector, or a matrix with
##' one column per variable.
##'
##' @title Read in MCMC output
##' @param saveFileDir a character vector of directories, each corresponding to
##'   a single chain from the analysis of the same data
##' @param burnin what fraction of each chain to discard as burnin or number of obs to discard
##' @return an mcmc.list
##' @author Chris Wallace
read_mcmc <- function(saveFileDir, burnin) {
  filenames=c("alpha.csv","K.csv","pLatentsGivenClusters.csv")
  data=lapply(saveFileDir, function(thisDir) {
    this_data=lapply(filenames, function(f) {
      scan(file.path(thisDir,f), what="", sep=",") %>% as.numeric()
    }) %>% do.call("cbind",.)
    colnames(this_data)=sub(".csv","",filenames)
    ntail=if(burnin < 1) { (1-burnin) * nrow(this_data) } else { nrow(this_data) - burnin }
    if(ntail < 1) {
        warning("burnin larger than number of samples. setting burnin to 0.5")
        ntail = .5 * nrow(this_data)
    }
    mcmc(tail(this_data, ntail))
  }) %>% as.mcmc.list()
}

is_seed_dir <- function(f)
  length(f)==1 && !file.exists(file.path(f, "alpha.csv"))

##' Once the samples are read, all functions in the coda library which accept an
##' mcmc.list can be used for the standard post-sampling diagnostics and
##' inference
##'
##' @title Read samples
##' @param saveFileDir character vector of directories, either one directory per
##'   chain run on the same data, or the "container" directory, holding one
##'   subdirectory per chain which will be automatically found and read
##' @param burnin fraction of each chain to discard as burnin (if 0 < burnin < 1) or number of observations to discard
##' @return mcmc.list of samples, compatible with coda library
##' @author Chris Wallace
##' @export
read_samples <- function(saveFileDir, burnin=.5) {
  ok=sapply(saveFileDir, file.exists)
  if(any(!ok))
    stop("directories not found: ", paste(saveFileDir[!ok], collapse=" "))
  ## is this a single directory or a directory of seeds?
  if(is_seed_dir(saveFileDir))
    read_samples(saveFileDir=list.files(saveFileDir,full.names=TRUE), burnin)
  else
    read_mcmc(saveFileDir, burnin)
  ## list(quant_alpha = calculate_quantiles(saveFileDir, "alpha.csv", ...),
  ##        quant_K = calculate_quantiles(saveFileDir, "K.csv", ...),
  ##        quant_latent = calculate_quantiles(saveFileDir, "pLatentsGivenClusters.csv", ...))
}
