#' Latent Class Analysis with Group Varaible
#'
#' Fits latent class analysis (LCA) including group variable and covariates. The group variable can be handled either by multilevel LCA described in Vermunt (2003) <DOI:10.1111/j.0081-1750.2003.t01-1-00131.x> or standard LCA at each level of group variable. The covariates can be incorporated in the form of logistic regression (Bandeen-Roche et al. (1997) <DOI:10.1080/01621459.1997.10473658>).
#'
#' @docType package
#' @import Rcpp MASS
#' @importFrom Rcpp evalCpp
#' @useDynLib glca, .registration = TRUE
#' @name glca-package
#' @aliases glca-package
NULL
