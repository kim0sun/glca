#' Fits Latent Class Models for Data Containing Group Variable and Covariates
#'
#' Function for fitting latent class models with multiple groups, which may or may not include latent class structure for group variable.
#'
#' @param formula a formula for specifying manifest items and covariates using the "\code{item}" function.
#' @param group an optional vector specifying a group of observations. Given group variable, group covariates can be incorporated.
#' @param data a data frame containing the manifest item, covariates and group variable.
#' @param nclass number of level-1 (individual-level) latent classes.
#' @param ncluster number of level-2 (group-level) latent classes. When \code{group} and \code{ncluster} (>1) are given the multilevel latent class models will be fitted.
#' @param std.err a logical value for whether calculating standard errors for estimates.
#' @param measure.inv a logical value of the measurement invariance assumption across groups.
#' @param coeff.inv a logical value of the coefficient invariance assumption across groups (random intercept model).
#' @param init.param A set of model parameters to be used as the user-defined initial values for the EM algorithm. It should be \code{list} with the named parameters and have same structure of \code{param} of the \code{glca} output. In default, initial parameters are randomly generated.
#' @param n.init number of randomly generated initial parameter sets to be used for avoiding the problem of local maxima.
#' @param testiter number of iterations in the EM algorithm for each initial parameter set. The initial parameter set that provides the largest log-likelihood will be selected for estimating the model.
#' @param maxiter maximum number of iterations for the EM algorithm.
#' @param eps a convergence tolerance value. When the largest absolute difference between former estimates and current estimates is less than \code{eps}, the algorithm will stop updating and consider the convergence to be reached.
#' @param na.rm a logical value for deleting the lines that have at least one missing manifest item. If \code{na.rm = FALSE}, MAR procedure will be conducted.
#' @param random.seed  In default, the set of initial parameters is drawn randomly. As the same value for \code{random.seed} guarantees the same initial parameters to be drawn, this argument can be used to generate an identical solution.
#' @param verbose a logical value indicating whether \code{glca} should print the estimation procedure onto the screen.
#'
#' @author Youngsun Kim
#'
#' @details The \code{glca} is the function for implementing LCA consist of two-type latent categorical variables (i.e., level-1 and level-2 latent class). The level-1 (individual-level) latent class is identified by the association among the individuals' responses to multiple manifest items, but level-2 (group-level) latent class is categorized by the prevalence of level-1 latent class for group variable. The function \code{glca} can handle two types of covariates: level-1 and level-2 covariates. If covariates vary across individuals, they are considered as level-1 covariates. When \code{group} and \code{ncluster} (>1) are given, covariates which are varying across groups are considered as level-2 covariates. Both types of covariates have effect on level-1 class prevalence.
#'
#' The formula should consist of an \code{~} operator between two sides. Manifest items should be indicated in LHS of formula using \code{item} function and covariates should be specified in RHS of formula. For example, \cr
#' \code{item(y1, y2, y3) ~ 1}\cr
#' \code{item(y1, y2, y3) ~ x1 + x2}\cr
#' where the first fomula indicates LCA with three manifest variables (\code{y1}, \code{y2}, and \code{y3}) and no covariate, and the second formula includes two covariates (\code{x1} and \code{x2}). Two types of covariates (i.e., level-1 and level-2 covariates) will be automatically detected by \code{glca}.
#'
#' The estimated parameters in \code{glca} are \code{rho}, \code{gamma}, \code{delta}, and \code{beta}. The set of item response probabilities for each level-1 class is \code{rho}. The sets of prevalences for level-1 and level-2 class are \code{gamma} and \code{delta}, respectively. The prevalence for level-1 class (i.e., \code{gamma}) can be modeled as logistic regression using level-1 and/or level-2 covariates. The set of logistic regression coefficients is \code{beta} in \code{glca} output.
#'
#' @return \code{glca} returns an object of class "\code{glca}".
#'
#' The function \code{summary} prints estimates for parameters and \code{glca.gof} function gives goodness of fit measures for the model.
#'
#' An object of class "\code{glca}" is a list containing the following components:
#'
#' \item{call}{the matched call.}
#' \item{terms}{the \code{\link{terms}} object used.}
#' \item{model}{a \code{list} of model description.}
#' \item{var.names}{a \code{list} of names of data.}
#' \item{datalist}{a \code{list} of data used for fitting.}
#' \item{param}{a \code{list} of parameter estimates.}
#' \item{std.err}{a \code{list} of standard errors for estimates.}
#' \item{coefficient}{a \code{list} of logistic regression coefficients for prevalence of level-1 class.}
#' \item{posterior}{a \code{data.frame} or a \code{list} of posterior probablities of each individaul for latent classes and each group for latent clusters.}
#' \item{gof}{a \code{list} of goodness of fit measures.}
#' \item{convergence}{a \code{list} containing information about convergence.}
#'
#' @seealso \code{\link{gss08}} \code{\link{nyts18}}
#'
#' @references
#' Vermunt, J.K. (2003) Multilevel latent class models. \emph{Sociological Methodology}, \bold{33}, 213--239. \doi{10.1111/j.0081-1750.2003.t01-1-00131.x}
#'
#' Collins, L.M. and Lanza, S.T. (2009) \emph{Latent Class and Latent Transition Analysis: With Applications in the Social, Behavioral, and Health Sciences}. John Wiley & Sons Inc.
#'
#' @examples
#' ##
#' ## Example 1. GSS dataset
#' ##
#' data("gss08")
#' # LCA
#' lca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'             data = gss08, nclass = 3)
#' summary(lca)
#'
#' # LCA with covariate(s)
#' lcr = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ AGE,
#'            data = gss08, nclass = 3)
#' summary(lcr)
#' coef(lcr)
#'
#' # Multiple-group LCA (MGLCA)
#' mglca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'              group = DEGREE, data = gss08, nclass = 3)
#' summary(mglca)
#'
#'
#' # Multiple-group LCA with covariate(s) (MGLCR)
#' mglcr = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ SEX,
#'              group = DEGREE, data = gss08, nclass = 3)
#' summary(mglcr)
#' coef(mglcr)
#'
#' \donttest{
#' ##
#' ## Example 2. NYTS dataset
#' ##
#' data("nyts18")
#' # Multilevel LCA (MLCA)
#' mlca = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ 1,
#'             group = SCH_ID, data = nyts18, nclass = 3, ncluster = 2)
#' summary(mlca)
#'
#' # MLCA with covariate(s) (MLCR)
#' # (SEX: level-1 covariate, SCH_LEV: level-2 covariate)
#' mlcr = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ SEX + SCH_LEV,
#'             group = SCH_ID, data = nyts18, nclass = 3, ncluster = 2)
#' coef(mlcr)
#' }
#'
#' @export

glca <- function(
   formula, group = NULL, data = NULL, nclass = 3, ncluster = NULL, std.err = TRUE,
   measure.inv = TRUE, coeff.inv = TRUE, init.param = NULL, n.init = 10, testiter = 50,
   maxiter = 1000, eps = 1e-6, na.rm = FALSE, random.seed = NULL, verbose = TRUE
)
{
   # Random seed
   if (is.numeric(random.seed))
      set.seed(random.seed)

   # Function call
   call <- match.call()
   mc <- match(c("formula", "group", "data"), names(call), 0L)
   cll <- call[c(1L, mc)]
   cll[[1L]] <- quote(stats::model.frame)
   mf <- eval(cll, parent.frame())
   terms <- attr(mf, "terms")

   # Ecoding arguments (model, datalist, vname)
   # (type, N, Ng, G, C, W, M, R, P, Q, npar)
   # (x, y, z, observed)
   # (y.names, g.names, r.names, x.names, z.names)
   encode = glca_encode(call, terms, mf, nclass, ncluster,
                        measure.inv, coeff.inv, na.rm, verbose)
   datalist = encode$datalist
   model = encode$model
   if(model$df <= 0L) {
      if (verbose) cat("Warning: Negative degree of freedom.\n")
      std.err = FALSE
   }
   vname = encode$vname

   # Initial parameter (init)
   # (delta, gamma, beta, rho)
   if (!is.null(init.param)) {
      init.random = glca_init(model)
      init = glca_init_test(init.param, init.random, verbose)
      miniter = 1L
   } else if (n.init == 1L) {
      init = glca_init(model)
      miniter = 1L
   } else {
      test = glca_em_test(model, datalist, n.init, testiter, eps, verbose)
      init = test$param
      miniter = test$niter
   }

   # EM iteration (param, posterior, fitted, loglike)
   # (delta, gamma, beta, rho)
   # (Post type differ)
   EM = glca_em(model, datalist, init, miniter, maxiter, eps, verbose)

   # Score & Std.error calculation (score, std.err)
   if (std.err)
      scores = glca_score(model, datalist, EM$param, EM$posterior)
   else
      scores = NULL

   # Output Design (output)
   # (model, x, y, group, z, param, posterior, gof, iter)
   ret = glca_output(call, terms, model, datalist, vname, EM, scores)
   class(ret) = "glca"

   return(ret)
}
