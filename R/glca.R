#' Fits Latent Class Analysis Including Group Variable and Covariates
#'
#' Function for fitting LCA models with multiple groups, which may include latent class structure for group variable.
#'
#' @param formula a formula for specifying manifest items using the "\code{item}" function and covariates.
#' @param group an optional vector specifying a group of observations, which can be include group covariates using "\code{group}" function. given group variable, group covariates can be incorporated.
#' @param data a data frame containing the manifest item, covariates and group variable.
#' @param nclass number of latent classes. default is 3.
#' @param ncluster number of level 2 latent classes. given ncluster and group variable, the MLCA will be fitted.
#' @param measure_inv a logical value of the assumption of measurement invariance across groups.
#' @param std_err a logical value whether calculating standard error of estimates. default is TRUE.
#' @param init_param a list which contains user-defined initial parameter.
#' @param n_init number of random initial parameter set.
#' @param maxiter an integer for maximum number of iteration.
#' @param eps positive convergence tolerance.
#' @param na.rm a logical value whether or not to remove observations missing at least 1 item.
#' @param random.seed random seed which generates the seed from R
#' @param verbose a logical value for whether or not to print the result of a function's execution.
#'
#' @author Youngsun Kim
#'
#' @details The formula should consist of an \code{~} operator between two sides. Manifest items should be indicated in LHS of formula using \code{item} function and covariates should be specified in RHS of formula. For example, \cr
#' \code{item(y1, y2, y3) ~ 1}\cr
#' \code{item(y1, y2, y3) ~ x1 + x2}
#'
#' In \code{glca}, manifest items should be categorical variable and the probability of each category of manifest items varies across latent classes. This probabilities are called "item response probability" (\code{rho} in \code{glca} output). The latent variables (i.e latent class or latent cluster) are categorical as well. The parameters indicating probability of each category is called "prevalence" (\code{delta}, \code{gamma} in \code{glca} output). The prevalence for latent class (\code{gamma}) can be modeled with covariates. Using logistic regression, coefficients for each covariates (\code{beta} in \code{glca} output) can be estimated.
#'
#' \code{glca} can handle two types of covariates. If covariates vary across individuals, the covariates are considered as level-1 covariates. Given group, and the covariates varying across groups are considered as level-2 covariates. Both type of covariates have an effect on class prevalence. These covariate type will be automatically detected by \code{glca}.
#'
#' @return \code{glca} returns an object of class "\code{glca}".
#'
#' The function \code{summary} prints estimates for parameters and \code{glca.gof} function gives goodness of fit measures for the model.
#'
#' An object of class "\code{glca}" is a list containing at least the following components:
#'
#' \item{call}{the matched call.}
#' \item{model}{a list which contains model descriptions.}
#' \item{datalist}{a list of data used for fitting.}
#' \item{param}{a list of parameter estimates.}
#' \item{std.err}{a list of standard error for estimates}
#' \item{coefficient}{a list of model coefficients for prevalence.}
#' \item{posterior}{a data frame with posterior probablity of each individaul for latent classes and each group for latent clusters}
#' \item{count}{a data frame with unique patterns of manifest items and corresponding observed and predicted counts.}
#' \item{gof}{a list of goodness of fit measures, i.e. AIC, BIC, and log-likelihood.}
#' \item{convergence}{a list about convergence which contains whether or not it has been converged, number of iterations and scores.}
#'
#' @seealso \code{\link{gss}} \code{\link{brfss}}
#'
#' @references
#' Jeroen K. Vermunt (2003). \emph{7. Multilevel Latent Class Models}. Sociological Methodology, 33(1), 213–239. \url{https://doi.org/10.1111/j.0081-1750.2003.t01-1-00131.x}
#'
#' Linda M. Collins, Stephanie T. Lanza (2009). \emph{Latent Class and Latent Transition Analysis: With Applications in the Social, Behavioral, and Health Sciences}. John Wiley & Sons Inc.
#'
#' @examples
#' ##
#' ## Example 1. GSS dataset
#' ##
#' data("gss")
#'
#' # LCA
#' lca = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'             data = gss, nclass = 4)
#' summary(lca)
#'
#' # LCA with covariate(s)
#' lcr = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ AGE,
#'            data = gss, nclass = 4)
#' summary(lcr)
#' coef(lcr)
#'
#' # Multitple Group LCA (MGLCA)
#' mglca = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'              group = DEGREE, data = gss, nclass = 4)
#' summary(mglca)
#'
#' # Multitple Group LCA with Covariates (MGLCR)
#' mglcr = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ SEX,
#'              group = DEGREE, data = gss, nclass = 3)
#' summary(mglcr)
#' coef(mglcr)
#'
#' ##
#' ## Example 2. BRFSS data
#' ##
#' data("brfss")
#'
#' # Multilevel LCA (MLCA)
#' brfss1000 = brfss[sample(1:nrow(brfss), 1000),]
#' mlca = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ 1,
#'             group = STATE, data = brfss1000, nclass = 3, ncluster = 2)
#' summary(mlca)
#'
#' # MLCA with Covariates (MLCR)
#' # (SEX: level-1 covariate, REGION: level-2 covariate)
#' mlcr = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ SEX + REGION,
#'             group = STATE, data = brfss1000, nclass = 3, ncluster = 2)
#' summary(mlcr)
#' coef(mlcr)
#'
#' @export

glca <- function(
   formula, group = NULL, data,
   nclass = 3, ncluster = 0,
   measure_inv = TRUE, std_err = TRUE,
   init_param = NULL, n_init = 1,
   maxiter = 5000, eps = 1e-10,
   na.rm = FALSE, random.seed = NULL,
   verbose = TRUE
)
{
   # Random seed
   if (is.numeric(random.seed))
      set.seed(random.seed)

   # Function call
   call <- match.call()
   mf <- match.call(expand.dots = FALSE)
   m <- match(c("formula", "group", "data"), names(mf), 0L)
   mf <- mf[c(1L, m)]
   mf[[1L]] <- quote(stats::model.frame)
   mf <- eval(mf, parent.frame())

   # Ecoding arguments (model, datalist, vname)
   # (type, N, Ng, G, C, W, M, R, P, Q, npar)
   # (x, y, z, pattern, observed)
   # (y.names, g.names, r.names, x.names, z.names)
   encode = glca_encode(mf, data, nclass, ncluster,
                        measure_inv, na.rm, verbose)
   datalist = encode$datalist
   model = encode$model
   if(model$df <= 0) {
      if (verbose) cat("Warning: Negative degree of freedom.\n")
      std_err = FALSE
   }
   vname = encode$vname

   # Initial parameter (init)
   # (delta, gamma, beta, rho)
   if (!is.null(init_param)) {
      n_init = 1
      init_random = glca_init(model)
      init = glca_init_test(init_param, init_random, verbose)
   } else {
      init = glca_em_test(model, datalist, n_init, verbose)
   }

   # EM iteration (param, posterior, fitted, loglike)
   # (delta, gamma, beta, rho)
   # (Post type differ)
   EM = glca_em(model, datalist, init, verbose, maxiter, eps)

   # Score & Std.error calculation (score, std.err)
   if (std_err)
      scores = glca_score(model, datalist, EM$param, EM$posterior)
   else
      scores = NULL

   # Output Design (output)
   # (model, x, y, group, z, param, posterior, gof, iter)
   ret = glca_output(call, model, datalist, vname, EM, scores)
   class(ret) = "glca"

   return(ret)
}
