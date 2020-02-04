#' Fitting Latent Class Analysis with Grouped Data
#'
#' Function for fitting LCA models with multiple group. Multiple Group Latent Class Analysis and Multilevel Latent Class Analysis can be fitted.
#' @param formula a formula for specifying manifest items using the "\code{item}" function and covariates.
#' @param group an optional vector specifying a group of observations, which can be include group covariates using "\code{group}" function
#' @param data a data frame containing the manifest item, covariates and group.
#' @param nclass number of latent classes. default is 3.
#' @param ncluster number of level 2 latent classes. default is 0.
#' @param measure_inv a logical value of the assumption of measurement invariance across groups.
#' @param std_err a logical value whether calculating standard error of estimates. default is TRUE.
#' @param init_param a list which contains user-defined initial parameter.
#' @param n_init number of random initial parameter set.
#' @param maxiter an integer for maximum number of iteration.
#' @param eps positive convergence tolerance.
#' @param na.rm a logical value whether or not to remove observations which has at least 1 item.
#' @param verbose a logical value for whether or not to print the result of a function's execution.
#'
#' @details The formula should consist of an \code{~} operator between two sides. Manifest items should be indicated in LHS of formula using \code{item} function and covariates should be specified in RHS of formula. For example, \cr
#' \code{item(y1, y2, y3) ~ 1}\cr
#' \code{item(y1, y2, y3) ~ x1 + x2}
#'
#' There are two type of covariates on \code{glca} model. If covariates are varies across individuals, the covariates are considered as level 1 covariates. Given group, if covariates are varies across groups, the covariates are considered as level 2 covariates. Both type of covariates have an effect on class prevalence.

#'
#' The \code{glca} models are latent class models which assumes latent categorical variable (i.e latent cluster or latent class). Since those latent variabels are categorical, there are parameters indicating probability of each category and it is called "prevalence". According to latent variable, each manifest item behaves differently. Since for \code{glca} models, manifest items should be categorical variable, behaviors of manifest items can be depicted with multinomial distribution. And the probability of each category (differs according to latent class) is called "item response probability".
#'
#' The parameters to be estimated are \code{delta}, \code{gamma}, \code{beta}, and \code{rho}. \code{delta} are prevalences of latent clusters, a latent categorical variables of groups, \code{gamma} are prevalences of latent classes (according to latent cluster for multilevel LCA), a latent categorical variables of individuals, \code{beta} are covariates coefficient for \code{gamma}, and \code{rho} are item response probabilities.
#'
#' @return \code{glca} returns an object of class "\code{glca}".
#'
#' The function \code{summary} prints estimates for parameters and \code{anova} function gives goodness of fit measures for the model.
#'
#' An object of class "\code{glca}" is a list containing at least the following components:
#'
#' \item{call}{the matched call.}
#' \item{model}{a list which contains model descriptions.}
#' \item{datalist}{a list of data used for fitting.}
#' \item{param}{a list of parameter estimates.}
#' \item{std.err}{a list of standard error for estimates}
#' \item{coefficient}{a list of model coefficients for prevalence.}
#' \item{posterior}{a data frame with posterior probablity of each individaul for latent classes}
#' \item{count}{a data frame with unique patterns of manifest items and corresponding observed and predicted counts.}
#' \item{gof}{a list of goodness of fit measures, i.e. AIC, BIC, and log-likelihood.}
#' \item{convergence}{a list about convergence which contains whether or not it has been converged, number of iterations and scores.}
#'
#' @references
#' Jeroen K. Vermunt (2003). \emph{7. Multilevel Latent Class Models}. Sociological Methodology, 33(1), 213–239. \url{https://doi.org/10.1111/j.0081-1750.2003.t01-1-00131.x}
#'
#' Linda M. Collins, Stephanie T. Lanza (2009). \emph{Latent Class and Latent Transition Analysis: With Applications in the Social, Behavioral, and Health Sciences}. John Wiley & Sons Inc.
#'
#' @examples
#' # GSS data (LCA)
#' data("gss")
#' lca1 = glca(item(ABDEFECT, ABNOMORE, ABHLTH, ABPOOR, ABRAPE, ABSINGLE, ABANY) ~ 1,
#'            data = gss, nclass = 2)
#' summary(lca1)
#' lca2 = glca(item(ABDEFECT, ABNOMORE, ABHLTH, ABPOOR, ABRAPE, ABSINGLE, ABANY) ~ 1,
#'            data = gss, nclass = 3)
#' summary(lca2)
#' anova(lca1, lca2)
#' anova(lca1, lca2, nboot = 100)
#'
#' # NHANES data (LCA with covariates)
#' data("nhanes")
#' lcr = glca(item(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050) ~ AGE,
#'             data = nhanes, nclass = 2)
#' summary(lcr)
#' coef(lcr)
#'
#' # GSS data (MGLCA)
#' mglca = glca(item(ABDEFECT, ABNOMORE, ABHLTH, ABPOOR, ABRAPE, ABSINGLE, ABANY) ~ 1,
#'              group = DEGREE, data = gss, nclass = 3)
#' summary(mglca)
#'
#' # BRFSS data (MLCA)
#' data("brfss")
#' brfss2000 = brfss[sample(1:nrow(brfss), 2000),]
#' mlca = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ 1,
#'             group = STATE, data = brfss2000, nclass = 3, ncluster = 3)
#' summary(mlca)
#'
#' # BRFSS data (MLCA with group covariates)
#' mlcr = glca(item(OBESE, PA300, FRTLT1A, VEGLT1A, SMOKER, DRNK30) ~ SEX + REGION,
#'             group = STATE, data = brfss2000, nclass = 3, ncluster = 3)
#' summary(mlcr)
#' coef(mlcr)
#'
#' @export

glca <- function(
   formula, group = NULL, data,
   nclass = 3, ncluster = 0,
   measure_inv = TRUE, std_err = TRUE,
   init_param = NULL, n_init = 1,
   maxiter = 1000, eps = 1e-10,
   na.rm = FALSE, verbose = TRUE
)
{
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
      if (verbose) cat("Warning: Negative degree of freedom.")
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
