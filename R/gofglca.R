#' Goodness of Fit Tests for Fitted \code{glca} Model
#'
#' Provides AIC, BIC, entropy and deviance statitistic for goodness of fit test for the fitted model. Given \code{object2}, the function computes the log-likelihood ratio (LRT) statisic for comparing the goodness of fit for two models. The bootstrap p-value can be obtained from the empirical distribution of LRT statistic by choosing \code{test = "boot"}.
#'
#' @param object an object of "\code{glca}", usually, a result of a call to \code{glca}.
#' @param ... an optional object of "\code{glca}" to be compared with \code{object}.
#' @param test a character string indicating type of test (chi-square test or bootstrap) to obtain the p-value for goodness of fit test (\code{"chisq"} or \code{"boot"}).
#' @param nboot number of bootstrap samples, only used when \code{test = "boot"}.
#' @param criteria a character vector indicating criteria to be printed.
#' @param maxiter an integer for maximum number of iteration for bootstrap sample.
#' @param eps positive convergence tolerance for bootstrap sample.
#' @param seed As the same value for seed guarantees the same datasets to be generated, this argument can be used for reproducibility of bootstrap results.
#' @param verbose an logical value for whether or not to print the result of a function's execution.
#'
#' @return
#' \item{gtable}{a matrix with model goodneess-of-fit criteria}
#' \item{dtable}{a matrix with deviance statistic and bootstrap p-value}
#' \item{boot}{a list of LRT statistics from each bootstrap sample}
#'
#' \code{gtable}, which is always included in output of this function, includes goodness-of-fit criteria which are indicated \code{criteria} arguments for the \code{object}(s). \code{dtable} are contained when the \code{object}s are competing models. (when used items of the models are identical) \code{dtable} prints deviance and p-value. (bootstrap or chi-square) Lastly, when the boostrap sample is used, the \code{G^2}-statistics for each bootstrap samples will be included in return object..
#'
#' @references
#' Akaike, H. (1974) A new look at the statistical model identification. \emph{IEEE Transactions on Automatic Control}, \bold{19}, 716–723. \doi{10.1109/tac.1974.1100705}
#'
#' Schwarz, G. (1978) Estimating the dimensions of a model. \emph{The Annals of Statistics}, \bold{6}, 461–464. \doi{10.1214/aos/1176344136}
#'
#' Langeheine, R., Pannekoek, J., and van de Pol, F. (1996) Bootstrapping goodness-of-fit measures in categorical data analysis. \emph{Sociological Methods and Research}. \bold{24}. 492-516. \doi{10.1177/0049124196024004004}
#'
#' Ramaswamy, V., Desarbo, W., Reibstein, D., & Robinson, W. (1993). An Empirical Pooling Approach for Estimating Marketing Mix Elasticities with PIMS Data. Marketing Science, 12(1), 103-124. \doi{10.1287/mksc.12.1.103}
#'
#'
#' @seealso \code{\link{glca}} \code{\link{gss08}} \code{\link{nyts18}}
#'
#' @examples
#' ## Example 1.
#' ## Model selection between two LCA models with different number of latent classes.
#' data(gss08)
#' class2 = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'               data = gss08, nclass = 2,  n.init = 1)
#' class3 = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'               data = gss08, nclass = 3,  n.init = 1)
#' class4 = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'               data = gss08, nclass = 4,  n.init = 1)
#'
#' gofglca(class2, class3, class4)
#' \dontrun{gofglca(class2, class3, class4, test = "boot")}
#'
#' ## Example 2.
#' ## Model selection between two MLCA models with different number of latent clusters.
#' cluster2 = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ 1,
#'                 group = SCH_ID, data = nyts18, nclass = 2, ncluster = 2, n.init = 1)
#' cluster3 = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ 1,
#'                 group = SCH_ID, data = nyts18, nclass = 2, ncluster = 3, n.init = 1)
#'
#' gofglca(cluster2, cluster3)
#' \dontrun{gofglca(cluster2, cluster3, test = "boot")}
#'
#' ## Example 3.
#' ## MGLCA model selection under the measurement (invariance) assumption across groups.
#' measInv = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'                group = DEGREE, data = gss08, nclass = 3, n.init = 1)
#' measVar = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'                group = DEGREE, data = gss08, nclass = 3, n.init = 1, measure.inv = FALSE)
#'
#' gofglca(measInv, measVar)
#'
#'
#' @export

gofglca <- function(
   object, ..., test = NULL, nboot = 50,
   criteria = c("AIC", "BIC", "entropy"),
   maxiter = 500, eps = 1e-4, seed = NULL, verbose = FALSE
)
{
   # Check_class
   if (inherits(object, "glca")) obj <- list(object, ...)
   else obj <- c(object, ...)
   if (!all(sapply(obj, inherits, "glca")))
      stop("All objects should be glca outputs.")

   # Test
   if (!is.null(test)) {
      match.test <- function(test = c("boot", "chisq")) match.arg(test)
      test <- match.test(test)
   }

   # Model
   nmodels <- length(obj)
   notes <- sapply(obj, function(x) {
      note <- paste(deparse(stats::formula(x)), collapse = "\n")
      if (x$model$G > 1L) {
         note <- paste0(note, "\n", strrep(" ", 8 + nchar(nmodels)),
                        "Group: ", x$call$group, ", nclass: ", x$model$C)
         if (x$model$W > 1L)
            note <- paste0(note, ", ncluster: ", x$model$W)
         else
            note <- paste0(note, ", measure.inv: ", x$model$measure.inv)
         if (x$model$P > 1L)
            note <- paste0(note, ", coeff.inv: ", x$model$coeff.inv)
      }
      else note <- paste0(note, "\n", strrep(" ", 8L + nchar(nmodels)),
                          "nclass: ", x$model$C)
      note
   })

   criteria <- match.arg(criteria, several.ok = TRUE)
   gof <- lapply(obj, function(x) x$gof[c("loglik", criteria, "df", "Gsq")])

   models <- sapply(obj, function(x) x$model)
   resp <- sapply(obj, function(x) paste0(x$var.names$y.names, collapse = ","))
   datn <- sapply(obj, function(x) x$model$N)
   cls  <- sapply(obj, function(x) c(x$model$C, x$model$W))

   rel <- all(resp == resp[1L], datn == datn[1L], length(obj) > 1L)
   if (length(obj) > 1L & !rel) warning("Since responses are different, deviance table does not printed.")
   nested <- all(apply(cls, 1L, function(x) x == x[1L]), rel)

   ord <- order(sapply(obj, function(x) x$model$npar))
   llik <- sapply(obj[ord], function(x) x$gof$loglik)
   gsqR <- 2L * diff(llik)

   gtable <- as.matrix(do.call(rbind, lapply(gof[ord], unlist)))
   rownames(gtable) <- ord

   # random seed
   if (is.null(seed)) set.seed(stats::runif(1, 0, .Machine$integer.max))
   else set.seed(seed)

   if (!is.null(test)) {
      # Bootstrap
      if (test == "boot" & nboot > 0L) {
         bgsq <- matrix(0L, length(obj), nboot)
         if (rel) bgsqR <- matrix(0L, length(obj) - 1L, nboot)

         init <- lapply(obj, function(x) {
            init <- x$param
            if (is.matrix(init$gamma) && is.null(init$delta))
               init$gamma <- lapply(seq_len(x$model$G), function(g)
                  matrix(init$gamma[g, ], x$model$Ng[g],
                         x$model$C, byrow = TRUE))
            if (!is.null(init$beta) && !is.null(init$delta))
               init$beta <- unlist(init$beta)
            init
         })

         for (b in 1L:nboot) {
            if (verbose) {
               if (b %% 10L == 0L) cat(".")
               if (b %% 100L == 0L) cat(" b =", b, "\n")
               if (b == nboot) cat("b =", b, "End\n")
            }

            for (x in seq_along(obj)) {
               obj0 <- obj[[ord[x]]]; obj1 <- obj[[ord[x + 1L]]]
               init0 <- init[[ord[x]]]; init1 <- init[[ord[x + 1L]]]

               bs0 <- glca_gnr(obj0$model, obj0$param, obj0$datalist)
               ll0 <- glca_em(obj0$model, bs0, init0, 1L, maxiter, eps, FALSE)$loglik
               bgsq[x, b]  <- 2L * (bs0$loglik0 - ll0)

               if (rel && x != length(obj)) {
                  bs1 <- bs0
                  if (obj0$model$G != obj1$model$G) {
                     Y <- do.call(rbind, bs0$y)
                     bs1$y <- lapply(seq_len(obj1$model$G), function(g)
                        Y[obj1$datalist$group == g, ])
                  }
                  if (obj0$model$P != obj1$model$P)
                     bs1$x <- obj1$datalist$x
                  if (obj0$model$Q != obj1$model$Q)
                     bs1$z <- obj1$datalist$z

                  ll1 <- glca_em(obj1$model, bs1, init1, 1L, maxiter, eps, FALSE)$loglik
                  bgsqR[x, b] <- 2L * (ll1 - ll0)
               }
            }
         }

         boot  <- rowMeans(bgsq  > sapply(gof[ord], function(x) x$Gsq))
         if (rel) bootR <- rowMeans(bgsqR > gsqR)
         gtable <- cbind(gtable, `Boot p-value` = boot)
      }
   }

   if (rel) {
      npar <- sapply(obj[ord], function(x) x$model$npar)
      Df  <- diff(npar)
      dtable <- cbind(npar, llik, c(NA, Df), c(NA, round(gsqR, 3L)))
      colnames(dtable) <- c("npar", "loglik", "Df", "Deviance")
      rownames(dtable) <- ord

      if (!is.null(test))
         dtable <- switch(test, "chisq" = {
            cbind(dtable, `Pr(>Chi)` = c(NA, 1L - stats::pchisq(gsqR, as.numeric(Df))))
         }, "boot" = cbind(dtable, `Boot p-value` = c(NA, round(bootR, 2L))))
   }

   ret <- list()
   ret$gtable <- gtable
   if (rel) ret$dtable <- dtable
   if (!is.null(test)) {
      if (test == "boot") {
         ret$boot <- list(boot.Gsq = bgsq)
         if (rel) ret$boot$boot.GsqR <- bgsqR
      }
   }
   attr(ret, "notes") <- notes
   class(ret) <- "gofglca"
   return(ret)
}
