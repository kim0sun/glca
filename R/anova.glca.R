#' Goodness of Fit Tests for Fitted \code{glca} Model.
#'
#' Basically, show AIC and BIC for model fit criteria. When \code{nboot} given, using parametric bootstrap, compute bootstrap p-value for "relative" and "absolute" model fit.
#'
#' @param object an object of "\code{glca}", usually, a result of a call to \code{glca}.
#' @param object2 an object of "\code{glca}" to be compared with former object.
#' @param nboot number of bootstrap samples.
#' @param maxiter an integer for maximum number of iteration for bootstrap sample.
#' @param eps positive convergence tolerance for bootstrap sample.
#' @param verbose an logical value for whether or not to print the result of a function's execution.
#' @param ... further arguments passed to or from other methods.
#'
#' @return This function returns a table with model fit criteria and bootstrap p-value.
#'
#' @references
#' Akaike H (1974). \emph{A New Look at the Statistical Model Identification.} IEEE Transactions on Automatic Control, 19, 716–723. \url{http://doi.org/10.1109/tac.1974.1100705}
#'
#' Schwarz G (1978). \emph{Estimating the Dimensions of a Model.} The Annals of Statistics, 6, 461–464. \url{http://doi.org/10.1214/aos/1176344136}
#'
#' bootstrap p-value references
#'
#' @seealso \code{\link{glca}}
#'
#' @examples
#' ## Example 1.
#' ## LCA model selection between two models with different number of latent classes.
#' data(gss)
#' class4 = glca(item(ABDEFECT, ABNOMORE, ABHLTH, ABPOOR, ABRAPE, ABSINGLE, ABANY) ~ 1,
#'               data = gss, nclass = 4)
#' class5 = glca(item(ABDEFECT, ABNOMORE, ABHLTH, ABPOOR, ABRAPE, ABSINGLE, ABANY) ~ 1,
#'               data = gss, nclass = 5, maxiter = 5000)
#'
#' anova(class4, class5)
#' anova(class4, class5, nboot = 25)
#'
#' ## Example 2.
#' ## MGLCA model selection between two models with different number of latent clusters.
#' cluster3 = glca(item(ABDEFECT, ABNOMORE, ABHLTH, ABPOOR, ABRAPE, ABSINGLE, ABANY) ~ 1,
#'                 REGION, data = gss, nclass = 4, ncluster = 3)
#' cluster4 = glca(item(ABDEFECT, ABNOMORE, ABHLTH, ABPOOR, ABRAPE, ABSINGLE, ABANY) ~ 1,
#'                 REGION, data = gss, nclass = 4, ncluster = 4, maxiter = 2000)
#'
#' anova(cluster3, cluster4)
#' anova(cluster3, cluster4, nboot = 25)
#'
#' ## Example 3.
#' ## MGLCA model selection under the measurement (invariance) assumption across groups.
#' measInv = glca(item(ABDEFECT, ABNOMORE, ABHLTH, ABPOOR, ABRAPE, ABSINGLE, ABANY) ~ 1,
#'                SEX, data = gss, nclass = 4)
#' measVar = glca(item(ABDEFECT, ABNOMORE, ABHLTH, ABPOOR, ABRAPE, ABSINGLE, ABANY) ~ 1,
#'                SEX, data = gss, nclass = 4, measure_inv = FALSE)
#'
#' anova(measInv, measVar)
#' anova(measInv, measVar, nboot = 25)
#'
#' @method anova glca
#' @export

anova.glca = function(
   object, object2 = NULL, nboot = 0,
   maxiter = 200, eps = 1e-5, verbose = TRUE, ...
)
{
   # Model
   model1 = object$call$formula
   m1 = object
   if (!is.null(object2)) {
      model2 = object2$call$formula
      m2 = object2
      m = list(m1, m2)
   }

   # Bootstrap
   Gsq1 = Gsq2 = Gsq3 = numeric(nboot)

   # Relative indicator
   if (!is.null(object2)) {
      Rel = identical(object$datalist, object2$datalist)
      if (object$model$npar < object2$model$npar) {
         H0 = 1; H1 = 2
      } else {
         H0 = 2; H1 = 1
      }
      if (Rel)
         GsqR = 2 * (m[[H1]]$gof$loglike - m[[H0]]$gof$loglike)
   }

   if (nboot > 0) {
      for (b in 1:nboot) {
         if (verbose) {
            if (b %% 10 == 0) cat(".")
            if (b %% 100 == 0) cat("b =", b, "\n")
            if (b == nboot) cat("b =", b, "End.\n")
         }

         b1 = glca_gnr(m1$model, m1$param, m1$datalist)
         init1 = m1$param
         if (is.matrix(init1$gamma) && is.null(init1$delta))
            init1$gamma = lapply(1:nrow(init1$gamma), function(g)
               matrix(init1$gamma[g, ], m1$model$Ng[g],
                      ncol(init1$gamma), byrow = TRUE))
         EMb1 = glca_em(m1$model, b1, init1, FALSE, maxiter, eps)
         if (is.null(EMb1))
            Gsq1[b] = NA
         else
            Gsq1[b] = 2 * sum((b1$observed *
               log(b1$observed / EMb1$fitted))[b1$observed != 0 & EMb1$fitted != 0])

         if (!is.null(object2)) {
            b2 = glca_gnr(m2$model, m2$param, m2$datalist)
            init2 = glca_init(m2$model)
            EMb2 = glca_em(m2$model, b2, init2, FALSE, maxiter, eps)
            if (is.null(EMb2))
               Gsq2[b] = NA
            else
               Gsq2[b] = 2 * sum((b2$observed *
                  log(b2$observed / EMb2$fitted))[b2$observed != 0 & EMb2$fitted != 0])

            if (Rel) {
               if (object$model$npar < object2$model$npar) {
                  h0b = b1
                  inith0 = init1
                  inith1 = init2
               } else {
                  h0b = b2
                  inith0 = init2
                  inith1 = init1
               }
               EMh0 = glca_em(m[[H0]]$model, h0b, inith0, FALSE, maxiter, eps)
               modelh1 = m[[H1]]$model; modelh1$Ng = m[[H0]]$model$Ng
               EMh1 = glca_em(modelh1, h0b, inith1, FALSE, maxiter, eps)
               if (is.null(EMh0) | is.null(EMh1))
                  Gsq3[b] <- NA
               else
                  Gsq3[b] <- 2 * (EMh1$loglike - EMh0$loglike)
            }
         }
      }

      boot1 <- mean(Gsq1 > m1$gof$Gsq, na.rm = TRUE)
      if (!is.null(object2)) {
         boot2 <- mean(Gsq2 > m2$gof$Gsq, na.rm = TRUE)
         if (Rel) boot3 = mean(Gsq3 > GsqR, na.rm = TRUE)
      }

   }

   if (is.null(object2)) {
      cat("\nModel Goodness of Fit Table\n\n")
      tab <- data.frame(
         "Res.Df" = m1$gof$df,
         "AIC" = round(m1$gof$aic, 2),
         "BIC" = round(m1$gof$bic, 2),
         "Gsq" = round(m1$gof$Gsq, 2)
      )
      if (nboot > 0)
         tab <- cbind(tab, "Boot Pr(Abs)" = round(boot1, 3))
   } else {
      if (Rel) {
         cat("\nModel Goodness of Fit Table\n\n")
         tab <- data.frame(
            "Res.Df" = c(m1$gof$df, m2$gof$df),
            "AIC" = round(c(m1$gof$aic, m2$gof$aic), 2),
            "BIC" = round(c(m1$gof$bic, m2$gof$bic), 2),
            "Gsq" = round(c(m1$gof$Gsq, m2$gof$Gsq), 2)
         )
         if (nboot > 0) {
            Df <- bGsqR <- bootrel <- c("", "")
            Df[H1] <- m[[H0]]$gof$df - m[[H1]]$gof$df
            bootrel[H1] <- round(boot3, 3)
            bGsqR[H1] <- round(GsqR, 3)
            tab <- cbind(tab, "Boot Pr(Abs)" = round(c(boot1, boot2), 3),
                         "Df" = Df, "Gsq(Rel)" = bGsqR,
                         "Boot Pr(Rel)" = bootrel)
         }

      } else {
         warning("Response are different.")
         cat("\nModel Goodness of Fit Table\n\n")
         tab <- data.frame(
            "Res.Df" = c(m1$gof$df, m2$gof$df),
            "AIC" = round(c(m1$gof$aic, m2$gof$aic), 2),
            "BIC" = round(c(m1$gof$bic, m2$gof$bic), 2),
            "Gsq" = round(c(m1$gof$Gsq, m2$gof$Gsq), 2)
         )
         if (nboot > 0)
            tab <- cbind(tab, "Boot Pr(Abs)" = round(c(boot1, boot2), 3))
      }
   }

   cat("Model 1:", paste(deparse(model1), sep = "", collapse = ""), "\n")
   cat("         nclass :", m1$model$C)
   if (m1$model$W > 1)
       cat(", ncluster :", m1$model$W, "\n")
   else cat("\n")
   if (!is.null(object2)) {
      cat("Model 2:", paste(deparse(model2), sep = "", collapse = ""), "\n")
      cat("         nclass :", m2$model$C)
      if (m2$model$W > 1)
         cat(", ncluster :", m2$model$W, "\n")
      else cat("\n")
   }
   print(tab)

   ret = list()
   ret$table = tab
   if (nboot > 0) {
      ret$boot = list(boot_Gsq1 = Gsq1)
      if (!is.null(object2)) {
         ret$boot$boot_Gsq2 = Gsq2
         ret$boot$boot_GsqR = Gsq3
      }
   }

   invisible(ret)
}
