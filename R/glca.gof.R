#' Goodness of Fit Tests for Fitted \code{glca} Model (AIC, BIC, Bootstrap p-value)
#'
#' Provides model fit criteria such as AIC, BIC, deviance statistics\eqn{(G^2)} and bootstrap p-value for "absolute" model fit. Given \code{object2}, it also computes bootstrap p-value for "relative" model fit. The bootstrap p-value is obtained from the empirical distribution of \eqn{G^2} statistics.
#'
#' @param object an object of "\code{glca}", usually, a result of a call to \code{glca}.
#' @param object2 an optional object of "\code{glca}" to be compared with former object.
#' @param test a character string, one of \code{"chisq"} or \code{"boot"}. given \code{"chisq"}, p-value of chisq-test for deviance will be computed, and given \code{"boot"}, bootstrap p-value for deviance will be computed.
#' @param nboot number of bootstrap samples, only used when \code{test = "boot"}
#' @param random.seed random seed which generates the seed from \code{R}
#' @param maxiter an integer for maximum number of iteration for bootstrap sample.
#' @param eps positive convergence tolerance for bootstrap sample.
#' @param verbose an logical value for whether or not to print the result of a function's execution.
#'
#' @return
#' \item{table}{a table with model fit criteria and bootstrap p-value.}
#' \item{boot}{a list of \eqn{G^2} statistics from each bootstrap sample}
#'
#' @references
#' Akaike H (1974). \emph{A New Look at the Statistical Model Identification.} IEEE Transactions on Automatic Control, 19, 716–723. \url{http://doi.org/10.1109/tac.1974.1100705}
#'
#' Schwarz G (1978). \emph{Estimating the Dimensions of a Model.} The Annals of Statistics, 6, 461–464. \url{http://doi.org/10.1214/aos/1176344136}
#'
#' LANGEHEINE, ROLF & Pannekoek, Jeroen & van de Pol, Frank. (1996). \emph{Bootstrapping Goodness-of-Fit Measures in Categorical Data Analysis.} Sociological Methods &amp Research. 24. 492-516. \url{http://doi.org/10.1177/0049124196024004004}
#'
#' @author Youngsun Kim
#'
#' @seealso \code{\link{glca}} \code{\link{gss}}
#'
#' @examples
#' ## Example 1.
#' ## LCA model selection between two models with different number of latent classes.
#' data(gss)
#' class4 = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'               data = gss, n_init = 10, nclass = 4)
#' class5 = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'               data = gss, n_init = 10, nclass = 5)
#'
#' glca.gof(class4, class5)
#' \dontrun{glca.gof(class4, class5, test = "chisq")}
#' \dontrun{glca.gof(class4, class5, test = "boot")}
#'
#' ## Example 2.
#' ## MLCA model selection between two models with different number of latent clusters.
#' cluster2 = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'                 group = REGION, data = gss, nclass = 4, ncluster = 2, na.rm = TRUE)
#' cluster3 = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'                 group = REGION, data = gss, nclass = 4, ncluster = 3, na.rm = TRUE)
#'
#' \dontrun{glca.gof(cluster2, cluster3)}
#' \dontrun{glca.gof(cluster2, cluster3, test = "chisq")}
#' \dontrun{glca.gof(cluster2, cluster3, test = "boot")}
#'
#' ## Example 3.
#' ## MGLCA model selection under the measurement (invariance) assumption across groups.
#' measInv = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'                group = SEX, n_init = 10, data = gss, nclass = 4)
#' measVar = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'                group = SEX, n_init = 10, data = gss, nclass = 4, measure_inv = FALSE)
#'
#' \dontrun{glca.gof(measInv, measVar)}
#' \dontrun{glca.gof(measInv, measVar, test = "chisq")}
#' \dontrun{glca.gof(measInv, measVar, test = "boot")}
#'
#' @export

glca.gof = function(
   object, object2 = NULL, test = NULL, nboot = 25,
   random.seed = NULL, maxiter = 500, eps = 1e-5, verbose = TRUE
)
{
   # Check
   if (!inherits(object, "glca"))
      stop("object should be glca class.")
   else if (!is.null(object2) && !inherits(object2, "glca"))
      stop("object2 should be glca class.")
   if (is.null(test))
      test = "none"
   else if (length(test) != 1)
      test = "none"

   # Random seed
   if (is.numeric(random.seed))
      set.seed(random.seed)

   # Model
   model1 = object$call
   m1 = object
   Dev1 = object$gof$resid.dev
   if (!is.null(object2)) {
      model2 = object2$call
      m2 = object2
      Dev2 = object2$gof$resid.dev
      m = list(m1, m2)
   }

   # Relative indicator
   if (!is.null(object2)) {
      Rel = identical(object$datalist$y, object2$datalist$y)
      if (!Rel) test = "none"
      Nestd = object$model$C == object2$model$C &&
         object$model$W == object2$model$W

      if (object$model$npar < object2$model$npar) {
         H0 = 1; H1 = 2
      } else {
         H0 = 2; H1 = 1
      }
      if (Rel)
         DevR = 2 * abs(m[[H1]]$gof$loglike - m[[H0]]$gof$loglike)
   }

   # Bootstrap
   if (test == "boot" & nboot > 0) {
      bDev1 = bDev2 = Dev3 = numeric(nboot)

      for (b in 1:nboot) {
         if (verbose) {
            if (b %% 10 == 0) cat(".")
            if (b %% 100 == 0) cat(" b =", b, "\n")
            if (b == nboot) cat("b =", b, "End\n")
         }

         b1 = glca_gnr(m1$model, m1$param, m1$datalist)
         init1 = m1$param
         if (is.matrix(init1$gamma) && is.null(init1$delta))
            init1$gamma = lapply(1:nrow(init1$gamma), function(g)
               matrix(init1$gamma[g, ], m1$model$Ng[g],
                      ncol(init1$gamma), byrow = TRUE))
         EMb1 = glca_em(m1$model, b1, init1, FALSE, maxiter, eps)

         if (is.null(EMb1))
            bDev1[b] = NA
         else
            bDev1[b] = 2 * (b1$loglike - EMb1$loglike)

         if (!is.null(object2)) {
            b2 = glca_gnr(m2$model, m2$param, m2$datalist)
            init2 = glca_init(m2$model)
            EMb2 = glca_em(m2$model, b2, init2, FALSE, maxiter, eps)
            if (is.null(EMb2))
               bDev2[b] = NA
            else
               bDev2[b] = 2 * (b2$loglike - EMb2$loglike)

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
                  Dev3[b] <- NA
               else
                  Dev3[b] <- 2 * (EMh1$loglike - EMh0$loglike)
            }
         }
      }

      boot1 <- mean(bDev1 > Dev1, na.rm = TRUE)
      if (!is.null(object2)) {
         boot2 <- mean(bDev2 > Dev2, na.rm = TRUE)
         if (Rel) boot3 = mean(Dev3 > DevR, na.rm = TRUE)
      }
   }

   if (is.null(object2))
      criteria <- data.frame(
         "Res.Df" = m1$gof$df,
         "AIC" = round(m1$gof$aic, 2),
         "BIC" = round(m1$gof$bic, 2),
         "Gsq" = round(m1$gof$Gsq, 2),
         "Resid.Dev" = round(m1$gof$resid.dev, 2)
      )
   else
      criteria <- data.frame(
         "Res.Df" = c(m1$gof$df, m2$gof$df),
         "AIC" = round(c(m1$gof$aic, m2$gof$aic), 2),
         "BIC" = round(c(m1$gof$bic, m2$gof$bic), 2),
         "Gsq" = round(c(m1$gof$Gsq, m2$gof$Gsq), 2),
         "Resid.Dev" = round(c(m1$gof$resid.dev, m2$gof$resid.dev), 2)
      )

   if (is.null(object2)) {
      dev.table <- data.frame(
         "Res.Df" = m1$gof$df,
         "Resid.Dev" = round(Dev1, 2)
      )
      if (test == "chisq") {
         dev.table <- cbind(dev.table, "Pr(>Chi)" =
            round(pchisq(Dev1, m1$gof$df, lower.tail = FALSE), 3))
      } else if (test == "boot")
         dev.table <- cbind(dev.table, "Pr(>BootDev)" =
            round(mean(Dev1 < bDev1), 3))
   } else {
      dev.table <- data.frame(
         "Res.Df" = c(m1$gof$df, m2$gof$df),
         "Resid.Dev" = round(c(Dev1, Dev2), 2)
      )

      if (test == "chisq") {
         if (!Nestd)
            warning("The models are not nested. Chi-square test is not appropriate.")
         dev.table <- cbind(dev.table, "Pr(>Chi)" =
           round(c(1 - pchisq(Dev1, m1$gof$df),
                   1 - pchisq(Dev2, m1$gof$df)), 3))
      } else if (test == "boot") {
         dev.table <- cbind(dev.table, "Boot p-value" =
           round(c(mean(Dev1 < bDev1), mean(Dev2 < bDev2)), 3))
      }

      if (Rel) {
         Df <- Vrel <- Prel <- c("", "")
         Df[H1] <- m[[H0]]$gof$df - m[[H1]]$gof$df
         Vrel[H1] <- round(DevR, 2)
         dev.table <- cbind(dev.table, "Df" = Df, "Deviance" = Vrel)
         if (test == "chisq") {
            Prel[H1] <- round(1 - pchisq(DevR, as.numeric(Df[H1])), 3)
            dev.table <- cbind(dev.table, "Pr(>Chi)" = Prel)
         } else if (test == "boot") {
            Prel[H1] <- round(boot3, 3)
            dev.table <- cbind(dev.table, "Boot p-value" = Prel)
         }
      }
   }

   cat("Model 1:", paste(paste(model1$formula)[c(2,1,3)], collapse = " "), "\n")
   if (m1$model$G > 1) {
      cat("         group :", model1$group)
      cat(", nclass :", m1$model$C)
   } else
      cat("         nclass :", m1$model$C)
   if (m1$model$W > 1)
       cat(", ncluster :", m1$model$W)
   cat(", measure_inv :", m1$model$measure_inv, "\n")
   if (!is.null(object2)) {
      cat("Model 2:", paste(paste(model1$formula)[c(2,1,3)], collapse = " "), "\n")
      if (m2$model$G > 1) {
         cat("         group :", model2$group)
         cat(", nclass :", m2$model$C)
      } else
         cat("         nclass :", m2$model$C)
      if (m2$model$W > 1)
         cat(", ncluster :", m2$model$W)
      cat(", measure_inv :", m2$model$measure_inv, "\n")
   }

   cat("\nModel Goodness of Fit Criteria :\n")
   print(criteria)
   cat("\nAnalysis of Deviance Table :\n")
   print(dev.table)

   ret = list()
   ret$criteria = criteria
   ret$dev.table = dev.table

   if (test == "boot" & nboot > 0) {
      ret$boot = list(boot_Dev1 = bDev1)
      if (!is.null(object2)) {
         ret$boot$boot_Dev2 = bDev2
         ret$boot$boot_DevR = Dev3
      }
   }

   invisible(ret)
}
