#' Goodness of Fit Tests for Fitted \code{glca} Model
#'
#' Provides AIC, BIC, deviance statitistic for goodness of fit test for the fitted model. Given \code{object2}, the function computes the log-likelihood ratio (LRT) statisic for comparing the goodness of fit for two models. The bootstrap p-value can be obtained from the empirical distribution of LRT statistic by choosing \code{test = "boot"}.
#'
#' @param object an object of "\code{glca}", usually, a result of a call to \code{glca}
#' @param object2 an optional object of "\code{glca}" to be compared with \code{object}
#' @param test a character string indicating type of test (chi-square test or bootstrap) to obtain the p-value for goodness of fit test (\code{"chisq"} or \code{"boot"})
#' @param nboot number of bootstrap samples, only used when \code{test = "boot"}
#' @param random.seed random seed to have the equivalent solution for every bootstrap trials
#' @param maxiter an integer for maximum number of iteration for bootstrap sample
#' @param eps positive convergence tolerance for bootstrap sample
#' @param verbose an logical value for whether or not to print the result of a function's execution
#'
#' @return
#' \item{criteria}{a table with model fit criteria}
#' \item{dev.table}{a table with deviance statistic and bootstrap p-value}
#' \item{boot}{a list of LRT statistics from each bootstrap sample}
#'
#' @references
#' Akaike, H. (1974) A new look at the statistical model identification. \emph{IEEE Transactions on Automatic Control}, \bold{19}, 716–723. \url{http://doi.org/10.1109/tac.1974.1100705}
#'
#' Schwarz, G. (1978) Estimating the dimensions of a model. \emph{The Annals of Statistics}, \bold{6}, 461–464. \url{http://doi.org/10.1214/aos/1176344136}
#'
#' Langeheine, R., Pannekoek, J., and van de Pol, F. (1996) Bootstrapping goodness-of-fit measures in categorical data analysis. \emph{Sociological Methods and Research}. \bold{24}. 492-516. \url{http://doi.org/10.1177/0049124196024004004}
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
#'               data = gss, n.init = 10, nclass = 4)
#' class5 = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'               data = gss, n.init = 10, nclass = 5)
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
#'                group = SEX, n.init = 10, data = gss, nclass = 4)
#' measVar = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'                group = SEX, n.init = 10, data = gss, nclass = 4, measure.inv = FALSE)
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
   # Check_
   if (!inherits(object, "glca"))
      stop("object should be glca class.")
   else if (!is.null(object2) && !inherits(object2, "glca"))
      stop("object2 should be glca class.")
   if (is.null(test))
      test <- "none"
   else if (length(test) != 1)
      test <- "none"

   # Random seed
   if (is.numeric(random.seed))
      set.seed(random.seed)

   # Model
   model1 <- object$call
   m1 <- object
   Gsq1 <- object$gof$Gsq
   if (!is.null(object2)) {
      model2 <- object2$call
      m2 <- object2
      Gsq2 <- object2$gof$Gsq
      m <- list(m1, m2)
   }

   # Relative indicator
   if (!is.null(object2)) {
      Rel <- identical(object$datalist$y, object2$datalist$y) &
         identical(object$datalist$group, object2$datalist$group)

      Nestd <- object$model$C == object2$model$C &&
         object$model$W == object2$model$W

      if (object$model$npar < object2$model$npar) {
         H0 <- 1; H1 <- 2
      } else {
         H0 <- 2; H1 <- 1
      }
      if (Rel)
         GsqR <- 2 * abs(m[[H1]]$gof$loglike - m[[H0]]$gof$loglike)
      else
         warning("Response or group might be different.")
   }

   # Bootstrap
   if (test == "boot" & nboot > 0) {
      bGsq1 <- bGsq2 <- bGsqR <- numeric(nboot)

      for (b in 1:nboot) {
         if (verbose) {
            if (b %% 10 == 0) cat(".")
            if (b %% 100 == 0) cat(" b =", b, "\n")
            if (b == nboot) cat("b =", b, "End\n")
         }

         b1 <- glca_gnr(m1$model, m1$param, m1$datalist)
         init1 <- m1$param
         if (is.matrix(init1$gamma) && is.null(init1$delta))
            init1$gamma <- lapply(1:nrow(init1$gamma), function(g)
               matrix(init1$gamma[g, ], m1$model$Ng[g],
                      ncol(init1$gamma), byrow = TRUE))
         EMb1 <- glca_em(m1$model, b1, init1, FALSE, maxiter, eps)

         if (is.null(EMb1))
            bGsq1[b] <- NA
         else {
            obsvd <- b1$obsvd[b1$obsvd != 0 & EMb1$fitted != 0]
            fitted <- EMb1$fitted[b1$obsvd != 0 & EMb1$fitted != 0]
            bGsq1[b] <- 2 * sum(obsvd * log(obsvd / fitted))
         }


         if (!is.null(object2)) {
            b2 <- glca_gnr(m2$model, m2$param, m2$datalist)
            init2 <- glca_init(m2$model)
            EMb2 <- glca_em(m2$model, b2, init2, FALSE, maxiter, eps)
            if (is.null(EMb2))
               bGsq2[b] <- NA
            else {
               obsvd <- b2$obsvd[b2$obsvd != 0 & EMb2$fitted != 0]
               fitted <- EMb2$fitted[b2$obsvd != 0 & EMb2$fitted != 0]
               bGsq1[b] <- 2 * sum(obsvd * log(obsvd / fitted))
            }

            if (Rel) {
               if (object$model$npar < object2$model$npar) {
                  h0b <- b1
                  inith0 <- init1
                  inith1 <- init2
               } else {
                  h0b <- b2
                  inith0 <- init2
                  inith1 <- init1
               }
               EMh0 <- glca_em(m[[H0]]$model, h0b, inith0, FALSE, maxiter, eps)
               modelh1 <- m[[H1]]$model; modelh1$Ng <- m[[H0]]$model$Ng
               EMh1 <- glca_em(modelh1, h0b, inith1, FALSE, maxiter, eps)
               if (is.null(EMh0) | is.null(EMh1))
                  bGsqR[b] <- NA
               else {
                  obsvd0 <- h0b$obsvd[h0b$obsvd != 0 & EMh0$fitted != 0]
                  obsvd1 <- h0b$obsvd[h0b$obsvd != 0 & EMh1$fitted != 0]
                  fitted0 <- EMh0$fitted[h0b$obsvd != 0 & EMh0$fitted != 0]
                  fitted1 <- EMh1$fitted[h0b$obsvd != 0 & EMh1$fitted != 0]

                  temp0 <- 2 * sum(obsvd0 * log(obsvd0 / fitted0))
                  temp1 <- 2 * sum(obsvd1 * log(obsvd1 / fitted1))
                  bGsqR[b] <- temp0 - temp1
               }
            }
         }
      }

      boot1 <- mean(bGsq1 > Gsq1, na.rm = TRUE)
      if (!is.null(object2)) {
         boot2 <- mean(bGsq2 > Gsq2, na.rm = TRUE)
         if (Rel) boot3 <- mean(bGsqR > GsqR, na.rm = TRUE)
      }
   }

   if (is.null(object2))
      criteria <- data.frame(
         "Res.Df" = m1$gof$df,
         "Loglik" = m1$gof$loglike,
         "AIC" = round(m1$gof$aic, 2),
         "BIC" = round(m1$gof$bic, 2),
         "Gsq" = round(m1$gof$Gsq, 2),
         "Pearson.Chisq" = round(m1$gof$chisq, 2)
      )
   else
      criteria <- data.frame(
         "Res.Df" = c(m1$gof$df, m2$gof$df),
         "Loglik" = c(m1$gof$loglike, m2$gof$loglike),
         "AIC" = round(c(m1$gof$aic, m2$gof$aic), 2),
         "BIC" = round(c(m1$gof$bic, m2$gof$bic), 2),
         "Gsq" = round(c(m1$gof$Gsq, m2$gof$Gsq), 2),
         "Pearson Chisq" = round(c(m1$gof$chisq, m2$gof$chisq), 2)
      )

   if (is.null(object2)) {
      dev.table <- as.data.frame(cbind(
         "Res.Df" = m1$gof$df,
         "Resid.Dev(Gsq)" = round(Gsq1, 2)
      ))
      if (test == "chisq") {
         dev.table <- cbind(dev.table, "Pr(>Chi)" =
            round(pchisq(Gsq1, m1$gof$df, lower.tail = FALSE), 3))
      } else if (test == "boot")
         dev.table <- cbind(dev.table, "Pr(>BootGsq)" =
            round(mean(Gsq1 < bGsq1), 3))
   } else {
      dev.table <- as.data.frame(cbind(
         "Res.Df" = c(m1$gof$df, m2$gof$df),
         "Resid.Dev(Gsq)" = round(c(Gsq1, Gsq2), 2)
      ))

      if (test == "chisq") {
         if (!Nestd)
            warning("The models are not nested. Chi-square test is not appropriate.")
         dev.table <- cbind(dev.table, "Pr(>Chi)" =
           round(c(1 - pchisq(Gsq1, m1$gof$df),
                   1 - pchisq(Gsq2, m1$gof$df)), 3))
      } else if (test == "boot") {
         dev.table <- cbind(dev.table, "Boot p-value" =
           round(c(mean(Gsq1 < bGsq1), mean(Gsq2 < bGsq2)), 3))
      }

      if (Rel) {
         Df <- Vrel <- Prel <- c("", "")
         Df[H1] <- m[[H0]]$gof$df - m[[H1]]$gof$df
         Vrel[H1] <- round(GsqR, 2)
         dev.table <- cbind(dev.table, "Df" = Df, "Deviance(Gsq)" = Vrel)
         if (test == "chisq") {
            Prel[H1] <- round(1 - pchisq(GsqR, as.numeric(Df[H1])), 3)
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
   cat(", measure.inv :", m1$model$measure.inv, "\n")
   if (!is.null(object2)) {
      cat("Model 2:", paste(paste(model1$formula)[c(2,1,3)], collapse = " "), "\n")
      if (m2$model$G > 1) {
         cat("         group :", model2$group)
         cat(", nclass :", m2$model$C)
      } else
         cat("         nclass :", m2$model$C)
      if (m2$model$W > 1)
         cat(", ncluster :", m2$model$W)
      cat(", measure.inv :", m2$model$measure.inv, "\n")
   }

   cat("\nModel Goodness of Fit Criteria :\n")
   print(criteria)
   cat("\nAnalysis of Deviance(Gsq) Table :\n")
   print(dev.table)

   ret <- list(
      criteria = criteria,
      dev.table = dev.table
   )

   if (test == "boot" & nboot > 0) {
      ret$boot <- list(boot_Gsq1 = bGsq1)
      if (!is.null(object2)) {
         ret$boot$boot_Gsq2 <- bGsq2
         ret$boot$boot_GsqR <- bGsqR
      }
   }

   invisible(ret)
}
