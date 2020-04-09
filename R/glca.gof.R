#' Goodness of Fit Tests for Fitted \code{glca} Model
#'
#' Provides AIC, BIC, and deviance statitistic for goodness of fit test for the fitted model. Given \code{object2}, the function computes the log-likelihood ratio (LRT) statisic for comparing the goodness of fit for two models. The bootstrap p-value can be obtained from the empirical distribution of LRT statistic by choosing \code{test = "boot"}.
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
#' ## Model selection between two LCA models with different number of latent classes.
#' data(gss)
#' \donttest{
#' class4 = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'               data = gss, nclass = 4)
#' class5 = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'               data = gss, nclass = 5)
#'
#' glca.gof(class4, class5)
#' \dontrun{glca.gof(class4, class5, test = "chisq")}
#' \dontrun{glca.gof(class4, class5, test = "boot")}
#'
#' mglca = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'              group = DEGREE, data = gss, nclass = 4)
#' \dontrun{glca.gof(mglca, test = "boot")}
#'
#' ## Example 2.
#' ## Model selection between two MLCA models with different number of latent clusters.
#' cluster2 = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'                 group = REGION, data = gss, nclass = 4, ncluster = 2, na.rm = TRUE)
#' cluster3 = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'                 group = REGION, data = gss, nclass = 4, ncluster = 3, na.rm = TRUE)
#'
#' glca.gof(cluster2, cluster3)
#' \dontrun{glca.gof(cluster2, cluster3, test = "chisq")}
#' \dontrun{glca.gof(cluster2, cluster3, test = "boot")}
#'
#' ## Example 3.
#' ## MGLCA model selection under the measurement (invariance) assumption across groups.
#' measInv = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'                group = SEX, data = gss, nclass = 4)
#' measVar = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABNOMORE, ABPOOR, ABSINGLE) ~ 1,
#'                group = SEX, data = gss, nclass = 4, measure.inv = FALSE)
#'
#' glca.gof(measInv, measVar)
#' \dontrun{glca.gof(measInv, measVar, test = "chisq")}
#' \dontrun{glca.gof(measInv, measVar, test = "boot")}
#' }
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
   Rel <- FALSE; nll <- FALSE
   if (!is.null(object2)) {
      model2 <- object2$call
      m2 <- object2
      Gsq2 <- object2$gof$Gsq
      m <- list(m1, m2)
   }

   # Relative indicator
   if (!is.null(object2)) {
      Rel <- identical(object$datalist$y, object2$datalist$y)
      Nestd <- object$model$C == object2$model$C &&
         object$model$W == object2$model$W

      if (object$model$npar < object2$model$npar) {
         H0 <- 1; H1 <- 2
      } else {
         H0 <- 2; H1 <- 1
      }
      if (Rel)
         GsqR <- 2 * abs(m[[H1]]$gof$loglik - m[[H0]]$gof$loglik)
      else
         warning("Response or group might be different.\n")
   } else if (object$model$type != "Standard LCA") {
      model0 <- object$call
      Rel <- FALSE; nll <- TRUE
      GsqR <- 2 * abs(m1$gof$loglik - m1$null$nullik)
   }

   # Bootstrap
   if (test == "boot" & nboot > 0) {
      bGsq0 <- numeric(nboot)
      bGsq1 <- numeric(nboot)
      bGsq2 <- numeric(nboot)
      bGsqR <- numeric(nboot)

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

         EMb1 <- glca_em(m1$model, b1, init1, 1, maxiter, eps,  FALSE)

         if (nll) {
            b0 <- glca_gnr(m1$null$model0, m1$null$param0, m1$datalist)
            EMb0 <- glca_em(m1$model, b0, init1, 1, maxiter, eps,  FALSE)
         }

         if (is.null(EMb1)) {
            bGsq1[b] <- NA
            if (nll) bGsqR[b] <- NA
         }
         else {
            bGsq1[b] <- 2 * (b1$loglik0 - EMb1$loglik)
            if (nll) {
               bGsq0[b] <- 2 * (b0$nullik0 - EMb0$nullik)
               bGsqR[b] <- 2 * (EMb0$loglik - EMb0$nullik)
            }
         }

         if (!is.null(object2)) {
            b2 <- glca_gnr(m2$model, m2$param, m2$datalist)
            init2 <- glca_init(m2$model)
            EMb2 <- glca_em(m2$model, b2, init2, 1, maxiter, eps, FALSE)

            if (is.null(EMb2)) bGsq2[b] <- NA
            else bGsq2[b] <- 2 * (b2$loglik0 - EMb2$loglik)

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
               EMh0 <- glca_em(m[[H0]]$model, h0b, inith0, 1, maxiter, eps, FALSE)
               modelh1 <- m[[H1]]$model; modelh1$Ng <- m[[H0]]$model$Ng
               EMh1 <- glca_em(modelh1, h0b, inith1, 1, maxiter, eps, FALSE)

               if (is.null(EMh0) | is.null(EMh1)) bGsqR[b] <- NA
               else bGsqR[b] <- 2 * (EMh1$loglik - EMh0$loglik)
            }
         }
      }

      boot1 <- mean(bGsq1 > Gsq1, na.rm = TRUE)
      if (nll) {
         boot0 <- mean(bGsq0 > m1$null$Gsq, na.rm = TRUE)
         boot3 <- mean(bGsqR > GsqR, na.rm = TRUE)
      }
      if (!is.null(object2)) {
         boot2 <- mean(bGsq2 > Gsq2, na.rm = TRUE)
         if (Rel) boot3 <- mean(bGsqR > GsqR, na.rm = TRUE)
      }
   }

   if (!is.null(object2))
      criteria <- data.frame(
         "AIC" = round(c(m1$gof$aic, m2$gof$aic), 2),
         "CAIC" = round(c(m1$gof$caic, m2$gof$caic), 2),
         "BIC" = round(c(m1$gof$bic, m2$gof$bic), 2),
         "Res.Df" = c(m1$gof$df, m2$gof$df),
         "Gsq" = round(c(m1$gof$Gsq, m2$gof$Gsq), 2)
      )
   else if (nll) {
      nullpar <- m1$model$C - 1 + m1$model$C * sum(m1$model$R - 1)
      nulldf <- min(prod(m1$model$R) - 1, m1$model$N) - nullpar
      criteria <- data.frame(
         "AIC" = round(c(m1$null$aic, m1$gof$aic), 2),
         "CAIC" = round(c(m1$null$caic, m1$gof$caic), 2),
         "BIC" = round(c(m1$null$bic, m1$gof$bic), 2),
         "Res.Df" = c(nulldf, m1$gof$df),
         "Gsq" = round(c(m1$null$Gsq, m1$gof$Gsq), 2),
         row.names = c("NULL", 1)
      )
   }
   else
      criteria <- data.frame(
         "AIC" = round(m1$gof$aic, 2),
         "CAIC" = round(m1$gof$caic, 2),
         "BIC" = round(m1$gof$bic, 2),
         "Res.Df" = m1$gof$df,
         "Gsq" = round(m1$gof$Gsq, 2)
      )

   if (!is.null(object2)) {
      if (test == "chisq") {
         if (!Nestd)
            warning("The models are not nested. Chi-square test is not appropriate.")
         criteria <- cbind(criteria, "Pr(>Chi)" =
                              round(c(1 - pchisq(Gsq1, m1$gof$df),
                                      1 - pchisq(Gsq2, m1$gof$df)), 3))
      } else if (test == "boot") {
         criteria <- cbind(criteria, "Boot p-value" =
                              round(c(boot1, boot2), 3))
      }
   } else if (nll) {
      if (test == "chisq")
         criteria <- cbind(criteria, "Pr(>Chi)" =
                              round(c(1 - pchisq(m1$null$Gsq, nulldf),
                                      1 - pchisq(Gsq1, m1$gof$df)), 3))
      else if (test == "boot")
         criteria <- cbind(criteria, "Boot p-value" =
                              round(c(boot0, boot1), 3))
   } else {
      if (test == "chisq") {
         criteria <- cbind(criteria, "Pr(>Chi)" =
                              round(pchisq(Gsq1, m1$gof$df, lower.tail = FALSE), 3))
      } else if (test == "boot")
         criteria <- cbind(criteria, "Boot p-value" = round(boot1, 3))
   }

   if (nll) {
      nullpar <- m1$model$C - 1 + m1$model$C * sum(m1$model$R - 1)
      deviance <- round(2 * (m1$gof$loglik - m1$null$nullik), 2)
      Df <- m1$model$npar - nullpar
      dev.table <- as.data.frame(cbind(
         "npar" = c(nullpar, m1$model$npar),
         "Loglik" = round(c(m1$null$nullik, m1$gof$loglik), 2),
         "Df" = c("", Df),
         "Deviance" = c("", deviance)
      ))
      row.names(dev.table) = c("NULL", 1)
      if (test == "chisq") {
         Pval <-  round(1 - pchisq(deviance, as.numeric(Df)), 3)
         dev.table <- cbind(dev.table, "Pr(>Chi)" = c("", Pval))
      } else if (test == "boot") {
         Pval <- round(boot3, 3)
         dev.table <- cbind(dev.table, "Boot p-value" = c("", Pval))
      }
   } else if (Rel) {
      dev.table <- as.data.frame(cbind(
         "npar" = c(m1$model$npar, m2$model$npar),
         "Loglik" = c(m1$gof$loglik, m2$gof$loglik)
      ))
      Df <- Vrel <- Prel <- c("", "")
      Df[H1] <- m[[H0]]$gof$df - m[[H1]]$gof$df
      Vrel[H1] <- round(GsqR, 2)
      dev.table <- cbind(dev.table, "Df" = Df, "Deviance" = Vrel)
      if (test == "chisq") {
         Prel[H1] <- round(1 - pchisq(GsqR, as.numeric(Df[H1])), 3)
         dev.table <- cbind(dev.table, "Pr(>Chi)" = Prel)
      } else if (test == "boot") {
         Prel[H1] <- round(boot3, 3)
         dev.table <- cbind(dev.table, "Boot p-value" = Prel)
      }
   }

   if (nll) {
      cat("NULL   :", paste(paste(model0$formula)[c(2,1)], collapse = " "), 1, "\n")
      cat("         nclass :", m1$model$C, "\n")
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

   cat("Goodness of Fit Table :\n")
   print(criteria)
   if (Rel | nll) {
      cat("\nAnalysis of Deviance Table :\n")
      print(dev.table)
   }

   ret <- list(criteria = criteria)
   if (Rel | nll)
      ret$dev.table = dev.table

   ret$boot <- list()
   if (test == "boot" & nboot > 0) {
      if (!is.null(object2)) {
         ret$boot$boot_Gsq1 <- bGsq1
         ret$boot$boot_Gsq2 <- bGsq2
         ret$boot$boot_GsqR <- bGsqR
      } else if (nll) {
         ret$boot$boot_Gsq0 <- bGsq0
         ret$boot$boot_Gsq1 <- bGsq1
         ret$boot$boot_GsqR <- bGsqR
      } else
         ret$boot$boot_Gsq1 <- bGsq1
   }

   invisible(ret)
}
