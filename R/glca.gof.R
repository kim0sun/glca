#' Goodness of Fit Tests for Fitted \code{glca} Model
#'
#' Provides AIC, CAIC, BIC, entropy and deviance statitistic for goodness of fit test for the fitted model. Given \code{object2}, the function computes the log-likelihood ratio (LRT) statisic for comparing the goodness of fit for two models. The bootstrap p-value can be obtained from the empirical distribution of LRT statistic by choosing \code{test = "boot"}.
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
#' Akaike, H. (1974) A new look at the statistical model identification. \emph{IEEE Transactions on Automatic Control}, \bold{19}, 716–723. \doi{10.1109/tac.1974.1100705}
#'
#' Schwarz, G. (1978) Estimating the dimensions of a model. \emph{The Annals of Statistics}, \bold{6}, 461–464. \doi{10.1214/aos/1176344136}
#'
#' Langeheine, R., Pannekoek, J., and van de Pol, F. (1996) Bootstrapping goodness-of-fit measures in categorical data analysis. \emph{Sociological Methods and Research}. \bold{24}. 492-516. \doi{10.1177/0049124196024004004}
#'
#' Ramaswamy, V., Desarbo, W., Reibstein, D., & Robinson, W. (1993). An Empirical Pooling Approach for Estimating Marketing Mix Elasticities with PIMS Data. Marketing Science, 12(1), 103-124. \doi{10.1287/mksc.12.1.103}
#'
#' @author Youngsun Kim
#'
#' @seealso \code{\link{glca}} \code{\link{gss12}}
#'
#' @examples
#' ## Example 1.
#' ## Model selection between two LCA models with different number of latent classes.
#' data(gss12)
#' class2 = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'               data = gss12, nclass = 2)
#' class3 = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'               data = gss12, nclass = 3)
#'
#' glca.gof(class2, class3)
#' \dontrun{glca.gof(class2, class3, test = "chisq")}
#' \dontrun{glca.gof(class2, class3, test = "boot")}
#'
#' ## Example 2.
#' ## Model selection between two MLCA models with different number of latent clusters.
#' cluster2 = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'                 group = REGION, data = gss12, nclass = 3, ncluster = 2, na.rm = TRUE)
#' cluster3 = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'                 group = REGION, data = gss12, nclass = 3, ncluster = 3, na.rm = TRUE)
#'
#' glca.gof(cluster2, cluster3)
#' \dontrun{glca.gof(cluster2, cluster3, test = "chisq")}
#' \dontrun{glca.gof(cluster2, cluster3, test = "boot")}
#'
#' \donttest{
#' ## Example 3.
#' ## MGLCA model selection under the measurement (invariance) assumption across groups.
#' measInv = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'                group = SEX, data = gss12, nclass = 3)
#' measVar = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'                group = SEX, data = gss12, nclass = 3, measure.inv = FALSE)
#'
#' glca.gof(measInv, measVar)
#' \dontrun{glca.gof(measInv, measVar, test = "chisq")}
#' \dontrun{glca.gof(measInv, measVar, test = "boot")}
#' }
#' @export

glca.gof <- function(
   object, object2 = NULL, test = NULL, nboot = 50,
   random.seed = NULL, maxiter = 500, eps = 1e-4, verbose = FALSE
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
   call1 <- object$call
   m1 <- object
   Gsq1 <- object$gof$Gsq
   Rel <- FALSE; nll <- FALSE
   if (!is.null(object2)) {
      call2 <- object2$call
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
      Rel <- FALSE; nll <- TRUE
      GsqR <- 2 * abs(m1$gof$loglik - m1$null$nullik)
   }

   # Bootstrap
   if (test == "boot" & nboot > 0) {
      bGsq0 <- numeric(nboot)
      bGsq1 <- numeric(nboot)
      bGsq2 <- numeric(nboot)
      bGsqR <- numeric(nboot)

      data0 = lapply(m1$datalist[1:3], function(x) list(Reduce(rbind, x)))
      init0 <- m1$null$param0
      init0_1 <- m1$null$param0
      init0_1$gamma = matrix(init0_1$gamma[[1]][1,], 1)
      init1 <- m1$param
      if (is.matrix(init1$gamma) && is.null(init1$delta))
         init1$gamma <- lapply(1:nrow(init1$gamma), function(g)
            matrix(init1$gamma[g, ], m1$model$Ng[g],
                   ncol(init1$gamma), byrow = TRUE))
      if (!is.null(init1$beta) && !is.null(init1$delta))
         init1$beta <- unlist(init1$beta)

      for (b in 1:nboot) {
         if (verbose) {
            if (b %% 10 == 0) cat(".")
            if (b %% 100 == 0) cat(" b =", b, "\n")
            if (b == nboot) cat("b =", b, "End\n")
         }

         b1 <- glca_gnr(m1$model, m1$param, m1$datalist)
         EMb1 <- glca_em(m1$model, b1, init1, 1, maxiter, eps,  FALSE)

         if (nll) {
            b0 <- glca_gnr(m1$null$model0, init0_1, data0)
            EMb0 <- glca_em(m1$null$model0, b0, init0, 1, maxiter, eps,  FALSE)
         }

         if (is.null(EMb1)) {
            bGsq1[b] <- NA
            if (nll) bGsqR[b] <- NA
         }
         else {
            bGsq1[b] <- 2 * (b1$loglik0 - EMb1$loglik)
            if (nll) {
               bGsq0[b] <- 2 * (b0$loglik0 - EMb0$loglik)
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
         "entropy" = round(c(m1$gof$entropy, m2$gof$entropy), 2),
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
         "entropy" = round(c(m1$null$entropy, m1$gof$entropy), 2),
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
         "entropy" = round(m1$gof$entropy, 2),
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

   dev.table <- NULL
   if (nll & test == "chisq") {
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
      Df[H1] <- m[[H1]]$model$npar - m[[H0]]$model$npar
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

   ret <- list()
   ret$type <- list(Rel = Rel, nll = nll, test = test)
   ret$model <- list()
   ret$model$model1 <- m1
   if (!is.null(object2)) ret$model$model2 <- m2
   ret$call <- list()
   ret$call$call1 <- call1
   if (!is.null(object2)) ret$call$call2 <- call2

   ret$criteria <- criteria
   ret$dev.table <- dev.table

   if (test == "boot" & nboot > 0) {
      ret$boot <- list()
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

   class(ret) <- "glca.gof"
   return(ret)
}
