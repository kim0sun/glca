anova.glca = function(
   object, object2 = NULL, nboot = 0,
   maxiter = 500, eps = 1e-5, verbose = TRUE
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
         }

         b1 = glca_gnr(m1$model, m1$param, m1$datalist)
         init1 = glca_init(m1$model)
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
      ret <- data.frame(
         "Res.Df" = m1$gof$df,
         "AIC" = round(m1$gof$aic),
         "BIC" = round(m1$gof$bic)
      )
      if (nboot > 0)
         ret <- cbind(ret, "Boot Pr(Abs)" = round(boot1, 3))
   } else {
      if (Rel) {
         cat("\nModel Goodness of Fit Table\n\n")
         ret <- data.frame(
            "Res.Df" = c(m1$gof$df, m2$gof$df),
            "AIC" = round(c(m1$gof$aic, m2$gof$aic)),
            "BIC" = round(c(m1$gof$bic, m2$gof$bic))
         )
         if (nboot > 0) {
            Df <- bootrel <- c("", "")
            Df[H1] <- m[[H0]]$gof$df - m[[H1]]$gof$df
            bootrel[H1] <- round(boot3, 3)
            ret <- cbind(ret, "Boot Pr(Abs)" = round(c(boot1, boot2), 3),
                         "Df" = Df, "Boot Pr(Rel)" = bootrel)
         }

      } else {
         warning("Response are different.")
         cat("\nModel Goodness of Fit Table\n\n")
         ret <- data.frame(
            "Res.Df" = c(m1$gof$df, m2$gof$df),
            "AIC" = round(c(m1$gof$aic, m2$gof$aic)),
            "BIC" = round(c(m1$gof$bic, m2$gof$bic))
         )
         if (nboot > 0)
            ret <- cbind(ret, "Boot Pr(Abs)" = round(c(boot1, boot2), 3))
      }
   }

   cat("Model 1:", deparse(eval(model1)), "\n")
   cat("         nclass :", m1$model$C)
   if (m1$model$W > 1)
       cat(", ncluster :", m1$model$W, "\n")
   else cat("\n")
   if (!is.null(object2)) {
      cat("Model 2:", deparse(eval(model2)), "\n")
      cat("         nclass :", m2$model$C)
      if (m2$model$W > 1)
         cat(", ncluster :", m2$model$W, "\n")
      else cat("\n")
   }

   return(ret)
}
