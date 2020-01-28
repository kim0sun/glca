print.glca = function(object)
{
   cat("\nCall:\n", deparse(object$call), "\n\n", sep = "")
   cat("Model :", object$model$type, "\n")
   if (object$model$W > 1){
      cat("Number of latent classes :", object$model$C, "\n")
      cat("Number of latent clusters :", object$model$W, "\n")
      cat("\nMean Prevalence for latent clusters:\n")
      print(round(colMeans(object$posterior$cluster), 5))
      cat("\nMean Prevalence for latent classes:\n")
      print(round(object$posterior$wclass, 5))
      cat("\n")
   } else {
      cat("Number of latent classes :", object$model$C, "\n")
      cat("\nMean Prevalence for latent classes:\n")
      prev = as.matrix(do.call(rbind, lapply(object$posterior, colMeans)))
      dimnames(prev) = list(object$var.names$g.names,
                            paste0("Class ", 1:object$model$C))
      print(round(prev, 5))
      cat("\n")
   }
   cat("Number of parameters :", object$model$npar, "\n")
   if (object$model$G > 1)
      cat("Number of groups :", object$model$G, "\n")

   cat("\nlog-likelihood :", object$gof$loglik,
       "\n     G-squared :", object$gof$Gsq,
       "\n           AIC :", object$gof$aic,
       "\n           BIC :", object$gof$bic, "\n\n")
}
