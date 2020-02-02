#' @method print glca
#' @export

print.glca = function(x, ...)
{
   cat("\nCall:\n",  paste(deparse(x$call), sep = "\n", collapse = "\n"),
       "\n\n", sep = "")
   cat("Model :", x$model$type, "\n")
   if (x$model$W > 1){
      cat("Number of latent classes :", x$model$C, "\n")
      cat("Number of latent clusters :", x$model$W, "\n")
      cat("\nMean Prevalence for latent clusters:\n")
      print(round(colMeans(x$posterior$cluster), 5))
      cat("\nMean Prevalence for latent classes:\n")
      print(round(x$posterior$wclass, 5))
      cat("\n")
   } else {
      cat("Number of latent classes :", x$model$C, "\n")
      cat("\nMean Prevalence for latent classes:\n")
      prev = as.matrix(do.call(rbind, lapply(x$posterior, colMeans)))
      dimnames(prev) = list(x$var.names$g.names,
                            paste0("Class ", 1:x$model$C))
      print(round(prev, 5))
      cat("\n")
   }
   cat("Number of parameters :", x$model$npar, "\n")
   if (x$model$G > 1)
      cat("Number of groups :", x$model$G, "\n")

   cat("\nlog-likelihood :", x$gof$loglik,
       "\n     G-squared :", x$gof$Gsq,
       "\n           AIC :", x$gof$aic,
       "\n           BIC :", x$gof$bic, "\n\n")
}
