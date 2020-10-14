#' @method print glca
#' @export

print.glca = function(x, ...)
{
   model <- x$model
   param <- x$param
   posterior <- x$posterior
   var.names <- x$var.names
   gof <- x$gof

   cat("\nCall:\n",  paste(deparse(x$call), sep = "\n", collapse = "\n"),
       "\n\n", sep = "")

   cat("Manifest items :\n", var.names$y.names, "\n")
   if (!is.null(x$call$group)) cat("Grouping variable :", x$call$group, "\n")
   if (model$W > 0L) {
      if (model$Q > 0L) cat("Covariates (Level 2) : \n", var.names$Z.names, "\n")
      if (model$P > 1L) cat("Covariates (Level 1) : \n", var.names$X.names, "\n")
   } else if (model$P > 1L) cat("Covariates : \n", var.names$X.names, "\n")

   cat("\nModel :", model$type, "\n")
   if (model$W > 1L){
      cat("Number of latent classes :", model$C, "\n")
      cat("Number of latent clusters :", model$W, "\n")
      cat("\nMean prevalence for latent clusters:\n")
      print(round(colMeans(posterior$cluster), 5L))
      cat("\nMean prevalence for latent classes:\n")
      print(round(posterior$wclass, 5L))
      cat("\n")
   } else {
      cat("Number of latent classes :", model$C, "\n")
      cat("\nMean prevalence for latent classes:\n")
      prev = as.matrix(do.call(rbind, lapply(posterior, colMeans)))
      dimnames(prev) = list(var.names$g.names,
                            paste0("Class ", 1L:model$C))
      print(round(prev, 5L))
      cat("\n")
   }

   cat("Number of observations :", model$N, "\n")
   cat("  Number of parameters :", model$npar, "\n")
   if (model$G > 1L)
      cat("      Number of groups :", model$G, "\n")

   cat("\nlog-likelihood :", gof$loglik,
       "\n     G-squared :", gof$Gsq,
       "\n           AIC :", gof$aic,
       "\n           BIC :", gof$bic, "\n\n")
}
