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

   cat("Manifest items :", var.names$y.names, "\n")
   if (!is.null(x$call$group)) cat("Grouping variable :", x$call$group, "\n")
   if (model$P > 1L) cat("Covariates (Level 1) :", var.names$X.names, "\n")
   if (model$Q > 0L) cat("Covariates (Level 2) :", var.names$Z.names, "\n")

   cat("\nModel :", model$type, "\n\n")
   if (model$W > 1L){
      cat("Number of latent classes :", model$C, "\n")
      cat("Number of latent clusters :", model$W, "\n")
   } else {
      cat("Number of latent classes :", model$C, "\n")
   }
   if (model$G > 1)
      cat("Number of groups :", model$G, "\n")
   cat("Number of observations :", model$N, "\n")
   cat("Number of parameters :", model$npar, "\n")

   cat("\nlog-likelihood :", gof$loglik,
       "\n     G-squared :", gof$Gsq,
       "\n           AIC :", gof$aic,
       "\n           BIC :", gof$bic, "\n")
}
