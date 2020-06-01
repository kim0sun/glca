#' @method print glca.gof
#' @export

print.glca.gof <- function(x, ...)
{
   nll <- x$type$nll; Rel <- x$type$Rel
   m1 <- x$model$model1; m2 <- x$model$model2
   call1 <- x$call$call1; call2 <- x$call$call2

   if (nll) {
      cat("NULL   :", paste(paste(call1$formula)[c(2,1)], collapse = " "), 1, "\n")
      cat("         nclass :", m1$model$C, "\n")
   }
   cat("Model 1:", paste(paste(call1$formula)[c(2,1,3)], collapse = " "), "\n")
   if (m1$model$G > 1) {
      cat("         group :", call1$group)
      cat(", nclass :", m1$model$C)
   } else
      cat("         nclass :", m1$model$C)
   if (m1$model$W > 1)
      cat(", ncluster :", m1$model$W)
   cat(", measure.inv :", m1$model$measure.inv, "\n")
   if (!is.null(m2)) {
      cat("Model 2:", paste(paste(call1$formula)[c(2,1,3)], collapse = " "), "\n")
      if (m2$model$G > 1) {
         cat("         group :", call2$group)
         cat(", nclass :", m2$model$C)
      } else
         cat("         nclass :", m2$model$C)
      if (m2$model$W > 1)
         cat(", ncluster :", m2$model$W)
      cat(", measure.inv :", m2$model$measure.inv, "\n")
   }

   cat("\nGoodness of Fit Table :\n")
   print(x$criteria)
   if (Rel | nll) {
      cat("\nAnalysis of Deviance Table :\n")
      print(x$dev.table)
   }
}
