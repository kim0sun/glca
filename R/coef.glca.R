coef.glca = function(
   object, digits = max(3, getOption("digits") - 3),
   show.signif.stars = getOption("show.signif.stars")
)
{
   if (is.null(object$param$beta))
      return(NULL)
   else {
      coef = object$coefficient
      if (object$model$W > 1) {
         cat("Level 1 Coefficients :\n\n")
         for (w in 1:object$model$W) {
            cat("Cluster", w, ":\n")

            for (c in 1:(object$model$C - 1)) {
               cat("Class", c, "/", object$model$C, ":\n")
               if (is.null(object$convergence$score))
                  print(coef[[1]][[w]][[c]])
               else
                  printCoefmat(coef[[1]][[w]][[c]],
                               digits = digits,
                               signif.stars = getOption("show.signif.stars"),
                               P.values = TRUE, has.Pvalue = TRUE)
               cat("\n")
            }
            cat("\n")
         }
         if (object$model$Q > 0) {
            cat("\nLevel 2 Coefficients :\n\n")
            for (c in 1:(object$model$C - 1)) {
               cat("Class", c, "/", object$model$C, ":\n")
               if (is.null(object$convergence$score))
                  print(coef[[2]][[c]])
               else
                  printCoefmat(coef[[2]][[c]],
                               digits = digits,
                               signif.stars = getOption("show.signif.stars"),
                               P.values = TRUE, has.Pvalue = TRUE)
               cat("\n")
            }
         }
      } else {
         if (object$model$G == 1) {
            for (c in 1:(object$model$C - 1)) {
               cat("Class", c, "/", object$model$C, ":\n")
               if (is.null(object$convergence$score))
                  print(coef[[1]][[c]])
               else
                  printCoefmat(coef[[1]][[c]],
                               digits = digits,
                               signif.stars = getOption("show.signif.stars"),
                               P.values = TRUE, has.Pvalue = TRUE)
               cat("\n")
            }
         } else {
            for (g in 1:object$model$G) {
               cat("Group ", object$var.names$g.names[[g]], ":\n", sep = "")

               for (c in 1:(object$model$C - 1)) {
                  cat("Class", c, "/", object$model$C, ":\n")
                  if (is.null(object$convergence$score))
                     print(coef[[g]][[c]])
                  else
                     printCoefmat(coef[[g]][[c]],
                                  digits = digits,
                                  signif.stars = getOption("show.signif.stars"),
                                  P.values = TRUE, has.Pvalue = TRUE)
                  cat("\n")
               }
               cat("\n")
            }
         }
      }
      invisible(coef)
   }
}
