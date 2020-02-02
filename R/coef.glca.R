#' Extract \code{glca} Model Coefficients.
#'
#' Basically, show AIC and BIC for model fit criteria. When \code{nboot} given, using parametric bootstrap, compute bootstrap p-value for "relative" and "absolute" model fit.
#'
#' @param object an object of "\code{glca}", usually, a result of a call to \code{glca}.
#' @param digits the number of significant digits to use when printing.
#' @param show.signif.stars logical. If TRUE, ‘significance stars’ are printed for each coefficient.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Coefficient matrix from the \code{glca} model.
#'
#' If the model has been calculated standard error, coefficient matrix contains standard error and t, p-value.
#'
#' @seealso \code{\link{glca}}
#'
#' @examples
#' ## For examples see example(glca)
#'
#' @method coef glca
#' @export

coef.glca = function(
   object, digits = max(3, getOption("digits") - 3),
   show.signif.stars = getOption("show.signif.stars"), ...
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
                  stats::printCoefmat(coef[[1]][[w]][[c]],
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
                  stats::printCoefmat(coef[[2]][[c]],
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
                  stats::printCoefmat(coef[[1]][[c]],
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
                     stats::printCoefmat(coef[[g]][[c]],
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
