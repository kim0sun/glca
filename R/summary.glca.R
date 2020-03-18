#' Summarizes the Estimated Parameters of Fitted glca Model
#'
#' \code{summary} method for class "\code{glca}".
#'
#' @param object an object of "\code{glca}", usually, a result of a call to \code{glca}
#' @param digits the number of digits to be printed
#' @param ... further arguments passed to or from other methods
#'
#' @return This function prints decriptions of model and its more detailed estimated parameters but returns \code{NULL}.
#'
#' @seealso \code{\link{glca}}
#'
#' @examples
#' ## For examples see example(glca)
#'
#' @method summary glca
#' @export

summary.glca <- function(
   object, digits = max(3, getOption("digits") - 3), ...
)
{
   cat("\nCall:\n",  paste(deparse(object$call), sep = "\n", collapse = "\n"),
       "\n\n", sep = "")
   cat("Model :", object$model$type, "\n")
   if (object$model$W > 1){
      cat("Number of latent classes :", object$model$C, "\n")
      cat("Number of latent clusters :", object$model$W, "\n")
   } else {
      cat("Number of latent classes :", object$model$C, "\n")
   }
   cat("Number of parameters :", object$model$npar, "\n")
   if (object$model$G > 1)
      cat("Number of groups :", object$model$G, "\n")

   cat("\nlog-likelihood :", object$gof$loglik,
       "\n     G-squared :", object$gof$Gsq,
       "\n           AIC :", object$gof$aic,
       "\n           BIC :", object$gof$bic)

   cat("\n\nResponse numbering:\n")
   print(object$var.names$resp.name)

   cat("\n\nParameters :\n")
   param = object$param
   if (object$model$W > 1) {
      cat("Delta :\n")
      print(round(param$delta, digits))
      cat("\n")
      if (object$model$P > 1 | object$model$Q > 0) {
         cat("Beta (level 1) :\n")
         print(lapply(param$beta[[1]], round, digits))
         if (object$model$Q > 0) {
            cat("Beta (level 2) :\n")
            print(round(param$beta[[2]], digits))
         }

         cat("\n")
      } else {
         cat("Gamma :\n")
         print(round(param$gamma, digits))
         cat("\n")
      }
      cat("Rho :\n")

      for (m in 1:object$model$M) {
         cat(object$var.names$y.names[m], "\n")
         print(round(param$rho[[m]], digits))
      }
      cat("\n")
   } else {
      if (object$model$P > 1) {
         cat("Beta :\n")

         if (object$model$G > 1) {
            for (g in 1:object$model$G) {
               cat("Group :", object$var.names$g.names[g], "\n")
               print(round(param$beta[[g]], digits))
            }
         } else {
            print(round(param$beta[[1]], digits))
         }
         cat("\n")
      } else {
         cat("Gamma :\n")
         print(round(param$gamma, digits))
         cat("\n")
      }

      if (object$model$G == 1) {
         for (m in 1:object$model$M) {
            cat(object$var.names$y.names[m], "\n")
            print(round(param$rho[[1]][[m]], digits))
         }
      } else {
         if (object$model$measure.inv) {
            cat("Rho (invariant across groups) :\n")

            for (m in 1:object$model$M) {
               cat(object$var.names$y.names[m], "\n")
               print(round(param$rho[[1]][[m]], digits))
            }

            cat("\n")
         } else {
            Rhomat = lapply(param$rho, function(g)
               sapply(g, function(m) apply(m, 1, which.max)))
            cat("Rho (Most likely response) : \n")

            for (g in 1:object$model$G) {
               cat("Group :", object$var.names$g.names[[g]], "\n")
               print(sapply(param$rho[[g]], function(m)
                  apply(m, 1, which.max)))
            }
         }
      }
   }
}
