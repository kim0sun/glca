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
   model <- object$model
   param <- object$param
   posterior <- object$posterior
   var.names <- object$var.names

   cat("\nCall:\n",  paste(deparse(object$call), sep = "\n", collapse = "\n"),
       "\n\n", sep = "")
   cat("Model :", model$type, "\n")
   if (model$W > 1){
      cat("Number of latent classes :", model$C, "\n")
      cat("Number of latent clusters :", model$W, "\n")
      cat("\nMean Prevalence for latent clusters:\n")
      print(round(colMeans(posterior$cluster), 5))
      cat("\nMean Prevalence for latent classes:\n")
      print(round(posterior$wclass, 5))
      cat("\n")
   } else {
      cat("Number of latent classes :", model$C, "\n")
      if (model$G < 10) {
         cat("\nMean Prevalence for latent classes for each group:\n")
         prev = as.matrix(do.call(rbind, lapply(posterior, colMeans)))
         dimnames(prev) = list(var.names$g.names,
                               paste0("Class ", 1:model$C))
         print(round(prev, 5))
      } else {
         cat("\nMean Prevalence for latent classes:\n")
         print(round(colMeans(do.call(rbind, posterior)), 5))
      }
   }
   cat("\nNumber of parameters :", model$npar, "\n")
   if (model$G > 1)
      cat("Number of groups :", model$G, "\n")

   cat("\nlog-likelihood :", object$gof$loglik,
       "\n     G-squared :", object$gof$Gsq,
       "\n           AIC :", object$gof$aic,
       "\n           BIC :", object$gof$bic)

   cat("\n\nResponse numbering:\n")
   print(var.names$resp.name)

   cat("\nEstimated model parameters :\n")
   if (model$W > 1) {
      cat("Delta :\n")
      print(round(param$delta, digits))
      cat("\n")

      if (model$P > 1 | model$Q > 0) {
         cat("Beta (level 1) :\n")
         print(lapply(param$beta[[1]], round, digits))
         if (model$Q > 0) {
            cat("Beta (level 2) :\n")
            print(round(param$beta[[2]], digits))
         }
         cat("\n")
      } else {
         cat("Gamma :\n")
         print(round(param$gamma, digits))
         cat("\n")
      }

      if (all(model$R == 2)) {
         cat ("Rho (Y = 1) :\n")
         Rhomat <- sapply(1:model$M, function(m)
            round(param$rho[[m]][,1], digits))
         colnames(Rhomat) <- var.names$y.names
         print(Rhomat)
      } else {
         cat("Rho :\n")
         for (m in 1:model$M) {
            cat(var.names$y.names[m], "\n")
            print(round(param$rho[[m]], digits))
         }
      }

      cat("\n")
   } else {
      if (model$P > 1) {
         cat("Beta :\n")
         if (model$G > 1) {
            if (model$coeff.inv) {
               cat("Intercepts :\n")
               int = do.call(rbind, lapply(param$beta, function(x) x[1,]))
               rownames(int) = paste0("Group :", var.names$g.names)
               print(round(int, digits))
               cat("\nCoefficients :\n")
               print(round(param$beta[[1]][2:model$P,], digits))
            } else {
               for (g in 1:model$G) {
                  cat("Group :", var.names$g.names[g], "\n")
                  print(round(param$beta[[g]], digits))
               }
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

      if (model$G == 1) {
         if (all(model$R == 2)) {
            cat ("Rho (Y = 1) :\n")
            Rhomat <- sapply(1:model$M, function(m)
               round(param$rho[[1]][[m]][,1], digits))
            colnames(Rhomat) <- var.names$y.names
            print(Rhomat)
         } else {
            cat ("Rho :\n")
            for (m in 1:model$M) {
               cat(var.names$y.names[m], "\n")
               print(round(param$rho[[1]][[m]], digits))
            }
         }
      } else {
         if (model$measure.inv) {
            if (all(model$R == 2)) {
               cat ("Rho (Y = 1) :\n")
               Rhomat <- sapply(1:model$M, function(m)
                  round(param$rho[[1]][[m]][,1], digits))
               colnames(Rhomat) <- var.names$y.names
               print(Rhomat)
            } else {
               cat("Rho (invariant across groups) :\n")
               for (m in 1:model$M) {
                  cat(var.names$y.names[m], "\n")
                  print(round(param$rho[[1]][[m]], digits))
               }
            }
         } else {
            if (all(model$R == 2)) {
               cat ("Rho (Y = 1) :\n")
               for (g in 1:model$G) {
                  cat("Group :", var.names$g.names[[g]], "\n")
                  Rhomat <- sapply(1:model$M, function(m)
                     round(param$rho[[g]][[m]][,1], digits))
                  colnames(Rhomat) <- var.names$y.names
                  print(Rhomat)
                  cat("\n")
               }
            } else {
               cat("Rho (Most likely response) :\n")
               for (g in 1:model$G) {
                  cat("Group :", var.names$g.names[[g]], "\n")
                  print(sapply(param$rho[[g]], function(m)
                     apply(m, 1, which.max)))
               }
            }
         }
      }
   }
}
