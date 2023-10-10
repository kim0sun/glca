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
   gof <- object$gof

   cat("\nCall:\n",  paste(deparse(object$call), sep = "\n", collapse = "\n"),
       "\n\n", sep = "")

   cat("Manifest items :", var.names$y.names, "\n")
   if (!is.null(object$call$group)) cat("Grouping variable :", object$call$group, "\n")
   if (model$P > 1L) cat("Covariates (Level 1) :", var.names$X.names, "\n")
   if (model$Q > 0L) cat("Covariates (Level 2) :", var.names$Z.names, "\n")

   cat("\nCategories for manifest items :\n")
   print(var.names$resp.name)

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
       "\n           AIC :", gof$AIC,
       "\n           BIC :", gof$BIC, "\n")

   if (model$W > 1L){
      cat("\nMarginal prevalences for latent classes :\n")
      print(round(colMeans(do.call(rbind, posterior$class)), 5L))
      cat("\nMarginal prevalences for latent clusters :\n")
      print(round(colMeans(posterior$cluster), 5L))
      cat("\nClass prevalences by cluster :\n")
      print(round(posterior$wclass, 5L))
      cat("\n")
   } else {
      cat("\nMarginal prevalences for latent classes :\n")
      print(round(colMeans(do.call(rbind, posterior)), 5L))
      if (model$G > 1) {
         if (model$G < 15L) {
            cat("\nClass prevalences by group :\n")
            prev = as.matrix(do.call(rbind, lapply(posterior, colMeans)))
            dimnames(prev) = list(var.names$g.names,
                                  paste0("Class ", 1L:model$C))
            print(round(prev, 5L))
            cat("\n")
         } else {
            cat("\nToo many groups to be printed.\n")
         }
      } else cat("\n")
   }

   if (model$W > 1L) {
      if (model$P > 1L | model$Q > 0L) {
         cat("\nLogistic regression coefficients (level 1) :\n")
         for (w in 1:model$W) {
            cat("Cluster", w, "\n")
            print(round(param$beta[[1L]][[w]], digits))
            cat("\n")
         }
         if (model$Q > 0L) {
            cat("Logistic regression coefficients (level 2) :\n")
            print(round(param$beta[[2L]], digits))
            cat("\n")
         }
         cat("\n")
      }

      if (all(model$R == 2L)) {
         cat ("Item-response probabilities (Y = 1) :\n")
         Rhomat <- sapply(1L:model$M, function(m)
            round(param$rho[[m]][,1L], digits))
         colnames(Rhomat) <- var.names$y.names
         print(Rhomat)
         cat ("\nItem-response probabilities (Y = 2) :\n")
         Rhomat <- sapply(1L:model$M, function(m)
            round(param$rho[[m]][,2L], digits))
         colnames(Rhomat) <- var.names$y.names
         print(Rhomat)
      } else {
         cat("Item-response probabilities :\n")
         for (m in 1L:model$M) {
            cat(var.names$y.names[m], "\n")
            print(round(param$rho[[m]], digits))
         }
      }
      cat("\n")
   } else {
      if (model$P > 1L) {
         cat("Logistic regression coefficients :\n")
         if (model$G > 1L) {
            for (g in 1L:model$G) {
               cat("Group :", var.names$g.names[g], "\n")
               print(round(param$beta[[g]], digits))
               cat("\n")
            }
         } else {
            print(round(param$beta[[1L]], digits))
         }
      }

      if (model$G == 1L) {
         if (all(model$R == 2L)) {
            cat ("Item-response probabilities (Y = 1) :\n")
            Rhomat <- sapply(1L:model$M, function(m)
               round(param$rho[[1L]][[m]][,1L], digits))
            colnames(Rhomat) <- var.names$y.names
            print(Rhomat)
            cat ("\nItem-response probabilities (Y = 2) :\n")
            Rhomat <- sapply(1L:model$M, function(m)
               round(param$rho[[1L]][[m]][,2L], digits))
            colnames(Rhomat) <- var.names$y.names
            print(Rhomat)
         } else {
            cat ("Item-response probabilities :\n")
            for (m in 1L:model$M) {
               cat(var.names$y.names[m], "\n")
               print(round(param$rho[[1L]][[m]], digits))
            }
         }
      } else {
         if (model$measure.inv) {
            if (all(model$R == 2)) {
               cat ("Item-response probabilities (Y = 1) :\n")
               Rhomat <- sapply(1L:model$M, function(m)
                  round(param$rho[[1L]][[m]][,1L], digits))
               colnames(Rhomat) <- var.names$y.names
               print(Rhomat)
               cat ("\nItem-response probabilities (Y = 2) :\n")
               Rhomat <- sapply(1L:model$M, function(m)
                  round(param$rho[[1L]][[m]][,2L], digits))
               colnames(Rhomat) <- var.names$y.names
               print(Rhomat)
            } else {
               cat("Item-response probabilities (invariant across groups) :\n")
               for (m in 1L:model$M) {
                  cat(var.names$y.names[m], "\n")
                  print(round(param$rho[[1L]][[m]], digits))
               }
            }
         } else {
            if (all(model$R == 2)) {
               cat ("Item-response probabilities (Y = 1) :\n")
               for (g in 1L:model$G) {
                  cat("Group :", var.names$g.names[[g]], "\n")
                  Rhomat <- sapply(1L:model$M, function(m)
                     round(param$rho[[g]][[m]][,1L], digits))
                  colnames(Rhomat) <- var.names$y.names
                  print(Rhomat)
                  cat("\n")
               }
               cat ("\nItem-response probabilities (Y = 2) :\n")
               for (g in 1L:model$G) {
                  cat("Group :", var.names$g.names[[g]], "\n")
                  Rhomat <- sapply(1L:model$M, function(m)
                     round(param$rho[[g]][[m]][,2L], digits))
                  colnames(Rhomat) <- var.names$y.names
                  print(Rhomat)
                  cat("\n")
               }
            } else {
               cat("Item-response probabilities (most likely response) :\n")
               for (g in 1L:model$G) {
                  cat("Group :", var.names$g.names[[g]], "\n")
                  print(sapply(param$rho[[g]], function(m)
                     apply(m, 1, which.max)))
               }
            }
         }
      }
   }
}
