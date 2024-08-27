#' Extracts \code{glca} Model Coefficients
#'
#' Extracts regression coefficients of \code{glca} model if the model includes covariates.
#'
#' @param object an object of "\code{glca}".
#' @param intercept a logical value for whether to print intercept".
#' @param digits number of significant digits to use when printing.
#' @param show.signif.stars logical. If TRUE, ‘significance stars’ are printed for each coefficient.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Coefficient matrix from the \code{glca} model
#'
#' If the model has calculated standard errors, coefficient matrix contains standard errors, t-statistic, and its p-value.
#'
#' @seealso \code{\link{glca}}
#'
#' @examples
#' ## For examples see example(glca)
#'
#' @method coef glca
#' @export

coef.glca = function(
   object, intercept = FALSE, digits = max(3, getOption("digits") - 3),
   show.signif.stars = getOption("show.signif.stars"), ...
)
{
   if (is.null(object$param$beta))
      return(NULL)
   else {
      # Coefficient design
      coef = list()
      if (object$model$W > 1) {
         coef$Level1 = list()
         for (w in 1L:object$model$W) {
            coef[[1L]][[w]] = list()
            for (c in 1L:(object$model$C - 1L)) {
               coef[[1L]][[w]][[c]] = data.frame(
                  exp(object$param$beta[[1L]][[w]][, c]),
                  b <- object$param$beta[[1L]][[w]][, c],
                  row.names = object$var.names$x.names
               )
               colnames(coef[[1L]][[w]][[c]]) =
                  c("Odds Ratio", "Coefficient")

               if (!is.null(object$convergence$score)) {
                  coef[[1L]][[w]][[c]] = cbind(
                     coef[[1L]][[w]][[c]],
                     se <- object$std.err$beta[[1L]][[w]][, c],
                     tval <- b / se,
                     2L * stats::pt(abs(tval), object$model$df, lower.tail = FALSE)
                  )
                  colnames(coef[[1L]][[w]][[c]]) =
                     c("Odds Ratio", "Coefficient", " Std. Error",
                       " t value", " Pr(>|t|)")
               }
            }
            names(coef[[1L]][[w]]) = paste0("Class", 1L:(object$model$C - 1L), "/", object$model$C)
         }
         names(coef[[1L]]) = paste0("Cluster", 1L:object$model$W)
         if (object$model$Q > 0L) {
            coef$Level2 = list()
            for (c in 1L:(object$model$C - 1L)) {
               coef[[2L]][[c]] <- data.frame(
                  exp(object$param$beta[[2L]][, c]),
                  b <- object$param$beta[[2L]][, c],
                  row.names = object$var.names$z.names
               )
               colnames(coef[[2L]][[c]]) <-
                  c("Odds Ratio", "Coefficient")

               if (!is.null(object$convergence$score)) {
                  coef[[2L]][[c]] <- cbind(
                     coef[[2L]][[c]],
                     se <- object$std.err$beta[[2L]][, c],
                     tval <- b / se,
                     2 * stats::pt(abs(tval), object$model$df, lower.tail = FALSE)
                  )
                  colnames(coef[[2L]][[c]]) <-
                     c("Odds Ratio", "Coefficient", " Std. Error",
                       " t value", " Pr(>|t|)")
               }
            }
            names(coef[[2L]]) <- paste0("Class", 1L:(object$model$C - 1L), "/", object$model$C)
         }

      } else {
         for (g in 1L:object$model$G) {
            coef[[g]] <- list()
            for (c in 1L:(object$model$C - 1L)) {
               coef[[g]][[c]] <- data.frame(
                  exp(object$param$beta[[g]][, c]),
                  b <- object$param$beta[[g]][, c],
                  row.names = object$var.names$x.names
               )
               colnames(coef[[g]][[c]]) <-
                  c("Odds Ratio", "Coefficient")

               if (!is.null(object$convergence$score)) {
                  coef[[g]][[c]] <- cbind(
                     coef[[g]][[c]],
                     se <- object$std.err$beta[[g]][, c],
                     tval <- b / se,
                     2 * stats::pt(abs(tval), object$model$df, lower.tail = FALSE)
                  )
                  colnames(coef[[g]][[c]]) <-
                     c("Odds Ratio", "Coefficient", " Std. Error",
                       " t value", " Pr(>|t|)")
               }
            }
            names(coef[[g]]) <- paste0("Class", 1L:(object$model$C - 1L), "/", object$model$C)
         }
         names(coef) <- object$var.names$g.names
      }
      if (object$model$coeff.inv) {
         if (object$model$W > 1) {
            if (intercept) {
               cat("Intercept :\n\n")
               intmat = lapply(1:object$model$W, function(w) {
                  mat = t(sapply(1:(object$model$C - 1), function(c) coef[[1]][[w]][[c]][1,]))
                  rownames(mat) = paste("Class", 1:(object$model$C - 1), "/", object$model$C)
                  as.data.frame(mat)
               })

               for (w in 1:object$model$W) {
                  cat("Cluster", w, ":\n")
                  stats::printCoefmat(intmat[[w]],
                                      digits = digits,
                                      signif.stars = getOption("show.signif.stars"),
                                      P.values = TRUE, has.Pvalue = TRUE)
                  cat("\n")
               }
            }

            if (object$model$P > 1) {
               cat("\nLevel 1 Coefficients :\n\n")
               for (c in 1:(object$model$C - 1)) {
                  cat("Class", c, "/", object$model$C, ":\n")
                  coefmat = coef[[1]][[1]][[c]][2:object$model$P,]
                  stats::printCoefmat(coefmat,
                                      digits = digits,
                                      signif.stars = getOption("show.signif.stars"),
                                      P.values = TRUE, has.Pvalue = TRUE)
                  cat("\n")
               }
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
               if (intercept) {
                  cat("Intercept :\n\n")
                  intmat = lapply(1:object$model$G, function(g) {
                     mat = t(sapply(1:(object$model$C - 1), function(c) coef[[g]][[c]][1,]))
                     rownames(mat) = paste("Class", 1:(object$model$C - 1), "/", object$model$C)
                     as.data.frame(mat)
                  })
                  for (g in 1:object$model$G) {
                     cat("Group ", object$var.names$g.names[g], ":\n", sep = "")
                     stats::printCoefmat(intmat[[g]],
                                         digits = digits,
                                         signif.stars = getOption("show.signif.stars"),
                                         P.values = TRUE, has.Pvalue = TRUE)
                     cat("\n")
                  }
               }

               cat("Coefficients :\n\n")
               for (c in 1:(object$model$C - 1)) {
                  cat("Class", c, "/", object$model$C, ":\n")
                  coefmat = coef[[1]][[c]][2:object$model$P,]
                  stats::printCoefmat(coefmat,
                                      digits = digits,
                                      signif.stars = getOption("show.signif.stars"),
                                      P.values = TRUE, has.Pvalue = TRUE)
                  cat("\n")
               }
            }
         }
      } else {
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
                  cat("Group ", object$var.names$g.names[g], ":\n", sep = "")

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
      }

      invisible(coef)
   }
}
