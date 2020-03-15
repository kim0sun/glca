#' Summarize the Estimated Parameters of Fitted glca Model
#'
#' \code{summary} method for class "\code{glca}".
#'
#' @param x an object of "\code{glca}", usually, a result of a call to \code{glca}
#' @param group.name
#' @param ... further arguments passed to or from other methods
#'
#' @return This function prints decriptions of model and its more detailed estimated parameters but returns \code{NULL}.
#'
#' @seealso \code{\link{glca}}
#'
#' @examples
#' ## For examples see example(glca)
#'
#' @method plot glca
#' @export

plot.glca <- function(x, group.name = NULL, ...)
{
   devAskNewPage(TRUE)

   model <- x$model
   param <- x$param
   post <- x$posterior
   par(mar = c(5.1, 4.1, 4.1, 4.5), mfrow = c(1, 1))

   if (model$W > 1) {
      # delta, gamma
      prev <- post$wclass
      rownames(prev) = paste0(rownames(prev), "\n(", round(param$delta, 2), ")")

      xpos <- barplot(t(prev), main = "Class Prevalence by Group",
                      ylab = "Class Prevalence")
      legend(x = max(xpos), xjust = 0, y = 1, legend = colnames(prev),
             fill = gray.colors(ncol(prev)), xpd = TRUE)
   } else {
      # gamma
      if (model$G == 1) {
         barplot(param$gamma[1,], main = "Class Prevalence",
                 ylab = "Class Prevalence",
                 col = gray.colors(model$C), space = 0)
      } else {
         prev <- t(sapply(post, colMeans))

         xpos <- barplot(t(prev), main = "Class Prevalence by Group")
         legend(x = max(xpos), xjust = 0, y = 1, legend = colnames(prev),
                fill = gray.colors(ncol(prev)), xpd = TRUE)
      }
   }

   # rho
   if (all(model$R == 2)) { # binary plot
      if (model$measure.inv | model$G == 1) {
         if (model$W > 1) rho <- param$rho
         else rho <- param$rho[[1]]
         irp <- sapply(rho, function(i) i[,1])
         plot(x = 1:model$M, y = c(0, rep(1, model$M - 1)),
              xlim = c(0.5, model$M + 0.5), type = "n",
              xlab = "Manifest Items", ylab = "Item Repsponse Probabilities",
              xaxt = "n")
         axis(side=1, at = 1:model$M, labels = x$var.names$y.name)
         for (c in 1:model$C) {
            lines(1:model$M, irp[c,], type = "b", pch = c)
         }
         legend(x= model$M + 0.5, y = 1, pch = 1:3, legend = rownames(irp), xpd = TRUE)
         title("Item Response Probabilities by Class")
      } else {
         if (is.null(group.name))
            warning("Input group names to be plotted Item Response Probabilities.")
         if (group.name == "all")
            group.name <- x$var.names$g.names
         for (g in match(group.name, x$var.names$g.names)) {
            irp <- sapply(param$rho[[g]], function(i) i[,1])
            plot(x = 1:model$M, y = c(0, rep(1, model$M - 1)),
                 xlim = c(0.5, model$M + 0.5), type = "n",
                 xlab = "Manifest Items", ylab = "Item Repsponse Probabilities",
                 xaxt = "n")
            axis(side=1, at = 1:model$M, labels = x$var.names$y.name)
            for (c in 1:model$C) {
               lines(1:model$M, irp[c,], type = "b", pch = c)
            }
            legend(x= model$M + 0.5, y = 1, pch = 1:3, legend = rownames(irp), xpd = TRUE)
            title(paste0("Item Response Probabilities by Class", "\n(Group : ",
                         x$var.names$g.names[g],")"))
         }
      }
   } else { # polytomous plot
      par(mar = c(2.5, 4.1, 4.1, 4.5))
      if (model$measure.inv | model$G == 1) {
         if (model$W > 1) rho <- param$rho
         else rho <- param$rho[[1]]
         for (m in 1:model$M) {
            xpos <- barplot(t(rho[[m]]), beside = TRUE, ylim = c(0, 1))
            legend(x = max(xpos), xjust = 0, y = 1, legend = colnames(rho[[m]]),
                   fill = gray.colors(ncol(prev)), xpd = TRUE)
            title(paste0("Item Response Probabilities by Class", "\n(Item : ",
                         names(rho[m]),")"))
         }
      } else {
         if (is.null(group.name))
            warning("Input group names to be plotted Item Response Probabilities.")
         if (group.name == "all")
            group.name <- x$var.names$g.names
         for (g in match(group.name, x$var.names$g.names)) {
            rho <- param$rho[[g]]
            for (m in 1:model$M) {
               xpos <- barplot(t(rho[[m]]), beside = TRUE, ylim = c(0, 1))
               legend(x = max(xpos), xjust = 0, y = 1, legend = colnames(rho[[m]]),
                      fill = gray.colors(ncol(prev)), xpd = TRUE)
               title(paste0("Item Response Probabilities by Class", "\n(Item : ",
                            names(rho[m]),"/", x$var.names$g.names[g], ")"))
            }
         }
      }
   }
   devAskNewPage(FALSE)
}

