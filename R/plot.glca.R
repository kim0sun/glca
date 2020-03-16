#' Plot the Estimated Parameters of Fitted glca Model
#'
#' \code{plot} method for class "\code{glca}".
#'
#' @param x an object of "\code{glca}", usually, a result of a call to \code{glca}
#' @param group.name a vector of strings which indicates groups, rho-parameters of which will be printed when \code{measure.inv = FALSE}.
#' @param ... further arguments passed to or from other methods
#'
#' @return This function plots estimated parameters of model.
#'
#' @seealso \code{\link{glca}}
#'
#' @examples
#' \dontrun{
#' # LCA
#' lca = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABPOOR, ABSINGLE, ABNOMORE) ~ 1,
#'             data = gss, nclass = 3, na.rm = TRUE)
#' plot(lca)
#'
#' # Multitple Group LCA (MGLCA)
#' mglca1 = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABPOOR, ABSINGLE, ABNOMORE) ~ 1,
#'              group = DEGREE, data = gss, nclass = 3)
#' plot(mglca1)
#'
#' # Multitple Group LCA (MGLCA) (measure.inv = FALSE)
#' mglca2 = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABPOOR, ABSINGLE, ABNOMORE) ~ 1,
#'              group = DEGREE, data = gss, nclass = 3, measure.inv = FALSE)
#' plot(mglca2)
#'
#' # Multilvel LCA (MLCA)
#' mlca = glca(item(ABDEFECT, ABHLTH, ABRAPE, ABPOOR, ABSINGLE, ABNOMORE) ~ 1,
#'              group = REGION, data = gss, nclass = 3, ncluster = 3)
#' plot(mlca)
#' }
#'
#' @method plot glca
#' @export

plot.glca <- function(x, group.name = NULL, ...)
{
   grDevices::devAskNewPage(TRUE)

   model <- x$model
   param <- x$param
   post <- x$posterior
   graphics::par(mar = c(5.1, 4.1, 4.1, 4.5), mfrow = c(1, 1))

   if (model$W > 1) {
      # delta, gamma
      prev <- post$wclass
      rownames(prev) = paste0(rownames(prev), "\n(", round(param$delta, 2), ")")

      xpos <- graphics::barplot(t(prev), main = "Class Prevalence by Group",
                      ylab = "Class Prevalence")
      graphics::legend(x = max(xpos), xjust = 0, y = 1, legend = colnames(prev),
             fill = grDevices::gray.colors(ncol(prev)), xpd = TRUE)
   } else {
      # gamma
      if (model$G == 1) {
         graphics::barplot(param$gamma[1,], main = "Class Prevalence",
                 ylab = "Class Prevalence",
                 col = grDevices::gray.colors(model$C), space = 0)
      } else {
         prev <- t(sapply(post, colMeans))

         xpos <- graphics::barplot(t(prev), main = "Class Prevalence by Group")
         graphics::legend(x = max(xpos), xjust = 0, y = 1, legend = colnames(prev),
                fill = grDevices::gray.colors(ncol(prev)), xpd = TRUE)
      }
   }

   # rho
   if (all(model$R == 2)) { # binary plot
      if (model$measure.inv | model$G == 1) {
         if (model$W > 1) rho <- param$rho
         else rho <- param$rho[[1]]
         irp <- sapply(rho, function(i) i[,1])
         graphics::plot(x = 1:model$M, y = c(0, rep(1, model$M - 1)),
              xlim = c(0.5, model$M + 0.5), type = "n",
              xlab = "Manifest Items", ylab = "Item Repsponse Probabilities",
              xaxt = "n")
         graphics::axis(side=1, at = 1:model$M, labels = x$var.names$y.name)
         for (c in 1:model$C) {
            graphics::lines(1:model$M, irp[c,], type = "b", pch = c)
         }
         graphics::legend(x = model$M + 0.5, y = 1, pch = 1:3, legend = rownames(irp), xpd = TRUE)
         graphics::title("Item Response Probabilities by Class")
      } else {
         if (is.null(group.name))
            warning("Input group names to be plotted Item Response Probabilities.")
         if (group.name == "all")
            group.name <- x$var.names$g.names
         for (g in match(group.name, x$var.names$g.names)) {
            irp <- sapply(param$rho[[g]], function(i) i[,1])
            graphics::plot(x = 1:model$M, y = c(0, rep(1, model$M - 1)),
                 xlim = c(0.5, model$M + 0.5), type = "n",
                 xlab = "Manifest Items", ylab = "Item Repsponse Probabilities",
                 xaxt = "n")
            graphics::axis(side=1, at = 1:model$M, labels = x$var.names$y.name)
            for (c in 1:model$C) {
               graphics::lines(1:model$M, irp[c,], type = "b", pch = c)
            }
            graphics::legend(x = model$M + 0.5, y = 1, pch = 1:3, legend = rownames(irp), xpd = TRUE)
            graphics::title(paste0("Item Response Probabilities by Class", "\n(Group : ",
                         x$var.names$g.names[g],")"))
         }
      }
   } else { # polytomous plot
      graphics::par(mar = c(2.5, 4.1, 4.1, 4.5))
      if (model$measure.inv | model$G == 1) {
         if (model$W > 1) rho <- param$rho
         else rho <- param$rho[[1]]
         for (m in 1:model$M) {
            xpos <- graphics::barplot(t(rho[[m]]), beside = TRUE, ylim = c(0, 1))
            graphics::legend(x = max(xpos), xjust = 0, y = 1, legend = colnames(rho[[m]]),
                   fill = grDevices::gray.colors(ncol(prev)), xpd = TRUE)
            graphics::title(paste0("Item Response Probabilities by Class", "\n(Item : ",
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
               xpos <- graphics::barplot(t(rho[[m]]), beside = TRUE, ylim = c(0, 1))
               graphics::legend(x = max(xpos), xjust = 0, y = 1, legend = colnames(rho[[m]]),
                      fill = grDevices::gray.colors(ncol(prev)), xpd = TRUE)
               graphics::title(paste0("Item Response Probabilities by Class", "\n(Item : ",
                            names(rho[m])," / Group : ", x$var.names$g.names[g], ")"))
            }
         }
      }
   }
   grDevices::devAskNewPage(FALSE)
}

