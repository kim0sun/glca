#' Plots the Estimated Parameters of Fitted glca Model
#'
#' \code{plot} method for class "\code{glca}".
#'
#' @param x an object of "\code{glca}", usually, a result of a call to \code{glca}
#' @param group.name a vector of strings which indicates groups, rho-parameters of which will be printed when \code{measure.inv = FALSE}.
#' @param ... further arguments passed to or from other methods
#'
#' @return This function plots estimated parameters of model.
#'
#' @seealso \code{\link{glca}} \code{\link{gss12}}
#'
#' @examples
#' \dontrun{
#' # LCA
#' lca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'             data = gss12, nclass = 3, na.rm = TRUE)
#' plot(lca)
#'
#' # Multitple Group LCA (MGLCA)
#' mglca1 = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'              group = DEGREE, data = gss12, nclass = 3)
#' plot(mglca1)
#'
#' # Multitple Group LCA (MGLCA) (measure.inv = FALSE)
#' mglca2 = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'              group = DEGREE, data = gss12, nclass = 3, measure.inv = FALSE)
#' plot(mglca2)
#' plot(mglca2, "all")
#'
#' # Multilvel LCA (MLCA)
#' mlca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'              group = REGION, data = gss12, nclass = 3, ncluster = 2)
#' plot(mlca)
#' }
#'
#' @method plot glca
#' @export

plot.glca <- function(x, group.name = "all", ...)
{
   oldpar <- par(no.readonly = TRUE)
   on.exit(par(oldpar))
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
                                ylab = "Class Prevalence", las = 1)
      graphics::legend(x = max(xpos), xjust = 0, y = 1, legend = colnames(prev),
                       fill = grDevices::gray.colors(ncol(prev)), xpd = TRUE, bg = "white")
   } else {
      # gamma
      if (model$G == 1) {
         graphics::barplot(param$gamma[1,], main = "Class Prevalence",
                           ylab = "Class Prevalence", las = 1,
                           col = grDevices::gray.colors(model$C))
      } else {
         prev <- t(sapply(post, colMeans))

         xpos <- graphics::barplot(t(prev), main = "Class Prevalence by Group", las = 1)
         graphics::legend(x = max(xpos), xjust = 0, y = 1, legend = colnames(prev),
                          fill = grDevices::gray.colors(ncol(prev)), xpd = TRUE, bg = "white")
      }
   }

   # rho
   if (all(model$R == 2)) { # binary plot
      if (model$measure.inv | model$G == 1) {
         if (model$W > 1) rho <- param$rho
         else rho <- param$rho[[1]]
         irp <- sapply(rho, function(i) i[,1])
         graphics::plot(x = 1:model$M, y = c(0, rep(1, model$M - 1)),
              xlim = c(0.8, model$M + 0.2), ylim = c(-0.1, 1.1),
              xlab = "Manifest Items", ylab = "Item Repsponse Probabilities",
              type = "n", xaxt = "n", yaxt = "n")
         graphics::axis(side=1, at = 1:model$M, labels = x$var.names$y.name)
         graphics::axis(side=2, at = (0:5)/5, las = "1")
         for (c in 1:model$C) {
            graphics::lines(1:model$M, irp[c,], type = "b", pch = c)
         }
         graphics::legend(x = model$M + 0.2, y = 1, pch = 1:model$C,
                          legend = rownames(irp), xpd = TRUE, bg = "white")
         graphics::title("Item Response Probabilities by Class")
      } else {
         if (is.null(group.name))
            stop("Input group names to be plotted Item Response Probabilities.")
         else if (group.name == "all")
            group.name <- x$var.names$g.names
         for (g in match(group.name, x$var.names$g.names)) {
            irp <- sapply(param$rho[[g]], function(i) i[,1])
            graphics::plot(x = 1:model$M, y = c(0, rep(1, model$M - 1)),
                 xlim = c(0.8, model$M + 0.2), ylim = c(-0.1, 1.1),
                 xlab = "Manifest Items", ylab = "Item Repsponse Probabilities",
                 type = "n", xaxt = "n", yaxt = "n")
            graphics::axis(side=1, at = 1:model$M, labels = x$var.names$y.name)
            graphics::axis(side=2, at = (0:5)/5, las = 1)
            for (c in 1:model$C) {
               graphics::lines(1:model$M, irp[c,], type = "b", pch = c)
            }
            graphics::legend(x = model$M + 0.2, y = 1, pch = 1:model$C,
                             legend = rownames(irp), xpd = TRUE, bg = "white")
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
                             fill = grDevices::gray.colors(ncol(rho[[m]])),
                             xpd = TRUE, bg = "white")
            graphics::title(paste0("Item Response Probabilities by Class", "\n(Item : ",
                         names(rho)[m],")"))
         }
      } else {
         if (is.null(group.name))
            stop("Input group names to be plotted Item Response Probabilities.")
         else if (group.name == "all")
            group.name <- x$var.names$g.names
         for (g in match(group.name, x$var.names$g.names)) {
            rho <- param$rho[[g]]
            for (m in 1:model$M) {
               xpos <- graphics::barplot(t(rho[[m]]), beside = TRUE, ylim = c(0, 1))
               graphics::legend(x = max(xpos), xjust = 0, y = 1, legend = colnames(rho[[m]]),
                                fill = grDevices::gray.colors(ncol(rho[[m]])),
                                xpd = TRUE, bg = "white")
               graphics::title(paste0("Item Response Probabilities by Class", "\n(Item : ",
                                      names(rho)[m]," / Group : ", x$var.names$g.names[g], ")"))
            }
         }
      }
   }
   grDevices::devAskNewPage(FALSE)
}

