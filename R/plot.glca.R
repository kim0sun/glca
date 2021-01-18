#' Plots the Estimated Parameters of Fitted glca Model
#'
#' \code{plot} method for class "\code{glca}".
#'
#' @param x an object of "\code{glca}", usually, a result of a call to \code{glca}.
#' @param ask a logical value whether to be asked before printing each plot.
#' @param ... further arguments passed to or from other methods.
#'
#' @return This function plots estimated parameters of model.
#'
#' @seealso \code{\link{glca}} \code{\link{gss08}} \code{\link{nyts18}}
#'
#' @examples
#' \dontrun{
#' # LCA
#' lca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'             data = gss08, nclass = 3, na.rm = TRUE)
#' plot(lca)
#'
#' # Multitple Group LCA (MGLCA)
#' mglca1 = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'              group = DEGREE, data = gss08, nclass = 3)
#' plot(mglca1)
#'
#' # Multitple Group LCA (MGLCA) (measure.inv = FALSE)
#' mglca2 = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'              group = DEGREE, data = gss08, nclass = 3, measure.inv = FALSE)
#' plot(mglca2)
#' plot(mglca2, "all")
#'
#' # Multilvel LCA (MLCA)
#' mlca = glca(item(ECIGT, ECIGAR, ESLT, EELCIGT, EHOOKAH) ~ 1,
#'             group = SCH_ID, data = nyts18, nclass = 3, ncluster = 3)
#' plot(mlca)
#' }
#'
#' @method plot glca
#' @import graphics grDevices
#' @export

plot.glca <- function(x, ask = TRUE, ...)
{
   oldpar <- par(no.readonly = TRUE)
   on.exit(par(oldpar))

   if (ask) grDevices::devAskNewPage(TRUE)

   model <- x$model
   param <- x$param
   post <- x$posterior

   par(mar = c(5.1, 4.1, 4.1, 5.1), mfrow = c(1, 1))

   # rho
   if (all(model$R == 2L)) { # binary plot
      if (model$measure.inv | model$G == 1L) {
         if (model$W > 1L) rho <- param$rho
         else rho <- param$rho[[1]]
         irp <- sapply(rho, function(i) i[,1L])
         grDevices::dev.hold()
         plot(x = 1L:model$M, y = c(0L, rep(1L, model$M - 1L)),
              xlim = c(0.8, model$M + 0.2), ylim = c(-0.1, 1.1),
              xlab = "Manifest Items", type = "n", xaxt = "n", yaxt = "n")
         axis(side=1, at = 1:model$M, labels = x$var.names$y.name)
         axis(side=2, at = (0:5)/5, las = "1")
         for (c in 1:model$C) {
            lines(1:model$M, irp[c,], type = "b", pch = c)
         }
         legend("topleft", pch = 1:model$C, inset = c(1,0),
                          legend = rownames(irp), xpd = TRUE, bg = "white")
         title("Item Response Probabilities by Class")
         grDevices::dev.flush()
      } else {
         for (g in 1:model$G) {
            irp <- sapply(param$rho[[g]], function(i) i[,1])
            grDevices::dev.hold()
            plot(x = 1:model$M, y = c(0, rep(1, model$M - 1)),
                 xlim = c(0.8, model$M + 0.2), ylim = c(-0.1, 1.1),
                 xlab = "Manifest Items", type = "n", xaxt = "n", yaxt = "n")
            axis(side=1, at = 1:model$M, labels = x$var.names$y.name)
            axis(side=2, at = (0:5)/5, las = 1)
            for (c in 1:model$C) {
               lines(1:model$M, irp[c,], type = "b", pch = c)
            }
            legend(x = model$M + 0.2, y = 1, pch = 1:model$C,
                   legend = rownames(irp), xpd = TRUE, bg = "white")
            title(paste0("Item Response Probabilities by Class", "\n(Group : ",
                         x$var.names$g.names[g],")"))
            grDevices::dev.flush()
         }
      }
   } else { # polytomous plot
      par(mar = c(2.5, 4.1, 4.1, 4.5))
      if (model$measure.inv | model$G == 1) {
         if (model$W > 1) rho <- param$rho
         else rho <- param$rho[[1]]
         for (m in 1:model$M) {
            grDevices::dev.hold()
            xpos <- barplot(t(rho[[m]]), beside = TRUE, ylim = c(0, 1))
            legend("topleft", inset = c(1, 0), legend = colnames(rho[[m]]),
                   fill = grDevices::gray.colors(ncol(rho[[m]])),
                   xpd = TRUE, bg = "white")
            title(paste0("Item Response Probabilities by Class", "\n(Item : ",
                         names(rho)[m],")"))
            grDevices::dev.flush()
         }
      } else {
         for (g in 1:model$G) {
            rho <- param$rho[[g]]
            for (m in 1:model$M) {
               grDevices::dev.hold()
               xpos <- barplot(t(rho[[m]]), beside = TRUE, ylim = c(0, 1))
               legend("topleft", inset = c(1, 0), legend = colnames(rho[[m]]),
                      fill = grDevices::gray.colors(ncol(rho[[m]])),
                      xpd = TRUE, bg = "white")
               title(paste0("Item Response Probabilities by Class", "\n(Item : ",
                            names(rho)[m]," / Group : ", x$var.names$g.names[g], ")"))
               grDevices::dev.flush()
            }
         }
      }
   }

   if (model$W > 1L) {
      # delta, gamma
      grDevices::dev.hold()
      par(mar = c(5.1, 4.1, 4.1, 2.1), mfrow = c(1, 1))
      barplot(colMeans(do.call(rbind, post$class)),
              main = "Marginal Class Prevalences", las = 1,
              col = grDevices::gray.colors(model$C))
      grDevices::dev.flush()

      prev <- apply(post$wclass, 1L, rev)
      colnames(prev) = paste0(colnames(prev), "\n(", round(param$delta, 2L), ")")

      grDevices::dev.hold()
      par(mar = c(5.1, 4.1, 4.1, 6.1), mfrow = c(1, 1))
      xpos <- barplot(prev, main = "Class Prevalences by Cluster", las = 1)
      legend("topleft", inset = c(1, 0), legend = rev(rownames(prev)),
             fill = rev(grDevices::gray.colors(nrow(prev))), xpd = TRUE, bg = "white")
      grDevices::dev.flush()
   } else {
      # gamma
      if (model$G == 1) {
         grDevices::dev.hold()
         par(mar = c(5.1, 4.1, 4.1, 2.1), mfrow = c(1, 1))
         barplot(colMeans(post[[1]]),
                 main = "Class Prevalences", las = 1,
                 col = grDevices::gray.colors(model$C))
         grDevices::dev.flush()
      } else {
         grDevices::dev.hold()
         par(mar = c(5.1, 4.1, 4.1, 2.1), mfrow = c(1, 1))
         barplot(colMeans(do.call(rbind, post)),
                 main = "Marginal Class Prevalences", las = 1,
                 col = grDevices::gray.colors(model$C))
         grDevices::dev.flush()

         prev <- t(apply(sapply(post, colMeans), 2, rev))
         rownames(prev) = paste0(rownames(prev), "\n(n=", model$Ng, ")")
         grDevices::dev.hold()
         par(mar = c(5.1, 4.1, 4.1, 6.1), mfrow = c(1, 1))
         xpos <- barplot(t(prev), main = "Class Prevalences by Group", las = 1)
         legend("topleft", inset = c(1, 0), legend = rev(colnames(prev)),
                fill = rev(grDevices::gray.colors(ncol(prev))),
                xpd = TRUE, bg = "white")
         grDevices::dev.flush()
      }
   }

   if (ask) grDevices::devAskNewPage(FALSE)
}

