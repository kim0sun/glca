#' Reorders the estimated parameters of glca model
#'
#' Function for reordering the estimated parameters for \code{glca} model.
#'
#' @param x an object of "\code{glca}", usually, a result of a call to \code{glca}.
#' @param class.order a integer vector of length equal to number of latent classes of the glca model, assigning the desired order of the latent classes
#' @param cluster.order a integer vector of length equal to number of latent clusters of the glca model, assigning the desired order of the latent clusters
#' @param decreasing logical, when the \code{class.order} or \code{cluster.order} are not given, whether to reordering the estimates by decreasing order of responding first-category probability for first manifest item.
#'
#' @author Youngsun Kim
#'
#' @details  Since the latent classes or clusters can be switched, the order of estimated parameters can be arbitrary according to the initial value of EM algorithm.
#'
#' @examples
#' lca = glca(item(DEFECT, HLTH, RAPE, POOR, SINGLE, NOMORE) ~ 1,
#'             data = gss08, nclass = 3, na.rm = TRUE)
#' plot(lca)
#'
#' # Given ordering number
#' lca321 = reorder(lca, 3:1)
#' plot(lca321)
#'
#' # Descending order
#' dec_lca = reorder(lca, decreasing = TRUE)
#' plot(dec_lca)
#'
#' # Ascending order
#' inc_lca = reorder(lca, decreasing = FALSE)
#' plot(inc_lca)
#'
#' @method reorder glca
#' @export

reorder.glca <- function(x, class.order = NULL, cluster.order = NULL, decreasing = TRUE, ...)
{
   if (!is.null(class.order)) {
      if (!setequal(as.numeric(class.order), 1:x$model$C))
         stop("class.order is not appropriate.")
      norder = as.numeric(class.order)
   }
   decreasing <- as.logical(decreasing)

   if (x$model$W > 0) {
      if (!is.null(cluster.order)) {
         if (!setequal(as.numeric(cluster.order), x$model$W))
            stop("cluster.order is not appropriate.")
         gorder = as.numeric(cluster.order)
      }

      if (is.null(class.order))
         norder = order(x$param$rho[[1]][,1], decreasing = decreasing)

      for (m in 1:x$model$M) {
         x$param$rho[[m]][] = x$param$rho[[m]][norder,]
         x$std.err$rho[[m]][] = x$std.err$rho[[m]][norder,]
      }

      x$posterior$wclass[] = x$posterior$wclass[, norder]

      if (is.null(cluster.order))
         gorder = order(x$posterior$wclass[,1], decreasing = decreasing)

      x$param$delta[] = x$param$delta[gorder]
      x$std.err$delta[] = x$std.err$delta[gorder]

      x$posterior$wclass[] = x$posterior$wclass[gorder, ]
      x$posterior$cluster[] = x$posterior$cluster[, gorder]

      for (g in 1:x$model$G) {
         x$posterior$class[[g]][] = x$posterior$class[[g]][, norder]
      }

      if (x$model$P > 1 | x$model$Q > 0) {
         for (g in 1:x$model$G) {
            for (w in 1:x$model$W) {
               x$param$gamma[[g]][[w]][] = x$param$gamma[[g]][[w]][, norder]
            }

            x$param$gamma[[g]][] = x$param$gamma[[g]][gorder]
         }

         for (w in 1:x$model$W) {
            tmp = cbind(x$param$beta[[1]][[w]], 0)
            x$param$beta[[1]][[w]][] =
               tmp[, norder[-x$model$C], drop = FALSE] -
               tmp[, norder[x$model$C]]
            tmp = cbind(x$std.err$beta[[1]][[w]], 0)
            x$std.err$beta[[1]][[w]][] =
               sqrt(tmp[, norder[-x$model$C], drop = FALSE]^2 +
                       tmp[, norder[x$model$C]]^2)
            #std.err beta
         }
         x$param$beta[[1]][] = x$param$beta[[1]][gorder]
         x$std.err$beta[[1]][] = x$std.err$beta[[1]][gorder]

         if (x$model$Q > 0) {
            tmp = cbind(x$param$beta[[2]], 0)
            x$param$beta[[2]][] =
               tmp[, norder[-x$model$C], drop = FALSE] -
               tmp[, norder[x$model$C]]
            tmp = cbind(x$std.err$beta[[2]], 0)
            x$std.err$beta[[2]][] =
               sqrt(tmp[, norder[-x$model$C], drop = FALSE]^2 +
                       tmp[, norder[x$model$C]]^2)
         }
      } else {
         x$param$gamma[] = x$param$gamma[gorder, norder]
         x$std.err$gamma[] = x$std.err$gamma[gorder, norder]
      }
   } else {
      for (g in 1:x$model$G) {
         if (is.null(class.order))
            norder = order(x$param$rho[[g]][[1]][,1], decreasing = decreasing)

         for (m in 1:x$model$M) {
            x$param$rho[[g]][[m]][] = x$param$rho[[g]][[m]][norder,]
            x$std.err$rho[[g]][[m]][] = x$std.err$rho[[g]][[m]][norder,]
         }

         x$posterior[[g]][] = x$posterior[[g]][, norder]

         if (x$model$P > 1) {
            x$param$gamma[[g]][] = x$param$gamma[[g]][, norder]
            tmp = cbind(x$param$beta[[g]], 0)
            x$param$beta[[g]][] = tmp[, norder[-x$model$C], drop = FALSE] -
               tmp[, norder[x$model$C]]
            # std.err beta
         }
      }

      x$param$gamma[] = x$param$gamma[, norder]
      x$std.err$gamma[] = x$std.err$gamma[, norder]
   }

   x
}
