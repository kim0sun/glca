reorder.glca <- function(x, decreasing = TRUE, ...)
{
   decreasing <- as.logical(decreasing)

   if (x$model$W > 0) {
      norder = order(x$param$rho[[1]][,1], decreasing = decreasing)

      for (m in 1:x$model$M) {
         x$param$rho[[m]][] = x$param$rho[[m]][norder,]
         x$std.err$rho[[m]][] = x$std.err$rho[[m]][norder,]
      }

      x$posterior$wclass[] = x$posterior$wclass[, norder]

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
