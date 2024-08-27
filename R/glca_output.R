glca_output <- function(
   call, terms, model, datalist, vname, EM, scores
)
{
   N <- model$N; Ng <- model$Ng;
   G <- model$G
   C <- model$C; W <- model$W
   M <- model$M; R <- model$R
   P <- model$P; Q <- model$Q
   n <- model$n
   npar <- model$npar
   df <- model$df

   y.names <- vname$y.names
   r.names <- vname$r.names
   r.names <- list()
   for (m in 1L:M) {
      r.names[[m]] = paste0("Y = ", 1L:R[m])
   }
   g.names <- vname$g.names
   x.names <- vname$x.names
   z.names <- vname$z.names

   param <- EM$param
   posterior <- EM$posterior

   if (!is.null(scores))
      std.err <- scores$std.err

   # Parameter design
   if (W > 0L) {
      names(param$delta) <- paste0("Cluster ", 1L:W)
      if (!is.null(scores))
         names(std.err$delta) <- paste0("Cluster ", 1L:W)


      if (P == 1 && Q == 0) {
         dimnames(param$gamma) <- list(paste0("Cluster ", 1L:W),
                                       paste0("Class ", 1L:C))
         if (!is.null(scores))
            dimnames(std.err$gamma) <- list(paste0("Cluster ", 1L:W),
                                            paste0("Class ", 1L:C))

      } else {
         beta1 <- list()
         se_b1 <- list()
         for (w in 1L:W) {
            beta1[[w]] <- matrix(
               param$beta[((w - 1L) * (C - 1L) * P + 1L):(w * (C - 1L) * P)],
               P, C - 1L)
            dimnames(beta1[[w]]) <-
               list(x.names, paste0("Class ", 1L:(C - 1L), "/", C))
            if (!is.null(scores)){
               se_b1[[w]] <- matrix(
                  std.err$beta[((w - 1L) * (C - 1L) * P + 1L):(w * (C - 1L) * P)],
                  P, C - 1L)
               dimnames(se_b1[[w]]) <-
                  list(x.names, paste0("Class ", 1L:(C - 1L), "/", C))
            }
         }
         names(beta1) <- paste0("Cluster", 1L:W)
         beta = list(Level1 = beta1)

         if (!is.null(scores)) {
            names(se_b1) <- paste0("Cluster", 1L:W)
            se_b = list(Level1 = se_b1)
         }

         if (Q > 0L) {
            beta$Level2 <- matrix(
               param$beta[(W * (C - 1L) * P + 1L):(W * (C - 1L) * P + Q * (C - 1L))],
               Q, C - 1L)
            dimnames(beta$Level2) <-
               list(z.names, paste0("Class ", 1L:(C - 1L), "/", C))

            if (!is.null(scores)) {
               se_b$Level2 <- matrix(
                  std.err$beta[(W * (C - 1L) * P + 1L):(W * (C - 1L) * P + Q * (C - 1L))],
                  Q, C - 1L)
               dimnames(se_b$Level2) <-
                  list(z.names, paste0("Class ", 1L:(C - 1L), "/", C))
            }
         }
         param$beta = beta;
         if (!is.null(scores))
            std.err$beta = se_b

         names(param$gamma) <- g.names
         for (g in 1L:G) {
            names(param$gamma[[g]]) <-
               paste0("Cluster ", 1L:W)
            for (w in 1L:W)
               dimnames(param$gamma[[g]][[w]]) <- list(
                  1L:Ng[g], paste0("Class ", 1L:C)
            )
         }
      }

      names(param$rho) <- y.names
      if (!is.null(scores))
         names(std.err$rho) <- y.names

      for (m in 1L:M) {
         dimnames(param$rho[[m]]) <-
            list(paste0("Class ", 1L:C),
                 substr(r.names[[m]], 1L, 10L)
            )
         if (!is.null(scores))
            dimnames(std.err$rho[[m]]) <-
               list(paste0("Class ", 1L:C),
                    substr(r.names[[m]], 1L, 10L)
               )
      }

   } else {
      if (P > 1) {
         names(param$beta) <- g.names
         if (!is.null(scores))
            names(std.err$beta) <- g.names
         for (g in 1L:G) {
            dimnames(param$beta[[g]]) <-
               list(x.names, paste0("Class ", 1L:(C - 1L), "/", C))
            if (!is.null(scores))
               dimnames(std.err$beta[[g]]) <-
                  list(x.names, paste0("Class ", 1L:(C - 1L), "/", C))
         }
      } else {
         param$gamma <- t(sapply(param$gamma, colMeans))
         dimnames(param$gamma) <- list(
            g.names, paste0("Class ", 1L:C)
         )
         if (!is.null(scores))
            dimnames(std.err$gamma) <- list(
               g.names, paste0("Class ", 1L:C)
            )
      }
      names(param$rho) <- g.names
      for (g in 1L:G) {
         names(param$rho[[g]]) <- y.names

         for (m in 1L:M) {
            dimnames(param$rho[[g]][[m]]) <-
               list(paste0("Class ", 1L:C),
                    substr(r.names[[m]], 1L, 10L)
            )
         }
      }

      if (!is.null(scores)) {
         names(std.err$rho) <- g.names
         for (g in 1L:G) {
            names(std.err$rho[[g]]) <- y.names

            for (m in 1L:M) {
               dimnames(std.err$rho[[g]][[m]]) <-
                  list(paste0("Class ", 1L:C),
                       substr(r.names[[m]], 1L, 10L)
                  )
            }
         }
      }
   }

   # Posterior design
   post = list()
   if (W > 0L) {
      post$cluster <- data.frame(
         posterior$PostW / rowSums(posterior$PostW),
         row.names = vname$g.names
      )
      names(post$cluster) <- paste0("Cluster ", 1L:W)
      post$wclass <- data.frame(
         posterior$PostWC / rowSums(posterior$PostWC),
         row.names = paste0("Cluster ", 1L:W)
      )
      names(post$wclass) <- paste0("Class ", 1L:C)

      pclass <- posterior$PostC
      post$class <- lapply(pclass, data.frame)
      names(post$class) <- g.names
      for (g in 1L:G)
         names(post$class[[g]]) <- paste0("Class ", 1L:C)
   } else {
      pclass <- posterior
      post <- lapply(pclass, data.frame)
      names(post) = g.names
      for (g in 1L:G)
         names(post[[g]]) <- paste0("Class ", 1L:C)
   }

   # Goodness of fit
   P = do.call(rbind, pclass)
   gof <- list(
      loglik = EM$loglik,
      AIC = -2 * EM$loglik + 2 * npar,
      BIC = -2 * EM$loglik + log(N) * npar,
      entropy = 1 - sum(-P[P != 0] * log(P)[P != 0]) / (N * log(C)),
      df = df,
      Gsq = 2 * (datalist$loglik0 - EM$loglik)
   )

   # Convergence
   convergence <- list(
      niter = EM$niter,
      converged = EM$converged
   )
   if (!is.null(scores))
      convergence$score = scores$score

   ret <- list()
   ret$call <- call
   ret$terms <- terms
   ret$model <- model
   ret$var.names <- vname
   ret$datalist <- datalist
   ret$param <- param
   if (!is.null(scores))
      ret$std.err <- std.err
   ret$posterior <- post
   ret$gof <- gof
   ret$convergence <- convergence

   return(ret)
}
