glca_em <- function(
   model, datalist, init,
   miniter, maxiter, eps, verbose
)
{
   Ng <- model$Ng; G <- model$G
   C <- model$C; W <- model$W
   M <- model$M; R <- model$R
   P <- model$P; Q <- model$Q
   measure.inv <- model$measure.inv
   coeff.inv <- model$coeff.inv

   y <- datalist$y; x <- datalist$x; z <- datalist$z

   delta <- init$delta
   gamma <- init$gamma
   beta  <- init$beta
   rho   <- init$rho

   if (verbose) cat(model$type, "Fitting...\n\n")

   converged <- FALSE
   param = list()

   # EM iteration
   if (W == 0L) {
      if (P == 1L) {
         for (iter in miniter:maxiter)
         {
            # E-step
            Post <- GetPost(y, gamma, rho, Ng, G, C, M, R)

            # M-step
            n_gamma <- UpGamma(Post, Ng, G, C)
            if (measure.inv)
               n_rho <- UpRhoR(y, Post, rho, Ng, G, C, M, R)
            else
               n_rho <- UpRhoU(y, Post, rho, Ng, G, C, M, R)

            maxdiff <- max(max(abs(unlist(n_gamma) - unlist(gamma))),
                           max(abs(unlist(n_rho) - unlist(rho))))

            if (verbose) {
               if (iter %% 100L == 0L)
                  cat(".")
               if (iter %% 1000L == 0L)
                  cat("", iter, "iteration \n")
            }

            if (maxdiff < eps) {
               converged <- TRUE
               if (verbose) cat("", iter, "iteration \n")
               break
            } else {
               gamma <- n_gamma
               rho <- n_rho
            }
         }
         param$gamma <- n_gamma
         param$rho   <- n_rho
      } else {
         for (iter in miniter:maxiter)
         {
            # E-step
            exb <- lapply(1L:G, function(g) exp(x[[g]] %*% beta[[g]]))
            gamma <- lapply(exb, function(x) cbind(x, 1L) / (rowSums(x) + 1L))
            Post <- GetPost(y, gamma, rho, Ng, G, C, M, R)

            if (coeff.inv) {
               Amat <- cbind(diag(G * (C - 1L)) %x% c(1L, numeric(P - 1L)),
                            rep(1L, G) %x% diag(C - 1L) %x% rbind(0L, diag(P - 1L)))
               gradhess <- GetDeriv2(Post, x, gamma, Ng, G, C, P)
               diff <- try(Amat %*% (MASS::ginv(gradhess[[2L]]) %*% gradhess[[1L]]), TRUE)

               if (inherits(diff, "try-error")) break
               n_beta = lapply(1L:G, function(g)
                  beta[[g]]- matrix(diff[((g - 1L) * (C - 1L) * P + 1L):(g* (C - 1L) * P)], P)
               )
            } else {
               n_beta = list()

               for (g in 1L:G) {
                  gradhess <- GetDeriv(Post[[g]], x[[g]], gamma[[g]], Ng[g], C, P)
                  diff <- try(matrix(MASS::ginv(gradhess[[2L]]) %*% gradhess[[1L]], P), TRUE)
                  if (inherits(diff, "try-error")) break
                  n_beta[[g]] = beta[[g]] - diff
               }
            }

            if (inherits(diff, "try-error")) {
               iter <- 1L
               if (verbose)
                  cat("Fail to get Hessian inverse, restarted with new initial value.\n")
               init <- glca_init(model)
               beta  <- init$beta
               rho   <- init$rho
               next
            }

            if (measure.inv)
               n_rho <- UpRhoR(y, Post, rho, Ng, G, C, M, R)
            else
               n_rho <- UpRhoU(y, Post, rho, Ng, G, C, M, R)

            maxdiff <- max(max(abs(unlist(n_beta) - unlist(beta))),
                           max(abs(unlist(n_rho) - unlist(rho))))

            if (verbose) {
               if (iter %% 100L == 0L)
                  cat(".")
               if (iter %% 1000L == 0L)
                  cat("", iter, "iteration \n")
            }

            if (maxdiff < eps) {
               converged <- TRUE
               if (verbose) cat("", iter, "iteration \n")
               break
            } else {
               beta <- n_beta
               rho <- n_rho
            }
         }
         param$beta  <- n_beta
         param$gamma <- gamma
         param$rho   <- n_rho
      }

      llik <- GetLik(y, gamma, rho, Ng, G, C, M, R)
   } else {
      if (P == 1L && Q == 0L) {
         for (iter in miniter:maxiter) {
            # E-step
            Post <- GetUDPost(y, delta, gamma, rho, Ng, G, W, C, M, R)

            # M-step
            n_delta <- UpDelta(Post$PostW / rowSums(Post$PostW))
            n_gamma <- UpGammaML(Post$PostWC, W, C)
            n_rho   <- UpRhoML(y, Post$PostC, rho, Ng, G, C, M, R)

            maxdiff <- max(max(abs(unlist(n_delta) - unlist(delta))),
                           max(abs(unlist(n_gamma) - unlist(gamma))),
                           max(abs(unlist(n_rho) - unlist(rho))))

            if (verbose) {
               if (iter %% 100L == 0L)
                  cat(".")
               if (iter %% 1000L == 0L)
                  cat("", iter, "iteration \n")
            }

            if (maxdiff < eps) {
               converged <- TRUE
               if (verbose) cat("", iter, "iteration \n")
               break
            } else {
               delta <- n_delta
               gamma <- n_gamma
               rho   <- n_rho
            }
         }
         param$delta <- n_delta
         param$gamma <- n_gamma
         param$rho   <- n_rho

         llik <- GetUDlik(y, delta, gamma, rho, Ng, G, W, C, M, R)
      } else {
         for (iter in miniter:maxiter) {
            # E-step
            if (Q > 0L)
               beta2 <- matrix(
                  beta[(W * (C - 1L) * P + 1L):(W * (C - 1L) * P + Q * (C - 1L))],
                  Q, C - 1L)
            else
               beta2 <- NULL

            gamma <- lapply(1L:G, function(g) lapply(1L:W, function(w)
            {
               beta1 <- matrix(
                  beta[((w - 1L) * (C - 1L) * P + 1L):(w * (C - 1L) * P)],
                  P, C - 1L)
               xb <- x[[g]] %*% beta1
               if (Q > 0L)
                  zb <- z[[g]] %*% beta2
               else
                  zb <- 0L
               exzb <- exp(xb + zb)
               return(cbind(exzb, 1L) / (rowSums(exzb) + 1L))
            }
            ))

            Post <- GetUDPostX(y, x, z, delta, gamma, rho,
                               Ng, G, W, C, P, Q, M, R)

            # M-step
            n_delta <- UpDelta(Post$PostW / rowSums(Post$PostW))

            if (coeff.inv) {
               if (P > 1L)
                  A1 = cbind(diag(W * (C - 1L)) %x% c(1L, numeric(P - 1L)),
                             rep(1L, W) %x% diag(C - 1L) %x% rbind(0L, diag(P - 1L)))
               else
                  A1 = diag(W * (C - 1L))
               A2 = diag((C - 1L) * Q)
               d1 = dim(A1); d2 = dim(A2)
               Amat = array(0L, dim = rev(d1 + d2))
               Amat[1L:d1[2L], 1L:d1[1L]] = t(A1)
               Amat[-(1L:d1[2L]), -(1L:d1[1L])] = A2

               hess <- Amat %*% Post$hess %*% t(Amat)
               grad <- Amat %*% Post$grad
               n_beta  <- try(beta - t(Amat) %*% MASS::ginv(hess) %*% grad, TRUE)
            } else
               n_beta  <- try(beta - MASS::ginv(Post$hess) %*% Post$grad, TRUE)

            if (inherits(n_beta, "try-error")) {
               iter <- 1L
               if (verbose)
                  cat("Fail to get Hessian inverse, restarted with new initial value.\n")
               init <- glca_init(model)
               delta <- init$delta
               beta  <- init$beta
               rho   <- init$rho
               next
            }

            n_rho   <- UpRhoML(y, Post$PostC, rho, Ng, G, C, M, R)

            maxdiff <- max(max(abs(unlist(n_delta) - unlist(delta))),
                           max(abs(unlist(n_beta) - unlist(beta))),
                           max(abs(unlist(n_rho) - unlist(rho))))

            if (verbose) {
               if (iter %% 100L == 0L)
                  cat(".")
               if (iter %% 1000L == 0L)
                  cat("", iter, "iteration \n")
            }

            if (maxdiff < eps) {
               converged <- TRUE
               if (verbose) cat("", iter, "iteration \n")
               break
            } else {
               delta <- n_delta
               beta  <- n_beta
               rho   <- n_rho
            }
         }
         param$delta <- n_delta
         param$beta  <- n_beta
         param$gamma <- gamma
         param$rho   <- n_rho

         llik = GetUDlikX(y, delta, gamma, rho, Ng, G, W, C, M, R)
      }
   }

   if (verbose) {
      if (converged)
         cat("\nConverged at ", iter, " iteration (loglik :",
             llik, ")\n", sep = "")
      else
         cat("\nIteration End, Not Converged",
         " (loglik : ", llik, ")\n", sep = "")
   }

   return(
      list(param = param, posterior = Post, loglik = llik,
           niter = iter, converged = converged)
   )
}
