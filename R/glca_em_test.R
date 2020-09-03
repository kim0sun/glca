glca_em_test <- function(
   model, datalist, n.init,
   testiter, eps, verbose
)
{
   Ng <- model$Ng; G <- model$G
   C <- model$C; W <- model$W
   M <- model$M; R <- model$R
   P <- model$P; Q <- model$Q
   measure.inv <- model$measure.inv
   coeff.inv <- model$coeff.inv

   y <- datalist$y; x <- datalist$x; z <- datalist$z

   init_list <- list()
   llik <- numeric(n.init)
   niters <- numeric(n.init)

   for (rep in 1L:n.init) {
      init.ran <- glca_init(model)

      delta <- init.ran$delta
      gamma <- init.ran$gamma
      beta  <- init.ran$beta
      rho   <- init.ran$rho

      if (verbose) cat("SET :", rep, rep("", nchar(n.init) - nchar(rep) + 1L))
      iter <- 0L
      maxdiff <- 0L

      # EM iteration
      if (W == 0L) {
         if (P == 1L) {
            while (iter <= testiter)
            {
               iter <- iter + 1L
               if (iter %% 10L == 0L && verbose) cat(".")
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

               if (maxdiff < eps) break

               gamma <- n_gamma
               rho <- n_rho
            }

            llik[rep] <- GetLik(y, gamma, rho, Ng, G, C, M, R)
            init_list[[rep]] <- list(
               gamma = gamma, rho = rho
            )

         } else {
            while (iter <= testiter)
            {
               iter <- iter + 1L
               if (iter %% 10L == 0L && verbose) cat(".")

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
                  n_beta <- list()

                  for (g in 1L:G) {
                     gradhess <- GetDeriv(Post[[g]], x[[g]], gamma[[g]], Ng[g], C, P)
                     diff <- try(matrix(MASS::ginv(gradhess[[2L]]) %*% gradhess[[1L]], P), TRUE)
                     if (inherits(diff, "try-error")) break
                     n_beta[[g]] = beta[[g]] - diff
                  }
               }

               if (inherits(diff, "try-error")) {
                  iter <- 0L
                  init <- glca_init(model)
                  beta  <- init$beta
                  rho   <- init$rho
               }

               if (measure.inv)
                  n_rho <- UpRhoR(y, Post, rho, Ng, G, C, M, R)
               else
                  n_rho <- UpRhoU(y, Post, rho, Ng, G, C, M, R)

               maxdiff <- max(max(abs(unlist(n_beta) - unlist(beta))),
                              max(abs(unlist(n_rho) - unlist(rho))))

               if (maxdiff < eps) break

               beta <- n_beta
               rho <- n_rho
            }

            llik[rep] <- GetLik(y, gamma, rho, Ng, G, C, M, R)
            init_list[[rep]] <- list(
               beta = beta, rho = rho
            )
         }
      } else {
         if (P == 1L && Q == 0L) {
            while (iter <= testiter) {
               iter <- iter + 1
               if (iter %% 10 == 0 && verbose) cat(".")

               # E-step
               Post <- GetUDPost(y, delta, gamma, rho, Ng, G, W, C, M, R)

               # M-step
               n_delta <- UpDelta(Post$PostW)
               n_gamma <- UpGammaML(Post$PostWC, W, C)
               n_rho   <- UpRhoML(y, Post$PostC, rho, Ng, G, C, M, R)

               maxdiff <- max(max(abs(unlist(n_delta) - unlist(delta))),
                              max(abs(unlist(n_gamma) - unlist(gamma))),
                              max(abs(unlist(n_rho) - unlist(rho))))

               if (maxdiff < eps) break

               delta <- n_delta
               gamma <- n_gamma
               rho   <- n_rho
            }

            llik[rep] <- GetUDlik(y, delta, gamma, rho, Ng, G, W, C, M, R)
            init_list[[rep]] <- list(
               delta = delta, gamma = gamma, rho = rho
            )

         } else {
            while (iter <= testiter) {
               iter <- iter + 1
               if (iter %% 10 == 0 && verbose) cat(".")

               # E-step
               if (Q > 0)
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
                  if (Q > 0)
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
               n_delta <- colSums(Post$PostW) / sum(Post$PostW)

               if (coeff.inv) {
                  if (P > 1)
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
                  iter <- 0L
                  init <- glca_init(model)
                  delta <- init$delta
                  beta  <- init$beta
                  rho   <- init$rho
               }

               n_rho   <- UpRhoML(y, Post$PostC, rho, Ng, G, C, M, R)

               maxdiff <- max(max(abs(unlist(n_delta) - unlist(delta))),
                              max(abs(unlist(n_beta) - unlist(beta))),
                              max(abs(unlist(n_rho) - unlist(rho))))

               if (maxdiff < eps) break

               delta <- n_delta
               beta  <- n_beta
               rho   <- n_rho
            }

            llik[rep] <- GetUDlikX(y, delta, gamma, rho, Ng, G, W, C, M, R)
            init_list[[rep]] <- list(
               delta = delta, beta = beta, rho = rho
               )
         }
      }

      if (verbose) cat("loglik :", llik[rep], "\n")
      niters[rep] <- iter
   }

   if (verbose) cat("\nStart with SET ", which.max(llik),
                    " (", llik[which.max(llik)],
                    ")\n\n", sep = "")

   param <- init_list[[which.max(llik)]]
   niter <- niters[which.max(llik)]

   return(list(param = param, niter = niter))
}
