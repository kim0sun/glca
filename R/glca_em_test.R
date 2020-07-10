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

   for (rep in 1:n.init) {
      init.ran <- glca_init(model)

      delta <- init.ran$delta
      gamma <- init.ran$gamma
      beta  <- init.ran$beta
      rho   <- init.ran$rho

      if (verbose) cat("SET :", rep, rep("", nchar(n.init) - nchar(rep) + 1))
      iter <- 0
      maxdiff <- 0

      # EM iteration
      if (W == 0) {
         if (P == 1) {
            while (iter <= testiter)
            {
               iter <- iter + 1
               if (iter %% 10 == 0 && verbose) cat(".")
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
               iter <- iter + 1
               if (iter %% 10 == 0 && verbose) cat(".")

               # E-step
               exb <- lapply(1:G, function(g) exp(x[[g]] %*% beta[[g]]))
               gamma <- lapply(exb, function(x) cbind(x, 1) / (rowSums(x) + 1))
               Post <- GetPost(y, gamma, rho, Ng, G, C, M, R)

               if (coeff.inv) {
                  Amat <- cbind(diag(G * (C - 1)) %x% c(1, numeric(P - 1)),
                                rep(1, G) %x% diag(C - 1) %x% rbind(0, diag(P - 1)))
                  gradhess <- GetDeriv2(Post, x, gamma, Ng, G, C, P)
                  diff <- try(Amat %*% (MASS::ginv(gradhess[[2]]) %*% gradhess[[1]]), TRUE)

                  if (inherits(diff, "try-error")) break
                  n_beta = lapply(1:G, function(g)
                     beta[[g]]- matrix(diff[((g - 1) * (C - 1) * P + 1):(g* (C - 1) * P)], P)
                  )
               } else {
                  n_beta <- list()

                  for (g in 1:G) {
                     gradhess <- GetDeriv(Post[[g]], x[[g]], gamma[[g]], Ng[g], C, P)
                     diff <- try(matrix(MASS::ginv(gradhess[[2]]) %*% gradhess[[1]], P), TRUE)
                     if (inherits(diff, "try-error")) break
                     n_beta[[g]] = beta[[g]] - diff
                  }
               }

               if (inherits(diff, "try-error")) {
                  iter <- 0
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
         if (P == 1 && Q == 0) {
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
                     beta[(W * (C - 1) * P + 1):(W * (C - 1) * P + Q * (C - 1))],
                     Q, C - 1)
               else
                  beta2 <- NULL

               gamma <- lapply(1:G, function(g) lapply(1:W, function(w)
               {
                  beta1 <- matrix(
                     beta[((w - 1) * (C - 1) * P + 1):(w * (C - 1) * P)],
                     P, C - 1)
                  xb <- x[[g]] %*% beta1
                  if (Q > 0)
                     zb <- z[[g]] %*% beta2
                  else
                     zb <- 0
                  exzb <- exp(xb + zb)
                  return(cbind(exzb, 1) / (rowSums(exzb) + 1))
               }
               ))

               Post <- GetUDPostX(y, x, z, delta, gamma, rho,
                                  Ng, G, W, C, P, Q, M, R)

               # M-step
               n_delta <- colSums(Post$PostW) / sum(Post$PostW)

               if (coeff.inv) {
                  if (P > 1)
                     A1 = cbind(diag(W * (C - 1)) %x% c(1, numeric(P - 1)),
                                rep(1, W) %x% diag(C - 1) %x% rbind(0, diag(P - 1)))
                  else
                     A1 = diag(W * (C - 1))
                  A2 = diag((C - 1) * Q)
                  d1 = dim(A1); d2 = dim(A2)
                  Amat = array(0, dim = rev(d1 + d2))
                  Amat[1:d1[2], 1:d1[1]] = t(A1)
                  Amat[-(1:d1[2]), -(1:d1[1])] = A2

                  hess <- Amat %*% Post$hess %*% t(Amat)
                  grad <- Amat %*% Post$grad
                  n_beta  <- try(beta - t(Amat) %*% MASS::ginv(hess) %*% grad, TRUE)
               } else
                  n_beta  <- try(beta - MASS::ginv(Post$hess) %*% Post$grad, TRUE)

               if (inherits(n_beta, "try-error")) {
                  iter <- 0
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
