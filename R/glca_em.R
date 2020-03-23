glca_em <- function(
   model, datalist, init,
   miniter, maxiter, eps, verbose
)
{
   Ng <- model$Ng; G <- model$G
   C <- model$C; W <- model$W
   M <- model$M; R <- model$R
   P <- model$P; Q <- model$Q

   y <- datalist$y; x <- datalist$x; z <- datalist$z

   delta <- init$delta
   gamma <- init$gamma
   beta  <- init$beta
   rho   <- init$rho

   if (verbose) cat(model$type, "Fitting...\n\n")

   converged <- FALSE
   param = list()

   # EM iteration
   if (W == 0) {
      if (P == 1) {
         for (iter in miniter:maxiter)
         {
            # E-step
            Post <- GetPost(y, gamma, rho, Ng, G, C, M, R)

            # M-step
            n_gamma <- UpGamma(Post, Ng, G, C)
            if (model$measure.inv)
               n_rho <- UpRhoR(y, Post, rho, Ng, G, C, M, R)
            else
               n_rho <- UpRhoU(y, Post, rho, Ng, G, C, M, R)

            maxdiff <- max(max(abs(unlist(n_gamma) - unlist(gamma))),
                           max(abs(unlist(n_rho) - unlist(rho))))

            if (verbose) {
               if (iter %% 100 == 0)
                  cat(".")
               if (iter %% 1000 == 0)
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
            exb <- lapply(1:G, function(g) exp(x[[g]] %*% beta[[g]]))
            gamma <- lapply(exb, function(x) cbind(x, 1) / (rowSums(x) + 1))
            Post <- GetPost(y, gamma, rho, Ng, G, C, M, R)
            n_beta = list()
            for (g in 1:G) {
               gradhess <- GetDeriv(Post[[g]], x[[g]], gamma[[g]], Ng[g], C, P)
               diff <- try(matrix(MASS::ginv(-gradhess[[2]]) %*% gradhess[[1]], P), TRUE)
               if (inherits(diff, "try-error")) break
               n_beta[[g]] = beta[[g]] + diff
            }
            if (inherits(diff, "try-error")) {
               iter <- 1
               if (verbose)
                  cat("Fail to get Hessian inverse, restarted with new initial value.\n")
               init <- glca_init(model)
               beta  <- init$beta
               rho   <- init$rho
               next
            }

            if (model$measure.inv)
               n_rho <- UpRhoR(y, Post, rho, Ng, G, C, M, R)
            else
               n_rho <- UpRhoU(y, Post, rho, Ng, G, C, M, R)

            maxdiff <- max(max(abs(unlist(n_beta) - unlist(beta))),
                           max(abs(unlist(n_rho) - unlist(rho))))

            if (verbose) {
               if (iter %% 100 == 0)
                  cat(".")
               if (iter %% 1000 == 0)
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
      gamma_m <- list(
         matrix(colMeans(do.call(rbind, Post)),
                sum(Ng), C, byrow = TRUE)
      )
      rho_m <- UpRhoR(y, Post, rho, Ng, G, C, M, R)[1]
      nullik <- GetLik(list(do.call(rbind, y)), gamma_m, rho_m,
                     sum(Ng), 1, C, M, R)
   } else {
      if (P == 1 && Q == 0) {
         for (iter in miniter:maxiter) {
            # E-step
            Post <- GetUDPost(y, delta, gamma, rho, Ng, G, W, C, M, R)

            # M-step
            n_delta <- UpDelta(Post$PostW)
            n_gamma <- UpGammaML(Post$PostWC, W, C)
            n_rho   <- UpRhoML(y, Post$PostC, rho, Ng, G, C, M, R)

            maxdiff <- max(max(abs(unlist(n_delta) - unlist(delta))),
                           max(abs(unlist(n_gamma) - unlist(gamma))),
                           max(abs(unlist(n_rho) - unlist(rho))))

            if (verbose) {
               if (iter %% 100 == 0)
                  cat(".")
               if (iter %% 1000 == 0)
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
            n_beta  <- try(beta - MASS::ginv(Post$hess) %*% Post$grad, TRUE)
            if (inherits(n_beta, "try-error")) {
               iter <- 1
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
               if (iter %% 100 == 0)
                  cat(".")
               if (iter %% 1000 == 0)
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

      gamma_m = list(
         matrix(colMeans(do.call(rbind, Post$PostC)),
                sum(Ng), C, byrow = TRUE)
      )
      rho_m = list(rho)
      nullik <- GetLik(list(do.call(rbind, y)), gamma_m, rho_m,
                     sum(Ng), 1, C, M, R)
   }

   model0 <- list(Ng = Ng, G = G, W = 0, C = C,
                  M = M, R = R, P = 1, Q = 0)
   param0 <- list(gamma = t(sapply(1:G, function(g) colMeans(gamma_m[[1]]))),
                  rho = lapply(1:G, function(g) rho_m[[1]]))

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
           model0 = model0, param0 = param0, nullik = nullik,
           niter = iter, converged = converged)
   )
}
