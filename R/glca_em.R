glca_em <- function(
   model, datalist, init,
   verbose, maxiter, eps
)
{
   Ng <- model$Ng; G <- model$G
   C <- model$C; W <- model$W
   M <- model$M; R <- model$R
   P <- model$P; Q <- model$Q

   y <- datalist$y; pattern <- datalist$pattern
   x <- datalist$x; z <- datalist$z

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
         for (iter in 1:maxiter)
         {
            # E-step
            Post <- GetPost(y, gamma, rho, Ng, G, C, M, R)

            # M-step
            n_gamma <- UpGamma(Post, Ng, G, C)
            if (model$measure_inv)
               n_rho <- UpRhoR(y, Post, rho, Ng, G, C, M, R)
            else
               n_rho <- UpRhoU(y, Post, rho, Ng, G, C, M, R)

            maxdiff <- max(max(unlist(n_gamma) - unlist(gamma)),
                          max(unlist(n_rho) - unlist(rho)))

            if (verbose)
               if (iter %% 100 == 0)
                  cat(iter, "iteration \n")

            if (maxdiff < eps) {
               converged <- TRUE
               break
            } else {
               gamma <- n_gamma
               rho <- n_rho
            }
         }
         param$gamma <- n_gamma
         param$rho   <- n_rho
      } else {
         for (iter in 1:maxiter)
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

            if (model$measure_inv)
               n_rho <- UpRhoR(y, Post, rho, Ng, G, C, M, R)
            else
               n_rho <- UpRhoU(y, Post, rho, Ng, G, C, M, R)

            maxdiff <- max(max(unlist(n_beta) - unlist(beta)),
                          max(unlist(n_rho) - unlist(rho)))

            if (verbose)
               if (iter %% 100 == 0)
                  cat(iter, "iteration \n")

            if (maxdiff < eps) {
               converged <- TRUE
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

      gamma_m = lapply(gamma, function(g)
         matrix(colMeans(g), nrow(pattern), C, byrow = TRUE))
      fitted <- GetFitted(pattern, gamma_m, rho, Ng, G, C, M, R)
      llik <- GetLik(y, gamma, rho, Ng, G, C, M, R)
   } else {
      if (P == 1 && Q == 0) {
         for (iter in 1:maxiter) {
            # E-step
            Post <- GetUDPost(y, delta, gamma, rho, Ng, G, W, C, M, R)

            # M-step
            n_delta <- UpDelta(Post$PostW)
            n_gamma <- UpGammaML(Post$PostWC, W, C)
            n_rho   <- UpRhoML(y, Post$PostC, rho, Ng, G, C, M, R)

            maxdiff <- max(max(unlist(n_gamma) - unlist(gamma)),
                          max(unlist(n_rho) - unlist(rho)))

            if (verbose)
               if (iter %% 100 == 0)
                  cat(iter, "iteration \n")

            if (maxdiff < eps) {
               converged <- TRUE
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
         gamma_m = lapply(1:G, function(g) gamma)
      } else {
         for (iter in 1:maxiter) {
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

            maxdiff <- max(max(n_beta - beta),
                          max(unlist(n_rho) - unlist(rho)))

            if (verbose)
               if (iter %% 100 == 0)
                  cat(iter, "iteration \n")

            if (maxdiff < eps) {
               converged <- TRUE
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
         gamma_m = lapply(1:G, function(g)
            t(sapply(gamma[[g]], function(w) colMeans(w))))
      }

      fitted = GetUDfit(pattern, delta, gamma_m, rho, Ng, G, W, C, M, R)
   }

   if (verbose)
      cat("\nIteration End. (log-like : ", llik, ")\n", sep = "")

   return(
      list(param = param, posterior = Post,
           fitted = fitted, loglike = llik,
           niter = iter, converged = converged)
   )
}
