glca_em_test <- function(
   model, datalist,
   n_init, verbose
)
{
   Ng <- model$Ng; G <- model$G
   C <- model$C; W <- model$W
   M <- model$M; R <- model$R
   P <- model$P; Q <- model$Q

   y <- datalist$y; pattern <- datalist$pattern
   x <- datalist$x; z <- datalist$z

   init_list = list()
   llik = numeric(n_init)

   for (rep in 1:n_init) {
      init_ran = glca_init(model)

      delta <- init_ran$delta
      gamma <- init_ran$gamma
      beta  <- init_ran$beta
      rho   <- init_ran$rho

      if (verbose) cat("SET :", rep, " ")
      iter <- 0

      # EM iteration
      if (W == 0) {
         if (P == 1) {
            while (iter <= 50)
            {
               iter <- iter + 1
               if (iter %% 10 == 0 && verbose) cat(".")
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

               gamma <- n_gamma
               rho <- n_rho
            }

            llik[rep] <- GetLik(y, gamma, rho, Ng, G, C, M, R)
            init_list[[rep]] <- list(
               gamma = gamma, rho = rho
            )

         } else {
            while (iter <= 50)
            {
               iter <- iter + 1
               if (iter %% 10 == 0 && verbose) cat(".")

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

               if (model$measure_inv)
                  n_rho <- UpRhoR(y, Post, rho, Ng, G, C, M, R)
               else
                  n_rho <- UpRhoU(y, Post, rho, Ng, G, C, M, R)

               beta <- n_beta
               rho <- n_rho

               if (inherits(diff, "try-error")) {
                  iter <- 0
                  init <- glca_init(model)
                  beta  <- init$beta
                  rho   <- init$rho
               }
            }

            llik[rep] <- GetLik(y, gamma, rho, Ng, G, C, M, R)
            init_list[[rep]] <- list(
               beta = beta, rho = rho
            )
         }
      } else {
         if (P == 1 && Q == 0) {
            while (iter <= 50) {
               iter <- iter + 1
               if (iter %% 10 == 0 && verbose) cat(".")

               # E-step
               Post <- GetUDPost(y, delta, gamma, rho, Ng, G, W, C, M, R)

               # M-step
               n_delta <- UpDelta(Post$PostW)
               n_gamma <- UpGammaML(Post$PostWC, W, C)
               n_rho   <- UpRhoML(y, Post$PostC, rho, Ng, G, C, M, R)

               delta <- n_delta
               gamma <- n_gamma
               rho   <- n_rho
            }

            llik[rep] <- GetUDlik(y, delta, gamma, rho, Ng, G, W, C, M, R)
            init_list[[rep]] <- list(
               delta = delta, gamma = gamma, rho = rho
            )

         } else {
            while (iter <= 50) {
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
               n_beta  <- try(beta - MASS::ginv(
                  Post$hess) %*% Post$grad, TRUE)
               n_rho   <- UpRhoML(y, Post$PostC, rho, Ng, G, C, M, R)

               delta <- n_delta
               beta  <- n_beta
               rho   <- n_rho

               if (inherits(n_beta, "try-error")) {
                  iter <- 0
                  init <- glca_init(model)
                  delta <- init$delta
                  beta  <- init$beta
                  rho   <- init$rho
               }
            }

            llik[rep] = GetUDlikX(y, delta, gamma, rho, Ng, G, W, C, M, R)
            init_list[[rep]] <- list(
               delta = delta, beta = beta, rho = rho
               )
         }
      }

      if (verbose) cat("log-like :", llik[rep], "\n")
   }

   if (verbose) cat("\nStart with SET ", which.max(llik),
                    " (", llik[which.max(llik)],
                    ")\n\n", sep = "")

   param = init_list[[which.max(llik)]]

   return(param)
}
