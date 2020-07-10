glca_score <- function(
   model, datalist, param, posterior
)
{
   Ng <- model$Ng; G <- model$G
   C <- model$C; W <- model$W
   M <- model$M; R <- model$R
   P <- model$P; Q <- model$Q
   measure.inv = model$measure.inv
   coeff.inv = model$coeff.inv

   y <- datalist$y; x <- datalist$x; z <- datalist$z

   delta <- param$delta
   gamma <- param$gamma
   beta  <- param$beta
   rho   <- param$rho

   std.err <- list()

   if (W == 0) {
      if (P == 1) {
         S = GetScore(y, posterior, gamma, rho, Ng, G, C, M, R)

         if (measure.inv) {
            Sg = do.call(rbind, S$g)
            Sr = do.call(rbind, S$r)
            fS = cbind(Sg, Sr)
         } else {
            Sg = do.call(rbind, S$g)
            Sr = matrix(0, sum(Ng), G * C * (sum(R) - M))
            row = 1; col = 1

            for (g in 1:G) {
               Sr[row:(row + Ng[g] - 1), col:(col + C * (sum(R) - M) - 1)] = S$r[[g]]
               row = row + Ng[g]
               col = col + C * (sum(R) - M)
            }

            fS = cbind(Sg, Sr)
         }

         score = colSums(fS)

         invI = MASS::ginv(t(fS) %*% fS)
         Ir = invI[(G * (C - 1) + 1):ncol(invI),
                   (G * (C - 1) + 1):ncol(invI)]

         std.err = list()
         std.err$gamma = do.call(rbind, lapply(1:G, function(g) {
            Ig = invI[((g - 1) * (C - 1) + 1):(g * (C - 1)),
                      ((g - 1) * (C - 1) + 1):(g * (C - 1))]
            tmp_gamma = diag(t(gmat1(gamma[[g]][1,])) %*% Ig %*% gmat1(gamma[[g]][1,]))
            tmp_gamma[tmp_gamma < 0] = 0
            return(sqrt(tmp_gamma))
         }))

         std.err$rho = list()
         if (measure.inv) {
            tmp_rho = diag(t(gmat3(rho[[1]])) %*% Ir %*% gmat3(rho[[1]]))
            tmp_rho[tmp_rho < 0] = 0
            for (m in 1:M) {
               std.err$rho[[m]] = matrix(
                  tmp_rho[(sum(R[1:m]) * C - R[m] * C + 1):(sum(R[1:m]) * C)],
                  C, R[m], byrow = TRUE
               )
            }
            std.err$rho = lapply(1:G, function(g) std.err$rho)
         } else {
            tmp_rho = lapply(1:G, function(g) {
               Irg = Ir[((g - 1) * C * (sum(R) - M) + 1):(g * C * (sum(R) - M)),
                        ((g - 1) * C * (sum(R) - M) + 1):(g * C * (sum(R) - M))]
               tmp_se = diag(t(gmat3(rho[[g]])) %*% Irg %*% gmat3(rho[[g]]))
               tmp_se[tmp_se < 0] = 0
               return(sqrt(tmp_se))
            })

            for (g in 1:G) {
               std.err$rho[[g]] = list()

               for (m in 1:M) {
                  std.err$rho[[g]][[m]] = matrix(
                     tmp_rho[[g]][(sum(R[1:m]) * C - R[m] * C + 1):(sum(R[1:m]) * C)],
                     C, R[m], byrow = TRUE
                  )
               }
            }
         }
      } else {
         S = GetScoreX(y, x, posterior, gamma, rho, Ng, G, C, M, R, P, coeff.inv)

         Sb = do.call(rbind, S$b)
         if (measure.inv)
            Sr = do.call(rbind, S$r)
         else {
            Sr = matrix(0, sum(Ng), G * C * (sum(R) - M))

            row = 1; col = 1
            for (g in 1:G) {
               Sr[row:(row + Ng[g] - 1), col:(col + C * (sum(R) - M) - 1)] = S$r[[g]]
               row = row + Ng[g]
               col = col + C * (sum(R) - M)
            }
         }

         fS = cbind(Sb, Sr)
         score = colSums(fS)
         invI = MASS::ginv(t(fS) %*% fS)

         std.err = list()

         std.err$beta = lapply(1:G, function(g) {
            Ig = invI[((g - 1) * P * (C - 1) + 1):(g * P * (C - 1)),
                      ((g - 1) * P * (C - 1) + 1):(g * P * (C - 1))]
            tmp_beta = diag(Ig)
            tmp_beta[tmp_beta < 0] = 0
            tmp_beta = matrix(sqrt(tmp_beta), P, C - 1)
            return(tmp_beta)
         })

         Ir = invI[(G * P * (C - 1) + 1):ncol(invI),
                   (G * P * (C - 1) + 1):ncol(invI)]

         std.err$rho = list()
         if (measure.inv) {
            tmp_rho = diag(t(gmat3(rho[[1]])) %*% Ir %*% gmat3(rho[[1]]))
            tmp_rho[tmp_rho < 0] = 0
            for (m in 1:M) {
               std.err$rho[[m]] = matrix(
                  sqrt(tmp_rho)[(sum(R[1:m]) * C - R[m] * C + 1):(sum(R[1:m]) * C)],
                  C, R[m], byrow = TRUE
               )
            }
            std.err$rho = lapply(1:G, function(g) std.err$rho)
         } else {
            tmp_rho = lapply(1:G, function(g) {
               Irg = Ir[((g - 1) * C * (sum(R) - M) + 1):(g * C * (sum(R) - M)),
                        ((g - 1) * C * (sum(R) - M) + 1):(g * C * (sum(R) - M))]
               tmp_se = diag(t(gmat3(rho[[g]])) %*% Irg %*% gmat3(rho[[g]]))
               tmp_se[tmp_se < 0] = 0
               return(sqrt(tmp_se))
            })

            for (g in 1:G) {
               std.err$rho[[g]] = list()
               for (m in 1:M) {
                  std.err$rho[[g]][[m]] = matrix(
                     tmp_rho[[g]][(sum(R[1:m]) * C - R[m] * C + 1):(sum(R[1:m]) * C)],
                     C, R[m], byrow = TRUE
                  )
               }
            }
         }
      }
   } else {
      if (P == 1 && Q == 0) {
         S <- GetUDScore(y, delta, gamma, rho, Ng, G, W, C, M, R)
         score <- colSums(S)

         invI <- MASS::ginv(t(S) %*% S)
         g1 <- gmat1(delta)
         g2 <- gmat2(gamma)
         g3 <- gmat3(rho)

         tmp_delta <- diag(t(g1) %*% invI[1:(W - 1), 1:(W - 1)] %*% g1)
         tmp_delta[tmp_delta < 0] = 0
         std.err$delta <- sqrt(tmp_delta)

         tmp_gamma <- diag(t(g2) %*% invI[W:(W * C - 1), W:(W * C - 1)] %*% g2)
         tmp_gamma[tmp_gamma < 0] = 0
         std.err$gamma <- matrix(sqrt(tmp_gamma), W, C, byrow = TRUE)

         std.err$rho <- list()
         tmp_rho <- diag(t(g3) %*% invI[(W * C):ncol(invI), (W * C):ncol(invI)] %*% g3)
         tmp_rho[tmp_rho < 0] = 0
         tmp_rho <- sqrt(tmp_rho)
         for (m in 1:M) {
            std.err$rho[[m]] <- matrix(
               tmp_rho[(sum(R[1:m]) * C - R[m] * C + 1):(sum(R[1:m]) * C)],
               C, R[m], byrow = TRUE
            )
         }
      } else {
         S <- GetUDScoreX(y, x, z, delta, gamma, rho, Ng, G, W, P, Q, C, M, R, coeff.inv)
         score <- colSums(S)

         invI <- MASS::ginv(t(S) %*% S)
         g1 <- gmat1(delta)
         g3 <- gmat3(rho)

         tmp_delta <- diag(t(g1) %*% invI[1:(W - 1), 1:(W - 1)] %*% g1)
         tmp_delta[tmp_delta < 0] = 0
         std.err$delta <- sqrt(tmp_delta)
         tmp_beta <- diag(invI[W:(W - 1 + (W * P + Q) * (C - 1)),
                               W:(W - 1 + (W * P + Q) * (C - 1))])
         tmp_beta[tmp_beta < 0] = 0
         std.err$beta <- sqrt(tmp_beta)

         std.err$rho = list()
         tmp_rho = diag(t(g3) %*% invI[(W + (W * P + Q) * (C - 1)):ncol(invI),
                      (W + (W * P + Q) * (C - 1)):ncol(invI)] %*% g3)
         tmp_rho[tmp_rho < 0] = 0
         tmp_rho = sqrt(tmp_rho)

         for (m in 1:M) {
            std.err$rho[[m]] = matrix(
               tmp_rho[(sum(R[1:m]) * C - R[m] * C + 1):(sum(R[1:m]) * C)],
               C, R[m], byrow = TRUE
            )
         }
      }
   }

   return(list(score = score, std.err = std.err))
}
