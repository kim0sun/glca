glca_gnr <- function(
   model, param, datalist
)
{
   Ng <- model$Ng; G <- model$G
   C <- model$C; W <- model$W
   M <- model$M; R <- model$R
   P <- model$P; Q <- model$Q

   delta <- param$delta
   gamma <- param$gamma
   beta  <- param$beta
   rho   <- param$rho

   if (W > 1) {
      if (is.null(beta)) {
         gamma = lapply(1:G, function(g)
            lapply(1:W, function(w)
               matrix(gamma[w, ], Ng[g], C, byrow = TRUE)))
      }
      # MLCA with constraint
      class2 = sample(1:W, G, prob = delta,
                       replace = TRUE)
      class = lapply(1:G, function(g) sapply(1:Ng[g], function(i)
            sample(1:C, 1, prob = gamma[[g]][[class2[g]]][i, ])))
      y = lapply(1:G, function(g) sapply(1:M, function(m)
         sapply(class[[g]], function(c)
            sample(1:R[m], 1, prob = rho[[m]][c, ]))))
      for (g in 1:G)
         y[[g]][datalist$y[[g]] == 0] = 0
   } else {
      # MGLCA with constraint
      if (P == 1)
         class = lapply(1:G, function(g) sapply(1:Ng[g], function(i)
            sample(1:C, 1, prob = gamma[g, ])))
      else {
         class = lapply(1:G, function(g) sapply(1:Ng[g], function(i)
            sample(1:C, 1, prob = gamma[[g]][i, ])))
      }
      y = lapply(1:G, function(g) sapply(1:M, function(m)
         sapply(class[[g]], function(c)
            sample(1:R[m], 1, prob = rho[[g]][[m]][c, ]))))

      for (g in 1:G)
         y[[g]][datalist$y[[g]] == 0] = 0
   }

   npatt <- prod(R)
   if (npatt < 1e+6) {
      pattern <- expand.grid(c(lapply(1:M, function(m)
         1:R[m]), list(1:G)))
      obsvd <- unlist(lapply(1:G, function(g)
         ObsCell(cbind(y[[g]], g), Ng[g], M, R, 1000, 1e-8)))
      loglikg <- numeric(G)
      for (g in 1:G)
         loglikg[[g]] <- ObsLik(as.matrix(y[[g]]),
                                Ng[g], M, R, 1000, 1e-8)

      loglik0 <- sum(loglikg)
   } else {
      Y = do.call(rbind, lapply(1:G, function(g) cbind(y[[g]], g)))
      Y0 <- Y[rowSums(Y == 0) == 0,]
      Y.sorted <- Y0[do.call(order, data.frame(Y0)[(M + 1):1]),]
      pattern <- unique(Y.sorted)

      obsvd <- unlist(ObsCell2(as.matrix(Y.sorted), as.matrix(pattern),
                               nrow(Y.sorted), nrow(pattern)))
      loglikg <- numeric(G)
      for (g in 1:G) {
         tmp = obsvd[pattern[, M + 1] == g]
         loglikg[[g]] <- sum(tmp * log(tmp / sum(tmp)))
      }
      loglik0 <- sum(loglikg)
   }

   return(list(y = y, x = datalist$x, z = datalist$z,
               pattern = as.matrix(pattern), observed = obsvd,
               loglike = loglik0))
}
