glca_init <- function(model)
{
   prob_gnr <- function(n) {
      ran <- runif(n)
      return(ran / sum(ran))
   }

   Ng <- model$Ng; G <- model$G
   C <- model$C; W <- model$W
   M <- model$M; R <- model$R
   P <- model$P; Q <- model$Q

   init = list()

   if (W == 0) {
      if (P == 1) {
         init$gamma <- lapply(1:G, function(g)
            matrix(1/C, Ng[g], C))
         init$rho   <- lapply(1:G, function(g) lapply(1:M, function(x)
            do.call(rbind, lapply(1:C, function(c) prob_gnr(R[x])))))
      } else {
         init$beta <- lapply(1:G, function(g) matrix(0, P, C - 1))
         init$rho  <- lapply(1:G, function(g) lapply(1:M, function(x)
            do.call(rbind, lapply(1:C, function(c) prob_gnr(R[x])))))
      }
   } else {
      if (P == 1 && Q == 0) {
         init$delta <- rep(1, W) / W;
         init$gamma <- do.call(rbind, lapply(1:W, function(w)
         {
            ran = rep(1, C) + runif(C, -0.01, + 0.01)
            return(ran / sum(ran))
         }))
         init$rho   <- lapply(1:M, function(x)
            do.call(rbind, lapply(1:C, function(c) prob_gnr(R[x]))))
      } else {
         init$delta <- rep(1, W) / W;
         init$beta  <- rnorm((W * P + Q) * (C - 1), 0, 0.1)
         init$rho   <- lapply(1:M, function(x)
            do.call(rbind, lapply(1:C, function(c) prob_gnr(R[x]))))
      }
   }

   return(init)
}
