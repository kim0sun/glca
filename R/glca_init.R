glca_init <- function(model)
{
   prob_gnr <- function(n) {
      ran <- stats::runif(n)
      return(ran / sum(ran))
   }

   Ng <- model$Ng; G <- model$G
   C <- model$C; W <- model$W
   M <- model$M; R <- model$R
   P <- model$P; Q <- model$Q
   coeff.inv <- model$coeff.inv

   init = list()

   if (W == 0L) {
      if (P == 1L) {
         init$gamma <- lapply(1L:G, function(g)
            matrix(1L/C, Ng[g], C))
         init$rho   <- lapply(1L:G, function(g) lapply(1L:M, function(x)
            do.call(rbind, lapply(1L:C, function(c) prob_gnr(R[x])))))
      } else {
         init$beta <- lapply(1L:G, function(g) matrix(0L, P, C - 1L))
         init$rho  <- lapply(1L:G, function(g) lapply(1L:M, function(x)
            do.call(rbind, lapply(1L:C, function(c) prob_gnr(R[x])))))
      }
   } else {
      if (P == 1L && Q == 0L) {
         init$delta <- rep(1L, W) / W;
         init$gamma <- do.call(rbind, lapply(1L:W, function(w)
         {
            ran = rep(1L, C) + stats::runif(C, -0.5, + 0.5)
            return(ran / sum(ran))
         }))
         init$rho   <- lapply(1L:M, function(x)
            do.call(rbind, lapply(1L:C, function(c) prob_gnr(R[x]))))
      } else {
         init$delta <- rep(1L, W) / W;
         if (coeff.inv)
            init$beta  <-
               c(stats::rnorm(W * (C - 1L), 0L, 0.1) %x% c(1L, numeric(P - 1L)),
                 numeric(Q * (C - 1L)))
         else
            init$beta  <- stats::rnorm((W * P + Q) * (C - 1L), 0L, 0.1)
         init$rho   <- lapply(1L:M, function(x)
            do.call(rbind, lapply(1L:C, function(c) prob_gnr(R[x]))))
      }
   }

   return(init)
}
