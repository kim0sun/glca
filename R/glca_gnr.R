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

   if (W > 1L) {
      if (is.null(beta)) {
         gamma = lapply(1L:G, function(g)
            lapply(1L:W, function(w)
               matrix(gamma[w, ], Ng[g], C, byrow = TRUE)))
      }
      class2 = sample(1L:W, G, prob = delta,
                       replace = TRUE)
      class = lapply(1L:G, function(g) sapply(1L:Ng[g], function(i)
            sample(1L:C, 1L, prob = gamma[[g]][[class2[g]]][i, ])))
      y = lapply(1L:G, function(g) sapply(1L:M, function(m)
         sapply(class[[g]], function(c)
            sample(1L:R[m], 1L, prob = rho[[m]][c, ]))))
      for (g in 1L:G)
         y[[g]][datalist$y[[g]] == 0L] = 0L
   } else {
      # MGLCA with constraint
      if (P == 1L)
         class = lapply(1L:G, function(g) sapply(1L:Ng[g], function(i)
            sample(1L:C, 1L, prob = gamma[g, ])))
      else {
         class = lapply(1L:G, function(g) sapply(1L:Ng[g], function(i)
            sample(1L:C, 1L, prob = gamma[[g]][i, ])))
      }
      y = lapply(1L:G, function(g) sapply(1L:M, function(m)
         sapply(class[[g]], function(c)
            sample(1L:R[m], 1L, prob = rho[[g]][[m]][c, ]))))

      for (g in 1L:G)
         y[[g]][datalist$y[[g]] == 0L] = 0L
   }

   if (prod(R) < 1e+6 & sum(sapply(y, function(x) sum(x == 0L))) != 0L) {
      loglikg <- numeric(G)
      for (g in 1L:G) {
         uniqx <- unique(datalist$x[[g]])
         xind <- match(data.frame(t(datalist$x[[g]])), data.frame(t(uniqx)))
         I <- nrow(uniqx)
         logliki <- numeric(I)
         for (i in 1L:I) {
            yi = y[[g]][xind == i, , drop = FALSE]
            logliki[i] <- ObsLik(as.matrix(yi), nrow(yi), M, R, 1000L, 1e-8)
         }
         loglikg[g] <- sum(logliki)
      }
      loglik0 <- sum(loglikg)
      Y = do.call(rbind, y)
      nullik0 <- ObsLik(as.matrix(Y), sum(Ng), M, R, 1000L, 1e-8)
   } else {
      loglikg <- numeric(G)
      for (g in 1L:G) {
         uniqx <- unique(datalist$x[[g]])
         xind <- match(data.frame(t(datalist$x[[g]])), data.frame(t(uniqx)))
         y0 <- y[[g]][rowSums(y[[g]] == 0L) == 0L, , drop = FALSE]
         x0 <- xind[rowSums(y[[g]] == 0L) == 0L]
         I <- nrow(uniqx)
         logliki <- numeric(I)
         for (i in 1L:I) {
            yi <- y0[x0 == i, , drop = FALSE]
            y.sorted <- yi[do.call(order, data.frame(yi)[M:1L]), , drop = FALSE]
            pattern <- as.matrix(unique(y.sorted))
            obsvd <- ObsCell2(as.matrix(y.sorted), pattern,
                              nrow(y.sorted), nrow(pattern))
            logliki[i] <- sum(obsvd * log(obsvd / sum(obsvd)))
         }
         loglikg[g] <- sum(logliki)
      }
      loglik0 <- sum(loglikg)
      Y = do.call(rbind, y)
      Y0 <- Y[rowSums(Y == 0L) == 0L,]
      Y.sorted <- Y0[do.call(order, data.frame(Y0)),]
      pattern <- as.matrix(unique(Y.sorted))
      obsvd <- ObsCell2(as.matrix(Y.sorted), pattern,
                        nrow(Y.sorted), nrow(pattern))
      nullik0 <- sum(obsvd * log(obsvd / sum(obsvd)))
   }

   return(list(y = y, x = datalist$x, z = datalist$z,
               loglik0 = loglik0, nullik0 = nullik0))
}
