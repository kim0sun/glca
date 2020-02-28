glca_gnr <- function(
   model, param, datalist
)
{
   if (model$W > 1) {
      if (is.null(param$beta)) {
         param$gamma = lapply(1:model$G, function(g)
            lapply(1:model$W, function(w)
               matrix(param$gamma[w, ], model$Ng[g], model$C, byrow = TRUE)))
      }
      # MLCA with constraint
      class2 = sample(1:model$W, model$G, prob = param$delta,
                       replace = TRUE)
      class = lapply(1:model$G, function(g) sapply(1:model$Ng[g], function(i)
            sample(1:model$C, 1, prob = param$gamma[[g]][[class2[g]]][i, ])))
      y = lapply(1:model$G, function(g) sapply(1:model$M, function(m)
         sapply(class[[g]], function(c)
            sample(1:model$R[m], 1, prob = param$rho[[m]][c, ]))))
      for (g in 1:model$G)
         y[[g]][datalist$y[[g]] == 0] = 0
   } else {
      # MGLCA with constraint
      if (model$P == 1)
         class = lapply(1:model$G, function(g) sapply(1:model$Ng[g], function(i)
            sample(1:model$C, 1, prob = param$gamma[g, ])))
      else {
         class = lapply(1:model$G, function(g) sapply(1:model$Ng[g], function(i)
            sample(1:model$C, 1, prob = param$gamma[[g]][i, ])))
      }
      y = lapply(1:model$G, function(g) sapply(1:model$M, function(m)
         sapply(class[[g]], function(c)
            sample(1:model$R[m], 1, prob = param$rho[[g]][[m]][c, ]))))

      for (g in 1:model$G)
         y[[g]][datalist$y[[g]] == 0] = 0
   }

   npatt <- prod(model$R)
   if (npatt < 1e+6) {
      pattern <- expand.grid(lapply(1:model$M, function(m) 1:model$R[m]))
      obsvd <- sapply(1:model$G, function(g)
         ObsCell(y[[g]], model$Ng[g], model$M, model$R, 1000, 1e-8))
   } else {
      Y = do.call(rbind, y)
      Y0 <- Y[rowSums(Y == 0) == 0,]
      Y.sorted <- Y0[do.call(order, data.frame(Y0)),]
      pattern <- unique(Y.sorted)

      y.sorted <- lapply(1:model$G, function(g)
         y[[g]][do.call(order, data.frame(y[[g]]))])
      obsvd <- lapply(1:model$G, function(g)
         ObsCell2(as.matrix(y.sorted[[g]]), as.matrix(pattern),
                  model$Ng[g], nrow(pattern)))
   }

   if (!is.null(model$W)) {
      obsvd <- rowSums(obsvd)
   }

   llik0 = sum((obsvd * log(obsvd / sum(obsvd)))[obsvd != 0])

   return(list(y = y, x = datalist$x, z = datalist$z,
               pattern = as.matrix(pattern), observed = obsvd,
               loglike = llik0))
}
