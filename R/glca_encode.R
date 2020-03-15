glca_encode <- function(
   call, mf, data, nclass, ncluster,
   measure.inv, na.rm, verbose
)
{
   # Import data
   Y <- stats::model.response(mf)
   if (is.null(dim(Y)))
      dim(Y) <- c(length(Y), 1)
   if (class(Y) != "items")
      stop("Manifest items should be indicated by item function.\n")

   dataN <- nrow(data)
   modelN <- nrow(mf)

   isna <- which(rowSums(Y == 0) > 0)
   totmis <- which(rowSums(Y != 0) == 0)

   Cov <- stats::model.matrix(stats::terms(mf), mf)
   grp <- stats::model.extract(mf, "group")

   if (is.null(grp) || ncol(Cov) == 1) {
      Zind <- rep(FALSE, ncol(Cov))
   }
   else {
      Zind <- c(FALSE, colSums(apply(Cov[, -1, drop = FALSE], 2, function(x)
         by(x, grp, function(y) length(unique(y))))) == length(unique(grp)))
   }

   X <- Cov[, !Zind, drop = FALSE]
   Z <- Cov[,  Zind, drop = FALSE]

   if (is.null(grp))
      grp <- factor(rep("ALL", nrow(Y)))
   else
      grp <- droplevels(factor(grp))

   # Model / variable name specification
   if (na.rm) N <- nrow(Y) - length(isna)
   else N <- nrow(Y) - length(totmis)
   M <- ncol(Y)
   R <- sapply(attr(Y, "y.level"), length)
   P <- ncol(X)
   Q <- ncol(Z)
   G <- nlevels(grp)

   y.names <- attr(Y, "y.names")
   r.names <- attr(Y, "y.level")
   resp.name <- data.frame(
      matrix("", M, max(R)), row.names = y.names,
      stringsAsFactors = FALSE
   )
   names(resp.name) <- paste0("Y = ", 1:max(R))
   for (m in 1:M)
      resp.name[m, 1:R[m]] <- c(r.names[[m]])
   x.names <- colnames(X)
   g.names <- levels(grp)
   z.names <- colnames(Z)

   if (verbose) {
      cat("Deleted observation(s) : \n")
      if (na.rm)
         cat(dataN - modelN + length(isna),
             "observation(s) for missing at least 1 variable \n\n")
      else
         cat(length(totmis), "observation(s) for missing all manifest items\n",
             dataN - modelN, "observation(s) for missing at least 1 covariates\n\n")
   }

   if (na.rm & length(isna) > 0) {
      Y <- Y[-isna, , drop = FALSE]
      X <- X[-isna, , drop = FALSE]
      Z <- Z[-isna, , drop = FALSE]
      grp <- droplevels(grp[-isna])
   } else if (length(totmis) > 0)
   {
      Y <- Y[-totmis, , drop = FALSE]
      X <- X[-totmis, , drop = FALSE]
      Z <- Z[-totmis, , drop = FALSE]
      grp <- droplevels(grp[-totmis])
   }

   # Grouping data
   grp <- as.numeric(grp)
   Ng <- sapply(1:G, function(g) sum(grp == g))
   y <- lapply(1:G, function(g) as.matrix(Y[grp == g, , drop = FALSE]))
   x <- lapply(1:G, function(g) as.matrix(X[grp == g, , drop = FALSE]))
   z <- lapply(1:G, function(g) as.matrix(Z[grp == g, , drop = FALSE]))

   if (nclass < 2)
      stop("Number of latent classes should be greater than 1.")

   if (ncluster > 0) {
      if (G == 1)
         W <- 0
      else if (G <= ncluster) {
         if (verbose)
            cat("Number of latent clusters should be less than number of groups.\n
              MGLCA will be fitted.")
         W <- 0
      } else {
         W <- floor(ncluster)
         measure.inv <- TRUE
      }
   } else {
      W <- 0
   }

   C <- floor(nclass)
   npatt <- prod(R) * G

   if (npatt < 1e+6) {
      pattern <- as.matrix(expand.grid(
         c(lapply(1:M, function(m) 1:R[m]), list(1:G))))
      obsvd <- ObsCell(as.matrix(cbind(Y, grp)),
                       N, M + 1, c(R, G), 1000, 1e-8)

      loglikg <- numeric(G)
      for (g in 1:G)
         loglikg[[g]] <- ObsLik(as.matrix(Y[grp == g, ]),
                                Ng[g], M, R, 1000, 1e-8)

      loglik0 <- sum(loglikg)
   } else {
      Y0 <- Y[rowSums(Y == 0) == 0,]
      g0 <- grp[rowSums(Y == 0) == 0]
      Yg <- cbind(Y0, g0)
      Y.sorted <- Yg[do.call(order, data.frame(Yg)[(M + 1):1]),]
      pattern <- as.matrix(unique(Y.sorted))
      obsvd <- ObsCell2(as.matrix(Y.sorted), pattern,
                        nrow(Y.sorted), nrow(pattern))
      loglikg <- numeric(G)
      for (g in 1:G) {
         tmp = obsvd[Y.sorted[, M + 1] == g]
         loglikg[[g]] <- sum(tmp * log(tmp / sum(tmp)))
      }

      loglik0 <- sum(loglikg)
   }
   dimnames(pattern) = list(NULL, c(y.names, deparse(call$group)))

   if (W == 0) {
      if (P == 1) {
         if (G == 1)
            type <- "Standard LCA"
         else
            type <- "Multigroup LCA"
         if (measure.inv)
            npar <- G * (C - 1) + C * sum(R - 1)
         else
            npar <- G * (C - 1) + G * C * sum(R - 1)
      } else {
         if (G == 1)
            type <- "Standard LCA with Covariates"
         else
            type <- "Multigroup LCA with Covariates"
         if (measure.inv)
            npar <- G * (C - 1) * P + C * sum(R - 1)
         else
            npar <- G * (C - 1) * P + G * C * sum(R - 1)
      }
   } else {
      if (P == 1) {
         type <- "Multilevel LCA"
         npar <- W - 1 + W * (C - 1) + C * sum(R - 1)
      } else {
         type <- "Multilevel LCA with Covariates"
         npar <- W - 1 + (W * P + Q) * (C - 1) + C * sum(R - 1)
      }
   }

   return(
      list(datalist = list(y = y, x = x, z = z, group = grp,
                           pattern = pattern,
                           observed = obsvd,
                           loglik0 = loglik0),
           model = list(type = type,
                        measure.inv = measure.inv,
                        N = N, Ng = Ng, G = G,
                        C = C, W = W, M = M, R = R,
                        P = P, Q = Q,
                        npar = npar,
                        df = min(N, npatt - 1) - npar),
           vname = list(y.names = y.names,
                        g.names = g.names,
                        r.names = r.names,
                        x.names = x.names,
                        z.names = z.names,
                        resp.name = resp.name))
   )
}
