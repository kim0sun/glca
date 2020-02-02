glca_encode <- function(
   mf, data, nclass, ncluster,
   measure_inv, verbose
)
{
   # Import data
   Y <- stats::model.response(mf)
   if (is.null(dim(Y)))
      dim(Y) = c(length(Y), 1)
   if (class(Y) != "items")
      stop("Manifest items should be indicated by item function.\n")

   dataN = nrow(data)
   modelN = nrow(mf)
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
   N <- nrow(Y) - length(totmis)
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

   cat("Among original", dataN, "observations,",
       "\nAt least 1 covariate missed :", dataN - modelN, "observation(s) and",
       "\nAll item missed :", length(totmis), "observation(s) are deleted.\n\n")
   if (length(totmis) > 0)
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

   if (G == 1)
      W <- 0
   else if (G <= ncluster) {
      if (verbose)
         cat("Number of latent clusters should be less than number of groups.\n
              MGLCA will be fitted.")
      W <- 0
   } else {
      W <- ncluster
      measure_inv <- TRUE
   }

   C <- nclass
   npatt <- prod(R)
   if (npatt < 1e+6) {
      pattern <- as.matrix(expand.grid(lapply(1:M, function(m) 1:R[m])))
      obsvd <- ObsCell(as.matrix(Y), N, M, R, 1000, 1e-8)
   } else {
      Y0 <- Y[rowSums(Y == 0) == 0,]
      Y.sorted <- Y0[do.call(order, data.frame(Y0)),]
      pattern <- as.matrix(unique(Y.sorted))
      obsvd <- ObsCell2(as.matrix(Y.sorted), pattern,
                        nrow(Y.sorted), nrow(pattern))
   }
   dimnames(pattern) = list(NULL, y.names)

   if (W == 0) {
      if (P == 1) {
         if (G == 1)
            type <- "Standard LCA"
         else
            type <- "Multigroup LCA"
         if (measure_inv)
            npar <- G * (C - 1) + C * sum(R - 1)
         else
            npar <- G * (C - 1) + G * C * sum(R - 1)
      } else {
         if (G == 1)
            type <- "Standard LCA with Covariates"
         else
            type <- "Multigroup LCA with Covariates"
         if (measure_inv)
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
                           observed = obsvd),
           model = list(type = type,
                        measure_inv = measure_inv,
                        N = N, Ng = Ng, G = G,
                        C = C, W = W, M = M, R = R,
                        P = P, Q = Q,
                        npar = npar,
                        df = min(nrow(unique(Y)), npatt - 1) - npar),
           vname = list(y.names = y.names,
                        g.names = g.names,
                        r.names = r.names,
                        x.names = x.names,
                        z.names = z.names,
                        resp.name = resp.name))
   )
}
