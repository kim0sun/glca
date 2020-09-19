glca_encode <- function(
   call, terms, mf,
   nclass, ncluster,
   measure.inv, coeff.inv,
   na.rm, verbose
)
{
   # Import data
   Y <- stats::model.response(mf)
   Yattr <- attributes(Y)
   if (is.null(dim(Y)))
      dim(Y) <- c(length(Y), 1L)
   if (class(Y) != "items")
      stop("Manifest items should be indicated by item function.\n")

   grp <- stats::model.extract(mf, "group")
   if (is.null(grp)) grp <- factor(rep("ALL", nrow(Y)))
   else grp <- droplevels(factor(grp))

   dataN <- Yattr$dataN
   modelN <- nrow(mf)
   isna <- which(rowSums(Y == 0L) >  0L)
   tmis <- which(rowSums(Y != 0L) == 0L)

   if (na.rm & length(isna) > 0L) {
      Y <- Y[-isna, , drop = FALSE]
      mf <- mf[-isna, , drop = FALSE]
      grp <- droplevels(grp[-isna])
   } else if (length(tmis) > 0L) {
      Y <- Y[-tmis, , drop = FALSE]
      mf <- mf[-tmis, , drop = FALSE]
      grp <- droplevels(grp[-tmis])
   }

   lbs <- labels(terms)
   if (length(lbs) > 0L) cvs <- stats::get_all_vars(stats::reformulate(lbs), mf)
   else cvs <- NULL

   # Model / variable name specification
   N <- nrow(Y)
   G <- nlevels(grp)
   M <- ncol(Y)
   R <- sapply(Yattr$y.level, length)

   if (nclass < 2L)
      stop("Number of latent classes should be greater than 1.")
   if (!is.null(ncluster)) {
      W <- ncluster
      if (W < 2L) {
         if (verbose)
            cat("Number of latent clusters should be less than number of groups.\nmgLCA will be fitted.\n\n")
         W <- 0L
      } else if (G <= ncluster) {
         if (verbose)
            cat("Number of latent clusters should be less than number of groups.\nmgLCA will be fitted.\n\n")
         W <- 0L
      } else {
         W <- floor(ncluster)
         measure.inv <- TRUE
      }
   } else W <- 0L
   C <- floor(nclass)

   y.names <- Yattr$y.names
   r.names <- Yattr$y.level
   resp.name <- data.frame(
      matrix("", M, max(R)), row.names = y.names,
      stringsAsFactors = FALSE
   )
   names(resp.name) <- paste0("Y = ", seq_len(max(R)))
   for (m in seq_len(M))
      resp.name[m, seq_len(R[m])] <- c(r.names[[m]])

   if (!is.null(cvs)) {
      cvs <- stats::get_all_vars(stats::reformulate(lbs), mf)
      Zind <- apply(cvs, 2L, function(x)
         all(by(x, grp, function(y) length(unique(y)) == 1L)))
      if (sum(!Zind) == 0) X <- stats::model.matrix(~ 1, mf)
      else X <- stats::model.matrix(stats::reformulate(lbs[!Zind]), mf)
      if (any(Zind))
         Z <- stats::model.matrix(stats::reformulate(lbs[ Zind]), mf)[, -1L, drop = FALSE]
      else if (W > 1) Z <- mf[FALSE]
      else Z <- NULL
   } else {
      X <- stats::model.matrix(terms, mf)
      Z <- if (W > 1) Z <- mf[FALSE] else NULL
   }
   P <- ncol(X)
   Q <- if (!is.null(Z)) ncol(Z) else 0L

   # Covariate names
   X.names <- if (!is.null(cvs)) lbs[!Zind] else NULL
   Z.names <- if (!is.null(cvs)) lbs[ Zind] else NULL
   x.names <- colnames(X)
   z.names <- colnames(Z)
   g.names <- levels(grp)

   # Grouping data
   grp <- as.numeric(grp)
   Ng <- sapply(seq_len(G), function(g) sum(grp == g))
   y <- lapply(seq_len(G), function(g) as.matrix(Y[grp == g, , drop = FALSE]))
   x <- lapply(seq_len(G), function(g) as.matrix(X[grp == g, , drop = FALSE]))
   if (W > 1L)
      z <- lapply(seq_len(G), function(g) as.matrix(Z[grp == g, , drop = FALSE]))
   else z <- NULL

   # Variable print
   if (verbose) {
      cat("Manifest items :\n", y.names, "\n")
      if (!is.null(call$group)) cat("Grouping variable :", call$group, "\n")
      if (W > 1L) {
         if (Q > 0L) cat("Covariates (Level 2) : \n", Z.names, "\n")
         if (P > 1L) cat("Covariates (Level 1) : \n", X.names, "\n")
      } else if (P > 1L) cat("Covariates : \n", X.names, "\n")

      cat("\nDeleted observation(s) : \n")
      if (na.rm)
         cat(dataN - modelN + length(isna),
             "observation(s) for missing at least 1 variable \n\n")
      else {
         cat(length(tmis), "observation(s) for missing all manifest items\n")
         cat(dataN - modelN, "observation(s) for missing at least 1 covariates\n\n")
      }
   }

   # Satuated model
   grpx <- cbind(grp, X)
   uniqH <- unique(grpx)
   hind <- match(data.frame(t(grpx)), data.frame(t(uniqH)))
   H <- nrow(uniqH)

   fulldf <- prod(R) * H

   if (prod(R) < 1e+6 & na.rm != TRUE & length(isna) != 0L) {
      loglikh <- numeric(H)
      for (h in seq_len(H)) {
         Yh <- Y[hind == h, , drop = FALSE]
         loglikh[h] <- ObsLik(as.matrix(Yh), nrow(Yh), M, R, 1000, 1e-8)
      }
      loglik0 <- sum(loglikh)
   } else {
      Y0 <- Y[rowSums(Y == 0L) == 0L, , drop = FALSE]
      h0 <- hind[rowSums(Y == 0L) == 0L]
      loglikh <- numeric(H)
      for (h in seq_len(H)) {
         Yh <- Y0[hind == h, , drop = FALSE]
         Y.sorted <- Yh[do.call(order, data.frame(Yh)[M:1]), , drop = FALSE]
         pattern <- as.matrix(unique(Y.sorted))
         obsvd <- ObsCell2(as.matrix(Y.sorted), pattern,
                           nrow(Y.sorted), nrow(pattern))
         loglikh[h] <- sum(obsvd * log(obsvd / sum(obsvd)))
      }
      loglik0 <- sum(loglikh)
   }

   if (W == 0L) {
      if (P == 1L) {
         if (G == 1L)
            type <- "Standard LCA"
         else
            type <- "Multigroup LCA"
         if (measure.inv)
            npar <- G * (C - 1L) + C * sum(R - 1L)
         else
            npar <- G * (C - 1L) + G * C * sum(R - 1L)
      } else {
         if (G == 1L)
            type <- "Standard LCA with Covariates"
         else
            type <- "Multigroup LCA with Covariates"
         if (coeff.inv)
            npar <- G * (C - 1L) + (C - 1L) * (P - 1L)
         else
            npar <- G * (C - 1L) * P
         if (measure.inv)
            npar <- npar + C * sum(R - 1L)
         else
            npar <- npar + G * C * sum(R - 1L)
      }
   } else {
      if (P == 1L && Q == 0) {
         type <- "Multilevel LCA"
         npar <- W - 1L + W * (C - 1L) + C * sum(R - 1L)
      } else {
         type <- "Multilevel LCA with Covariates"
         if (coeff.inv)
            npar <- W - 1L + W * (C - 1L) + (P - 1L + Q) * (C - 1L) +
               C * sum(R - 1L)
         else
            npar <- W - 1L + (W * P + Q) * (C - 1L) + C * sum(R - 1L)
      }
   }

   return(
      list(datalist = list(y = y, x = x, z = z, group = grp,
                           loglik0 = loglik0),
           model = list(type = type,
                        measure.inv = measure.inv,
                        coeff.inv = coeff.inv,
                        N = N, Ng = Ng, G = G,
                        C = C, W = W, M = M, R = R,
                        P = P, Q = Q,
                        npar = npar,
                        df = min(N - 1L, fulldf - 1L) - npar),
           vname = list(y.names = y.names,
                        g.names = g.names,
                        r.names = r.names,
                        X.names = X.names,
                        Z.names = Z.names,
                        x.names = x.names,
                        z.names = z.names,
                        resp.name = resp.name))
   )
}
