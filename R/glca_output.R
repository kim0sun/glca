glca_output <- function(
   call, terms, model, datalist, vname, EM, scores
)
{
   N <- model$N; Ng <- model$Ng;
   G <- model$G
   C <- model$C; W <- model$W
   M <- model$M; R <- model$R
   P <- model$P; Q <- model$Q
   n <- model$n
   npar <- model$npar
   df <- model$df

   y.names <- vname$y.names
   r.names <- vname$r.names
   r.names <- list()
   for (m in 1:M) {
      r.names[[m]] = paste0("Y = ", 1:R[m])
   }
   g.names <- vname$g.names
   x.names <- vname$x.names
   z.names <- vname$z.names

   param <- EM$param
   posterior <- EM$posterior

   if (!is.null(scores))
      std.err <- scores$std.err

   # Parameter design
   if (W > 0) {
      names(param$delta) <- paste0("Cluster ", 1:W)
      if (!is.null(scores))
         names(std.err$delta) <- paste0("Cluster ", 1:W)


      if (P == 1 && Q == 0) {
         dimnames(param$gamma) <- list(paste0("Cluster ", 1:W),
                                       paste0("Class ", 1:C))
         if (!is.null(scores))
            dimnames(std.err$gamma) <- list(paste0("Cluster ", 1:W),
                                            paste0("Class ", 1:C))

      } else {
         beta1 <- list()
         se_b1 <- list()
         for (w in 1:W) {
            beta1[[w]] <- matrix(
               param$beta[((w - 1) * (C - 1) * P + 1):(w * (C - 1) * P)],
               P, C - 1)
            dimnames(beta1[[w]]) <-
               list(x.names, paste0("Class ", 1:(C - 1), "/", C))
            if (!is.null(scores)){
               se_b1[[w]] <- matrix(
                  std.err$beta[((w - 1) * (C - 1) * P + 1):(w * (C - 1) * P)],
                  P, C - 1)
               dimnames(se_b1[[w]]) <-
                  list(x.names, paste0("Class ", 1:(C - 1), "/", C))
            }
         }
         names(beta1) <- paste0("Cluster", 1:W)
         beta = list(Level1 = beta1)

         if (!is.null(scores)) {
            names(se_b1) <- paste0("Cluster", 1:W)
            se_b = list(Level1 = se_b1)
         }

         if (Q > 0) {
            beta$Level2 <- matrix(
               param$beta[(W * (C - 1) * P + 1):(W * (C - 1) * P + Q * (C - 1))],
               Q, C - 1)
            dimnames(beta$Level2) <-
               list(z.names, paste0("Class ", 1:(C - 1), "/", C))

            if (!is.null(scores)) {
               se_b$Level2 <- matrix(
                  std.err$beta[(W * (C - 1) * P + 1):(W * (C - 1) * P + Q * (C - 1))],
                  Q, C - 1)
               dimnames(se_b$Level2) <-
                  list(z.names, paste0("Class ", 1:(C - 1), "/", C))
            }
         }
         param$beta = beta;
         if (!is.null(scores))
            std.err$beta = se_b

         names(param$gamma) <- g.names
         for (g in 1:G) {
            names(param$gamma[[g]]) <-
               paste0("Cluster ", 1:W)
            for (w in 1:W)
               dimnames(param$gamma[[g]][[w]]) <- list(
                  1:Ng[g], paste0("Class ", 1:C)
            )
         }
      }

      names(param$rho) <- y.names
      if (!is.null(scores))
         names(std.err$rho) <- y.names

      for (m in 1:M) {
         dimnames(param$rho[[m]]) <-
            list(paste0("Class ", 1:C),
                 substr(r.names[[m]], 1, 10)
            )
         if (!is.null(scores))
            dimnames(std.err$rho[[m]]) <-
               list(paste0("Class ", 1:C),
                    substr(r.names[[m]], 1, 10)
               )
      }

   } else {
      if (P > 1) {
         names(param$beta) <- g.names
         if (!is.null(scores))
            names(std.err$beta) <- g.names
         for (g in 1:G) {
            dimnames(param$beta[[g]]) <-
               list(x.names, paste0("Class ", 1:(C - 1), "/", C))
            if (!is.null(scores))
               dimnames(std.err$beta[[g]]) <-
                  list(x.names, paste0("Class ", 1:(C - 1), "/", C))
         }
      } else {
         param$gamma <- t(sapply(param$gamma, colMeans))
         dimnames(param$gamma) <- list(
            g.names, paste0("Class ", 1:C)
         )
         if (!is.null(scores))
            dimnames(std.err$gamma) <- list(
               g.names, paste0("Class ", 1:C)
            )
      }
      names(param$rho) <- g.names
      for (g in 1:G) {
         names(param$rho[[g]]) <- y.names

         for (m in 1:M) {
            dimnames(param$rho[[g]][[m]]) <-
               list(paste0("Class ", 1:C),
                    substr(r.names[[m]], 1, 10)
            )
         }
      }

      if (!is.null(scores)) {
         names(std.err$rho) <- g.names
         for (g in 1:G) {
            names(std.err$rho[[g]]) <- y.names

            for (m in 1:M) {
               dimnames(std.err$rho[[g]][[m]]) <-
                  list(paste0("Class ", 1:C),
                       substr(r.names[[m]], 1, 10)
                  )
            }
         }
      }
   }

   # Coefficient design
   if (!is.null(param$beta)) {
      coeff = list()
      if (W > 1) {
         coeff$Level1 = list()
         for (w in 1:W) {
            coeff[[1]][[w]] = list()
            for (c in 1:(C - 1)) {
               coeff[[1]][[w]][[c]] = data.frame(
                  exp(param$beta[[1]][[w]][, c]),
                  b <- param$beta[[1]][[w]][, c],
                  row.names = x.names
               )
               colnames(coeff[[1]][[w]][[c]]) =
                  c("Odds Ratio", "Coefficient")

               if (!is.null(scores)) {
                  coeff[[1]][[w]][[c]] = cbind(
                     coeff[[1]][[w]][[c]],
                     se <- std.err$beta[[1]][[w]][, c],
                     tval <- b / se,
                     2 * stats::pt(abs(tval), df, lower.tail = FALSE)
                  )
                  colnames(coeff[[1]][[w]][[c]]) =
                     c("Odds Ratio", "Coefficient", " Std. Error",
                       " t value", " Pr(>|t|)")
               }
            }
            names(coeff[[1]][[w]]) = paste0("Class", 1:(C - 1), "/", C)
         }
         names(coeff[[1]]) = paste0("Cluster", 1:W)
         if (Q > 0) {
            coeff$Level2 = list()
            for (c in 1:(C - 1)) {
               coeff[[2]][[c]] <- data.frame(
                  exp(param$beta[[2]][, c]),
                  b <- param$beta[[2]][, c],
                  row.names = z.names
               )
               colnames(coeff[[2]][[c]]) <-
                  c("Odds Ratio", "Coefficient")

               if (!is.null(scores)) {
                  coeff[[2]][[c]] <- cbind(
                     coeff[[2]][[c]],
                     se <- std.err$beta[[2]][, c],
                     tval <- b / se,
                     2 * stats::pt(abs(tval), df, lower.tail = FALSE)
                  )
                  colnames(coeff[[2]][[c]]) <-
                     c("Odds Ratio", "Coefficient", " Std. Error",
                       " t value", " Pr(>|t|)")
               }
            }
            names(coeff[[2]]) <- paste0("Class", 1:(C - 1), "/", C)
         }

      } else {
         for (g in 1:G) {
            coeff[[g]] <- list()
            for (c in 1:(C - 1)) {
               coeff[[g]][[c]] <- data.frame(
                  exp(param$beta[[g]][, c]),
                  b <- param$beta[[g]][, c],
                  row.names = x.names
               )
               colnames(coeff[[g]][[c]]) <-
                  c("Odds Ratio", "Coefficient")

               if (!is.null(scores)) {
                  coeff[[g]][[c]] <- cbind(
                     coeff[[g]][[c]],
                     se <- std.err$beta[[g]][, c],
                     tval <- b / se,
                     2 * stats::pt(abs(tval), df, lower.tail = FALSE)
                  )
                  colnames(coeff[[g]][[c]]) <-
                     c("Odds Ratio", "Coefficient", " Std. Error",
                       " t value", " Pr(>|t|)")
               }
            }
            names(coeff[[g]]) <- paste0("Class", 1:(C - 1), "/", C)
         }
         names(coeff) <- g.names
      }
   } else {
      coeff = NULL
   }

   # Posterior design
   post = list()
   if (W > 0) {
      post$cluster <- data.frame(
         posterior$PostW / rowSums(posterior$PostW),
         row.names = vname$g.names
      )
      names(post$cluster) <- paste0("Cluster ", 1:W)
      post$wclass <- data.frame(
         posterior$PostWC / rowSums(posterior$PostWC),
         row.names = paste0("Cluster ", 1:W)
      )
      names(post$wclass) <- paste0("Class ", 1:C)

      pclass <- posterior$PostC
      post$class <- lapply(pclass, data.frame)
      names(post$class) <- g.names
      for (g in 1:G)
         names(post$class[[g]]) <- paste0("Class ", 1:C)
   } else {
      pclass <- posterior
      post <- lapply(pclass, data.frame)
      names(post) = g.names
      for (g in 1:G)
         names(post[[g]]) <- paste0("Class ", 1:C)
   }

   # Goodness of fit
   P = do.call(rbind, pclass)
   gof <- list(
      loglik = EM$loglik,
      aic = -2 * EM$loglik + 2 * npar,
      caic = -2 * EM$loglik + (log(N) + 1) * npar,
      bic = -2 * EM$loglik + log(N) * npar,
      entropy = 1 - sum(-P[P != 0] * log(P)[P != 0]) / (N * log(C)),
      df = df,
      Gsq = 2 * (datalist$loglik0 - EM$loglik)
   )

   # Convergence
   convergence <- list(
      niter = EM$niter,
      converged = EM$converged
   )
   if (!is.null(scores))
      convergence$score = scores$score

   ret <- list()
   ret$call <- call
   ret$terms <- terms
   ret$model <- model
   ret$var.names <- vname
   ret$datalist <- datalist
   ret$param <- param
   if (!is.null(scores))
      ret$std.err <- std.err
   ret$coefficient <- coeff
   ret$posterior <- post
   ret$gof <- gof
   ret$convergence <- convergence

   return(ret)
}
