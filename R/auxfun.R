# Constarint matrix form
gmat1 <- function(d) matrix((diag(d) - d %*% t(d))[, -length(d)], length(d) - 1)
gmat2 <- function(g) {
   W <- nrow(g)
   C <- ncol(g)
   mat <- matrix(0, W * (C - 1), W * C)

   for(w in 1:W) {
      for(c in 1:C - 1) {
         mat[((w - 1) * (C - 1) + 1):(w * (C - 1)),
             ((w - 1) * C + 1):(w * C)] <-
            (diag(g[w, ]) - g[w, ] %*% t(g[w, ]))[-C, ]
      }
   }

   return(mat)
}
gmat3 <- function(r) {
   C <- nrow(r[[1]])
   R <- sapply(r, ncol)
   M <- length(r)
   mat <- matrix(0, C * sum(R - 1), C * sum(R))
   row <- 1; col <- 1

   for(m in 1:M) {
      for(c in 1:C) {
         mat[row:(row + R[m] - 2), col:(col + R[m] - 1)] <-
            (diag(r[[m]][c,]) - r[[m]][c,] %*% t(r[[m]][c,]))[, -R[m]]
         row <- row + R[m] - 1
         col <- col + R[m]
      }
   }

   return(mat)
}

