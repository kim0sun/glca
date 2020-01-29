glca <- function(
   formula, group = NULL, data,
   nclass = 3, ncluster = 0,
   measure_inv = TRUE, std_err = TRUE,
   init_param = NULL, n_init = 1,
   maxiter = 1000, eps = 1e-10,
   verbose = TRUE
)
{
   # Function call
   call <- match.call()
   mf <- match.call(expand.dots = FALSE)
   m <- match(c("formula", "group", "data"), names(mf), 0L)
   mf <- mf[c(1L, m)]
   mf[[1L]] <- quote(stats::model.frame)
   mf <- eval(mf, parent.frame())

   # Ecoding arguments (model, datalist, vname)
   # (type, N, Ng, G, C, W, M, R, P, Q, npar)
   # (x, y, z, pattern, observed)
   # (y.names, g.names, r.names, x.names, z.names)
   encode = glca_encode(mf, data, nclass, ncluster,
                        measure_inv, verbose)
   datalist = encode$datalist
   model = encode$model
   if(model$df <= 0) {
      if (verbose) cat("Warning: Negative degree of freedom.")
      std_err = FALSE
   }
   vname = encode$vname

   # Initial parameter (init)
   # (delta, gamma, beta, rho)
   if (!is.null(init_param)) {
      n_init = 1
      init_random = glca_init(model)
      init = glca_init_test(init_param, init_random, verbose)
   } else {
      init = glca_em_test(model, datalist, n_init, verbose)
   }

   # EM iteration (param, posterior, fitted, loglike)
   # (delta, gamma, beta, rho)
   # (Post type differ)
   EM = glca_em(model, datalist, init, verbose, maxiter, eps)

   # Score & Std.error calculation (score, std.err)
   if (std_err)
      scores = glca_score(model, datalist, EM$param, EM$posterior)
   else
      scores = NULL

   # Output Design (output)
   # (model, x, y, group, z, param, posterior, gof, iter)
   ret = glca_output(call, model, datalist, vname, EM, scores)
   class(ret) = "glca"

   return(ret)
}
