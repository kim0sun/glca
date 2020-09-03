glca_init_test <- function(
   init.param, init.random, verbose
)
{
   if (is.null(init.param))
      return(init.random)

   init = list()
   pars = names(init.random)

   if (is.matrix(init.param$gamma) && is.null(init.param$delta))
      init.param$gamma = lapply(1L:nrow(init.param$gamma), function(g)
         matrix(init.param$gamma[g, ], nrow(init.random$gamma[[g]]),
                ncol(init.param$gamma), byrow = TRUE))

   for (par in pars) {
      cust = getElement(init.param, par)
      rand = getElement(init.random, par)
      if (par != "rho") {
         cust = list(cust)
         rand = list(rand)
      }

      str1 = unname(lapply(rapply(cust, function(x)
         if(is.matrix(x)) dim(unname(x)) else length(unname(x)),
         how = "replace"), unname))
      str2 = rapply(rand, function(x)
         if(is.matrix(x)) dim(unname(x))
         else length(unname(x)), how = "replace")
      identical(str1, str2)
      if (!identical(str1, str2, ignore.environment = TRUE)) {
         if (verbose) cat(par, "is randomly generated.\n")
         init[[par]] = getElement(init.random, par)
      } else {
         if (verbose) cat(par, "is user-defined.\n")
         init[[par]] = getElement(init.param, par)
      }
   }
   if (verbose) cat("\n")

   return(init)
}


