glca_init_test <- function(
   init_param, init_random, verbose
)
{
   if (is.null(init_param))
      return(init_random)

   init = list()
   pars = names(init_random)

   for (par in pars) {
      cust = getElement(init_param, par)
      rand = getElement(init_random, par)

      str1 = unname(lapply(rapply(list(cust), function(x)
         if(is.matrix(x)) dim(x) else length(x),
         how = "replace"), unname))
      str2 = rapply(list(rand), function(x)
         if(is.matrix(x)) dim(x)
         else length(x), how = "replace")
      identical(str1, str2)
      if (!identical(str1, str2, ignore.environment = TRUE)) {
         if (verbose) cat(par, "is randomly generated.\n")
         init[[par]] = getElement(init_random, par)
      } else {
         if (verbose) cat(par, "is user-defined.\n")
         init[[par]] = getElement(init_param, par)
      }
   }
   if (verbose) cat("\n")

   return(init)
}


