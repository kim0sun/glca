#' @method logLik glca
#' @export

logLik.glca <- function(object, ...)
{
   val <- object$gof$loglik
   attr(val, "nall") <- object$model$N
   attr(val, "nobs") <- object$model$N
   attr(val, "df") <- object$model$npar
   class(val) <- "logLik"
   val
}
