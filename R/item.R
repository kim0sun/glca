#' Specify Manifest Items for \code{glca}.
#'
#' Specifying manifest items in formula of \code{glca} function.
#'
#' @param \dots vectors of manifest items. These can be given as named arguments which is colnames of data.frame
#'
#' @return
#' a \code{matrix} of specified variables, which contains following attributes about items.
#'
#' Dimensions of \code{data.frmae}, names of items and level of items.
#'
#' @seealso \code{\link{glca}}
#'
#' @examples
#' ## For examples see example(glca)
#'
#' @export

item <- function(...)
{
   obj <- substitute(list(...))
   args <- list(...)
   argf <- lapply(args, as.factor)

   y <- do.call("cbind", lapply(argf, as.numeric))
   y[is.na(y)] <- 0

   attr(y, "y.names") <- sapply(obj, deparse)[-1]
   attr(y, "y.level") <- lapply(argf, levels)
   class(y) <- "items"

   return(y)
}
