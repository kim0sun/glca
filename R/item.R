#' Specifies Manifest Items for \code{glca}
#'
#' Specifying manifest items in formula of \code{glca} function.
#'
#' @param \dots vectors of manifest items. These can be given as named arguments which is colnames of \code{data.frame}.
#' @param starts.with a string for prefix of variable names to be selected.
#' @param ends.with a string for suffix of variable names to be selected.
#'
#' @return
#' a \code{matrix} of specified variables, which contains names and levels of manifest items.
#'
#' @seealso \code{\link{glca}}
#'
#' @examples
#' ## For examples see example(glca)
#'
#' @export

item <- function(..., starts.with = NULL, ends.with = NULL)
{
   if (!is.character(starts.with) && !is.character(ends.with)) {
      obj <- substitute(list(...))
      args <- list(...)
      argf <- lapply(args, as.factor)

      y <- do.call("cbind", lapply(argf, as.numeric))
      if (is.null(y))
         stop("No manifest items were entered.")
      y[is.na(y)] <- 0

      yname <- sapply(obj, deparse)[-1]
      ylevel <- lapply(argf, levels)
   } else {
      sname <- ename <- name <- ls(parent.frame())

      if (is.character(starts.with)) {
         sname <- name[startsWith(name, starts.with)]
      }
      if (is.character(ends.with)) {
         ename <- name[endsWith(name, ends.with)]
      }

      name <- intersect(sname, ename)
      if (length(name) == 0)
         stop("No variables match with starts.with or ends.with")

      args <- lapply(name, get, envir = parent.frame())
      argf <- lapply(args, as.factor)

      y <- do.call("cbind", lapply(argf, as.numeric))
      y[is.na(y)] <- 0

      yname <- name
      ylevel <- lapply(argf, levels)
   }

   ylen <- sapply(ylevel, length)
   if (sum(ylen == 1) > 1) {
      lev1 <- which(ylen == 1)
      y <- y[,-lev1]
      message(paste(length(lev1), "variables has only 1 category, so that removed.\n\n"))
      yname <- yname[-lev1]
      ylevel <- ylevel[-lev1]
   }

   attr(y, "y.names") <- yname
   attr(y, "y.level") <- ylevel
   attr(y, "dataN") <- nrow(y)
   class(y) <- "items"
   return(y)
}
