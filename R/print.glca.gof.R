#' @method print glca.gof
#' @export

print.glca.gof <- function(
   x, nsmall = max(2, getOption("digits") - 2), ...
)
{
   notes <- attr(x, "notes")
   cat(paste0("Model ", format(seq_along(notes)), ": ",
              notes), sep = "\n")

   cat("\nGoodness of Fit Table :\n")
   gt <- x$gtable
   gt <- round(gt, 2)
   print(gt, na.print = "")

   if (!is.null(x$dtable)) {
      cat("\nAnalysis of Deviance Table :\n")
      dt <- x$dtable
      dt <- round(dt, 2)
      print(dt, na.print = "")
   }
}
