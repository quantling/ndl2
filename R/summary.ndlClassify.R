#' A summary of a Naive Discriminatory Learning Model
#' 
#' A summarization method for an object of the class \code{"ndlClassify"}.
#' 
#' Calculates descriptive statistics of a fitted Naive Discriminatory Learning
#' model and prints a nice summary of the key results.
#' 
#' @aliases summary.ndlClassify print.summary.ndlClassify
#' @rdname summary.ndlClassify
#' @param object An object of class \code{"ndlClassify"}, resulting from a call
#' to \code{ndlClassify}.
#' @param x An object of class \code{"summary.ndlClassify"}, usually resulting
#' from a call to \code{summary.ndlClassify}.
#' @param digits The number of significant digits to use when printing.
#' @param max.print The maximum number of rows of \code{weights} to be output
#' when printing; by default equal to 10; ; if set to \code{NA} all rows will
#' be output.
#' @param \dots Control arguments passed to or from other methods, e.g.
#' \code{ndlStatistics} and \code{modelStatistics}.
#' @return \code{summary.ndlClassify} returns an object of the class
#' \code{"summary.ndlClassify"}, a list with the following components:
#' \describe{ \item{\code{call}}{The call matched to fit the
#' \code{"ndlClassify"} object.} \item{\code{formula}}{The formula specified
#' for the \code{"ndlClassify"} object.} \item{\code{weights}}{The estimated
#' weights.} \item{\code{statistics}}{A range of descriptive statistics
#' calculated with \code{ndlStatistics}.} }
#' @author Antti Arppe
#' @seealso \code{\link{ndlClassify}, \link{ndlStatistics},
#' \link{modelStatistics}}
#' @references Arppe, A. and Baayen, R. H. (in prep.)
#' @keywords classif
#' @examples
#' 
#' 
#' ## For examples see examples(ndlClassify).
#' 
#' 
summary.ndlClassify <- function(object, ...)
{
  statistics <- ndlStatistics(object, ...)

  weights <- object$weightMatrix

  sumry <- list(call=object$call, formula=object$formula, weights=weights, statistics=statistics, ...)

  class(sumry) <- "summary.ndlClassify"
  return(sumry)
}

#' @rdname summary.ndlClassify
print.summary.ndlClassify <- function(x, digits=max(3,getOption("digits")-3), max.print=10, ...)
{ 
  if(is.na(max.print))
    max.print= NROW(x$weights)
#  if(!is.null(x$digits) & is.numeric(x$digits))
#    digits=x$digits
#  if(!is.null(x$max.print) & is.numeric(x$max.print))
#    max.print=x$max.print

  cat("\nCall:\n")
  print(x$call)
  cat("\nFormula:\n")
  print(x$formula)
  cat("\nWeights:\n")
  print.table(x$weights[1:min(nrow(x$weights),max.print),], digits)
  if(nrow(x$weights)>max.print)
    cat(paste("... [ omitted ",nrow(x$weights)-max.print," rows ] ...\n",sep=""))
  cat("\n")
  statistics <- x$statistics
  deviances <- format(c(signif(statistics$deviance.null,digits),signif(statistics$deviance.model,digits)))
  DFs <- format(c(statistics$df.null,statistics$df.model))
  cat(c("Null deviance:             ", deviances[1], " on ", DFs[1], " degrees of freedom\n"))
  cat(c("Residual (model) deviance: ", deviances[2], " on ", DFs[2], " degrees of freedom\n"))
  cat(format(c("\nR2.likelihood: ", signif(statistics$R2.likelihood,digits), "\nAIC: ", signif(statistics$AIC.model,digits), "\nBIC: ", signif(statistics$BIC.model,digits))))
  cat("\n\n")

  invisible(x)
}
