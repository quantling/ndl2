#' A summary of a crossvalidation of a Naive Discriminatory Reader Model
#' 
#' A summarization method for an object of the class \code{"ndlCrossvalidate"}.
#' 
#' Calculates overall descriptive statistics of the crossvalidation of a fitted
#' Naive Discriminatory Reader model and prints a nice summary of the key
#' results.
#' 
#' @aliases summary.ndlCrossvalidate print.summary.ndlCrossvalidate
#' @param object An object of class \code{"ndlCrossvalidate"}, resulting from a
#' call to \code{ndlCrossvalidate}.
#' @param x An object of class \code{"summary.ndlCrossvalidate"}, usually
#' resulting from a call to \code{summary.ndlCrossvalidate}.
#' @param digits the number of significant digits to use when printing.
#' @param \dots further arguments passed to or from other methods.
#' @return \code{summary.ndlCrossvalidate} returns an object of the class
#' \code{"summary.ndlCrossvalidate"}, a list with the following components:
#' \describe{ \item{\code{call}}{The call matched to fit the
#' \code{"ndlCrossvalidate"} object.} \item{\code{formula}}{The formula
#' specified for the \code{"ndlCrossvalidate"} object.}
#' \item{\code{statistics.summary}}{The means, minima and maxima of a range
#' descriptive statistics for the fit and performance of individual folds; see
#' \code{\link{ndlStatistics}}.} \item{\code{crosstable.summary}}{The means
#' of the crosstabulation of observed and predicted outcomes for the held-out
#' test data.} \item{\code{recall.predicted.summary}}{The means of the
#' recall values for the individual outcomes predicted with the held-out test
#' data.} \item{\code{precision.predicted.summary}}{The means of the
#' precision values for the individual outcomes predicted with the held-out
#' test data.} \item{\code{statistics.all}}{All the values for a range
#' descriptive statistics for the fit and performance of individual folds on
#' the held-out test data; see \code{\link{ndlStatistics}}.}
#' \item{\code{k}}{The number of folds.} \item{\code{n.total}}{The sum
#' frequency of all data points in \code{data}.} \item{\code{n.train}}{The
#' sum frequency of data points used for training the individual models
#' (excluding the individual folds).} \item{\code{n.test}}{The sum frequency
#' of data points in the individual held-out folds used for testing the
#' individual models.} }
#' @author Antti Arppe
#' @seealso \code{\link{ndlCrossvalidate}, \link{ndlClassify},
#' \link{ndlStatistics}}
#' @references Arppe, A. and Baayen, R. H. (in prep.)
#' @keywords classif
#' @examples
#' 
#' 
#' ## For examples see examples(ndlCrossvalidate).
#' 
#' 
summary.ndlCrossvalidate <- function(object, ...)
{ n.fits = object$k
  fits = object$fits
  if(n.fits>=2)
    { names.fits <- names(fits[[1]])
      names.fits <- names.fits[-which(names.fits %in% c("df.null","df.model","crosstable","recall.predicted","precision.predicted"))]
      statistics.all <- data.frame(sapply(names.fits, function(statistic) sapply(fits, function(x) x[[statistic]])))
      rownames(statistics.all) <- 1:n.fits
      statistics.summary <- data.frame(matrix(c(apply(statistics.all,2,mean),apply(statistics.all,2,min),apply(statistics.all,2,max)),3,byrow=TRUE,dimnames=list(c("Mean","Minimum","Maximum"),names.fits)))
      statistics.summary[c("Minimum","Maximum"),"n.test"] <- NA
      crosstables <- array(,dim=c(dim(fits[[1]]$crosstable),n.fits), dimnames=c(dimnames(fits[[1]]$crosstable),NULL))
      recall.predicted <- NULL
      precision.predicted <- NULL
      for(i in 1:n.fits)
         { crosstables[,,i] <- fits[[i]]$crosstable
           recall.predicted <- rbind(recall.predicted, fits[[i]]$recall.predicted)
           precision.predicted <- rbind(precision.predicted, fits[[i]]$precision.predicted)
         }
      crosstable.summary <- apply(crosstables,c(1,2),mean)
      recall.predicted.summary <- apply(recall.predicted,2,mean)
      precision.predicted.summary <- apply(precision.predicted,2,mean)

      sumry = list(call=object$call, formula=object$formula, statistics.summary=statistics.summary, crosstable.summary=crosstable.summary, recall.predicted.summary = recall.predicted.summary, precision.predicted.summary = precision.predicted.summary, statistics.all=statistics.all, k=n.fits, n.total=object$n.total, n.train=object$n.train, n.test=object$n.test, ...)

    }
  else
    sumry=fits

  class(sumry) <- "summary.ndlCrossvalidate"  
  sumry;

}

print.summary.ndlCrossvalidate <- function(x, digits=max(3,getOption("digits")-3), ...)
{
  if(!is.null(x$digits))
    digits=x$digits

  cat("\nCross-validation summary statistics\n\n")

  cat("Call:\n")
  print(x$call)
  cat("\nFormula:\n")
  print(x$formula)
  cat(c("\nNumber of folds: ",x$k))
  cat(c("\nN(total): ",x$n.total))
  cat(c("\nN(train): ",x$n.train))
  cat(c("\nN(test):  ",x$n.test))
  cat("\n\n")

  sumry <- data.frame(format(apply(signif(x$statistics.summary[-which(names(x$statistics.summary) %in% c("n.test","d.lambda.prediction","d.tau.classification","p.lambda.prediction","p.tau.classification"))],digits),c(1,2),as.character),justify="right"))
  rownames(sumry) <- format(rownames(sumry),justify="left")
  colnames(sumry) <- format(colnames(sumry),justify="left")

  print(t(sumry),quote=FALSE)
  cat("\n")

  invisible(x)
}
