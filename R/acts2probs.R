#Internal Function


#' Calculate probability matrix from activation matrix, as well as predicted
#' values
#' 
#' \code{acts2probs} takes the activation matrix returned by
#' \code{\link{ndlClassify}} and calculates the matrix of probabilities for the
#' estimated activation matrix, as well as the predicted values of the response
#' variable.
#' 
#' Probabilities in \code{p} are obtained by adding the absolute value of the
#' minimum activation to the activation matrix, and renorming. The selection
#' rule used to produce \code{predicted} is to choose for each instance in the
#' data the outcome value that has received the highest probability estimate.
#' 
#' @param acts A matrix of activations (number of observations by number of
#' levels of the response variable).
#' @return A list with the following components: \describe{\item{\code{p}}{a
#' matrix with the probabilities of the levels of the response variable for
#' each observation.} \item{\code{predicted}}{a character vector with
#' predicted values.} }
#' @author Harald Baayen and Antti Arppe
#' @seealso See also \code{\link{ndlClassify}}.
#' @references Arppe, A. and Baayen, R. H. (in prep.). Statistical
#' classification and principles of human learning.
#' @keywords discriminative learning
#' @examples
#' 
#' data(think)
#' think.ndl <- ndlClassify(Lexeme ~ Person + Number + Agent + Register, data=think)
#' pdata <- acts2probs(think.ndl$activationMatrix)
#' 
acts2probs <- function(acts) {
  acts.minimum.correction=1e-10
  acts.minimum = abs(min(acts))
  if(acts.minimum==0)
    acts.minimum = acts.minimum.correction
  acts = acts + acts.minimum

  rowsums = apply(acts,1,sum)

  m = matrix(rep(rowsums, rep(ncol(acts),nrow(acts))), nrow(acts), ncol(acts), byrow=TRUE)

  p = acts/m
  colnames(p) = colnames(acts)

  predictions = apply(p, 1, function(v) names(which.max(v)))

  result <- list(p = p, predicted = predictions)
  return(result)
}

