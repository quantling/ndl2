#' Predict method for ndlClassify objects
#' 
#' 
#' Obtains predictions on the basis of a fitted \code{"ndlClassify"} object on
#' data already incorporated in the object or on new data with the same
#' predictors as the originally fitted model object.
#' 
#' 
#' If \code{newdata} is omitted the predictions are based on the data used for
#' the fit.
#' 
#' @param object
#' 
#' objects of class \code{"ndlClassify"}, typically the result of a call to
#' \code{ndlClassify}.
#' 
#' @param newdata
#' 
#' optionally, a data frame in which to look for variables with which to
#' predict.  If omitted (i.e. set to \code{NULL}), the original data used to
#' fit the \code{object} are used.
#' @param frequency
#' 
#' A numeric vector (or the name of a column in the (new) data frame
#' \code{newdata}) with the frequencies of the exemplars. If absent, each
#' exemplar is assigned a frequency equal to 1.
#' 
#' @param type
#' 
#' the type of prediction requested.  The default option \code{type="choice"}
#' produces the predicted individual discrete choices (i.e. Outcomes), given
#' the predictor Cues selected for fitting the original \code{object}. The
#' option \code{type="acts"} provides the sum activations for each Outcome
#' given the Cue combinations in \code{newdata} (or in the original data in
#' \code{object}, while the alternative \code{type="probs"} yields the
#' distributions of predicted probabilities (based on the activations) over the
#' Outcome responses.
#' 
#' @param \dots
#' 
#' further arguments passed to and from other functions.
#' 
#' @return
#' 
#' a vector \code{predicted}, or matrix of activations \code{activations}, or a
#' matrix of predictions \code{probabilities}.
#' @author Antti Arppe
#' @seealso \code{\link{ndlClassify}}, \code{\link{estimateActivations}},
#' \code{\link{acts2probs}}
#' @references Arppe, A. and Baayen, R. H. (in prep.) Statistical
#' classification and principles of human learning.
#' @keywords classif
#' @examples
#' 
#' 
#' data(think)
#' think.ndl <- ndlClassify(Lexeme ~ Agent + Patient, data=think[1:300,])
#' head(predict(think.ndl, type="choice"))
#' predict(think.ndl, newdata=think[301:320,], type="probs")
#' predict(think.ndl, newdata=think[301:320,], type="acts")
#' 
#' 
predict.ndlClassify <- function(object, newdata=NULL, frequency=NA, type="choice", ...)
{
  if(!("ndlClassify" %in% class(object)))
    stop(paste("object: '",object,"' not created with function 'ndlClassify'.",sep=""))
  if(!(type %in% c("choice","acts","probs")))
    stop(paste("type: '",type,"' not supported.",sep=""))

  if(is.null(newdata))
    newdata <- object$data
  newCuesOutcomes <- ndlCuesOutcomes(object$formula, data=newdata, frequency=frequency)

  oldWeights <- object$weightMatrix
  newActivations <- estimateActivations(newCuesOutcomes, weightMatrix=oldWeights)
  activations <- newActivations$activationMatrix

  predictions <- acts2probs(newActivations$activationMatrix)
  probabilities <- predictions$p
  predicted <- predictions$predicted

  if(type=="choice")
    return(predicted)
  if(type=="acts")
    return(activations)
  if(type=="probs")
    return(probabilities)
}
