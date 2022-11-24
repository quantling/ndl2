#' Classification using naive discriminative learning.
#' 
#' \code{ndlClassify} uses the equilibrium equations of Danks (2003) for the
#' Rescorla-Wagner model (1972) to estimate association strengths (weights) for
#' cues (typically levels of factorial predictors) to outcomes (typically a
#' binary or polytomous response variable).  Given the association strengths,
#' the probability of a response level is obtained by summation over the
#' weights on active incoming links.
#' 
#' Classification by naive discriminative learning.
#' 
#' @aliases ndlClassify print.ndlClassify
#' @rdname ndlClassify
#' @param formula An object of class \code{formula} (or one that can be coerced
#' to that class): a symbolic description of the model to be fitted.
#' @param data A data frame containing the variables in the model.
#' @param method A string specifiying the method to use. The default
#' is the Danks equilibria equations \code{"danks"}. Setting the
#' method to \code{"irw"} will change to the iterative method. See the
#' help for \code{\link{ndl2.package}} for a comparison of the two methods.
#' @param frequency A numeric vector (or the name of a column in the input data
#' frame) with the frequencies of the exemplars.  If absent, each exemplar is
#' assigned a frequency equal to 1.
#' @param x An object of the class \code{"ndlClassify"} fitted with
#' \code{ndlClassify} to be printed with \code{print.ndlClassify}.
#' @param max.print The maximum number of rows of the \code{weightMatrix} to be
#' output when printing with \code{print.ndlClassify}; by default equal to 10;
#' if set to \code{NA} all rows will be output.
#' @param variable.value.separator A character string which will separate
#' variable names from variable values in their combination as cue values; by
#' default an empty character string (\code{=""}).
#' @param \dots Control arguments to be passed along to
#' \code{\link{ndlCuesOutcomes}}, \code{\link{estimateWeights}},
#' \code{\link{estimateActivations}}, and/or \code{\link{print.ndlClassify}}.
#' @return A list of the class \code{"ndlClassify"} with the following
#' components: \describe{ \item{\code{activationMatrix}}{A matrix specifying
#' for each row of the input data frame the activations (probabilities) of the
#' levels of the response variable (\code{nrow} observations by \code{nlevels}
#' of response variable).} \item{\code{weightMatrix}}{ A matrix specifying for
#' each cue (predictor value) the association strength (weight) to each outcome
#' (level of the response variable) (number of distinct predictor values by
#' number of response levels).} \item{\code{cuesOutcomes}}{ The input data
#' structure for naive discriminative learning created by
#' \code{ndlCuesOutcomes} based on the \code{data} argument (number of
#' observations by 3: \code{Frequency, Cues, Outcomes}).}
#' \item{\code{call}}{The call matched to fit the resulting
#' \code{"ndlClassify"} object.} \item{\code{formula}}{The formula specified
#' for fitting the resulting \code{"ndlClassify"} object.}
#' \item{\code{data}}{The supplied \code{data} argument, excluding all
#' elements not specified for the modeling task in \code{formula} and
#' \code{frequency}.} }
#' @author R. H. Baayen and Antti Arppe
#' @seealso \code{\link{summary.ndlClassify}, \link{plot.ndlClassify},
#' \link{anova.ndlClassify}, \link{predict.ndlClassify},
#' \link{ndlCuesOutcomes}, \link{estimateWeights}, \link{cueCoding}}
#' @references
#' 
#' Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix, P. and
#' Marelli, M., An amorphous model for morphological processing in visual
#' comprehension based on naive discriminative learning. Psychological Review,
#' 118, 438-482.
#' 
#' Danks, D. (2003). Equilibria of the Rescorla-Wagner model. Journal of
#' Mathematical Psychology, 47 (2), 109-121.
#' 
#' Rescorla, R. A., & Wagner, A. R. (1972). A theory of Pavlovian conditioning:
#' Variations in the effectiveness of reinforcement and nonreinforcement. In
#' Black, A. H., & Prokasy, W. F. (Eds.), Classical conditioning II: Current
#' research and theory (pp. 64-99). New York: Appleton-Century-Crofts.
#' 
#' Arppe, A. and Baayen, R. H. (in prep.) Statistical classification and
#' principles of human learning.
#' @keywords classif
#' @examples
#' 
#' data(think)
#' set.seed(314)
#' think <- think[sample(1:nrow(think),500),]
#' think.ndl <- ndlClassify(Lexeme ~ (Person * Number * Agent) + Register,
#'    data=think)
#' summary(think.ndl)
#' 
#' \dontrun{
#' think.ndl.SA <- ndlClassify(Lexeme ~ (Polarity + Voice + Mood + Person +
#'   Number + Covert + ClauseEquivalent + Agent + Patient + Manner + Time +
#'   Modality1 + Modality2 + Source + Goal + Quantity + Location +
#'   Duration + Frequency + MetaComment + ReasonPurpose + Condition +
#'   CoordinatedVerb)^2 + Author + Section, data=think)
#' summary(think.ndl.SA)
#' }
#' 
#' \dontrun{
#' data(dative)
#' out <- which(is.element(colnames(dative), c("Speaker","Verb")))
#' dative <- dative[-out]
#' dative.ndl <- ndlClassify(RealizationOfRecipient ~ ., data=dative)
#' summary(dative.ndl)
#' 
#' }
#' 
ndlClassify <- function(formula, data, method="danks",frequency=NA, variable.value.separator="", ...)
{
  call <- match.call()

#  response = as.character(formula[2])
  response=gsub("[ ]+"," ",paste(deparse(formula[[2]],width.cutoff=500),collapse=""))
  predictors=gsub("[ ]+"," ",paste(deparse(formula[[3]],width.cutoff=500),collapse=""))

  if(predictors == ".")
    { predictors = colnames(data)
      predictors = predictors[predictors!=response]
      if(!is.na(frequency) & is.character(frequency) & length(frequency)==1)
        predictors = predictors[predictors!=frequency]
      formula = as.formula(paste(c(response,paste(predictors,collapse=" + ")),collapse=" ~ "))
    } 
  else
    predictors = levels(as.factor(unlist(lapply(attr(terms.formula(formula),"term.labels"),function(x) strsplit(x,"[ ]*([\\+]|[\\|]|[:])[ ]*")))))

  if(is.character(frequency) & length(frequency)==1 & frequency %in% colnames(data))
    data <- data[c(frequency, response, predictors)]
  else
    data <- data[c(response, predictors)]

  cuesOutcomes = ndlCuesOutcomes(formula=formula, data=data, frequency=frequency, variable.value.separator=variable.value.separator, ...)

  if (method == "danks") {
      weightMatrix = estimateWeights(cuesOutcomes, ...)
  } else {
      ## NOTE: THIS IS BROKEN RIGHT NOW!!! 
      ## NEED TO FIGURE OUT WHY THIS IS BROKEN!!!
      weightMatrix = learnWeights(cuesOutcomes, ...)
  }
  weightMatrix = weightMatrix[order(rownames(weightMatrix)),,drop=FALSE]
  weightMatrix = weightMatrix[,order(colnames(weightMatrix)),drop=FALSE]

  activationMatrix = estimateActivations(cuesOutcomes, weightMatrix, ...)$activationMatrix 

  result <- list(activationMatrix=activationMatrix,  weightMatrix=weightMatrix, cuesOutcomes=cuesOutcomes, frequency=frequency, call=call, formula=formula, data=data)
  class(result) <- "ndlClassify"

  return(result)

}

#' @rdname ndlClassify
print.ndlClassify <- function(x, max.print=10, ...)
{
  digits=max(3,getOption("digits")-3)
  if(is.na(max.print))
    max.print=NROW(x$weightMatrix)
#  if(!is.null(x$max.print) & is.numeric(x$max.print))
#       max.print=x$max.print
  cat("\n")
  print(x$call)
#  cat("\n")
#  print.formula(x$formula)
  cat("\n")
  print.table(x$weightMatrix[1:min(nrow(x$weightMatrix),max.print),], digits=digits)
  if(nrow(x$weightMatrix)>max.print)
    cat(paste("... [ omitted ",nrow(x$weightMatrix)-max.print," rows ] ...\n",sep=""))
  cat("\n")

  invisible(x)
}
