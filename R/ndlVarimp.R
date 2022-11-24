#' Permutation variable importance for classification using naive
#' discriminative learning.
#' 
#' \code{ndlVarimp} uses permutation variable importance for naive
#' discriminative classification models, typically the output of
#' \code{ndlClassify}.
#' 
#' Variable importance is assessed using predictor permutation. Currently,
#' conditional permutation variable importance (as for \code{varimp} for random
#' forests in the \code{party} package) is not implemented.
#' 
#' @param object An object of class \code{"ndlClassify"} (or one that can be
#' coerced to that class); typically a model object as produced by
#' \code{\link{ndlClassify}}.
#' @param verbose A logical (default TRUE) specifying whether the successive
#' predictors being evaluated should be echoed to stdout.
#' @return A list with two numeric vectors: \describe{
#' \item{\code{concordance}}{For binary response variables, a named vector
#' specifying for each predictor the index of concordance when that predictor
#' is permuted.  For polytomous response variables, NA.}
#' \item{\code{accuracy}}{ A named vector specifying for each predictor the
#' accuracy of the model with that predictor permuted.} }
#' @author R. H. Baayen and Antti Arppe
#' @seealso \code{\link{summary.ndlClassify}, \link{plot.ndlClassify},
#' \link{anova.ndlClassify}, \link{ndlCuesOutcomes}, \link{estimateWeights},
#' \link{cueCoding}}
#' @references R. Harald Baayen (2011). Corpus linguistics and naive
#' discriminative learning. Brazilian journal of applied linguistics, 11,
#' 295-328.
#' 
#' Carolin Strobl, Anne-Laure Boulesteix, Thomas Kneib, Thomas Augustin and
#' Achim Zeileis (2008).  Conditional Variable Importance for Random Forests.
#' BMC Bioinformatics, 9, 307.
#' @keywords classif
#' @examples
#' 
#' \dontrun{
#' data(dative)
#' dative <- dative[!is.na(dative$Speaker),-2]
#' dative.ndl <- ndlClassify(RealizationOfRecipient ~ ., data=dative)
#' dative.varimp <- ndlVarimp(dative.ndl)
#' 
#' library(lattice)
#' dotplot(sort(summary(dative.ndl)$statistics$accuracy-dative.varimp$accuracy), 
#'    xlab="permutation variable importance")
#' }
#' 
ndlVarimp <-
function(object, verbose=TRUE) {

  formula = object$formula
  data = object$data
  response = as.character(formula[2])
  predictors = gsub("[ ]+", " ", paste(deparse(formula[[3]], 
      width.cutoff = 500), collapse = ""))
  n.predictors = length(predictors)
  if (predictors == ".") {
    predictors = colnames(data)
    predictors = predictors[predictors != response]
    formula = as.formula(paste(c(response, paste(predictors, collapse = " + ")), 
            collapse = " ~ "))
  } else {
     predictors = levels(as.factor(unlist(lapply(attr(terms.formula(formula), 
       "term.labels"), function(x) strsplit(x, "[ ]*([\\+]|[\\|]|[:])[ ]*")))))
  }

  if (nlevels(data[,response]) == 2) binary = TRUE
  else binary = FALSE

  if (binary) cvals = rep(0,length(predictors))
  avals = rep(0,length(predictors))
  for (i in 1:length(predictors)) {
    if (verbose) cat(predictors[i],"\n")
    orig = data[,predictors[i]]
    data[,predictors[i]] = sample(data[,predictors[i]])
    tmp.ndl=ndlClassify(formula, data=data)
    data[,predictors[i]] = orig
    if (binary) cvals[i]= summary(tmp.ndl)$statistics$C
    avals[i]= summary(tmp.ndl)$statistics$accuracy
  }
  if (binary) names(cvals) = predictors
  names(avals) = predictors
  
  if (binary) return(list(concordance=cvals, accuracy=avals))
  else return(list(concordance=NA, accuracy=avals))

}
