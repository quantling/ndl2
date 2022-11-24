#' Estimation of the activations of outcomes (meanings)
#' 
#' \code{estimateActivations} is used to estimate the activations for outcomes
#' (meanings) using the equilibrium association strengths (weights) for the
#' Rescorla-Wagner model.
#' 
#' 
#' The activation of an outcome is defined as the sum of the weights on the
#' incoming links from active cues.  When the input (the \code{Cues} in
#' \code{cuesOutcomes}) contain elements that are not present in the rownames
#' of the \code{weightMatrix}, such new cues are added to the
#' \code{weightMatrix} with zero entries. The set of exemplars in
#' \code{cuesOutcomes} may contain rows with identical cue sets but different
#' outcome sets. Consequently, for such rows, identical vectors of activations
#' of outcomes are generated.  In the activation matrix returned by
#' \code{estimateActivations}, such duplicate entries are removed.
#' 
#' For examples of how the \code{cuesOutcomes} data frame should be structured,
#' see the data sets \code{\link{danks}}, \code{\link{plurals}}, and
#' \code{\link{serbian}}.  For examples of how the \code{weightMatrix} should
#' be structured, see the corresponding output of
#' \code{\link{estimateWeights}}.
#' 
#' @param cuesOutcomes A data frame with three variables specifying frequency,
#' cues, and outcomes: \describe{ \item{\code{Cues}}{A character vector
#' specifying the cues. When there is more than one cue, the cues should be
#' separated by underscores.} \item{\code{Outcomes}}{A character vector
#' specifying the outcomes.  When there is more than one outcome, the outcomes
#' should be separated by underscores.} \item{\code{Frequency}}{A numeric
#' vector specifying the frequency with which a combination of cues and
#' outcomes occurs.} }
#' @param weightMatrix A numeric matrix with as dimensions the number of cues
#' (horizontal) and number of outcomes (vertical).  Rows and columns should be
#' labeled with cues and outcomes.
#' @param unique A logical that, if \code{=TRUE}, removes duplicate rows from
#' the activation matrix.
#' @param \dots Control arguments to be passed along from
#' \code{\link{ndlClassify}} and/or \code{\link{ndlCrossvalidate}}.
#' @return A list with the following components: \describe{
#' \item{\code{activationMatrix}}{A matrix with as dimensions, for rows, the
#' number of exemplars (by-row cue sets, typically word forms), and for
#' columns, the number of unique outcomes (meanings), specifying the activation
#' of a meaning given the cues in the input for a given exemplar.}
#' \item{\code{newCues}}{A vector of cues encountered in \code{cuesOutcomes}
#' which were not present in \code{weightMatrix}.} \item{list()}{ Control
#' arguments to be passed along from \code{\link{ndlClassify}}, and/or
#' \code{\link{ndlCrossvalidate}}.} }
#' @author R. H. Baayen & Antti Arppe
#' @seealso \code{\link{estimateWeights}, \link{danks}, \link{plurals},
#' \link{serbian}}
#' @references
#' 
#' Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix, P. and
#' Marelli, M., An amorphous model for morphological processing in visual
#' comprehension based on naive discriminative learning. Psychological Review,
#' 118, 438-482.
#' @keywords classif
#' @examples
#' 
#' data(serbian)
#' serbian$Cues <- orthoCoding(serbian$WordForm, grams=2)
#' serbian$Outcomes <- serbian$LemmaCase
#' sw <- estimateWeights(cuesOutcomes=serbian)
#' sw[1:5,1:6]
#' activations <- estimateActivations(unique(serbian["Cues"]), sw)$activationMatrix
#' rownames(activations) <- unique(serbian[["WordForm"]])
#' activations[1:5,1:6]
#' 
#' syntax <- c("acc", "dat", "gen", "ins", "loc", "nom", "Pl", "Sg") 
#' activations2 <- activations[,!is.element(colnames(activations),syntax)]
#' head(rownames(activations2), 50)
#' head(colnames(activations2), 8)
#' image(activations2, xlab="word forms", ylab="meanings", xaxt="n", yaxt="n")
#' mtext(c("yena", "...", "zvuke"), side=1, line=1, at=c(0, 0.5, 1), adj=c(0,0,1))
#' mtext(c("yena", "...", "zvuk"), side=2, line=1, at=c(0, 0.5, 1), adj=c(0,0,1))
#' 
estimateActivations <- function(cuesOutcomes, weightMatrix, unique=FALSE,...) {

  cues = rownames(weightMatrix)
  outcomes = colnames(weightMatrix)

  ## Check for NAs

  NA.cue_strings <- grep("(^NA_)|(_NA_)|(_NA$)",cuesOutcomes$Cues)
  NA.outcome_strings <- grep("(^NA)|(_NA_)|(_NA$)",cuesOutcomes$Outcomes)
  if(length(NA.cue_strings)>0)
    warning(paste("Potential NA's in ",length(NA.cue_strings)," 'Cues'.",sep=""))
  if(length(NA.outcome_strings)>0)
    warning(paste("Potential NA's in ",length(NA.outcome_strings)," 'Outcomes'.",sep=""))
  NA.cues <- which(is.na(cuesOutcomes$Cues))

  if("Outcomes" %in% names(cuesOutcomes))
    NA.outcomes <- which(is.na(cuesOutcomes$Outcomes))
  else
    {
      NA.outcomes = NULL
      warning("No 'Outcomes' column specified in 'cuesOutcomes'.")
    }

  if(length(NA.cues)>0)
    stop(paste("NA's in 'Cues': ",length(NA.cues)," cases.",sep=""))
  if(length(NA.outcomes)>0)
    stop(paste("NA's in 'Outcomes': ",length(NA.outcomes)," cases.",sep=""))

  obsCues = strsplit(as.character(cuesOutcomes$Cues), "_")
  uniqueObsCues = unique(unlist(obsCues))
  newCues = uniqueObsCues[!is.element(uniqueObsCues, cues)]

  if(length(newCues) > 0) {
    wnew = matrix(0, length(newCues), ncol(weightMatrix))
    rownames(wnew)=newCues
    colnames(wnew)=colnames(weightMatrix)
    w = rbind(weightMatrix, wnew)
    cues = c(cues, newCues)
  } else {
    w = weightMatrix
  }

  obsOutcomes = strsplit(as.character(cuesOutcomes$Outcomes), "_")
  uniqueObsOutcomes = unique(unlist(obsOutcomes))
  newOutcomes = uniqueObsOutcomes[!is.element(uniqueObsOutcomes, outcomes)]

  m = matrix(0, length(cues), nrow(cuesOutcomes))
  rownames(m) = cues
#  colnames(m) = cuesOutcomes$WordForm
#  colnames(m) = cuesOutcomes$Outcomes

  v = rep(0, length(cues))
  names(v) = cues

  for(i in 1:nrow(cuesOutcomes)) {
      v[obsCues[[i]]]=1
      m[,i] = v
      v[obsCues[[i]]]=0
  }

  a = t(w) %*% m

  if (unique) {
    activationMatrix <- unique(t(a))
  } else {
    activationMatrix <- t(a)
  }

  if (length(newCues)>0)
    warning(paste("There were ", length(newCues), " cues not present in 'weightMatrix'.",sep=""))  
  if (length(newOutcomes)>0)
    { # activationMatrix = cbind(activationMatrix,matrix(0,NROW(activationMatrix),length(newOutcomes),dimnames=list(NULL,newOutcomes)))
      warning(paste("There were ", length(newOutcomes), " outcomes not present in 'weightMatrix'.",sep=""))  
    }

  result <- list(activationMatrix = activationMatrix, newCues = newCues, newOutcomes = newOutcomes)
  
  return(result)

}
