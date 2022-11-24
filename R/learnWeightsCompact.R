#### Similar to estimateWeightsCompact, but uses the exact iterative Rescorla Wagner equations
#### See R documentation for details.


#' Iterative weights learning based on Rescorla-Wagner equations using compact
#' binary event files.
#' 
#' A function to learn the weights (associative strengths) for cue-outcome
#' pairs using the Rescorla-Wagner equations and events in a binary format.
#' Such binary event files can be created using
#' \code{\link{ndlPreprocessCorpus}}, \code{\link{ndlPreprocessTabular}} or
#' \code{\link{ndlPreprocessDataframe}}. For doing both these steps, creating
#' the binary event files and learning from them, together,
#' \code{\link{learnWeights}}, \code{\link{learnWeightsCorpus}} or
#' \code{\link{learnWeightsTabular}} can be used.\cr\cr
#' By default the function returns a \code{\link{ndlLearner}}
#' \link[=ReferenceClasses]{reference class} object that, unlike most
#' R objects, can change its state, doesn't get copied on variable assignment
#' or for function calls and cannot save the weights over R sessions.
#' 
#' The weights are learned in the order of the events specified in the binary
#' event files. Therefore binary event files created with
#' \code{\link{ndlPreprocessTabular}} or \code{\link{ndlPreprocessDataframe}}
#' require each cue-outcome entry to have a frequency of one.
#' 
#' @param dataSource The data source of preprocessed events to use.
#'   This parameter can be set to either the result of one of the
#'   \code{ndlPreprocess} functions (\code{\link{ndlPreprocessingResult}}),
#'   the information from another learning session (
#'   weight matrix, \code{\link{ndlLearner}} object or the result
#'   of the \code{$getInfo} method) or a string. In the last case the string
#'   needs to be the common prefix of the files created by the
#'   \code{ndlPreprocess} functions. The prefix string can also contain the
#'   path to the data source, but then the \code{path} parameter must be kept
#'   at its default value (\code{NA}).
#' @param path Used only if \code{dataSource} is a string. In that case this
#'   parameter specifies the path to the folder that contains the files
#'   created by any of the \code{ndlPreprocess} functions. By default the
#'   path component in the \code{dataSource} string is used for the path.
#'   If it doesn't contain a path component, the working directory is used.
#' @param alpha The salience of the cues (learning rate).
#' @param beta The salience of the outcomes (learning rate).
#' @param lambda The maximum possible level of associative strength.
#' @param numEventsToLearn How many of the events should be learned at first.                                     
#'   If \code{asMatrix} is set to \code{FALSE} (default), then more events can
#'   be learned afterwards using the \code{$learn} method of the returned
#'   \code{\link{ndlLearner}} object.
#' @param removeDuplicates If \code{TRUE} cues and outcomes are only counted once
#'   per event, even if they occur several times in the cue or outcome list for
#'   that event.
#' @param asMatrix Whether the function should return the weight matrix instead
#'   of a \code{\link{ndlLearner}} object. Defaults to \code{FALSE}.
#' @param retainFiles Whether the intermediary preprocessed files should be
#'   kept even after all the events have been learned and also after the
#'   \code{\link{ndlLearner}} object gets deleted (or to be precise, after
#'   it gets \link[=gc]{garbage collected)}.
#' @template Threads_Verbose_Debug
#' @template LearnWeights-ReturnValue
#' @seealso \code{\link{estimateWeightsCompact}},
#'   \code{\link{ndlPreprocessCorpus}}, \code{\link{ndlPreprocessTabular}},
#'   \code{\link{ndlPreprocessDataframe}}, \code{\link{learnWeightsCorpus}},
#'   \code{\link{learnWeightsTabular}}, \code{\link{learnWeights}}
#' 
#' @examples
#' data(lexample)
#' lexample$Cues <- orthoCoding(lexample$Word, grams=2)
#' 
#' # Simulate the order of the events by assuming occurrence in random order
#' # (as frequencies greater than 1 are not allowed for iterative learning).
#' lexampleLong <- lexample[sample(rep(1:nrow(lexample), lexample$Frequency)),]
#' lexampleLong$Frequency <- 1
#' 
#' # Create the preprocessed files.
#' lexamplePreprocessed <- ndlPreprocessDataframe(lexampleLong)
#' 
#' # Learn all event files.
#' compactLearner <- learnWeightsCompact(lexamplePreprocessed)
#' compactLearner
#' 
#' # Get the weights
#' compactLearnerWeights <- compactLearner$getWeights()
#' 
#' # Compare the weights to the directly returned weights.
#' compactWeights <- learnWeightsCompact(lexamplePreprocessed, asMatrix=TRUE)
#' stopifnot(isTRUE(all.equal(compactLearnerWeights, compactWeights,
#'                            check.attributes = FALSE)))
#' 
#' # Compare with preprocessing and learning done together in one function.
#' # For demonstration purposes we now learn in several steps.
#' learner2 <- learnWeights(lexampleLong, numEventsToLearn=2)
#' learner2$learn(3) # Learn the next 3 events.
#' learner2$learn() # Learn the remaining events.
#' 
#' # Compare the weights.
#' stopifnot(isTRUE(all.equal(compactLearnerWeights, learner2$getWeights(),
#'                            check.attributes = FALSE)))
#'                            
#' # Delete the preprocessed files, as retainFiles defaults to TRUE.
#' stopifnot(compactLearner$filesExist())
#' compactLearner$deleteFiles()
#' 
#' # The preprocessed files from learner2 do not need to deleted, as the
#' # ndlLearnWeights() function defaults to retainFiles=FALSE
#' stopifnot(!learner2$filesExist())
#' 
learnWeightsCompact <- 
  function(dataSource, alpha = 0.1, beta = 0.1, lambda = 1, path = NA,
           numEventsToLearn = "all", removeDuplicates = TRUE,
           asMatrix = FALSE, retainFiles = TRUE,
           numThreads = Sys.getenv("OMP_NUM_THREADS"), verbose = FALSE,
           debug = FALSE)
{
  # A set debug parameter overrules the setting for verbosity
  if (debug)
    verbose <- TRUE
  
  if (is.matrix(dataSource))
    dataSource <- getInfoFromWeightMatrix(dataSource)
  
  if (inherits(dataSource, "ndlPreprocessingResult") || is.list(dataSource)) {
    sourceName <- dataSource$sourceName
    path <- getBestPreviouslyUsedPath(dataSource)
  } else if (is.na(path)) {
    path <- dirname(dataSource)
    sourceName <- basename(dataSource)
  } else {
    sourceName <- dataSource
  }
  
  if (verbose) { message("Initializing iterative learning."); flush.console() }
  
  # Use C++ implementation of iterative Rescorla-Wagner on binary files.
  learner <- ndlLearner$new(sourceName, path, alpha, beta, lambda, removeDuplicates,
                            retainFiles, numThreads, verbose, debug)

  nCues <- length(learner$cueIndices)
  nOutcomes <- length(learner$outcomeIndices)
  # To avoid integer overflow nCues and nOutcomes are multiplied as doubles.
  if (asMatrix & ((as.double(nCues) * as.double(nOutcomes)) >= 2^31)) {
    stop(paste("Cannot return weight matrix, because it is bigger than the",
               "maximum allowed size for matrices in R (2^31 elements)"))
  }
  
  if (verbose) { message("Starting iterative learning."); flush.console() }
  if (numEventsToLearn == "all")
    numEventsToLearn <- learner$totalNumEvents
  learner$learn(numEventsToLearn)
  if (verbose) message("Finished iterative learning.")
  
  if (asMatrix)
    return(learner$getWeights())
  return(learner)
}
