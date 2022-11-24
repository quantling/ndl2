#### Get weight matrice for dataframe, tabular file or corpus file.
#### See R documentation for details.

# Common learning function for tabular and corpus files.
learnWeightsCommon = function(preprocFun, preprocArgs, learnArgs,
                              sourceFile, dirPath, useExistingFiles) {
  learnParamIndex = function(name) { which(names(formals(learnWeightsCompact)) == name) }
  preprocArgs$numThreads <- checkNumThreads(preprocArgs$numThreads)
  learnArgs[[learnParamIndex("numThreads")]] <- preprocArgs$numThreads
  
  outputDirWithNamePrefix <- getOutputDirWithNamePrefix(dirPath, sourceFile)
  sourceName <- basename(outputDirWithNamePrefix)
  learnArgs[[learnParamIndex("dataSource")]] <- sourceName
  path <- dirname(outputDirWithNamePrefix)
  learnArgs[[learnParamIndex("path")]] <- path
  
  alreadyPreprocessed <- all(file.exists(getPreprocessedFileNames(sourceName, path)))
  sameAsPreprocessed <- hasSameSettingsAsPreprocessedFiles(preprocFun, preprocArgs,
                                                           sourceName, path)
  if (!useExistingFiles || !sameAsPreprocessed) {
    do.call(preprocFun, preprocArgs)
  } else {
    # As we didn't create the preprocessed files, we also don't delete them.
    learnArgs[[learnParamIndex("retainFiles")]] <- T
    if (preprocArgs$verbose || preprocArgs$debug)
      message("Using existing temporary binary representation.")
  }
  
  # Use C++ implementation of iterative Rescorla-Wagner on binary preprocessed files.
  do.call(learnWeightsCompact, learnArgs)
}


#' Iterative weights learning based on Rescorla-Wagner equations from a
#' cue-outcome event structure.
#'
#' A function for learning the weights of cue-outcome pairs from a data frame
#' containing cues and outcomes based on the Rescorla-Wagner equations.\cr\cr
#' By default the function returns a \code{\link{ndlLearner}}
#' \link[=ReferenceClasses]{reference class} object that, unlike most
#' R objects, can change its state, doesn't get copied on variable assignment
#' or for function calls and cannot save the weights over R sessions.
#' 
#' This iterative learning method learns the events in the same order as they
#' appear in the data frame. As a precaution against accidentally using data
#' in which several instances of the same cue-outcome pair are reduced to
#' one row (with a frequency greater than one), all entries in the frequency
#' column (if present) need to be be one.
#' 
#' @param cuesOutcomes A data frame with cues and outcomes in which each row is
#'   a single learning event, therefore no frequencies above one are allowed in
#'   the optional frequency column. The data frame may be created with
#'   \code{\link{ndlCuesOutcomes}} or with the accessory script in the
#'   inst/scripts directory:
#'    \describe{
#'      \item{\code{Cues}}{A character vector specifying the cues. When there
#'        is more than one cue, the cues should be separated by underscores.}
#'      \item{\code{Outcomes}}{A character vector specifying the outcomes.
#'        When there is more than one outcome, the outcomes should be separated
#'        by underscores.}
#'      \item{\code{Frequency}}{Optional: A numeric vector specifying the
#'        frequency with which a combination of cues and outcomes occurs.
#'        All frequencies need to be set to 1 for this function.} }
#' @param sourceName The common prefix that should be used for naming the
#'   temporary files that get created during processing. By default
#'   the name of the object passed to \code{cuesOutcomes} (if an expression
#'   is used that contains characters other than 0-9, a-Z, _, - or . then
#'   'dataFrame' is used instead).
#' @param dirPath The path of the folder where the temporary files that get
#'   created during processing should be stored. By default a temporary
#'   directory is used that is very likely to be a new directory.
#' @param overwrite Whether the intermediary files (preprocessed data source)
#'   should be overwritten if they already exist in the same \code{dirPath}
#'   and with the same \code{sourceName}
#'   (with the default value of \code{dirPath} this should never happen).
#' @template LearnWeights-PreprocessingComment
#' @template Threads_Verbose_Debug
#' @inheritParams learnWeightsCompact
#' @inheritParams ndlPreprocessTabular
#' @template LearnWeights-ReturnValue
#' @seealso \code{\link{estimateWeightsCompact}},
#'   \code{\link{ndlPreprocessDataframe}}, \code{\link{learnWeightsCompact}},
#'   \code{\link{learnWeightsCorpus}}, \code{\link{learnWeightsTabular}}
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
#' # Learn the events and return a ndlLearner object.
#' learner <- learnWeights(lexampleLong)
#' 
#' # Show summary information about the state of the learner object.
#' learner
#' 
#' # Get the weight matrix.
#' learnerWeights <- learner$getWeights()
#' 
#' # Unlike the other learnWeights* methods, learnWeigths() by default saves
#' # the preprocessed files in the temporary directory of the R session and,
#' # unless a different dirPath is used, also defaults to retainFiles = FALSE.
#' # Therefore the files get deleted automatically after learning.
#' stopifnot(!learner$filesExist())
#' 
#' 
#' # We can also learn in several steps instead of all events at once.
#' # This time we don't delete the event files automatically after learning.
#' learner2 <- learnWeights(lexampleLong, numEventsToLearn=8, retainFiles=TRUE)
#' 
#' # So far only 8 events have been learned.
#' learner2
#' 
#' # Saving the weight matrix is also useful for saving the weights, as the
#' # learner objects loose the weight information if saved over R sessions.
#' # Below we resume learning at this state (learner2Resumed).
#' learner2WeigthsAfter8Events <- learner2$getWeights()
#' 
#' # Assigning a learner object to another variable does not copy it!
#' # Now we have just introduced a new name to refer to the same object.
#' # If one changes, then the other one changes as well.
#' learner2Assigned <- learner2
#' 
#' # We can however copy the learner object to avoid these problems:
#' learner2Copied <- learner2$copy()
#' 
#' # Now we learn at first the next 40 events and then all remaining events.
#' learner2$learn(40)
#' learner2$learn()
#' 
#' # Now learner 2 also learned all events. Therefore the weights are the same.
#' stopifnot(isTRUE(all.equal(learnerWeights, learner2$getWeights(),
#'                            check.attributes = FALSE)))
#' 
#' 
#' # As learner2Assigned was created by assigning learner2 to a new variable,
#' # learner2Assigned still has the same state as learner2.
#' stopifnot(isTRUE(all.equal(learner2, learner2Assigned)))
#' 
#' # learner2Copied is still at the same state as when it was copied, e.g.
#' # the next event is 8 (as event ids are 0 based and 8 events were learned).
#' stopifnot(learner2Copied$nextEventToLearn == 8)
#' 
#' 
#' # We can also create a new learner object from the weight matrix we saved.
#' learner2Resumed <- resumeLearning(learner2WeigthsAfter8Events)
#' 
#' # The copied and the resumed learning object are at the same state
#' learner2Resumed
#' stopifnot(isTRUE(all.equal(learner2Copied, learner2Resumed)))
#' 
#' # And of course we can continue learning with both.
#' learner2Copied$learn()
#' learner2Resumed$learn()
#' 
#' # And get the same result
#' stopifnot(isTRUE(all.equal(learnerWeights, learner2Copied$getWeights(),
#'                            check.attributes=FALSE)))
#' stopifnot(isTRUE(all.equal(learnerWeights, learner2Resumed$getWeights(),
#'                            check.attributes=FALSE)))
#' 
#' 
#' # As we set retainFiles to TRUE for learner2, the files still exist,
#' # even though all events have been learned already.
#' stopifnot(learner2$filesExist())
#' 
#' # Lets delete the preprocessed files as we don't need them anymore.
#' learner2$deleteFiles()
#' 
learnWeights <-
  function(cuesOutcomes, normalizeCase = FALSE, maxNumberCues = 30000,
           maxNumberOutcomes = 60000, alpha=0.1, beta=0.1, lambda=1,
           numEventsToLearn = "all", removeDuplicates = TRUE,
           sourceName = deparse(substitute(cuesOutcomes)),
           dirPath = NA, overwrite = FALSE,
           preprocessingComment = "", asMatrix = FALSE,
           retainFiles = !is.na(dirPath),
           numThreads = Sys.getenv("OMP_NUM_THREADS"),
           verbose = FALSE, debug = FALSE)
{
  # Just to evaluate retainFiles before dirPath changes
  dummy <- capture.output(print(retainFiles))
  
  # Check for okayish source name when automatically choosing it.
  if (sourceName == deparse(substitute(cuesOutcomes)) &
        !grepl("^[\\.0-9a-zA-Z_-]*$", sourceName)) {
    sourceName <- "dataFrame"
  }
  
  numThreads <- checkNumThreads(numThreads)
  if (is.na(dirPath))
    dirPath <- tempfile(pattern=paste0(sourceName,"-"))
  
  if ("Frequency" %in% colnames(cuesOutcomes)) {
    if (any(cuesOutcomes$Frequency != 1)) {
      stop(paste(
        "You are attempting to learn from events that were not collected",
        "sequentially (at least one event has a frequency greater than 1).",
        "This does not work with the iterative learning method.",
        "Please use the Danks equations based estimateWeights function instead."))
    }
  } else {
    cuesOutcomes$Frequency <- rep(1, nrow(cuesOutcomes))
  }
  
  # Generate compact binary representation.
  ndlPreprocessDataframe(cuesOutcomes, sourceName, dirPath, normalizeCase,
    maxNumberCues, maxNumberOutcomes, overwrite, preprocessingComment,
    numThreads, verbose, debug)
  
  # Use C++ implementation of iterative Rescorla-Wagner on binary files.
  learnWeightsCompact(sourceName, alpha, beta, lambda, dirPath,
    numEventsToLearn, removeDuplicates, asMatrix, retainFiles,
    numThreads, verbose, debug)
}

#' Iterative weights learning based on Rescorla-Wagner equations from a
#' text corpus.
#'
#' A function for learning the weights of cue-outcome pairs using the
#' Rescorla-Wagner equations and a text corpus as input. It first creates
#' events from the text corpus, with letter n-grams or words as cues (depending
#' on \code{useLetterNGrams}) and words as outcomes, and then learns the weights
#' from these events.\cr\cr
#' By default the function returns a \code{\link{ndlLearner}}
#' \link[=ReferenceClasses]{reference class} object that, unlike most
#' R objects, can change its state, doesn't get copied on variable assignment
#' or for function calls and cannot save the weights over R sessions.
#' 
# TODO Move the next two paragraphs into a template to share with ndlPreprocessCorpus
# (currently only the first @details section is used by roxygen2...)
#' @details
#' Each of the created events consists of one or more words as outcomes and
#' their letter n-grams as cues if \code{useLetterNGrams} is set to
#' \code{TRUE}. Otherwise two of every three words are used as cues and the
#' other as outcome (see \code{outcomeWordPosition}).
#' When letter n-grams are used as cues, the n-gram size can be set with
#' \code{maxNGramSize} and \code{includeSmallerNGrams} can be used for
#' including n-grams smaller than \code{maxNGramSize} as well.
#' With \code{useOutcomeTrigrams} word triples instead of single words can be
#' used as outcomes and subsequently as cues.
#' 
#' Only words that appear in \code{outcomesFile} are used as outcomes and
#' all other words are ignored. Furthermore, if \code{useLetterNGrams} is set
#' to \code{TRUE} words that contain letters which are not included in
#' \code{cueFile} are not used as outcomes either.
#' 
#' The created events are stored in binary event files and in some additional
#' text files (see \code{\link{ndlPreprocessCorpus}} for details). Afterwards
#' these files are used for learning the weights (using
#' \code{\link{learnWeightsCompact}}).
#' 
#' @template LearnWeights-DirPath_UseExistingFiles_Overwrite
#' @template LearnWeights-PreprocessingComment
#' @template Threads_Verbose_Debug
#' @inheritParams learnWeightsCompact
#' @inheritParams ndlPreprocessCorpus
#' @template LearnWeights-ReturnValue
#' @seealso \code{\link{estimateWeightsCompact}},
#'   \code{\link{ndlPreprocessCorpus}}, \code{\link{learnWeightsCompact}},
#'   \code{\link{learnWeightsTabular}}, \code{\link{learnWeights}}
#' 
#' @examples
#' # Convenience function for writing text files with UTF-8 encoding.
#' writeFile <- function(content, filePath) {
#'   out <- file(filePath, encoding="UTF-8")
#'   writeLines(content, out)
#'   close(con=out)
#' }
#'
#' filePath <- tempfile(pattern="corpus-")
#' writeFile(paste("This is probably the most tiny corpus ever,",
#'   "but enough for demonstrating how ndlPreprocessCorpus works."), filePath)
#' writeFile(letters, paste0(filePath, "-alphabet"))
#' writeFile(c("but", "corpus", "demonstrating", "enough", "ever", "for",
#'             "how", "is", "most", "probably", "the", "this", "tiny",
#'             "works", "xylophone"),
#'           paste0(filePath, "-dictionary"))
#' 
#' # The actual learning:
#' corpusLearner <- learnWeightsCorpus(filePath, paste0(filePath, "-alphabet"),
#'   paste0(filePath, "-dictionary"), normalizeCase=TRUE)
#' 
#' # Show summary information about the learning status
#' corpusLearner
#' 
#' # Inspect the first few events to see if it did what was intended
#' corpusLearner$viewEventsById(numEvents=3)
#' # Let's also look at the last events.
#' corpusLearner$viewEvents(numNext=0, numPrev=3)
#' 
#' # Get the weight matrix
#' corpusWeights <- corpusLearner$getWeights()
#' corpusWeights[1:2,]
#' # Note that there is no column "learnWeightsCorpus" in lexampleWeights, as
#' # that word is not in the outcomes file and neither is "xylophone", which is
#' # in the outcomes file, but not in the source (corpus) file.
#' 
#' # Step-wise learning leads to the same result
#' corpusLearner2 <- learnWeightsCorpus(filePath, paste0(filePath, "-alphabet"),
#'   paste0(filePath, "-dictionary"), normalizeCase=TRUE, numEventsToLearn=6)
#' 
#' # So far only 6 events have been learned:
#' corpusLearner2
#' 
#' # Lets learn at first another 2 and then the remaining events.
#' corpusLearner2$learn(2)
#' corpusLearner2$learn()
#' 
#' # The weights of one-shot and step-wise learning are the same.
#' stopifnot(isTRUE(all.equal(corpusWeights, corpusLearner2$getWeights())))
#' 
#' # Delete the preprocessed files
#' corpusLearner$deleteFiles()
#' 
#' # corpusLearner2 uses the same preprocessing source as corpusLearner, as by
#' # default useExistingFiles=TRUE and both used the same parameters.
#' # Therefore the preprocessed files of corpusLearner2 are already delted.
#' stopifnot(!corpusLearner2$filesExist())
#' 
#' # Remove the corpus file we created manually.
#' file.remove(filePath)
#' 
learnWeightsCorpus <-
  function(sourceFile, cueFile, outcomesFile, dirPath = "",
           normalizeCase = FALSE, useLetterNGrams = TRUE, maxNGramSize = 3,
           includeSmallerNGrams = FALSE, useOutcomeTrigrams = TRUE,
           outcomeWordPosition = 2, alpha=0.1, beta=0.1, lambda=1,
           numEventsToLearn = "all", removeDuplicates=TRUE,
           useExistingFiles = TRUE, overwrite = TRUE,
           preprocessingComment = "", asMatrix = FALSE, retainFiles = TRUE,
           numThreads = Sys.getenv("OMP_NUM_THREADS"),
           verbose = FALSE, debug = FALSE) {
  #TODO: check arguments (cueFile, outcomesFiles,...)
  
  preprocArgs <- list(normalizePath(sourceFile), cueFile, outcomesFile, normalizeCase,
                      useLetterNGrams, maxNGramSize, includeSmallerNGrams,
                      useOutcomeTrigrams, outcomeWordPosition, dirPath,
                      overwrite = overwrite, comment = preprocessingComment,
                      numThreads = numThreads, verbose = verbose, debug = debug)
  
  # sourceName and path (args 1 and 5) get set by learnWeightsCommon()
  learnArgs <- list(NA, alpha, beta, lambda, NA, numEventsToLearn,
                    removeDuplicates, asMatrix, retainFiles,
                    numThreads, verbose, debug)
  
  learnWeightsCommon(ndlPreprocessCorpus, preprocArgs, learnArgs,
                     sourceFile, dirPath, useExistingFiles)
}


#' Iterative weights learning based on Rescorla-Wagner equations from a
#' cue-outcome file.
#'
#' A function for learning the weights of cue-outcome pairs using the
#' Rescorla-Wagner equations from a text file containing cues, outcomes and
#' frequencies.\cr\cr
#' By default the function returns a \code{\link{ndlLearner}}
#' \link[=ReferenceClasses]{reference class} object that, unlike most
#' R objects, can change its state, doesn't get copied on variable assignment
#' or for function calls and cannot save the weights over R sessions.
#' 
#' This iterative learning method learns the events in the same order as they
#' appear in the input file. As a precaution against accidentally using data
#' in which several instances of the same cue-outcome pair are reduced to
#' one row (with a frequency greater than one), all entries in the frequency
#' column need to be one.
#' 
#' The input text file is first converted into a binary file format (see
#' \code{\link[ndl2]{ndlPreprocessTabular}} for details). Afterwards
#' \code{\link{learnWeightsCompact}} is used to learn the weights from these
#' binary event files.
#' 
#' @param sourceFile The path to a text file with 3 tab-seperated columns
#'   (cues, outcomes and frequencies), in which each row is a single learning
#'   event. Therefore all the entries in the frequency column need to be one.
#'   Multiple cues and outcomes within a line are seperated by underscores.
#'   The first line is assumed to be the header and is ignored.
#'   A text file in this format may be created with the accessory script in the
#'   inst/scripts directory or with the help of \code{\link{ndlCuesOutcomes}}.
#' @template LearnWeights-DirPath_UseExistingFiles_Overwrite
#' @template LearnWeights-PreprocessingComment
#' @template Threads_Verbose_Debug
#' @inheritParams learnWeightsCompact
#' @inheritParams ndlPreprocessTabular
#' @template LearnWeights-ReturnValue
#' @seealso \code{\link{estimateWeightsCompact}},
#'   \code{\link{ndlPreprocessTabular}}, \code{\link{learnWeightsCompact}},
#'   \code{\link{learnWeightsCorpus}}, \code{\link{learnWeights}}
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
#' # For demonstration purposes we create a tabular file in the desired format.
#' # If you do not have such a file already, just use learnWeights() instead.
#' filePath <- tempfile(pattern="learn_tabular-")
#' write.table(lexampleLong[c("Cues", "Outcomes", "Frequency")],
#'   file = filePath, sep="\t", quote=FALSE, fileEncoding = "UTF-8",
#'   row.names=FALSE, col.names=TRUE)
#'
#' # The actual learning
#' lexampleLearner <- learnWeightsTabular(filePath, retainFiles=FALSE)
#' 
#' # Show summary information about the learner object.
#' lexampleLearner
#' 
#' # Get the weight matrix
#' lexampleWeights <- lexampleLearner$getWeights()
#' 
#' # The preprocessed files have already been deleted, as we set retainFiles to
#' # FALSE and we already finished learning all events (to not learn all events
#' # right away use the numEventsToLearn parameter).
#' stopifnot(!lexampleLearner$filesExist())
#' 
#' # Remove the tabular file we created manually.
#' file.remove(filePath)
#' 
learnWeightsTabular <-
  function(sourceFile, dirPath = "", normalizeCase = FALSE,
           maxNumberCues = 30000, maxNumberOutcomes = 60000,
           alpha=0.1, beta=0.1, lambda=1,
           numEventsToLearn = "all", removeDuplicates=TRUE,
           useExistingFiles = TRUE, overwrite = TRUE,
           preprocessingComment = "", asMatrix = FALSE,
           retainFiles = TRUE, numThreads = Sys.getenv("OMP_NUM_THREADS"),
           verbose = FALSE, debug = FALSE) {  
  preprocArgs <- list(normalizePath(sourceFile), dirPath, normalizeCase, maxNumberCues,
                      maxNumberOutcomes, overwrite = overwrite,
                      comment = preprocessingComment, numThreads = numThreads,
                      verbose = verbose, debug = debug)

  # sourceName and path (args 1 and 5) get set by learnWeightsCommon()
  learnArgs <- list(NA, alpha, beta, lambda, NA, numEventsToLearn,
                    removeDuplicates, asMatrix, retainFiles,
                    numThreads, verbose, debug)
  
  learnWeightsCommon(ndlPreprocessTabular, preprocArgs, learnArgs,
                     sourceFile, dirPath, useExistingFiles)
  
}

#' Resume learning from a weight matrix
#' 
#' This function returns a \code{\link{ndlLearner}} object with the weights specified
#' in the weight matrix and at the same state as the original \code{\link{ndlLearner}}
#' object (using the information saved in the attributes of the weight matrix).
#' This is useful for resuming learning, e.g. in a different R session, as
#' \code{\link{ndlLearner}} objects cannot keep the weights over sessions.
#' 
#' @param ndlWeightMatrix A weight matrix from a \code{\link{ndlLearner}} object.
#'  To create such a weight matrix use the \code{$getWeights()} method of a
#'  \code{\link{ndlLearner}} object in a valid state to get the whole weight matrix.
#' @param newPath Path to the folder containing the preprocessed files if a different
#'  path then the one used in the original \code{\link{ndlLearner}} object should be
#'  used. If left at the default value the absolute or relative path of the previous
#'  learner object is used.
#' @param force Resume learning even if it seems that a different data source is at the
#'  (implicitly or explicitly) specified path. In this case the only condition is that
#'  the cues and outcomes of the originally used and the new data source match.
#' @param defaultNumThreads The new default setting for the number of threads to use.
#'  By default the maximum the same setting as in the previous learning is used, but if
#'  this value is higher than the value of the OMP_NUM_THREADS environment variable, it
#'  gets restricted to the latter (and a warning is shown).
#' @param defaultVerbose The new default verbosity setting. This setting also applies to
#'  the \code{resumeLearning} function itself. By default the old value is used.
#' @param defaultDebug The new default debug output setting. This setting also applies to
#'  the \code{resumeLearning} function itself. By default the old value is used.
#'  
#' @return Returns a \code{\link{ndlLearner}} object at the same state and with the same weights
#'  as the \code{\link{ndlLearner}} object with which the weight matrix was generated.
#'  If the data source doesn't exist anymore or has changed, the function stops with an error.
#'
#' @seealso \code{\link{ndlLearner}}, \code{\link{learnWeights}},
#' \code{\link{learnWeightsCompact}}, \code{\link{learnWeightsTabular}},
#' \code{\link{learnWeightsCorpus}}
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
#' # Learn the events and return a ndlLearner object.
#' learner <- learnWeights(lexampleLong, numEventsToLearn=40, retainFiles=TRUE)
#' 
#' # Show summary information about the state of the learner object.
#' learner
#' 
#' # Save the weight matrix
#' learnerWeightsAfter40 <- learner$getWeights()
#' 
#' # Now we can do something else with the learner object. We already saved
#' # the weights so it doesn't affect them.
#' learner$learn(20)
#' 
#' # Now we create a new learner object from the weight matrix (this can also
#' # happen in a different R session, as long as the preprocessed files exist).
#' learnerResumed <- resumeLearning(learnerWeightsAfter40)
#' 
#' # Show summary information about the state of the resumed learner object.
#' learnerResumed
#' 
#' # Lets finish learning for both ndlLearner objects.
#' learner$learn()
#' learnerResumed$learn()
#' 
#' # Now both learners should have learned the same weights.
#' stopifnot(isTRUE(all.equal(learner$getWeights(), learnerResumed$getWeights(),
#'                            check.attributes = FALSE)))
#' 
#' # Now we delete the preprocessed files. As both learners use the same,
#' # we need to only delete them once.
#' learner$deleteFiles()
#' 
resumeLearning <- function(ndlWeightMatrix, newPath=NA, force=FALSE,
                           defaultNumThreads=NA, defaultVerbose=NA, defaultDebug=NA) {
  info <- getInfoFromWeightMatrix(ndlWeightMatrix, includeIndices = TRUE)
  if (is.na(newPath))
    newPath <- getBestPreviouslyUsedPath(info)
  
  # Check if it is the same data source as last time.
  if (force) { stopOn <- c("cues_outcomes")
  } else stopOn <- eval(formals(checkSamePreprocessingSource)$errors)
  newDataSource <- ndlPreprocessingResult$new(info$sourceName, newPath)
  checkSamePreprocessingSource(info, newDataSource, "previously used", "new", errors=stopOn)
  
  newDefaultNumThreads <- getNumThreadsToUse(info$defaultNumThreads, defaultNumThreads)    
  newDefaultVerbose <- if(is.na(defaultVerbose)) info$defaultVerbose else defaultVerbose
  newDefaultDebug <- if(is.na(defaultDebug)) info$defaultDebug else defaultDebug
  
  obj <- with(info, ndlLearner$new(sourceName, newPath, defaultAlpha, defaultBeta, lambda,
                                   removeDuplicates, retainFiles, newDefaultNumThreads,
                                   newDefaultVerbose, newDefaultDebug))
  
  if (obj$totalNumEvents != info$totalNumEvents) {
    stop(paste0("The total number of events differs for the loaded ",
                "preprocessed files (", obj$totalNumEvents, ") and for the ",
                "preprocessed files used for learning the input weight ",
                "matrix (", info$totalNumEvents, ")."))
  }
  
  obj$setNextEventToLearn(info$nextEventToLearn)
  obj$setWeightMatrix(ndlWeightMatrix)
  
  return(obj)
}
