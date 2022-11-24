#### Converts various text files into compact binary format used by
####   estimateWeightsCompact() and learnWeightsCompact().
#### See R documentation for details.

# Common preprocessing part for preprocess tabular and corpus
ndlPreprocessCommon = function(preprocArgs, overwrite, returnPath, typeName) {
  # Debug setting overrules verbosity setting.
  if (preprocArgs$debug)
    preprocArgs$verbose <- TRUE
  
  preprocArgs$numThreads <- checkNumThreads(preprocArgs$numThreads)
  
  if (!preprocArgs$onlySettingsFile)
    with(preprocArgs, checkNoEventsDir(outputDirWithNamePrefix, overwrite, verbose))
  
  if (preprocArgs$verbose & !preprocArgs$onlySettingsFile)
    message(paste0("Starting preprocessing of ", typeName, "."))
  flush.console()
  
  # Using external C++ function through Rcpp attributes
  result <- do.call(.ndlpreprocess, preprocArgs)
  
  if (!result) stop("An error occurred during preprocessing.")
  else if (preprocArgs$verbose & !preprocArgs$onlySettingsFile)
    message("Successfully finished preprocessing.")
  
  if (returnPath)
    return(preprocArgs$outputDirWithNamePrefix)
  return(with(preprocArgs, ndlPreprocessingResult$new(
    basename(outputDirWithNamePrefix), dirname(outputDirWithNamePrefix))))
}

#' Convert text corpus into events stored in a binary format
#'
#' Creates events for discrimination learning from a text corpus and saves them
#' in the special binary format used by \code{\link{estimateWeightsCompact}}
#' and \code{\link{learnWeightsCompact}} (see also
#' \code{\link{learnWeightsCorpus}}). Depending on \code{useLetterNGrams} the
#' events have either letter n-grams or word n-grams as cues and words as
#' outcomes. By default a \code{\link{ndlPreprocessingResult}} object gets
#' returned.
#' 
# TODO Move the next two paragraphs into a template to share with learnWeightsCorpus
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
#' The order of the events is the same as the order in which the words appear
#' in the text corpus. The events are stored in one or several numbered event
#' files in the event folder. These event files are in a specific binary format
#' and can therefore not be opened with a text editor. They can be inspected
#' with the \code{\link{ndlViewEvents}} function or the \code{$viewEventsById}
#' method of the result object (if \code{returnPath} is \code{FALSE} as it is
#' by default).
#' This function also creates text files which list all the cues and outcomes
#' that occur in the events, as well as one that lists the words that occurred
#' in the input file but were not valid outcomes.
#' 
#' An alternative to calling this function first and then
#' \code{\link{learnWeightsCompact}} is to use \code{\link{learnWeightsCorpus}}
#' instead, which combines both of these functions.
#' 
#' @param sourceFile The file path of the input text file.
#' @param cueFile The file path to the file containing one cue per line.
#'   If \code{useLetterNGrams} is set to \code{TRUE}, it is not the cues
#'   themselves this file should contain but rather a list of valid letters,
#'   one letter per line, that is then used for creating the letter n-grams
#'   cues and for removing outcomes that contain other letters.
#' @param outcomesFile The file path of the outcome file (a list of outcomes,
#'   one outcome per line - outcomes containing invalid letters are removed).
#' @param useLetterNGrams If \code{TRUE} then letter n-grams of the outcome
#'   words are used as cues (see \code{maxNGramSize},
#'   \code{includeSmallerNGrams} and \code{useOutcomeTrigrams}).
#'   Otherwise whole words are used as cues, with always two of three words
#'   as cues and the other one as outcome (see \code{outcomeWordPosition}).
#' @param maxNGramSize The letter n-gram size to use for the cues or the
#'   maximum letter n-gram size to use if \code{includeSmallerNGrams=TRUE}.
#'   Needs to be either 1, 2 or 3. This setting is ignored when
#'   \code{useLetterNGrams} is set to \code{FALSE}.
#' @param includeSmallerNGrams Whether all letter n-grams up to
#'   \code{maxNGramSize} should be included as cues.
#'   This setting is ignored when \code{useLetterNGrams} is set to
#'   \code{FALSE}.
#' @param useOutcomeTrigrams Whether word trigrams instead of unigrams should
#'   be used for the outcomes of each event. In that case events also have
#'   three outcomes instead of one and every word appears in three events:
#'   1. with the two words before it,
#'   2. with the word before it and the word behind it, and
#'   3. with the two words behind it.
#'   This setting is ignored when \code{useLetterNGrams} is set to
#'   \code{FALSE}.
#' @param outcomeWordPosition Which word in each triple should be used as the
#'  outcome. Needs to be either 1, 2 or 3. This settings is ignored when
#'  \code{useLetterNGrams} is set to \code{TRUE}.
#' @template Preprocess-OutputDir_ReturnPath
#' @template NormalizeCase_Overwrite
#' @template Threads_Verbose_Debug
#' @template Preprocess-ReturnValue
#' @seealso \code{\link{learnWeightsCorpus}},
#'   \code{\link{learnWeightsCompact}}, \code{\link{estimateWeightsCompact}},
#'   \code{\link{ndlPreprocessTabular}}, \code{\link{ndlPreprocessDataframe}}
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
#' # The actual preprocessing.
#' resultCorpus <- ndlPreprocessCorpus(filePath, paste0(filePath, "-alphabet"),
#'   paste0(filePath, "-dictionary"), normalizeCase=TRUE)
#'   
#' # Show summary information about the preprocessed corpus.
#' resultCorpus
#' 
#' # Return the first three events in human readable form as a data frame.
#' resultCorpus$viewEventsById(startId=0, numEvents=3)
#' 
#' # Delete the files and folders that were created during preprocessing.
#' resultCorpus$deleteFiles()
#' 
#' # Remove the corpus file we created manually.
#' file.remove(filePath)
#' 
ndlPreprocessCorpus <- 
  function(sourceFile, cueFile, outcomesFile, normalizeCase = TRUE,
           useLetterNGrams = TRUE, maxNGramSize = 3, includeSmallerNGrams = FALSE,
           useOutcomeTrigrams = TRUE, outcomeWordPosition = 2, outputDir = "",
           overwrite = FALSE, comment = "", returnPath = FALSE,
           onlySettingsFile = FALSE,
           numThreads = Sys.getenv("OMP_NUM_THREADS"),
           verbose = FALSE, debug = FALSE)
{
  #TODO: check arguments (cueFile, outcomesFile)

  # Using absolute paths for the C++ function.
  sourceFile <- checkFileExistsAndNormalizePath(sourceFile)
  cueFile <- checkFileExistsAndNormalizePath(cueFile)
  outcomesFile <- checkFileExistsAndNormalizePath(outcomesFile)
  outputDirWithNamePrefix <- getOutputDirWithNamePrefix(outputDir, sourceFile)
  
  inclusiveValue <- ifelse(includeSmallerNGrams, 1, 0)
  windowSizeValue <- ifelse(useOutcomeTrigrams | !useLetterNGrams, 1, 0)
  maxN <- ifelse(useLetterNGrams, maxNGramSize, outcomeWordPosition)
  
  preprocArgs <- list(filename = sourceFile, corpusType = "N",
                      cueFile = cueFile, outcomeFile = outcomesFile,
                      numThreads = numThreads, outputDirWithNamePrefix = outputDirWithNamePrefix,
                      NormCase = normalizeCase,  maxN = maxN,
                      inclusive = inclusiveValue, WindowSize = windowSizeValue,
                      useLetterNGrams = useLetterNGrams,
                      fileInfoComment = comment, onlySettingsFile=onlySettingsFile,
                      verbose = verbose, debug = debug)
  
  ndlPreprocessCommon(preprocArgs, overwrite, returnPath, "corpus file (full text)")
}


#' Convert tabular cue-outcome file into binary format
#' 
#' Converts the given tabular file with cues, outcomes and frequencies into the
#' special binary format used by \code{\link{estimateWeightsCompact}} and
#' \code{\link{learnWeightsCompact}} (see also
#' \code{\link{learnWeightsTabular}}). It saves the binary files together
#' with additional text files in the \code{outputDir} and returns by default a
#' \code{\link{ndlPreprocessingResult}} object.
#' 
#' The tabular input file needs to contain 3 tab-seperated columns: cues,
#' outcomes and frequencies (in this order).
#' The first line is assumed to be the header and is therefore ignored.
#' If an event (one row) contains multiple cues or outcomes, then they
#' need to be seperated by underscores.
#' 
#' @param sourceFile The file path of the input tabular file.
#' @param maxNumberCues The maximum number of cues to use. If there are more
#'   cues in the input file then only the most frequent \code{maxNumberCues}
#'   cues are used. Please note that events containing any excluded cue are
#'   completely excluded (rather than consisting just of the remaining cues).
#'   Defaults to 30,000.
#' @param maxNumberOutcomes The maximum number of outcomes to use. If there are
#'   more outcomes in the input file then only the most frequent
#'   \code{maxNumberOutcomes} outcomes are used. Events are kept as long as
#'   there is at least one outcome in it that has not been excluded.
#'   Defaults to 60,000.
#' @template Preprocess-OutputDir_ReturnPath
#' @template NormalizeCase_Overwrite
#' @template Threads_Verbose_Debug
#' @template Preprocess-ReturnValue
#' @seealso \code{\link{learnWeightsTabular}},
#'   \code{\link{learnWeightsCompact}}, \code{\link{estimateWeightsCompact}},
#'   \code{\link{ndlPreprocessCorpus}}, \code{\link{ndlPreprocessDataframe}}
#' 
#' @examples
#' # For demonstration purposes we create a tabular file in the desired format.
#' # If you do not have such a file already, just use ndlPreprocessDataframe().
#' data(lexample)
#' lexample$Cues <- orthoCoding(lexample$Word, grams=2)
#' 
#' filePath <- tempfile(pattern="tabular-")
#' write.table(lexample[c("Cues", "Outcomes", "Frequency")],
#'   file = filePath, sep="\t", quote=FALSE, fileEncoding = "UTF-8",
#'   row.names=FALSE, col.names=TRUE)
#'
#' resultTabular <- ndlPreprocessTabular(filePath)
#' 
#' # Show summary information about the preprocessed tabular file.
#' resultTabular
#' 
#' # Delete the files and folders that were created during preprocessing.
#' resultTabular$deleteFiles()
#' 
#' # Remove the tabular file we created manually.
#' file.remove(filePath)
#' 
ndlPreprocessTabular <- 
  function(sourceFile, outputDir = "", normalizeCase = TRUE,
           maxNumberCues = 30000, maxNumberOutcomes = 60000, overwrite = FALSE,
           comment = "", returnPath = FALSE, onlySettingsFile = FALSE,
           numThreads = Sys.getenv("OMP_NUM_THREADS"), verbose = FALSE, debug = FALSE)
{
  sourceFile <- checkFileExistsAndNormalizePath(sourceFile)
  outputDirWithNamePrefix <- getOutputDirWithNamePrefix(outputDir, sourceFile)
  
  preprocArgs <- list(filename=sourceFile, corpusType="T",
                      numThreads = numThreads, outputDirWithNamePrefix = outputDirWithNamePrefix,
                      NormCase = normalizeCase, cueThreshold = maxNumberCues,
                      outcomeThreshold = maxNumberOutcomes, fileInfoComment = comment,
                      onlySettingsFile=onlySettingsFile, verbose = verbose, debug = debug)

  ndlPreprocessCommon(preprocArgs, overwrite, returnPath, "tabular file")
}


commentDataFrame <- "from temporary file (ndlPreprocessDataframe)"

#' Save cue-outcome data frame as binary file
#'
#' Converts a data frame with with cues, outcomes and frequencies into the
#' special binary format used by \code{\link{estimateWeightsCompact}}
#' and \code{\link{learnWeightsCompact}} (see also \code{\link{learnWeights}}).
#' It saves the binary files together with additional text files in the
#' \code{outputDir} and returns by default a
#' \code{\link{ndlPreprocessingResult}} object.
#' 
#' @param cuesOutcomes A data frame with cues, outcomes  and frequencies.
#'   If the preprocessed data is intended for iterative learning (any of the
#'   \code{learnWeights} functions) all frequencies need to be 1.
#'   The data frame may be created with \code{\link{ndlCuesOutcomes}} or
#'   with the accessory script in the inst/scripts directory:
#'    \describe{
#'      \item{\code{Cues}}{A character vector specifying the cues. When there
#'        is more than one cue, the cues should be separated by underscores.}
#'      \item{\code{Outcomes}}{A character vector specifying the outcomes.
#'        When there is more than one outcome, the outcomes should be separated
#'        by underscores.}
#'      \item{\code{Frequency}}{A numeric vector specifying the
#'        frequency with which a combination of cues and outcomes occurs.} }
#' @param sourceName The common prefix used for naming the files. By default
#'   the name of the object passed to \code{cuesOutcomes} (if an expression
#'   is used that contains characters other than 0-9, a-Z, _, - or . then
#'   'dataFrame' is used instead).
#' @param outputDir The file path to the folder where the binary files and
#'   text files are saved. If the folder does not exist yet, a new folder is
#'   created. Defaults to a folder in the temporary directory of the R session
#'   whose name is formed by concatenating \code{sourceName} with a dash and
#'   a random string in hex.
#' @template NormalizeCase_Overwrite
#' @template Threads_Verbose_Debug
#' @inheritParams ndlPreprocessTabular
#' @template Preprocess-ReturnValue
#' @seealso \code{\link{learnWeights}},
#'   \code{\link{learnWeightsCompact}}, \code{\link{estimateWeightsCompact}},
#'   \code{\link{ndlPreprocessCorpus}}, \code{\link{ndlPreprocessTabular}}
#'   
#' @examples
#' data(lexample)
#' lexample$Cues <- orthoCoding(lexample$Word, grams=2)
#' 
#' # Preprocess the data frame.
#' resultDfDanks <- ndlPreprocessDataframe(lexample)
#' resultDfDanks # Print summary information about the preprocessed data frame.
#' 
#' # When the data is to be used with the Rescorla-Wagner equations rather
#' # than the Danks equilibrium equations the order of the events is important.
#' # learnWeightsCompact() therefore does not accept data with frequencies
#' # greater than 1.
#' # Here we simulate the order, assuming they are encountered randomly.
#' lexampleLong <- lexample[sample(rep(1:nrow(lexample), lexample$Frequency)),]
#' lexampleLong$Frequency <- 1
#' resultDfIter <- ndlPreprocessDataframe(lexampleLong)
#' resultDfIter # Print summary information about the preprocessed data frame.
#' 
#' # Compare the two preprocessed data frames.
#' resultDfIter$compareDataSource(resultDfDanks)
#' 
#' # Delete the created files
#' resultDfDanks$deleteFiles()
#' resultDfIter$deleteFiles()
#' 
ndlPreprocessDataframe <-
  function(cuesOutcomes, sourceName = deparse(substitute(cuesOutcomes)),
           outputDir = tempfile(pattern = paste0(sourceName, "-")),
           normalizeCase = TRUE, maxNumberCues = 30000,
           maxNumberOutcomes = 60000, overwrite = FALSE,
           comment = "", returnPath = FALSE,
           numThreads = Sys.getenv("OMP_NUM_THREADS"),
           verbose = FALSE, debug = FALSE)
{
	if (!is.data.frame(cuesOutcomes)) {
		stop("Argument 'cuesOutcomes' needs to be a data frame.")
	}
  
  # Check for okayish source name when automatically choosing it.
  if (sourceName == deparse(substitute(cuesOutcomes)) &
        !grepl("^[\\.0-9a-zA-Z_-]*$", sourceName)) {
    sourceName <- "dataFrame"
    outputDir <- tempfile(pattern = paste0(sourceName, "-"))
  }
  
  if (comment == "") { comment <- commentDataFrame
  } else comment <- paste0(commentDataFrame, " - ", comment)

  # In this case we use the output dir also as location for the temporary
  # input file and we use the prefix common to all files created by preprocess
  # as the file name for the input file.
  filePath = getOutputDirWithNamePrefix(outputDir, sourceName)
  
  if (file.exists(filePath) && !overwrite) {
    stop(paste0("Temporary file '", filePath, "' already exists and ",
                "the parameter 'overwrite' was set to false."))
  }
  else {
    write.table(cuesOutcomes[c("Cues", "Outcomes", "Frequency")],
      file = filePath, quote=FALSE, sep="\t", fileEncoding = "UTF-8",
      row.names=FALSE, col.names=TRUE)
  }
  
  return(ndlPreprocessTabular(filePath, outputDir, normalizeCase, maxNumberCues,
    maxNumberOutcomes, overwrite, comment, returnPath, F, numThreads, verbose, debug))
}
