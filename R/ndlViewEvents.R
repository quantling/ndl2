#' Inspecting the output of the ndlPreprocess* functions
#'
#' \code{ndlViewEvents} reads the binary files and text files produced by one
#' of the ndlPreprocess* functions (\code{\link{ndlPreprocessCorpus}},
#' \code{\link{ndlPreprocessTabular}} or \code{\link{ndlPreprocessDataframe}})
#' and returns the selected events as rows in
#' a data frame.
#' 
#' This function is mostly intended for checking the output of the
#' ndlPreprocess* functions. For using the output either for the iterative
#' learning with the Rescorla-Wagner equations or for the equilibrium weights
#' with the Danks equations the output of the ndlPreprocess* functions does
#' _not_ need to be converted. Instead \code{\link{estimateWeightsCompact}}
#' or \code{\link{learnWeightsCompact}} can be used.
#' 
#' @param sourceName The common prefix of the files created by one of the
#'   \code{ndlPreprocess} functions. It is also the prefix of the folder
#'   containing the event files (the part before .events/). The source name
#'   can be determined from the return value of any of the \code{ndlPreprocess}
#'   functions with \code{\link[base]{basename}}.
#' @param start The event number of the first event to output. Note that the
#'   numbering of events starts from 0. Defaults to 0.
#' @param numEvents The (maximum) number of events to return.
#' @param path The path to the folder that contains the files and events folder
#'   created by any of the \code{ndlPreprocess} functions. This path can be
#'   be derived from the return value of a \code{ndlPreprocess} function
#'   with \code{\link[base]{dirname}}.
#'   Defaults to the current working directory.
#' @param showFrequency Include the frequency column in the result. Default: TRUE
#' @param verbose Show verbose output.
#' @param glue The character inserted between cues or outcomes in the same
#'   event.
#' @return A data frame with columns \code{Event} (the event number),
#'   \code{Cues} (the cues present at the event, seperated by \code{glue}) and
#'   \code{Outcomes} (the outcomes present at the event, seperated by
#'   \code{glue}), as well as \code{Frequency} if \code{showFrequency == TRUE}.
#'   The data frame will contain \code{numEvents} rows if that many events with
#'   event numbers >= \code{start} are available, and otherwise all events with
#'   event numbers >= \code{start}.
#' @seealso \code{\link{estimateWeightsCompact}},
#'   \code{\link{learnWeightsCompact}}
#'   \code{\link{ndlPreprocessCorpus}}, \code{\link{ndlPreprocessTabular}},
#'   \code{\link{ndlPreprocessDataframe}}
#' @examples
#' data(lexample)
#' lexample$Cues <- orthoCoding(lexample$Word, grams=2)
#' outputDirWithPrefix <- ndlPreprocessDataframe(lexample, normalizeCase=FALSE,
#'                                               returnPath=TRUE)
#' 
#' preprocessResult <- ndlViewEvents(basename(outputDirWithPrefix), 0, 10^6,
#'                                   dirname(outputDirWithPrefix))
#'
#' # For data frames and tabular files the ndlPreprocess* functions just save
#' # the contents in a binary format (if normalizeCase=F, as well as both
#' # maxNumberCues and maxNumberOutcomes being set to a high enough value).
#' #
#' # Therefore, the common columns of lexample and preprocessResult should be
#' # the same apart from the order of cues/outcomes within cue/outcome strings.
#' # (In preprocessResult the cues/outcomes are sorted alphabetically within
#' #  an event, but the sort order in R can be different, making it cumbersome
#' #  to compare, but see below for a comparison method that works.)
#' stopifnot(isTRUE(all.equal(lexample$Frequency, preprocessResult$Frequency)))
#' stopifnot(isTRUE(all.equal(
#'   lapply(strsplit(lexample$Cues, "_"), sort),
#'   lapply(strsplit(preprocessResult$Cues, "_"), sort))))
#' stopifnot(isTRUE(all.equal(
#'   lapply(strsplit(lexample$Outcomes, "_"), sort),
#'   lapply(strsplit(preprocessResult$Outcomes, "_"), sort))))
ndlViewEvents <- function(sourceName, start = 0, numEvents = 10,
    path = getwd(), showFrequency = TRUE, verbose = FALSE, glue = "_") {
  path <- normalizePath(path, winslash="/")
  
  preprocessedFilesExist(sourceName, path, stopOnFalse = TRUE)
  .viewEvents(start, numEvents, path, sourceName, showFrequency, verbose, glue)
}  
