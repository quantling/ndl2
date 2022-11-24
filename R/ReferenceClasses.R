#' Information about the preprocessing output.
#' 
#' Objects of this class contain useful information about the output from the
#' preprocessing step (e.g. the result from \code{\link{ndlPreprocessTabular}},
#' \code{\link{ndlPreprocessCorpus}} or \code{\link{ndlPreprocessDataframe}})
#' and ways to interact with it it, such as viewing it as a data frame in
#' human readable form (\code{$viewEventsById}), deleting it
#' (\code{$deleteFiles}) or comparing it to a different
#' preprocessing output (\code{$compareDataSource}).
#' 
#' All the fields are read-only, as most of the information in this object is
#' extracted from the preprocessed files itself. Only the sourceName and path
#' could change while keeping the same data source. In that case use
#' ndlPreprocessingResult$new(sourceName, path) to create a new object.
#' 
#' @field rawPath The path of the location of the preprocessed files as input
#'  by the user (can be a relative path).
#' @field normalizedPath Absolute path to the preprocessed file location.
#' @field sourceName The name of the source, usually the file name of the input
#'  file/object. It is also the prefix of (almost) all files that are created
#'  in the preprocessing step.
#' @field cueIndices A named numeric vector specifying the indices of the cues.
#' @field outcomeIndices A named numeric vector specifying the indices of the
#'  outcomes.
#' @field totalNumEvents The number of events in this data source.
#' @field preprocessingSettings A list containing settings and other information
#'  about the preprocessing (such as preprocessor version). This information is
#'  used to compare preprocessed data sources.
#' @field fromDataFrame Boolean indicating whether the input to the
#'  preprocessing was derived from an R data frame (instead of a text file).
#'  
#' @seealso \code{\link{ndlLearner}} which inherits all fields and methods,
#'  \code{\link{learnWeightsCompact}}, \code{\link{estimateWeightsCompact}},
#'  \code{\link{ndlPreprocessCorpus}}, \code{\link{ndlPreprocessTabular}},
#'  \code{\link{ndlPreprocessDataframe}}
#' 
#' @examples
#' data(lexample)
#' lexample$Cues <- orthoCoding(lexample$Word, grams=2)
#' 
#' # Preprocess the data frame (contains frequencies > 1, only useful for the
#' # Danks equilibrium equation, i.e. for the estimateWeightsCompact function).
#' preprocessingResult <- ndlPreprocessDataframe(lexample)
#' 
#' # Show summary information about the preprocessed data source.
#' preprocessingResult
#'
#' # Return the events in readable form as a data frame (especially useful when
#' # using the ndlPreprocessCorpus function, as it creates the events itself,
#' # rather than just changing it into the binary format).
#' preprocessingResult$viewEventsById(numEvents=3)
#' 
#' # Get the cues and outcomes
#' preprocessingResult$getCues()
#' preprocessingResult$getOutcomes()
#' 
#' # Show the file paths of all created and still existing text files and also
#' # of the folder containing the binary event files.
#' preprocessingResult$getFiles()
#' 
#' # Show help information about methods (contains the same information as the
#' # help file).
#' preprocessingResult$help()
#' preprocessingResult$help("compareDataSource")
#' 
#' # Lets compare it with another data source.
#' preprocessingResult2 <- ndlPreprocessDataframe(lexample)
#' 
#' # Although we preprocess the same R data frame, the preprocessor cannot know
#' # that and therefore preprocessed it again. With tabular or corpus files the
#' # preprocessor can tell from the modification date of the input file,
#' # whether or not it is (most likely) the same input and can act accordingly.
#' preprocessingResult$compareDataSource(preprocessingResult2)
#' 
#' # Finally, lets delete the files we created.
#' preprocessingResult$deleteFiles()
#' 
ndlPreprocessingResult <- setRefClass(
  "ndlPreprocessingResult",
   fields = list(
     rawPath="character",
     normalizedPath="character",
     sourceName="character",
     cueIndices="numeric",
     outcomeIndices="numeric",
     totalNumEvents="numeric",
     preprocessingSettings="list",
     fromDataFrame="logical"
   ),
   methods = list(
     initialize = function(sourceName="", path="") {
       # When the object is copied, the constructor is called without arguments.
       # All fields are locked, i.e. can only be assigned to once, and they are set
       # in the copy method after creating the new object, therefore we need to
       # avoid any assignments in the constructor.
       if (sourceName != "" || path != "") {
         sourceName <<- sourceName
         rawPath <<- path
         normalizedPath <<- normalizePath(path, mustWork = FALSE)
       
         if (preprocessedFilesExist(sourceName, rawPath, warn=T)) {
           cueDf <- read.delim(file.path(path, paste0(sourceName, ".cues")),
                               header=F, stringsAsFactors=F)
           cueIndices <<- getCueOrOutcomeIndices(cueDf[,1])
           
           outcomesDf <- read.delim(file.path(path, paste0(sourceName, ".outcomes")),
                                    header=F, stringsAsFactors=F)
           outcomeIndices <<- getCueOrOutcomeIndices(outcomesDf[,1])
           
           totalNumEvents <<- .getNumberOfEvents(file.path(path, paste0(sourceName, ".events")))
         
           preprocessingSettings <<- readPreprocessingSettingsFile(sourceName, path)
           if("comment" %in% names(preprocessingSettings)) {
             comment <- substr(preprocessingSettings[["comment"]],
                               1, nchar(commentDataFrame))
             fromDataFrame <<- comment == commentDataFrame
           } else fromDataFrame <<- F # Can only guess (better default, more output).
         } else {
           totalNumEvents <<- -1
           fromDataFrame <<- NA
         }
       }
     },
     
     getCues = function() {
       "Returns the cues as a vector in ascending order along cue id."
       names(cueIndices)
     },
     
     getOutcomes = function() {
       "Returns the outcomes as a vector in ascending order along outcome id."
       names(outcomeIndices)
     },
     
     viewEventsById = function(startId=0, numEvents=10, showFrequency = TRUE,
                               verbose = FALSE, glue = "_") {
       "Returns the selected preprocessed events as a data frame based on event id.
       The first event has id 0 and the last event has id totalNumEvents-1
       When an event contains multiple cues or outcomes, then they are
       seperated by 'glue', by default '_'."
       if (startId < 0) {
         stop(paste0("Start id ", startId, " is invalid. ",
                     "Events start with event id 0."))
       }
       
       if ((startId+numEvents) > totalNumEvents) {
         if (startId >= totalNumEvents)
           stop(paste0("No events with id >= ", startId, " exist."))
         
         warning(paste0("Only ", totalNumEvents - startId,
                        " event(s) with id >= ", startId, " exist(s)."))
         numEvents <- totalNumEvents - startId
       }
       
       ndlViewEvents(sourceName, startId, numEvents, normalizedPath,
                     showFrequency, verbose, glue)
     },
     
     compareDataSource = function(otherDataSource, path=NA, printOutput=TRUE) {
       "Compares this data source to another one.
       Any differences are printed to the console, if printOutput is set to TRUE.
       Further, it returns a logical vector indicating category wise whether
       both data sources are different. This logical vector can contain NAs
       for categories were this comparison is not applicable.
       The parameter otherDataSource can be either a ndlPreprocessingResult object,
       a ndlLearner object, a weight matrix, the output of the $getInfo method of a
       ndlLearner object or a string. In the last case, otherDataSource contains the
       name of the data source. It can additionally contain the path to the data
       source, if the path parameter is not set.
       The path parameter is only considered if otherDataSource is a string."
       
       if (inherits(otherDataSource, "ndlPreprocessingResult") | is.matrix(otherDataSource)) {
         otherObj <- otherDataSource
       } else if (is.list(otherDataSource)) { # info objects don't contain cue/outcome indices
         otherSourceName <- otherDataSource$sourceName
         otherPath <- getBestPreviouslyUsedPath(otherDataSource, stopOnFailure=FALSE)
         
         if (otherPath != FALSE & preprocessedFilesExist(otherSourceName, otherPath)) {
           obj <- ndlPreprocessingResult$new(otherSourceName, otherPath)
           isSameSource <- checkSamePreprocessingSource(.self, obj, errors=c(),warnings=c(), output=c())
           if (all(isSameSource[-"cues_outcomes"], na.rm = TRUE)) {
             otherObj <- obj
           } else otherObj <- otherDataSource
         } else otherObj <- otherDataSource
       } else {
         if (is.na(path)) {
           otherPath <- dirname(otherDataSource)
           otherSourceName <- basename(otherDataSource)
         } else {
           otherPath <- path
           otherSourceName <- otherDataSource
         }
         
         if(!preprocessedFilesExist(otherSourceName, otherPath)) {
           stop(paste0("Could not find (all) required preprocessing files for the ",
                       "other data source.\nSource name: '", otherSourceName,
                       ", path: '", otherPath, "'"))
         }
         otherObj <- ndlPreprocessingResult$new(otherSourceName, otherPath)
       }
      
       if (printOutput) { toOutput <- eval(formals(checkSamePreprocessingSource)$output)
       } else toOutput <- c()
       
       checkSamePreprocessingSource(.self, otherObj, "current", "other",
                                    errors=c(), warnings=c(), output=toOutput)
     },
     
     filesExist = function(warn = FALSE) {
       "Returns whether all the necessary preprocessed files exist.
       If warn is set to TRUE, a warning is issued if this is not the case."
       preprocessedFilesExist(sourceName, normalizedPath, stopOnFalse=F, warn=warn)
     },
     
     getFiles = function() {
       "Returns the paths of existing files and folders from this data source.
       It returns the absolute paths of all existing text files and of the folder
       containing the binary event files that were created during preprocessing."
       fileNames <- getPreprocessedFileNames(sourceName, normalizedPath, includeOptional=T)
       fileNames[file.exists(fileNames)]
     },
     
     deleteFiles = function(path = NA, force = FALSE, quiet = FALSE) {
       "Deletes the preprocessed files.
       By default it only deletes them if they seem to belong to this data source,
       otherwise it stops with an error. To delete them anyways use force=TRUE.
       If the folder only contains the preprocessed files, it gets deleted as well.
       The parameter quiet can be used to suppress comparison results, including errors.
       It returns invisibly true if the deletion was successful, false if it didn't succeed and
       NA if the files don't seem to belong to this data source (if quiet=TRUE and force=FALSE)."
       if (is.na(path))
         path <- normalizedPath
       
       if (!any(file.exists(getPreprocessedFileNames(sourceName, path, includeOptional=T))))
           return(invisible(TRUE))
       
       if (force) { stopOn <- c()
       } else stopOn <- eval(formals(checkSamePreprocessingSource)$errors)
       
       # Check whether the preprocessed files at that location didn't change.
       otherObj <- ndlPreprocessingResult$new(sourceName, path)
       if (quiet) {
         checkRes <- checkSamePreprocessingSource(.self, otherObj, "initial", "now present",
                                                  errors=c(), warnings=c(), output=c())
       } else {
         checkRes <- checkSamePreprocessingSource(.self, otherObj, "initial", "now present",
                                                  errors=stopOn)
       }
       if (all(checkRes, na.rm=T) || force) {
         result <- unlink(getPreprocessedFileNames(sourceName, path, includeOptional=T),
                          recursive=T)
         
         if (fromDataFrame) { # also delete the tabular text file that was created
           unlink(file.path(path, sourceName))
         }
         
         if (length(list.files(path, all.files=T, no..=T)) == 0) {
           unlink(path, recursive=T)
         }
         
         if (invisible(result == 1)) {
           allFiles <- getPreprocessedFileNames(sourceName, path, includeOptional=T)
           remainingFiles <- allFiles[file.exists(allFiles)]
           cat("Could not delete all preprocessed files. The following files remain:",
               remainingFiles, sep="\n")
         }
               
         return(invisible(result == 0)) # unlink returns 0 on success
       }
       return(invisible(NA))
     },
     
     help = function(methodName="") {
       "Show the help text for the specified method name."
       refC <- getRefClass(class(.self))
       if (methodName == "") {
         cat("Usage: $help(methodName)",
             paste0("See also: ?", refC@className, "\n"),
             "Methods:", paste(getRCFunctions(refC), collapse=", "),
             "Fields:", paste(names(refC$fields()), collapse=", "), sep="\n")
       } else {
         refC$help(eval(methodName))
       }
     },
     
     show = function() {
       "Displays summary information about the preprocessed data source."
       indentation <- 11
       fmt <- paste0("%-", indentation, "s %s")
       
       cat("ndlPreprocessingResult object", sep="\n")
       showPreprocessingInfo(fmt)
       cat(sprintf(fmt, "Output:", normalizedPath), sep="\n")
       if(!fromDataFrame) {
         showPreprocessingInputFile(fmt)
         showPreprocessingInputFileTime(fmt)
       }
       showCuesOutcomes(fmt)
       
       if (preprocessedFilesExist(sourceName, normalizedPath, stopOnFalse=F, warn=F)) {
#          firstEvents <- viewEventsById(numEvents=min(3, totalNumEvents))
#          if(totalNumEvents > 3)
#            firstEvents <- rbind(firstEvents, "...")
#          firstEventsText <- capture.output(print.data.frame(
#            firstEvents[,c("Cues", "Outcomes")], right=F))
#          cat(sprintf(fmt, "Events:", paste(totalNumEvents, "events")),
#              sapply(firstEventsText,#capture.output(firstEvents[,c("Cues", "Outcomes")]),
#                     function(x) sprintf(fmt, "", substr(x, 3, nchar(x)))), sep="\n")
         firstEvent <- viewEventsById(numEvents=min(1, totalNumEvents))
         cat(sprintf(fmt, "Events:", paste0(totalNumEvents, ": ", firstEvent[, "Cues"],
                                            " -> ", firstEvent[, "Outcomes"],
                                            ifelse(totalNumEvents > 1, ", ...", ""))), sep="\n")
       }
       showPreprocessingParameters(fmt)
     },
     
     showPreprocessingInfo = function(fmt = "%-11s %s", colName = "Info") {
       colName <- paste0(colName, ":")
       preproc <- preprocessingSettings
       
       sourceNameDesc <- basename(sourceName)
       if(length(preprocessingSettings) > 0) {
         if (fromDataFrame) {
           sourceType <- paste0(" (R data frame, ", preproc[["type"]], ")")
         } else {
           sourceType <- paste0(" (text file, ", preproc[["type"]], ")")
         }
         
         sourceInfo <- paste0("preprocessed: ", preproc[["creation time"]])         
         cat(sprintf(fmt, c(colName, ""),
                     c(paste0(sourceNameDesc, sourceType), sourceInfo)), sep="\n")
       } else {
         cat(sprintf(fmt, colName, basename(sourceName)), sep="\n")
       }
       if (!preprocessedFilesExist(sourceName, normalizedPath, stopOnFalse=F, warn=F))
         cat(sprintf(fmt, "", "necessary preprocessed file(s) missing!"), sep="\n")
     },
     
     showPreprocessingInputFile = function(fmt = "%-11s %s") {
       if(length(preprocessingSettings) > 0) {
         cat(sprintf(fmt, "Input File:",
                     preprocessingSettings[["input file"]]), sep="\n")
       }
     },
     
     showPreprocessingInputFileTime = function(fmt = "%-11s %s", colName="",
                                               name="last modification") {
       if(length(preprocessingSettings) > 0) {
         desc <- paste0(name, ": ", preprocessingSettings[["file modification time"]])
         cat(sprintf(fmt, colName, desc), sep="\n")
       }
     },
     
     showCuesOutcomes = function(fmt = "%-11s %s") {
       nCues <- length(cueIndices)
       nOutcomes <- length(outcomeIndices)
       
       firstCues <- paste(names(cueIndices)[1:min(5, nCues)], collapse=", ")
       if (nCues > 5) firstCues <- paste(firstCues, "...", sep=", ")
       firstOutcomes <- paste(names(outcomeIndices)[1:min(5, nOutcomes)], collapse=", ")
       if (nOutcomes > 5) firstOutcomes <- paste(firstOutcomes, "...", sep=", ")
       
       ncharCueOutc <- max(nchar(as.character(nCues)), nchar(as.character(nOutcomes)))
       fmtCueOutc <- paste0("%-", ncharCueOutc+1, "s %s")
       
       cat(sprintf(fmt, "Cues:", sprintf(fmtCueOutc, paste0(nCues, ":"), firstCues)),
           sprintf(fmt, "Outcomes:", sprintf(fmtCueOutc, paste0(nOutcomes, ":"), firstOutcomes)),
           sep="\n")
     },
     
     showPreprocessingParameters = function(fmt = "%-11s %s") {
       preproc <- preprocessingSettings
       preprocParam <- "no preprocessing information available (settings file missing)"
       
       if(length(preprocessingSettings) > 0) {         
         preprocParam <- c(paste0("normalizeCase = ", preproc[["normalize case"]]))
         if (preproc[["type"]] == "tabular") {
           preprocParam <- c(preprocParam, paste0(
             "maxNumberCues = ", preproc[["maximum number cues"]],
             ", maxNumberOutcomes = ", preproc[["maximum number outcomes"]]))
         } else {
           preprocParam <- c(preprocParam, paste0(
             "cue file = ", basename(preproc[["cue file"]]),
             ", outcome file = ", basename(preproc[["outcome file"]])))
           if (preproc[["type"]] == "corpus") {
             preprocParam <- c(preprocParam, paste0(
               "outcome word n-gram = ", preproc[["window size"]],
               ", use letter n-gram cues = ", preproc[["use letter ngrams"]]))
             if (preproc[["use letter ngrams"]]) {
               preprocParam <- c(preprocParam, paste0(
                 "maximum cue n-gram size = ", preproc[["maximum ngram size"]],
                 ", include smaller cue n-grams = ", preproc[["include smaller ngrams"]]))
             }
           }
         }
       }
       
       preprocHeadings <- c("Preprocess:", rep.int("", length(preprocParam)-1))
       cat(mapply(sprintf, preprocHeadings, preprocParam, fmt=fmt), sep="\n")
     }
   )
)

# Set fields to read-only (after the first assignment, which happens either
# in the constructor or in the copy method)
ndlPreprocessingResult$lock("rawPath", "normalizedPath", "sourceName",
                              "cueIndices", "outcomeIndices", "totalNumEvents",
                              "preprocessingSettings", "fromDataFrame")

getRCFunctions = function(refC) {
  res <- refC$methods()
  # Remove some general methods inherited from envRefClass that are not useful to the users.
  res <- setdiff(res, c("callSuper", "export", "field", "finalize", "import",
                        "initFields", "initialize", "trace", "untrace", "usingMethods"))
  # Don't show different versions for the same function (parent definition and this object)
  res <- res[!grepl("#", res)]
  # The showXyz methods are mostly for internal usage by show(). Better not pollute the output...
  res[!grepl("show.+", res)]
}

# Make the method and field names show up in the tab completion (on the console or in RStudio).
# If this is not used, only some of the methods show up.
# TODO: find out if there is any way to get argument completion for RC methods
.DollarNames.ndlPreprocessingResult <- function(x, pattern) {
  refC <- getRefClass(class(x))
  utils:::findMatches(pattern, sort(c(getRCFunctions(refC), names(refC$fields()))))
}

all.equal.ndlPreprocessingResult <- function(target, current, ...) {
  if (class(target) != class(current)) {
    warning(paste0("Object classes differ. Target = ", class(target),
                   ", current = ", class(current)))
    return(FALSE)
  }
  
  refC <- getRefClass(class(current))
  fieldCheck <- sapply(names(refC$fields()),
                       function(x) all.equal(target[[x]], current[[x]]),
                       simplify=FALSE)
  if (all(sapply(fieldCheck, isTRUE)))
      return(invisible(TRUE))
  differingFields <- fieldCheck[!sapply(fieldCheck, isTRUE)]
  unlist(lapply(names(differingFields), function(name) {
    paste0(name, ": ", differingFields[[name]])}))
}


#' Reference class object for iterative learning.
#' 
#' \code{\link{ReferenceClasses}} object for learning weights between cues and outcomes
#' iteratively according to the Rescorla Wagner equations. Objects of this class get
#' returned by the learn weights function family (\code{\link{learnWeights}},
#' \code{\link{learnWeightsCompact}}, \code{\link{learnWeightsCorpus}},
#' \code{\link{learnWeightsTabular}}) and can be used for learning more events
#' (\code{$learn}), accessing the weights (\code{$getWeights}) and for interacting
#' with the preprocessed data that is used for learning (using the same methods and fields
#' as \code{\link{ndlPreprocessingResult}}).\cr\cr
#' Unlike most R objects, this object can change its state, doesn't get copied on variable
#' assignments and the weights don't persist over R saved sessions. However, the
#' \code{$copy} method can be used for copying and the weights can be saved over sessions
#' by assigning the result of the \code{$getWeights()} method to a variable.
#' From the weight matrix a new \code{ndlLearner} object at the same state as the original
#' object can be created with the \code{\link{resumeLearning}} function.
#' 
#' The fields based on learning settings and data source information are all read-only.
#' Settings related to the learner object itself, like \code{retainFiles},
#' \code{defaultVerbose}, or \code{defaultNumThreads}, can be changed. Furthermore,
#' the information about where to continue with learning, \code{nextEventToLearn},
#' can also be changed, but this should be used with care and even then it is safer to
#' use the \code{setNextEventToLearn} method.
#' 
#' \code{ndlLearner} objects from a different R session cannot be used, as the learned
#' weights are lost. There are several ways to deal with this situation. To resume
#' learning were you left off, you can use the \code{$getWeights()} method to get and
#' save the weights matrix in the intial session. This weight matrix can then be used
#' to create a new learner object in the same state with the \code{\link{resumeLearning}}
#' function in the same or a different R session. If you don't want to save the learned
#' weights or forgot to do so, the \code{$relearn} and \code{$reset} methods can be used
#' on the \code{ndlLearner} object in the new R session to make them useable again.
#' With the first method all events up to now are relearned again, whereas with the
#' second method the object is reset to the initial state without any events learned.
#' 
#' @field defaultAlpha The salience of the cues (learning rate) for which no other value
#'  has been specified.
#' @field defaultBeta The salience of the outcomes (learning rate) for which no other value
#'  has been specified.
#' @field lambda The maximum possible level of associative strength.
#' @field removeDuplicates If \code{TRUE} cues and outcomes are only counted once
#'  per event, even if they occur several times in the cue or outcome list for
#'  that event.
#' @field nextEventToLearn From which event id the learning continues. Note that the first
#'  event has event id \code{0} and the last event \code{totalNumEvents-1}.
#' @field retainFiles Whether the preprocessed files used for learning should be
#'  retained even after all events have been learned and also after this
#'  object get deleted (garbage collected to be exact). Even when it is set to \code{FALSE}
#'  the files are only deleted if they seem to be the same ones that were used for learning.
#' @field defaultVerbose Whether the output should be verbose for method calls without
#'  any value for \code{verbose} set.
#' @field defaultDebug Whether to show debugging output for method calls without any
#'  value for \code{debug} set.
#' @field defaultNumThreads How many threads to use in methods, for which no other
#'  value for \code{numThreads} is set and that support execution with more than one thread.
#'  
#' @inheritParams ndlPreprocessingResult
#' @seealso \code{\link{ndlPreprocessingResult}} as all of its fields and methods are also
#'  accessible for \code{ndlLearner} objects,
#'  \code{\link{learnWeights}}, \code{\link{learnWeightsCompact}},
#'  \code{\link{learnWeightsCorpus}}, \code{\link{learnWeightsTabular}}
#'  
#' @examples
#' 
#' # For preprocessed file interaction see example(ndlPreprocessingResult)
#' # For copying see example(learnWeights)
#' # For resuming learning see example(resumeLearning) or example(learnWeights)
#' 
#' data(lexample)
#' lexample$Cues <- orthoCoding(lexample$Word, grams=2)
#' 
#' # Simulate the order of the events by assuming occurrence in random order
#' # (as frequencies greater than 1 are not allowed for iterative learning).
#' lexampleLong <- lexample[sample(rep(1:nrow(lexample), lexample$Frequency)),]
#' lexampleLong$Frequency <- 1
#' 
#' # Now we create the ndlLearner object.
#' learner <- learnWeights(lexampleLong, numEventsToLearn=30, retainFiles=TRUE)
#' 
#' # Show status information about the learner object.
#' learner
#' 
#' # To learn more events use the $learn method, for a custom number of events:
#' learner$learn(20)
#' # or all remaining events
#' learner$learn()
#' 
#' # With the $isUsable method we can check if the object is in a usable state.
#' # Unless the object is from a different R session this should return TRUE.
#' stopifnot(learner$isUsable())
#' 
#' # If the object is in an unusable state (from a different R session), we can
#' # either use the resumeLearning function if we have saved a weight matrix,
#' # or we can relearn all the events up to now:
#' learner$relearn(verbose=TRUE)
#' 
#' # Or we can also reset the learning object to the initial state (no events
#' # learned yet) and continue learning from there.
#' learner$reset()
#' learner$learn(30)
#' 
#' # These last two options work, as long as the preprocessed data source is
#' # still available (if the path changed, you can use the newPath parameter).
#' # To check if the files still exist you can use:
#' learner$filesExist()
#' 
#' # We don't need the preprocessed files anymore, so lets delete them.
#' learner$deleteFiles()
#'  
ndlLearner <- setRefClass(
  "ndlLearner",
  contains = "ndlPreprocessingResult",
  fields = list(
    defaultAlpha="numeric",
    defaultBeta="numeric",
    lambda="numeric",
    removeDuplicates="logical",
    nextEventToLearn="numeric",
    retainFiles="logical",
    defaultVerbose="logical",
    defaultDebug="logical",
    defaultNumThreads="numeric"
  ),
  methods = list(
    initialize = function(sourceName="", path="", alpha=0.1, beta=0.1, lambda=1,
                          removeDuplicates=F, retainFiles=T,
                          numThreads=Sys.getenv("OMP_NUM_THREADS"), verbose=F, debug=F) {
      if (sourceName == "") # Required for copy(), as most fields are locked.
        return()
      
      callSuper(sourceName, path)
      
      defaultAlpha <<- alpha
      defaultBeta <<- beta
      lambda <<- lambda
      removeDuplicates <<- removeDuplicates
      retainFiles <<- retainFiles
      nextEventToLearn <<- 0
      
      defaultVerbose <<- verbose
      defaultDebug <<- debug
      defaultNumThreads <<- checkNumThreads(numThreads)
      
      if (preprocessedFilesExist(sourceName, rawPath)) {
        assign(".implementation", new(testle$IterativeRescorlaWagner, path, sourceName),
               envir=as.environment(.self))
        .implementation$init(defaultAlpha, defaultBeta, verbose)
      }
    },
    
    finalize = function() {
#       cat("Called finalize on learner object with source path:", normalizedPath,
#           paste("Deleting files =", !retainFiles), sep="\n")
      if (!retainFiles)
        deleteFiles(quiet=T)
    },
    
    copy = function(shallow = FALSE) {
      "Copies the reference class object.
      If shallow is set to FALSE (default), then the copy is completely independent from the
      source object (apart from using the same data source). In particular the learning in one
      object does not affect the other object.
      If shallow is set to TRUE, then only the settings and other state information are copied,
      but they still use the same weights. One learning occurs in one object, it also affects
      the other. However, the state information in the other object (nextEventToLearn) is in
      this case not updated, leading to incorrect state information. Therefore, setting shallow
      to TRUE should be avoided."
      newObj <- callSuper(shallow)
      
      if (shallow) {
        assign(".implementation", .implementation, env=as.environment(newObj))
      } else {
        if (preprocessedFilesExist(sourceName, normalizedPath, stopOnFalse=T)) {
          assign(".implementation", new(testle$IterativeRescorlaWagner,
                                        normalizedPath, sourceName),
                 envir=as.environment(newObj))
          newObj$.implementation$init(defaultAlpha, defaultBeta, defaultDebug)
          newObj$setWeightMatrix(.self$getWeights())
        }
      }
      return(newObj)
    },
    
    learn = function(numEvents=(totalNumEvents-nextEventToLearn),
                     numThreads=defaultNumThreads,
                     verbose=defaultVerbose, debug=defaultDebug) {
      "Learns numEvents (by default all remaining events).
      If the numThreads, verbose or debug parameters are set, then these are used instead
      of the default settings for the object. A warning is issued if more events are requested
      then available or if all events were already learned."
      if ((totalNumEvents - nextEventToLearn) == 0) {
        warning("No more events left to learn.")
        return(invisible())
      } else if (nextEventToLearn > totalNumEvents || nextEventToLearn < 0) {
        stop(paste0("Invalid value for nextEventToLearn (", nextEventToLearn, "). ",
                     "It needs to be positive and less than or equal to totalNumEvents."))
      }
      
      isUsable(stopOnFalse=T)
      numLearned <- .implementation$learn(nextEventToLearn, numEvents, lambda,
                                          removeDuplicates, numThreads, verbose, debug)
      nextEventToLearn <<- nextEventToLearn + numLearned

      if (numLearned < numEvents) {
        if ((totalNumEvents - nextEventToLearn) == 0) {
          warning(paste0("Exhausted all events after learning ", numLearned, 
                         " of the ", numEvents, " requested events."))
        } else {
          stop(paste0("Only ", numLearned, " out of the requested ",
                      numEvents, " were learned.")) }
      }
      if (nextEventToLearn == totalNumEvents & !retainFiles) {
        deleteFiles()
      }
      return(invisible(nextEventToLearn))
    },
    
    getInfo = function(includeCueAndOutcomeIndices = TRUE) {
      "Returns information about the data source and the learning state, but not about the weights.
      With this information the current state of the learner object can be recreated, however,
      in order to achieve this all the events that were already learned need to be learned again.
      To access and save the entire state use the getWeights() method without any further
      arguments. The result of that includes the weights and all the information from this
      method."
      fields <- names(.refClassDef@fieldClasses)
      if (!includeCueAndOutcomeIndices)
        fields <- setdiff(fields, c("cueIndices", "outcomeIndices"))
      return(mget(fields, envir = as.environment(.self)))
    },
    
    getWeights = function(cues=NA, outcomes=NA, includeDimNames=T, checkRange=T) {
      "Returns the weights between cues and outcomes (default) or a subset of them.
      Cue and outcomes selection works as for any other R object (by index, name or logical),
      apart from negative indices, which are not being supported yet.
      By default the matching cue and outcome names are included as the dimension names
      of the weight matrix. Furthermore, unless requested otherwise, the cues and outcome ids
      are checked to determine whether they are inside the range of available cues/outcomes."
      result <- FALSE
      names(cues) <- NULL
      names(outcomes) <- NULL
      
      # Helper functions
      checkUseAll <- function(x, name) {
        if (!any(is.na(x))) return(FALSE)
        if(length(x) == 1) return(TRUE)
        stop(paste0("When NA is used in ", name, ", then ", x,
                    "needs to be a vector of length 1 (not ", length(x), ")."), call. = F)
      }
      
      hasNoEntries <- function(x, name) {
        if (length(x) > 0) return(FALSE)
        warning(paste0("No ", name, "s selected"), call. = F)
        return(TRUE)
      }
      
      rangeCheck <- function(useAll, x, indices, name) {
        if(!useAll) {
          xRange <- range(x)
          indRange <- c(indices[1], indices[length(indices)])
          if (xRange[1] < indRange[1] | xRange[2] > indRange[2]) {
            stop(paste0("Requested ", name, "s outside of ", name, " range. ",
                        "Requested: ", xRange[1], "-", xRange[2],
                        "Available: ", indRange[1], "-", indRange[2]),
                 call. = F) } }
      }
      
      checkNAs <- function(x, xOrig, name) {
        if (anyNA(x)) {
          xErr <- paste0("[", which(is.na(x)), "]: '", xOrig[is.na(x)], "'",
                         collapse=", ")
          stop(paste0("Invalid ", name, "s selected:\n", xErr), call. = F) }
      }
      # End helper functions
            
      useAllCues <- checkUseAll(cues, "cues")
      # Change to numeric cues (instead of cue names or booleans)
      if(!useAllCues & !is.numeric(cues)) {
        cuesOrig <- cues
        cues <- cueIndices[cues]
        checkNAs(cues, cuesOrig, "cue") # check and stop if NAs were introduced
      }
      
      useAllOutcomes <- checkUseAll(outcomes, "outcomes")
      # Change to numeric outcomes (instead of cue names or booleans)
      if(!useAllOutcomes & !is.numeric(outcomes)) {
        outcomesOrig <- outcomes
        outcomes <- outcomeIndices[outcomes]
        checkNAs(outcomes, outcomesOrig, "outcome") # check and stop if NAs were introduced
      }
      
      if (hasNoEntries(cues, "cue") | hasNoEntries(outcomes, "outcome")) {
        result <- matrix(0, nrow=length(cues), ncol=length(outcomes))
      }	else if (checkRange) {
        rangeCheck(useAllCues, cues, cueIndices, "cue")
        rangeCheck(useAllOutcomes, outcomes, outcomeIndices, "outcome")
      }
      
      nCues <- ifelse(useAllCues, length(cueIndices), length(cues))
      nOutcomes <- ifelse(useAllOutcomes, length(outcomeIndices), length(outcomes))
      if ((as.double(nCues) * as.double(nOutcomes)) >= 2^31) {
        stop(paste0("Cannot return the requested weight matrix, as it is bigger than the ",
                    "maximum allowed size for matrices in R (2^31 elements)\n",
                    "While you cannot get the whole weight matrix at once, you can get it ",
                    "in several parts by specifying smaller cue or outcome ranges."))
      }
      
      isUsable(stopOnFalse=T)
      
      dimNames <- list()
      if (includeDimNames) {
        dimNames[[1]] <- if (useAllCues) names(cueIndices) else names(cueIndices)[cues]
        dimNames[[2]] <- if (useAllOutcomes) names(outcomeIndices) else names(outcomeIndices)[outcomes]
      }
      
      if(!is.matrix(result)) { # check whether we already have a result
        if(useAllCues & useAllOutcomes) {
          # When the complete weight matrix is requested, we attach attributes
          # that include all the information necessary for creating a new object
          # that has the same state as the current object.
          info <- structure(getInfo(includeCueAndOutcomeIndices=F), class="ndlWeightMatrixInfo")
          result <- .implementation$getMatrix(info, dimNames)
        } else if(useAllOutcomes) {
          result <- .implementation$getRows(cues-1, dimNames)
        } else if(useAllCues) {
          result <- .implementation$getCols(outcomes-1, dimNames)
        } else {
          result <- .implementation$getWeights(cues-1, outcomes-1, dimNames)
        }
      }
      
      return(result)
    },
    
    setWeightMatrix = function(newWeightMatrix) {
      "Mostly for internal usage - use with care. Sets the weights to the requested matrix
      if the dimension and dimension names match. Otherwise it stops with an error."
      if((nrow(newWeightMatrix) != length(cueIndices)) |
           ncol(newWeightMatrix) != length(outcomeIndices))
        stop("New weight matrix has wrong dimensions")
      
      if (!identical(rownames(newWeightMatrix), names(cueIndices)))
        stop("The cues of the new weight matrix differ from the current cues.")
      
      if (!identical(colnames(newWeightMatrix), names(outcomeIndices)))
        stop("The outcomes of the new weight matrix differ from the current outcomes.")
      
      isUsable(stopOnFalse=T)
      .implementation$setMatrix(newWeightMatrix)
    },
    
    setNextEventToLearn = function(newNextEventToLearn) {
      "Sets the location from where the learning continues to the requested event id.
      It should be used with care. Be also aware that the first event has event id 0."
      if (newNextEventToLearn > totalNumEvents)
        stop("Requested next event is higher than number of available events.")
      if (newNextEventToLearn < 0)
        stop("No negative event numbers are allowed.")
      
      nextEventToLearn <<- newNextEventToLearn
    },
    
    viewEvents = function(numNext=1, numPrev=0, showFrequency = FALSE,
                          verbose = defaultDebug, glue = "_") {
      "Returns the next and/or previous preprocessed events as a data frame.
      When an event contains multiple cues or outcomes, then they are seperated by glue,
      by default '_'. By default the frequency column is omitted, as a frequency of 1 is
      required for iterative, Rescorla-Wagner based learning."
      if (nextEventToLearn > totalNumEvents || nextEventToLearn < 0) {
        stop(paste0("Invalid value for nextEventToLearn (", nextEventToLearn, "). ",
                    "It needs to be positive and less than or equal to totalNumEvents."))
      }
      
      start <- nextEventToLearn-numPrev
      if (start < 0) {
        warning(paste0("Only ", nextEventToLearn, " previous events exist."))
        start <- 0
      }
      
      lastEventIncl <- nextEventToLearn + numNext - 1
      if (lastEventIncl >= totalNumEvents) {
        warning(paste0("Only ", totalNumEvents - nextEventToLearn, 
                       " next events exist."))
        lastEventIncl <- totalNumEvents - 1
      }
      numEventsToLearn <- lastEventIncl - start + 1
      
      ndlViewEvents(sourceName, start, numEventsToLearn, normalizedPath,
                    showFrequency, verbose, glue)
    },

    # Just changing the default of showFrequency to false, as iterative RW
    # requires frequencies of 1.
    viewEventsById = function(startId=0, numEvents=10, showFrequency = FALSE,
                              verbose = defaultDebug, glue = "_") {
      "Returns the selected preprocessed events as a data frame based on event id.
      The first event has id 0 and the last event has id totalNumEvents-1.
      When an event contains multiple cues or outcomes, then they are seperated by 'glue',
      by default '_'. Further, by default the frequency column is omitted, as a frequency of 1
      is required for iterative, Rescorla-Wagner based learning."
      callSuper(startId, numEvents, showFrequency, verbose, glue)
    },

    writeInfo = function(filePath) {
      info <- getInfo(includeCueAndOutcomeIndices=FALSE)
      write(listToString(info), file=filePath, sep="\n")
    },

    writeWeightMatrix = function(filePath, binary = TRUE, overwrite = FALSE, includeInfo = TRUE, digits = 12) {
      "Write the complete weight matrix to disk (directly from C++, without loading it into R).
      The resulting files can be used for further computation using other programming languages,
      but can also be used from R (see readWeightMatrix). However, due to the limitations
      discussed below, these files are not the recommended way for storing weight matrices.
      For that purpose it is better to get weights as an R matrix with the $getWeights() method
      and to save them using one of the R serialization options, such as the saveRDS function.
      This function creates three or four files:
      filePath - the weight matrix itself;
      paste0(filePath, c('.cues', '.outcomes') - text file containing the cue/outcome names
        in the same order as the rows/columns;
      paste0(filePath, '.info') - text file containing information about the learner object
        and the data source that was used (only written if includeInfo is set to TRUE).
      When binary is set to TRUE (default), the data is saved in a lossless and compact format.
      However, the binary file format is _not_ platform indepedent and also not readable with
      a text editor. It can be used for example for working on the weight matrix in a different
      programming language (like Python) on the same computer. This is especially useful for
      matrices that are too large for R (2^31 elements or more). The main file contains
      nrow*ncol double values, one row of the weight matrix after the other.
      When binary is set to FALSE, the data is saved as a text file with tabs separating columns
      and newlines separating the rows. In this case the parameter digits specifies how many
      significant digits are included (with binary files this parameter is ignored as it is
      lossless). The output can be decimal and scientific notation intermixed."
      isUsable(stopOnFalse=T)
      filePath <- normalizePath(filePath, mustWork=FALSE)
      
      if (!binary && digits < 1)
        stop("digits needs to be >= 1 for text files (binary is set to FALSE)")
      
      allFiles <- paste0(filePath, c("", ".cues", ".outcomes", ".info"))
      if(any(existing <- file.exists(allFiles))) {
        if (overwrite) {
          unlink(allFiles[existing])
        } else {
          stop(paste0("File(s) with same name already exist(s) and overwrite is set to FALSE:\n",
                      paste(allFiles[existing], collapse="\n")))
        }
      }
  
      .implementation$writeMatrixWithoutInfo(filePath, nextEventToLearn-1, binary, digits)
      write(names(cueIndices), file=paste0(filePath, ".cues"), sep="\n")
      write(names(outcomeIndices), file=paste0(filePath, ".outcomes"), sep="\n")
      
      if (includeInfo)
        writeInfo(paste0(filePath, ".info"))
    },

    relearn = function(newPath = NA, force = FALSE, numThreads = defaultNumThreads,
                       verbose = defaultVerbose, debug = defaultDebug) {
      "Relearns all the events that were learned previously.
      This is useful, if the object is in an unusable state, in particular if it was created
      in a different R session. To avoid relearning all the events, the weights can be saved
      with getWeights() while the object is still useable (e.g. in the original R session) and
      then afterwards a new learner object in the same state can be created with the
      resumeLearning() function."
      if (!is.na(newPath)) {
        path <- newPath
      } else {
        path <- normalizedPath
      }
      preprocessedFilesExist(sourceName, path, stopOnFalse = TRUE)
      
      if (force) { stopOn <- c("cues_outcomes")
      } else stopOn <- eval(formals(checkSamePreprocessingSource)$errors)
      
      checkSamePreprocessingSource(getInfo(), ndlPreprocessingResult$new(sourceName, path),
                                  "previously used", "new", errors=stopOn)
      
      require(ndl2)
      assign(".implementation", new(testle$IterativeRescorlaWagner, path, sourceName),
             envir=as.environment(.self))
      .implementation$init(defaultAlpha, defaultBeta, verbose)
      #totalNumEvents <<- .implementation$getNumberOfEvents()
      
      numEvents <- nextEventToLearn
      numLearned <- .implementation$learn(0, numEvents, lambda, removeDuplicates,
                                          numThreads, verbose, debug)
      if (numLearned < nextEventToLearn) {
        warning(paste0("Only learned ", numLearned, " instead of ", nextEventToLearn, "."))
        setNextEventToLearn(numLearned)
      }
    },
    
    reset = function(newPath = NA, force = FALSE, numThreads = defaultNumThreads,
                     verbose = defaultVerbose, debug = defaultDebug) {
      "Resets the state of the learner object to the intial state.
      This may be used when the object has become unusable, but to recreate the previous state
      the relearn method should be used instead."
      nextEventToLearn <<- 0
      relearn(newPath, force, numThreads, verbose, debug)
    },
    
    show = function() {
      "Displays information about the learner object.
      This includes information about the learning status, the data source and
      the parameter settings."
      indentation <- 11
      fmt <- paste0("%-", indentation, "s %s")
      
      cat("ndlLearner object", sep="\n")
      cat(sprintf(fmt, "Parameters:", paste0(
        "alpha = ", defaultAlpha, ", beta = ", defaultBeta,
        ", lambda = ", lambda, ", removeDuplicates = ", removeDuplicates)), sep="\n")
      showLearningStatus(fmt)
      showPreprocessingInfo(fmt, colName = "Source")
      if(!fromDataFrame) showPreprocessingInputFileTime(fmt, name="input file modification time")
      
      showCuesOutcomes(fmt)
      showPreprocessingParameters(fmt)
      cat(sprintf(fmt, "Settings:", paste0("retainFiles = ", retainFiles,
                                           ", numThreads = ", defaultNumThreads,
                                           ", verbose = ", defaultVerbose,
                                           ", debug = ", defaultDebug)), sep="\n")
    },
    
    showLearningStatus = function(fmt = "%-11s %s") {
      nCues <- length(cueIndices)
      nOutcomes <- length(outcomeIndices)
      
      cat(sprintf(fmt, "Status:", paste("learned", nextEventToLearn, "of",
                                       totalNumEvents, "events")), sep="\n")
      
      if (is.null.externalptr(.implementation$.pointer)) {
        cat(sprintf(fmt, "", "in unusable state (object from previous session?)"),
            sprintf(fmt, "", "use relearn() to get the previous state back"), sep="\n")
      } else {
        w <- getWeights(1:min(3, nCues), 1:min(5, nOutcomes))
        if (nCues > 3) {
          w <- rbind(w, NA)
          rownames(w)[nrow(w)] <- "..."
        }
        if (nOutcomes > 5) {
          w <- cbind(w,NA)
          colnames(w)[ncol(w)] <- "..."
        }
        weightsTable <- capture.output(print.table(w, zero.print=".", na.print="", digits=3))
        weightsStartPos <- max(1, 11 - (max(nchar(rownames(w))) + 1))
        fmtWeights <- paste0("%-", weightsStartPos, "s %s")
        cat(sprintf(fmtWeights, "Weights:", substr(weightsTable[1], max(0, 9-weightsStartPos), nchar(weightsTable[1]))),
            sprintf(fmtWeights, "", weightsTable[-1]), sep="\n")
      }
    },
    
    isUsable = function(stopOnFalse = FALSE) {
      "Whether the learner object is in a usable state.
      In particular, this will not be the case if the learner object was loaded from a
      different R session and neither the relearn() or the reset() methods have been called yet.
      This always gets checked before any learning is attempted and results in an error, if the
      object is not in a usable state. These errors can be avoided by using this method with 
      stopOnFalse set to FALSE (default), before calling the learn method."
      
      if(is.null.externalptr(.implementation$.pointer)) {
        if (!stopOnFalse)
          return(FALSE)
        stop(paste0("The object is in an invalid state, ",
                    "presumably because it was created in another session.\n",
                    "The weights have been lost, but you can return to the previous state ",
                    "by using the relearn() or the reset() method."), call. = F)
      }
      return(TRUE)
    }
  )
)

# Set fields to read-only (after the first assignment, which happens either
# in the constructor or in the copy method)
ndlLearner$lock("defaultAlpha", "defaultBeta", "lambda", "removeDuplicates")

# Tab completion is inherited from the ndlPreprocessingResult class.

all.equal.ndlLearner <- function(target, current,...) {
  # TODO optionally check weight matrices
  all.equal.ndlPreprocessingResult(target, current)
}

# To not show the added information by default
print.ndlWeightMatrixInfo = function(x,...) {
  furtherArgs <- as.list(match.call(call=sys.call(1)))
  if ("all" %in% names(furtherArgs)) {
    if (furtherArgs$all == TRUE | furtherArgs$all == "T")
      return(print.simple.list(x, ...))
  }
  print("To show the ndlWeightMatrixInfo use print with all=TRUE or str().")
}
