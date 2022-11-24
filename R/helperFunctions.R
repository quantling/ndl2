## Just some helper functions

# Import iterative rescorla wagner Rcpp module
testle <- Module( "testle" )
#loadModule("testle", TRUE)

is.null.externalptr <- function(pointer) {
  if (class(pointer)!="externalptr") stop("pointer is not an externalptr.", call. = F)
  is_null_externalptr(pointer)
}

listToString <- function(xs, prefix = "", prefixSubList = "  ") {
  fmt <- paste0(prefix, "%-", max(nchar(names(xs))) + 1, "s %s")
  paste(
    sapply(names(xs), function(name) {
      x <- xs[[name]]
      if (!is.list(x)) {
        sprintf(fmt, paste0(name, ":"), x)
      } else {
        sprintf(fmt, paste0(name, ":"), paste0(
          "\n", listToString(x, prefix=paste(prefix, prefixSubList))))
      }
    }, USE.NAMES = FALSE), collapse = "\n")
}

stringToList <- function(lines, separator=": ", prefix="", prefixSubList="  ") {
  res <- list()
  nextPrefix <- paste0(prefix, prefixSubList)
  
  i <- 1L
  while (i <= length(lines)) {
    fields <- strsplit(lines[[i]], separator, fixed=T)[[1]]
    
    if (length(fields) == 2) {
      val <- gsub("^\\s+|\\s+$", "", fields[2]) # trim whitespace
      valNum <- suppressWarnings(as.numeric(val)) # try casting it
      if(!is.na(valNum)) { val <- valNum  # check if cast was successful
      } else if(toupper(val) == "TRUE") { val <- TRUE
      } else if(toupper(val) == "FALSE") { val <- FALSE }
      res[[gsub("^\\s+|\\s+$", "", fields[1])]] <- val
    } else { # sublist -> check how many lines belong to it
      i <- i + 1
      start <- i
      while (i < length(lines) && substr(lines[[i+1]], 1, nchar(nextPrefix)) == nextPrefix)
        i <- i + 1
      res[[gsub("^\\s+|\\s+$", "", fields[[1]])]] <- stringToList(
        lines[seq.int(start, i)], separator, nextPrefix, prefixSubList)
    }
    i <- i + 1
  }
  return(res)
}

# Read file into named list. For further arguments see stringToList() above
readList <- function(filePath, ...) {
  if(!file.exists(filePath))
    return(list())
  
  lines <- readLines(filePath)
  stringToList(lines, ...)
}

dirExists <- function(path)
{
  fileInfo <- file.info(path)
  return (!is.na(fileInfo$isdir) && fileInfo$isdir == TRUE);
}

createDirIfNeeded <- function(dirPath)
{
  if (file.exists(dirPath)) {
    fileInfo <- file.info(dirPath)
    if (!fileInfo$isdir) {
      stop(paste0("Could not create specified output directory '", dirPath,
                  "', as a file with the same name exists."))
    }
    if (file.access(dirPath, 2) == -1)
      stop(paste0("No write access to output directory '", dirPath, "'."))
  }
  else {
    if (!dir.create(dirPath))
      stop(paste0("Failed to create output directory '", dirPath, "'."))
  }
}

checkDirExists <- function(dirPath)
{
  if (!file.exists(dirPath))
    stop(paste0("Path '", dirPath, "' does not exist!"))
  
  fileInfo <- file.info(dirPath)
  if (!fileInfo$isdir)
    stop(paste0("Specified path '", dirPath, "' is a file, not a folder."))
  if (file.access(dirPath, 4) == -1)
    stop(paste0("No read permissions for path '", dirPath, "'."))
}

checkFileExistsAndNormalizePath <- function(filePath)
{
  if (file.exists(filePath)) {
    fileInfo <- file.info(filePath)
    if (fileInfo$isdir)
      stop(paste0("Specified file '", filePath, "' is a directory."))
    if (file.access(filePath, 4) == -1)
      stop(paste0("No read permissions for file '", filePath, "'."))
  }
  return(normalizePath(filePath))
}

getOutputDirWithNamePrefix <- function(outputDir, sourceFileName)
{
  if (!is.character(outputDir) || outputDir == "") {
    return(normalizePath(sourceFileName))
  }
  else {
    createDirIfNeeded(outputDir)
    outputDir <- normalizePath(outputDir)
    prefix <- basename(sourceFileName)
    return(file.path(outputDir, prefix))
  }
}

checkNoEventsDir <- function(outputDirWithNamePrefix, overwrite, verbose)
{
  eventFilesDir <- paste0(outputDirWithNamePrefix, ".events")
  if (dirExists(eventFilesDir)) {
    if (!overwrite) {
      stop(paste0("The target folder '", eventFilesDir, "' already exists.",
                  "\nIf you want to overwrite the event files, then set the ",
                  "parameter 'overwrite' to TRUE."))
    }
    else {
      if (unlink(eventFilesDir, recursive = TRUE) == 0) {
        if (verbose) message("Succesfully deleted existing event file folder.")
      }
      else{
        stop(paste0("Failed to delete exsiting event file folder.",
                    "\nPlease try to delete '", eventFilesDir, "' by hand."))
      }
    }
  }
}

getPreprocessedFileNames <- function(sourceName, path = "", includeOptional = FALSE) {
  requiredFilesAndFolders <- c("cues", "outcomes", "events")
  optionalFiles <- c("settings", "unknown.words.txt")
  optionalFilesNoPrefix <- c("outcomes.with.invalid.characters.txt",
                             "cuefreq.cues", "crap.outcomes")
  
  files <- paste0(sourceName, ".", requiredFilesAndFolders)
  if (includeOptional) {
    files <- c(files, paste0(sourceName, ".", optionalFiles))
    files <- c(files, optionalFilesNoPrefix)
  }
  if (path != "")
    files <- file.path(path, files)
  return(files)
}

preprocessedFilesExist <- function(sourceName, path = "", stopOnFalse = FALSE, warn = FALSE) {
  files <- getPreprocessedFileNames(sourceName, path, includeOptional = F)
  filesExist <- file.exists(files)
  
  if (!stopOnFalse && !warn)
    return(all(filesExist))
  
  text <- ""
  if (!any(filesExist)) {
    text <- paste0("None of the required preprocessed files exist. Path: ", path)
  } else if (!all(filesExist)) {
    nonExistingFiles <- files[!filesExist]
    text <- paste0("Required preprocessing file(s) or folder(s) missing: ",
                   paste(nonExistingFiles, collapse=", "))
  } else return(TRUE)
  
  ifelse(stopOnFalse, stop(text, call. = F), warning(text, call. = F))  
  return(FALSE);
}

readPreprocessingSettingsFile <- function(sourceName, path, suffix="") {
  readList(file.path(path, paste0(sourceName, ".settings", suffix)), separator="\t")
}

getCueOrOutcomeIndices <- function(nameVec) {
  indices <- seq_len(length(nameVec))
  names(indices) <- nameVec
  return(indices)
}


checkNumThreads <- function(numThreads)
{
  if (numThreads == "") {
    warning(paste0("Neither the numThreads parameter nor ",
                   "the OMP_NUM_THREADS system variable is set.",
                   "\nFalling back to using just 1 thread."))
    numThreads <- 1
  }
  else if (is.character(numThreads)) {
    numThreads <- strtoi(numThreads)
  }
  return (numThreads)
}

getNumThreadsToUse <- function(oldDefaultNumThreads, newDefaultNumThreads) {
  if(!is.na(newDefaultNumThreads))
    return(newDefaultNumThreads)
  
  ompThreads <- Sys.getenv("OMP_NUM_THREADS")
  if (ompThreads == "") ompThreads <- 1 else ompThreads <- strtoi(ompThreads)
  
  if(ompThreads < oldDefaultNumThreads) {
    warning(paste0(
      "The number of threads used in previous learning (",
      oldDefaultNumThreads,") is higher than the current OMP_NUM_THREADS ",
      "setting (", Sys.getenv("OMP_NUM_THREADS"), ").\nTo avoid accidentally ",
      "using too many threads, only ", ompThreads, " thread(s) is/are used.",
      "\nFor a different number of threads you can use the ",
      "setDefaultNumThreads method of the returned object or call this ",
      "function with the defaultNumThreads parameter set."), call. = F)
    return(ompThreads)
  } else
    return(oldDefaultNumThreads)
}

getInfoFromWeightMatrix <- function(weightMatrix, includeIndices = FALSE) {
  info <- attr(weightMatrix, "info")
  if (!is.null(info) & class(info) == "ndlWeightMatrixInfo") {
    if (includeIndices) {
      info$cueIndices <- getCueOrOutcomeIndices(dimnames(weightMatrix)[[1]])
      info$outcomeIndices <- getCueOrOutcomeIndices(dimnames(weightMatrix)[[2]])
    }
    return(info)
  }
  return(list())
}

getBestPreviouslyUsedPath <- function(oldSource, stopOnFailure = TRUE) {
  if (is.matrix(oldSource)) oldSource <- getInfoFromWeightMatrix(oldSource)
  sourceName <- oldSource[["sourceName"]]
  rawPath <- oldSource[["rawPath"]]
  normalizedPath <- oldSource[["normalizedPath"]]
  
  evaluateSource <- function(path, otherSource=oldSource) {
    newSource <- ndlPreprocessingResult$new(sourceName, path)
    checkSamePreprocessingSource(oldSource, newSource, errors=c(), warnings=c(), output=c())
  }
  
  if (preprocessedFilesExist(sourceName, normalizedPath)) {
    if (normalizedPath == normalizePath(rawPath, mustWork=F)) {
      return(rawPath) # To also keep the relative path information.
    }
    if (!preprocessedFilesExist(sourceName, rawPath)) {
      return(normalizedPath)
    } else {
      # Both the normalized and rawPath contains preprocessing info,
      # but they are not the same path.
      normalizedPathCheck <- evaluateSource(normalizedPath)
      rawPathCheck <- evaluateSource(rawPath)
      rawVsNormalizedPathCheck <- evaluateSource(rawPath, ndlPreprocessingResult$new(
        sourceName, normalizedPath))
      
      if (all(rawPathCheck)) {
        return(rawPath)
      } else if (all(normalizedPathCheck)) {
        return(normalizedPath)
      } else if (all(rawVsNormalizedPathCheck)) {
        # Relative and normalized path contain both the same preprocessed files.
        return(rawPath) # To also keep the relative path information.
      } else {
        warning(paste0("The absolute path (", normalizedPath, ") and the relative path (",
                       rawPath, ") both contain preprocessed files, but in both cases they ",
                       "differ from the previously used preprocessed files.\n",
                       "Falling back to using the absolute path."))
      }
    }
  } else if (preprocessedFilesExist(sourceName, rawPath)) {
    rawPathCheck <- evaluateSource(rawPath)
    if (all(rawPathCheck)) {
      return(rawPath)
    } else {
      textAbsolutePath <- ifelse(!file.exists(normalizedPath), "does not exist.",
                                 "does not contain (all) required preprocessed files.")
      warning(paste0("The absolute path used in previous learning (", normalizedPath, ") ",
                     textAbsolutePath, "\nUsing the relative path '", rawPath, "' instead."),
              call. = F)
    }
  }
  
  if (stopOnFailure) {
    stop(paste0("Could not find valid preprocessed files in the previously used ",
                "path (neither in ", normalizedPath, " nor in ", rawPath, ")"), call. = F)
  }
  return(FALSE)
}

checkSamePreprocessingSource <- function(oldSource, newSource, oldName="old", newName="new",
                                         errors = c("version", "type", "no_settings", "cues_outcomes", "parameter", "number_of_events"),
                                         warnings = c("version", "type", "no_settings", "cues_outcomes", "parameter", "number_of_events", "input_file_path", "input_file_time"),
                                         output = c("version", "type", "no_settings", "cues_outcomes", "parameter", "number_of_events", "input_file_path", "input_file_time", "preprocessing_time")) {
  allFields <- c("version", "type", "no_settings", "cues_outcomes", "parameter", "number_of_events", "input_file_path", "input_file_time", "preprocessing_time")
  oldNameCapitalized <- paste0(toupper(substring(oldName, 1, 1)), substring(oldName, 2))
  
  # oldSource and newSource can be an object, $getInfo() or $weightMatrix()
  if (is.matrix(oldSource)) oldSource <- getInfoFromWeightMatrix(oldSource, includeIndices=T)
  if (is.matrix(newSource)) newSource <- getInfoFromWeightMatrix(newSource, includeIndices=T)  
  
  equal <- rep(NA, length(allFields))
  names(equal) <- allFields
  report <- function(show, category, text, otherLines="",...) {
    # Only set if no value is set so far or if they are different now.
    if (is.na(equal[category])) { equal[category] <<- !show
    } else if (show) equal[category] <<- F
    
    if (show) {
      textToShow <- sprintf(text, ...)
      if(otherLines != "") textToShow <- paste0(textToShow, "\n", otherLines)
      
      if (category %in% errors) { stop(textToShow, call. = F)
      } else if (category %in% warnings) { warning(textToShow, call. = F)
      } else if (category %in% output) cat(textToShow, sep="\n")
    }
  }
  
  isEqualPPSetting <- function(field) {
    return(oldSource$preprocessingSettings[[field]] == newSource$preprocessingSettings[[field]])
  }
  
  comparedFields <- c()
  compare <- function(field, category, line1 = "Preprocessing information for %s differs.",
                      useFieldName = T, line2 = paste0(oldNameCapitalized, " source: %s, ",
                                                       newName, " source: %s."),
                      ...) {
    outerEnv <- parent.env(environment())
    comparedFields <<- c(comparedFields, field)
    if (!isEqualPPSetting(field)) {
      text <- paste(line1, line2, sep=ifelse(useFieldName, "\n", "\n%0.s"))
      report(T, category, text, "", field, oldSource$preprocessingSettings[[field]],
             newSource$preprocessingSettings[[field]])
    } else if (is.na(equal[category])) equal[category] <<- TRUE
  }
  
  otherVersionText <- "Preprocessor versions differ."
  noPPInfoText <- "contains no preprocessing information (version < 0.06)."
  if (length(oldSource$preprocessingSettings) == 0) {
    report(length(newSource$preprocessingSettings) == 0, "no_settings",
           paste("Both the", oldName, "and the", newName, "preprocessed data",
                 "contain no preprocessing information (from version < 0.06 ?)."))
    report(length(newSource$preprocessingSettings) != 0, "version",
           paste(otherVersionText, "The", oldName, "preprocessed data", noPPInfoText))
  } else if (length(newSource$preprocessingSettings) == 0) {
    report(length(oldSource$preprocessingSettings) != 0, "version",
           paste(otherVersionText, "The", newName, " preprocessing source", noPPInfoText))
  } else { # both this object and otherSettings contain preprocessing info
    compare("ndlpreprocess version", "version", otherVersionText, F)
    compare("type", "type")
    compare("creation time", "preprocessing_time")
    compare("file modification time", "input_file_time")
    compare("input file", "input_file_path")
    
    remainingFields <- setdiff(names(oldSource$preprocessingSettings), comparedFields)
    if (!isEqualPPSetting("type") || !isEqualPPSetting("ndlpreprocess version"))
      remainingFields <- intersect(remainingFields, names(newSource$preprocessingSettings))
    lapply(remainingFields, function(x) compare(x, "parameter"))
  }
  
  compareCuesOutcomesAllEqual <- function(field, category, text = "Difference in %s:", ...) {
    dim <- ifelse(substr(field, 1, 3) == "cue", 1, 2)
    getFieldValues <- function(x) {
      if (field %in% names(x) || inherits(x, "ndlPreprocessingResult"))
        return(x[[field]])
      return(getCueOrOutcomeIndices(x$dimnames[[dim]]))
    }
    result <- all.equal(getFieldValues(oldSource), getFieldValues(newSource))
    report(!(is.logical(result) && result), category, text, paste(result, collapse="\n"), field)
  }
  hasCuesOutcomes <- function(x) {
    all(c("cueIndices", "outcomeIndices") %in% names(x)) | inherits(x, "ndlPreprocessingResult")
  }
  if (hasCuesOutcomes(oldSource) & hasCuesOutcomes(newSource)) {
    compareCuesOutcomesAllEqual("cueIndices", "cues_outcomes")
    compareCuesOutcomesAllEqual("outcomeIndices", "cues_outcomes")
  }
  report(oldSource$totalNumEvents != newSource$totalNumEvents, "number_of_events",
         (paste0("The total number of events is different.\n",
                 oldNameCapitalized, " source: ", oldSource$totalNumEvents, ", ",
                 newName, " source: ", newSource$totalNumEvents)))
  return(invisible(equal))
}

hasSameSettingsAsPreprocessedFiles = function(preprocessFunction, preprocessArgs,
                                              sourceName, path) {
  if(!all(file.exists(getPreprocessedFileNames(sourceName, path))))
    return(FALSE)
  
  preprocessArgs <- c(preprocessArgs, onlySettingsFile = T)
  do.call(preprocessFunction, preprocessArgs)
  
  existingSource <- ndlPreprocessingResult$new(sourceName, path)
  newSettings <- readPreprocessingSettingsFile(sourceName, path, ".comparison")
  newSource <- list(preprocessingSettings=newSettings,
                    cueIndices=c(), outcomeIndices=c(), totalNumEvents=-1)
  check <- checkSamePreprocessingSource(existingSource, newSource,
                                        errors=c(), warnings=c(), output=c())
  check <- check[!names(check) %in% c("cues_outcomes", "number_of_events", "preprocessing_time")]
  #print(check)
  unlink(file.path(path, paste0(sourceName, ".settings.comparison")))
  return(all(check, na.rm=T))
}