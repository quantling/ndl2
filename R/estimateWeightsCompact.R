#### Companion function to estimateWeights, but uses compact binary event data files
#### See R documentation for details.


#' Estimation of the association weights using the equilibrium equations of
#' Danks (2003) for the Rescorla-Wagner equations using a compact binary event
#' file.
#' 
#' A function to estimate the weights (associative strengths) for cue-outcome
#' pairs when learning is in equilibrium, using the equilibrium equations for
#' the Rescorla-Wagner model of Danks (2003) using a compact binary event file.
#' Such binary event files can be created using
#' \code{\link{ndlPreprocessCorpus}}, \code{\link{ndlPreprocessTabular}} or
#' \code{\link{ndlPreprocessDataframe}}.
#' 
#' 
#' Using Rcpp, a C++ based implementation processes all of the data RAM. The
#' module will check the amount of RAM you have available in your system and
#' warn you of RAM is insufficient to build your model.
#' 
#' 
#' @param datasource The data source of preprocessed events to use.
#'   This parameter can be set to either the result of one of the
#'   \code{ndlPreprocess} functions (\code{\link{ndlPreprocessingResult}}),
#'   the information from previous learning or estimating (weight matrix,
#'   \code{\link{ndlLearner}} object or the result of the \code{$getInfo}
#'   method) or a string. In the last case the string
#'   needs to be the common prefix of the files created by the
#'   \code{ndlPreprocess} functions. The prefix string also needs to contain
#'   the path to the data source unless it is in the working directory.
#' @param removeDuplicates A logical specifying whether multiple occurrences of
#' a Cue in conjunction with an Outcome shall each be counted as a distinct
#' occurrence of that Cue (\code{FALSE}), or only as a single occurrence
#' (\code{TRUE}: default).
#' @param saveCounts A logical specifying whether the co-occurrence matrices
#' should be saved.  If set equal to \code{TRUE}, the files \code{coocCues.rda}
#' and \code{coocCuesOutcomes.rda} will be saved in the current workspace.
#' Default is FALSE.
#' @param verbose If set to \code{TRUE}, display diagnostic messages.
#' @param MaxEvents If changed from the default value, the learning algorithm
#' will stop learning after using the first N events in the training data. This
#' actually number of events used may be slightly higher than the number
#' specified.
#' @param addBackground If you would like to add a background rate for all your
#' cues and outcomes, but did not include an general environment cue to all
#' your events, one will be added for you to the matrices, as specified in
#' Danks (2003). If changed from the default (FALSE) to TRUE, background cues
#' will be added. The name used for the background rates is "Environ", and will
#' be included in the output weight matrix.
#' @param trueCondProb The conditional probability calculations used will be
#' those specified in Danks (2003). If changed from the default (TRUE) to
#' FALSE, the normalization specified in Baayen, et al (2011) is used.
#' @param \dots Control arguments to be passed along from
#' \code{\link{ndlClassify}} and/or \code{\link{ndlCrossvalidate}}.
#' @return A matrix with cue-to-outcome association strengths. Rows are cues,
#' and columns are outcomes.  Rows and columns are labeled. If addBackground=T,
#' a row named "Environ" will be added to the output.
#' @note Add a note here.
#' @section Acknowledgements: Thanks to all the beta testers of the ndl
#' package.
#' @author Cyrus Shaoul, R. H. Baayen and Petar Milin, with contributions from
#' Antti Arppe and Peter Hendrix.
#' @seealso \code{\link{estimateActivations}},
#'   \code{\link{ndlPreprocessCorpus}}, \code{\link{ndlPreprocessTabular}},
#'   \code{\link{ndlPreprocessDataframe}}, \code{\link{learnWeightsCompact}}
#' @references
#' Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix, P. and
#' Marelli, M., (2011) An amorphous model for morphological processing in
#' visual comprehension based on naive discriminative learning. Psychological
#' Review, 118, 438-482.
#' @keywords discriminative learning
#' @examples
#' data(lexample)
#' lexample$Cues <- orthoCoding(lexample$Word, grams=2)
#' 
#' # Preprocess the data to get the required events in binary format.
#' preprocessedDfDanks <- ndlPreprocessDataframe(lexample)
#' lexampleWeightsDanks <- estimateWeightsCompact(preprocessedDfDanks)
#' 
#' # Delete the preprocessed files.
#' preprocessedDfDanks$deleteFiles()
#' 
estimateWeightsCompact <- 
  function(datasource, removeDuplicates=TRUE, saveCounts=FALSE, verbose=FALSE, MaxEvents=100000000000000, trueCondProb=TRUE, addBackground=FALSE, ...)
{
    if (is.matrix(datasource))
      datasource <- getInfoFromWeightMatrix(datasource)
    
    if (inherits(datasource, "ndlPreprocessingResult") || is.list(datasource)) {
      dirWithPrefix <- file.path(getBestPreviouslyUsedPath(datasource),
                                 datasource$sourceName)
    } else {
      dirWithPrefix <- datasource
    }
    
    
    ## Check for pre-existing counts.
    coocFile = paste(dirWithPrefix,".coocCues.rds",sep='')
    coocOutFile = paste(dirWithPrefix,".coocCuesOutcomes.rds",sep='')
    if ((file.exists(coocFile) & file.exists(coocOutFile)) & MaxEvents >= 100000000000000 ) {
        warning("Did not remove duplicates because there were pre-computed cooccurrence matrices availbe. Remove these files and run again.")
        message(paste(c("\nNOTE: Loading pre-computed coocurrence matrices.\nIgnoring DataFrame '", basename, "' Provided.\nPlease remove the files ",coocFile," and ",coocOutFile, " if this behavior is not desired.")),sep="")
        flush.console()
        coocCues= readRDS(coocFile)
        coocCuesOutcomes= readRDS(coocOutFile)
    } else {
        if (verbose) message("Reading compact binary data from disk.")
        flush.console()
        ## call external C++ function using RCpp
        CuAndCo = learn(data=dirWithPrefix,RemoveDuplicates=removeDuplicates, verbose=verbose, MaxEvents=MaxEvents, addBackground=addBackground) 
        coocCues = CuAndCo[[1]]
        coocCuesOutcomes = CuAndCo[[2]]
        coocOutcomesFreq = CuAndCo[[3]]
        if ((nrow(coocCuesOutcomes) <2) | (ncol(coocCuesOutcomes) <2)) {
            stop("Your data had insufficient number of unique cues or outcomes. Please make sure that you have at least two cues and at least two outcomes.")
        }
        ## Save the cooc matrices for later reuse (after doing Background rates and normalization.
        if (saveCounts) {
            if (verbose) message("Completed Event Counts. Saving so-occurrence data for future calculations.")
            flush.console()
            saveRDS(coocCues, file=coocFile)
            if (verbose) message(paste("Saved",coocFile))
            flush.console()
            saveRDS(coocCuesOutcomes, file=coocOutFile)
            if (verbose) message(paste("Saved",coocOutFile))
            flush.console()
        }
    }
    if (verbose) message("Starting to process matrices.")
    ## Check sanity of arguments
    if ((addBackground) & (!trueCondProb)) {
        message("*WARNING: Can't add background rates without true conditional probabilities. \n*ACTION: Proceeding without background rates.")
        addBackground = FALSE
    }
    if (addBackground & trueCondProb) {
      # Add background for Cue-Cue cooc
      Environ = diag(coocCues)
      grandTotal = sum(Environ)
      coocCues = rbind(Environ,coocCues)
      Environ=c(grandTotal,Environ)
      coocCues = cbind(Environ,coocCues)
      rownames(coocCues)[1]=c("Environ")
      # Add background for Cue-Outcome cooc
      Environ=coocOutcomesFreq 
      coocCuesOutcomes = rbind(Environ,coocCuesOutcomes)
    }
    if (trueCondProb) {
      ## Convert Cue-Outcome counts to Cue-Outcome Probabilities using diagonal
      cueTotals = diag(coocCues) 
      cueTotals[cueTotals == 0] = 1
      condProbsCues = coocCues/cueTotals
      probsOutcomesGivenCues = coocCuesOutcomes/cueTotals
    } else {
      ## use the original algorithm for normalization
      rowsums = rowSums(coocCuesOutcomes)
      rowsums[rowsums == 0] = 1
      condProbsCues = coocCues/rowsums
      probsOutcomesGivenCues = coocCuesOutcomes/rowsums
    }

    if (verbose) message("Starting to calculate pseudoinverse.")
    flush.console()
    n = dim(condProbsCues)[1]
    ## Check to see if the number of cues is too big for reasonable hardware.
    if (n < 20000) {
      pseudoinverse = ginv(condProbsCues)
    } else {
        ## Use an approximation of the pseudoinverse here to make this feasible
        ## average hardware.
        if (verbose) message("Number of cues was too large for standard pseudoinverse. Switching to lower-rank approximation.")
        pseudoinverse = random.pseudoinverse(condProbsCues,verbose=verbose)
    }
    ## Calculate the weights by multiplying the pseudoinver of the c-c
    ## counts by the probabilites of the outcomes given the cues.
    weightMatrix = pseudoinverse %*% probsOutcomesGivenCues
    rownames(weightMatrix) = rownames(coocCues)
    colnames(weightMatrix) = colnames(coocCuesOutcomes)
    if (verbose) message("Completed calculations. Returning weight matrix.")
    flush.console()
    return(weightMatrix)
  }


