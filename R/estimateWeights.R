## estimateWeights uses an Rcpp module for faster computation of co-occurrence counts.
### See R documentation for more info.



#' Estimation of the association weights using the equilibrium equations of
#' Danks (2003) for the Rescorla-Wagner equations.
#' 
#' A function to estimate the weights (associative strengths) for cue-outcome
#' pairs when learning is in equilibrium, using the equilibrium equations for
#' the Rescorla-Wagner model of Danks (2003).
#' 
#' 
#' Using Rcpp, a C++ based implementation processes all of the data in RAM. The
#' module will check the amount of RAM you have available in your system and
#' warn you if the amount of RAM is insufficient to build your model.
#' 
#' For examples of how the \code{cuesOutcomes} data frame should be structured,
#' see the data sets \code{\link{danks}}, \code{\link{plurals}}, and
#' \code{\link{serbian}}. N.B. Empty \code{Cues} or \code{Outcomes}
#' (effectively having \code{length = 0}), e.g. \code{Cues} or \code{Outcomes}
#' strings with an initial or final underscore or two immediately adjacent
#' underscores, will result in an error.
#' 
#' @param cuesOutcomes A data frame with three variables specifying frequency,
#' cues, and outcomes, that may be created with \code{\link{ndlCuesOutcomes}}
#' or with the accessory script in the inst/scripts directory:
#' \describe{
#'  \item{\code{Cues}}{A character vector specifying the cues.  When there is
#'    more than one cue, the cues should be separated by underscores.}
#'  \item{\code{Outcomes}}{A character vector specifying the outcomes.  When
#'    there is more than one outcome, the outcomes should be separated by
#'    underscores.}
#'  \item{\code{Frequency}}{A numeric vector specifying the
#'    frequency with which a combination of cues and outcomes occurs.} }
#' @param removeDuplicates A logical specifying whether multiple occurrences of
#' a Cue in conjunction with an individual instance of an Outcome shall each be
#' counted as a distinct occurrence of that Cue (\code{FALSE}: default), or
#' only as a single occurrence (\code{TRUE}).
#' @param saveCounts A logical specifying whether the co-occurrence matrices
#' should be saved.  If set equal to \code{TRUE}, the files \code{coocCues.rda}
#' and \code{coocCuesOutcomes.rda} will be saved in the current working
#' directory.
#' @param verbose If set to \code{TRUE}, display diagnostic messages.
#' @param addBackground If you would like to add a background rate for all your
#' cues and outcomes, but did not include an general environment cue to all
#' your events, one will be added for you to the matrices, as specified in
#' Danks (2003). If changed from the default (FALSE) to TRUE, background cues
#' will be added. The name used for the background rates is "Environ", and will
#' be included in the output weight matrix.
#' 
#' @param trueCondProb The conditional probability calculations used will be
#' those specified in Danks (2003). If changed from the default (TRUE) to
#' FALSE, the normalization specified in Baayen, et al (2011) is used.
#' @param hasUnicode A logical specifying whether to apply a UTF-8 to integer
#' conversion to the names of the cues. This was implemented to solve issues
#' with differences Unicode cue names.
#' @param \dots Control arguments to be passed along from
#' \code{\link{ndlClassify}} and/or \code{\link{ndlCrossvalidate}}.
#' @return A matrix with cue-to-outcome association strengths. Rows are cues,
#' and columns are outcomes.  Rows and columns are labeled. If addBackground=T,
#' a row named "Environ" will be added to the output.
#' @note Add a note here.
#' @section Acknowledgements: The assistance of Uwe Ligges in getting the C
#' function \code{cooc} to work within the R framework is greatly appreciated.
#' This C function was removed in version 0.2.0 and replaced with the C++
#' function by Cyrus Shaoul.
#' @author Cyrus Shaoul, R. H. Baayen and Petar Milin, with contributions from
#' Antti Arppe and Peter Hendrix.
#' @seealso \code{\link{estimateActivations}, \link{ndlCuesOutcomes},
#' \link{danks}, \link{plurals}, \link{serbian}}
#' @references
#' 
#' Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix, P. and
#' Marelli, M. (2011), An amorphous model for morphological processing in
#' visual comprehension based on naive discriminative learning. Psychological
#' Review, 118, 438-482.
#' @keywords classif
#' @examples
#' 
#'   data(danks)
#'   estimateWeights(cuesOutcomes=danks)
#' 
#'   data(plurals)
#'   plurals$Cues <- orthoCoding(plurals$WordForm, grams=1)
#'   round(estimateWeights(cuesOutcomes=plurals),2)
#'   
#'   data(serbian)
#'   serbian$Cues <- orthoCoding(serbian$WordForm, grams=2)
#'   serbian$Outcomes <- serbian$LemmaCase
#'   sw <- estimateWeights(cuesOutcomes=serbian)
#'   round(sw[1:5,1:6],2)
#' 
estimateWeights <- function(cuesOutcomes, removeDuplicates=TRUE, saveCounts=FALSE, verbose=FALSE, trueCondProb=TRUE, addBackground=FALSE, hasUnicode=FALSE, ...) {
    ## Internal functions.
    ## Convert UTF8 code points to integers
    toInt = function(char) {
        if (char != '_') { 
            return(paste(utf8ToInt(char),"*",sep='')) 
        } else { 
            return(char)
        }
    }

    ## Convert integers to UTF8 code points
    toUTF8 = function(token) {
        token = sub('_','',token)
        tok = type.convert(token,as.is=T)
            if (is.numeric(tok)) {
                return(intToUtf8(tok)) 
            } else { 
                return(token)
            }
    }

    ## Operates on lists of strings
    convertCues = function(cuelist) { 
        return(paste(unlist(lapply(unlist(strsplit(cuelist,'')), FUN=toInt)),collapse=''))
    }
    
    ## Operates on lists of strings
    convertBack = function(cuelist) { 
        return(paste(unlist(lapply(unlist(strsplit(cuelist,'*',fixed=T)), FUN=toUTF8)),collapse=''))
    } 

    if (!is.data.frame(cuesOutcomes)) {
        stop("Error: The argument 'cuesOutcomes' must be a dataframe.")
    }
    
    basename <- NULL
    basename = paste(substitute(cuesOutcomes))
    coocFile = paste(basename,".coocCues.rds",sep='')
    coocOutFile = paste(basename,".coocCuesOutcomes.rds",sep='')
    # Check if pre-computed counts exist. If so, load them
    if (file.exists(coocFile) && file.exists(coocOutFile) ) {
        message(paste(c("NOTE: Loading pre-computed coocurrence matrices.\nIgnoring DataFrame '", basename, "' Provided.\nPlease remove the files ",coocFile," and ",coocOutFile, " if this behavior is not desired.")),sep="")
        if (removeDuplicates && verbose) {
            warning("Did not remove duplicates because there were pre-computed cooccurrence matrices availabe. Please Manually remove the files and run again to make sure that duplicates are removed.")
        }
        flush.console()
        coocCues = readRDS(coocFile)
        coocCuesOutcomes = readRDS(coocOutFile)
    } else {
        ## check for valid column names
        if (!("Cues" %in% colnames(cuesOutcomes))) {
            stop("The 'Cues' column is missing from your dataframe. Please correct the column name and try again. ")
        }
        if (!("Outcomes" %in% colnames(cuesOutcomes))) {
            stop("The 'Outcomes' column is missing from your dataframe. Please correct the column name and try again. ")
        }
        if (!("Frequency" %in% colnames(cuesOutcomes))) {
            warning("The 'Frequency' column is missing from your dataframe. A column of constant frequencies (1) was added.")
            cuesOutcomes$Frequency=1
        }
        NA.cues <- which(is.na(cuesOutcomes$Cues))
        NA.outcomes <- which(is.na(cuesOutcomes$Outcomes))
        if(length(NA.cues)>0)
            stop(paste("NA's in 'Cues': ",length(NA.cues)," cases.",sep=""))
        if(length(NA.outcomes)>0)
            stop(paste("NA's in 'Outcomes': ",length(NA.outcomes)," cases.",sep=""))
        
        ## Fixing unicode sorting errors.
        if (hasUnicode) {
            cuesOutcomes$Cues = unlist(lapply(cuesOutcomes$Cues,FUN=convertCues))
        }
        
        ## Call Rcpp function to process all events.
        coocCues = matrix()
        CuAndCo = learnLegacy(DFin=cuesOutcomes, RemoveDuplicates=removeDuplicates, verbose=verbose)
        coocCues = CuAndCo[[1]]
        coocCuesOutcomes = CuAndCo[[2]]
        ## Recommended for removal by Brian Ripley
        #        rm(CuAndCo)
        # gc()
    }
    # At this point we should have cooc counts for Cue-Cue and Cue-Outcome
    
    if (verbose) message("Starting to process matrices.")
    ## Check sanity of arguments
    if ((addBackground) & (!trueCondProb)) {
        message("*WARNING: Can't add background rates without true conditional probabilities. \n*ACTION: Proceeding without background rates.")
        addBackground = FALSE
    }
    ## If requested, add background rates for Cue-Cue cooccurrence
    if (addBackground & trueCondProb) {
        ## Check first if Environ has already been computed
        if (sum(coocCues["Environ",]) == 0 & sum(coocCues[,"Environ"] == 0)) {
            cueTotals = diag(coocCues)
#            grandTotal = sum(cueTotals)
            coocCues["Environ",] = cueTotals
            coocCues[,"Environ"] = cueTotals
            coocCues["Environ","Environ"] = sum(cuesOutcomes$Frequency)
        }
    }
    else {
        ## remove rows and columns reserved for background rates
        coocCues=coocCues[!rownames(coocCues) %in% "Environ", !colnames(coocCues) %in% "Environ" ,drop=FALSE]
        coocCuesOutcomes=coocCuesOutcomes[!rownames(coocCuesOutcomes) %in% "Environ",,drop=FALSE]
    }

    ## Check for single cue and outcome.
    if ((nrow(coocCuesOutcomes) <2) & (ncol(coocCuesOutcomes) <2)) {
        stop("Your data had only one unique cue and one unique outcome, making it impossible to estimate the 'weight'. Please make sure that your training data has more than one and cue or more than one outcome.")
    }

    ## Save the cooc matrices for later reuse (after doing Background rates and normalization.
    if (saveCounts) {
        if (verbose) message("Completed Event Counts. Saving Cooc Data for future calculations.")
        flush.console()
        saveRDS(coocCues, file=coocFile)
        if (verbose) message(paste("Saved ",coocFile))
        flush.console()
        saveRDS(coocCuesOutcomes, file=coocOutFile)
        if (verbose) message(paste("Saved ",coocOutFile))
        flush.console()
    }

    
    # At this point we begin to normalize the counts.
    
    if (trueCondProb) {
        ##Convert Cue-Outcome counts to Cue-Outcome Probabilities using diagonal
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
    if (n < 20000) {
        pseudoinverse = ginv(condProbsCues)
        ## Could be faster!!! Do some tests!
        ## pseudoinverse = (t(solve(crossprod(condProbsCues),condProbsCues)))
    } else {
        ## Use an approximation of the pseudoinverse here to make this feasible
        ## average hardware.
        if (verbose) message("Number of cues was too large for standard pseudoinverse. Switching to lower-rank approximation.")
        pseudoinverse = random.pseudoinverse(condProbsCues,verbose=verbose)
    }
    ## Calculate the weights by multiplying the pseudoinver of the c-c
    ## counts by the probabilites of the outcomes given the cues.
    weightMatrix = pseudoinverse %*% probsOutcomesGivenCues
    ## Deal with Unicode issue.
    if (hasUnicode) {
	rownames(weightMatrix) = unlist(lapply(rownames(coocCues),FUN=convertBack))
    } else {
        rownames(weightMatrix) = rownames(coocCues)
    }
    colnames(weightMatrix) = colnames(coocCuesOutcomes)
    if (verbose) message("Completed calculations. Returning weight matrix.")
    flush.console()
    return(weightMatrix)
  }



