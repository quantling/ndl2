#This is a general config section
ndl2.package <- function() {
  library(help=ndl2)$info[[1]] -> version
  if (!is.null(version)) {
    version <- version[pmatch("Version",version)]
    um <- strsplit(version," ")[[1]]
    version <- um[nchar(um)>0][2]
    hello <- paste("This is ndl2 version ",version,". \nFor an overview of the package, type 'help(\"ndl2.package\")'.",sep="")
    packageStartupMessage(hello)
  } else {
    packageStartupMessage("Are we in devtools mode?")
  }
}

#set.ndl.options <- function()
## function used to set options
#{ 
  #add options here
  #ex: options()
#}

.onLoad <- function(...) {
#  set.ndl.options()
#   loadRcppModules(FALSE)
}

.onAttach <- function(...) { 
  ndl2.package()
#  set.ndl.options()
}

.onUnload <- function(libpath) library.dynam.unload("ndl2", libpath)



#' Naive discriminative learning: an implementation in R
#' 
#' Naive discriminative learning implements classification models based on the
#' Rescorla-Wagner equations and the equilibrium equations of the
#' Rescorla-Wagner equations.  This package provides three kinds of
#' functionality: (1) discriminative learning based directly on the
#' Rescorla-Wagner equations or via an approximation
#' proposed by Danks (2) a function implementing the naive
#' discriminative reader, and a model for silent (single-word) reading, and (3)
#' a classifier based on the equilibrium equations.  The functions and datasets
#' for the naive discriminative reader model make it possible to replicate the
#' simulation results for Experiment 1 of Baayen et al. (2011).  The classifier
#' is provided to allow for comparisons between machine learning (svm, TiMBL,
#' glm, random forests, etc.) and discrimination learning.  Compared to
#' standard classification algorithms, naive discriminative learning may
#' overfit the data, albeit gracefully.
#' 
#' \tabular{ll}{
#'   Package: \tab ndl2\cr
#'   Type: \tab Package\cr
#'   Version: \tab 0.1.1\cr
#'   Date: \tab 2019-11-01\cr
#'   License: \tab GPL (>= 2)\cr
#'   LazyLoad: \tab yes\cr
#' }
#' 
#' For more detailed information on the core Rescorla-Wagner equations, see the
#' functions \code{\link{RescorlaWagner}} and
#' \code{\link{plot.RescorlaWagner}}, as well as the data sets
#' \code{\link{danks}}, \code{\link{numbers}} (data courtesy of Michael
#' Ramscar), and \code{\link{lexample}} (an example discussed in Baayen et al.
#' 2011).
#' 
#' The functions for the naive discriminative learning (at the user level) are
#' \code{\link{estimateWeights}} and \code{\link{estimateActivations}}. The
#' relevant data sets are \code{\link{serbian}},
#' \link{serbianUniCyr},\link{serbianUniLat}, and \code{\link{serbianLex}}.
#' The examples for \code{\link{serbianLex}} present the full simulation for
#' Experiment 1 of Baayen et al. (2011).
#' 
#' Key functionality for the user is provided by the functions
#' \code{\link{orthoCoding}}, \code{\link{estimateWeights}}, and
#' \code{estimateActivations}.  \code{orthoCoding} calculates the letter
#' n-grams for character strings, to be used as cues.  It is assumed that
#' meaning or meanings (separated by underscores if there are more then one)
#' are available as outcomes.  The frequency with which each (unique)
#' combination of cues and outcomes occurs are required.  For some example
#' input data sets, see: \code{\link{danks}}, \code{\link{plurals}},
#' \code{\link{serbian}}, \code{\link{serbianUniCyr}} and
#' \code{\link{serbianUniLat}}.
#'
#' There are two NDL algorigthms provided in this package: The first and
#' more recently implemented is the iterative R-W algorithm. This algorithm 
#' changes all the relevant weights at every learning event, which is why
#' it is called the "iterative" algorithm (shortened to IRW).
#'
#' The simplest way of using the IRW method is with the function 
#' \code{\link{learnWeights}}. There are also other helper functions that
#' allow the user to process a corpus directly, such at
#' \code{\link{learnWeightsCorpus}}. See the documentation for these
#' functions for more information. This algorithm is implemented in C++
#' and RCpp is used to interface R and C++.
#'
#' The second method is based on an asympotic estimate of the
#' end-state if learning.  The function \code{\link{estimateWeights}}
#' estimates the association strengths of cues to outcomes, using the
#' equilibrium equations presented in Danks (2003).  The function
#' \code{\link{estimateActivations}} estimates the activations of
#' outcomes (e.g. meanings) given cues (e.g. letter n-grams).
#' 
#' The Rcpp-based \code{\link{learn}} and \code{\link{learnLegacy}} functions
#' use a C++ function to compute the conditional co-occurrence matrices
#' required in the equilibrium equations. These are internally used by
#' \code{\link{estimateWeights}} and should not be used directly by users of
#' the package.
#'
#' Which method should you use? If you are interested in processing speed and
#' are not concerned about the differences in the weights that will inevitably
#' occur, then the following guidelines should guide your choice:
#'
#' Many Cues (more than 10,000), Fewer Outcomes (less than 20,000):
#' The IRW algorithm will usually run faster
#'
#' Many Outcomes (more than 20,000), Fewer Cues (less than 10,000):
#' The equilibria equations (Danks, 2003) estimation will usually run
#' faster.
#'
#' Both algorithms can take advantage of multiple cores are large
#' memory availability. The IRW algorithm has an explicit option to
#' specify the number of threads. The Danks algorithm uses the
#' capabilities of your BLAS library. To get the most performance with
#' multiple cores, the OpenBLAS libraries are highly recommended.
#' 
#' The key function for naive discriminative classification is
#' \code{\link{ndlClassify}}; see data sets \code{\link{think}} and
#' \code{\link{dative}} for examples.
#' 
#' @name ndl2.package
#' @aliases ndl2.package ndl2
#' @docType package
#' @author Cyrus Shaoul, Samuel Bitschau, Nathanael Schilling, Antti Arppe, Peter Hendrix, Petar Milin, and R. Harald
#' Baayen.
#' 
#' Maintainer: Cyrus Shaoul <cyrus.shaoul@@uni-tuebingen.de>
#' 
#' Author Contributions: Initial concept by R. Harald Baayen with contributions
#' from Petar Milin and Peter Hendrix. First R coding done by R. Harald Baayen.
#' 
#' Initial R package development until version 0.1.6 by Antti Arppe. Initial
#' documentation by Antti Arppe.  Initial optimizations in C by Petar Milin and
#' Antti Arppe.
#' 
#' Classification functionality developed further by Antti Arppe.
#' 
#' In version 0.2.14 onwards, improvements to the NDL algorithm by Petar Milin
#' and Cyrus Shaoul. In version 0.2.14 onwards, improved performance
#' optimizations (C++ and Rcpp) by Cyrus Shaoul.
#'
#' In version 1.9 onwards, iterative R-W computation core by Nathanael Schilling,
#' R and RCpp integration by Samuel Bitschau
#' 
#' @references Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and
#' Hendrix, P. and Marelli, M., An amorphous model for morphological processing
#' in visual comprehension based on naive discriminative learning.
#' Psychological Review, 118, 438-482.
#' 
#' Baayen, R. H. (2011) Corpus linguistics and naive discriminative learning.
#' Brazilian Journal of Applied Linguistics, 11, 295-328.
#' 
#' Arppe, A. and Baayen, R. H. (in prep.) Statistical classification and
#' principles of human learning.
#' @keywords naive discriminative learning
#' @examples
#' 
#' \dontrun{
#' # Rescorla-Wagner 
#' data(lexample)
#' 
#' lexample$Cues <- orthoCoding(lexample$Word, grams=1)
#' lexample.rw <- RescorlaWagner(lexample, nruns=25, traceCue="h",
#'    traceOutcome="hand")
#' plot(lexample.rw)
#' mtext("h - hand", 3, 1)
#' 
#' data(numbers)
#'      
#' traceCues <- c( "exactly1", "exactly2", "exactly3", "exactly4", "exactly5",
#'    "exactly6", "exactly7", "exactly10", "exactly15")
#' traceOutcomes <- c("1", "2", "3", "4", "5", "6", "7", "10", "15")
#' 
#' ylimit <- c(0,1)
#' par(mfrow=c(3,3), mar=c(4,4,1,1))
#' 
#' for (i in 1:length(traceCues)) {
#'   numbers.rw <- RescorlaWagner(numbers, nruns=1, traceCue=traceCues[i],
#'      traceOutcome=traceOutcomes[i])
#'   plot(numbers.rw, ylimit=ylimit)
#'   mtext(paste(traceCues[i], " - ", traceOutcomes[i], sep=""), side=3, line=-1,
#'     cex=0.7)
#' }
#' par(mfrow=c(1,1))
#' 
#' # naive discriminative learning (for complete example, see serbianLex)
#' # This function uses a Unicode dataset.
#' data(serbianUniCyr)
#' serbianUniCyr$Cues <- orthoCoding(serbianUniCyr$WordForm, grams=2)
#' serbianUniCyr$Outcomes <- serbianUniCyr$LemmaCase
#' sw <- estimateWeights(cuesOutcomes=serbianUniCyr,hasUnicode=T)
#' 
#' desiredItems <- unique(serbianUniCyr["Cues"])
#' desiredItems$Outcomes=""
#' activations <- estimateActivations(desiredItems, sw)$activationMatrix
#' rownames(activations) <- unique(serbianUniCyr[["WordForm"]])
#' 
#' syntax <- c("acc", "dat", "gen", "ins", "loc", "nom", "Pl",  "Sg") 
#' activations2 <- activations[,!is.element(colnames(activations), syntax)]
#' head(rownames(activations2),50)
#' head(colnames(activations2),8)
#' 
#' image(activations2, xlab="word forms", ylab="meanings", xaxt="n", yaxt="n")
#' mtext(c("yena", "...", "zvuke"), side=1, line=1, at=c(0, 0.5, 1),  adj=c(0,0,1))
#' mtext(c("yena", "...", "zvuk"), side=2, line=1, at=c(0, 0.5, 1),   adj=c(0,0,1))
#' 
#' # naive discriminative classification
#' data(think)
#' think.ndl <- ndlClassify(Lexeme ~ Person + Number + Agent + Patient + Register,
#'    data=think)
#' summary(think.ndl)
#' plot(think.ndl, values="weights", type="hist", panes="multiple")
#' plot(think.ndl, values="probabilities", type="density")
#' }
#' 
NULL




#' Example data from Danks (2003), after Spellman (1996).
#' 
#' Data of Spellman (1996) used by Danks (2003) to illustrate the equilibrium
#' equations for the Rescorla-Wagner model.  There are two liquids (red and
#' blue) that are potentially fertilizers, and the experimental participant is
#' given the rates at which flowers bloom for the four possible conditions (no
#' liquid, red liquid, blue liquid, and both liquids).
#' 
#' For details, see Danks (2003: 112).
#' 
#' @name danks
#' @docType data
#' @format A data frame with 8 observations on the following 3 variables.
#' \describe{ \item{\code{Cues}}{A character vector specifying the cues. The
#' pots in which the flowers are grown, and the color of the fertilizer.
#' Individual cues are separated by underscores.} \item{\code{Outcomes}}{A
#' character vector specifying whether plants flowered (y or n).}
#' \item{\code{Frequency}}{A numeric vector specifying the frequency of
#' flowering.} }
#' @references D. Danks (2003), Equilibria of the Rescorla-Wagner model.
#' Journal of Mathematical Psychology 47, 109-121.
#' 
#' B. A. Spellman, (1996). Conditionalizing causality. In Shanks, D. R.,
#' Holyoak, K. J., & Medin, D. L. (Eds.), Causal learning: the psychology of
#' learning and motivation, Vol. 34 (pp. 167-206). San Diego, CA: Academic
#' Press.
#' @source B. A. Spellman, (1996). Conditionalizing causality. In Shanks, D.
#' R., Holyoak, K. J., & Medin, D. L. (Eds.), Causal learning: the psychology
#' of learning and motivation, Vol. 34 (pp. 167-206). San Diego, CA: Academic
#' Press.
#' @keywords datasets
#' @examples
#' 
#' data(danks)
#' estimateWeights(cuesOutcomes=danks)
#' 
NULL





#' Dative Alternation
#' 
#' Data describing the realization of the dative as NP or PP in the Switchboard
#' corpus and the Treebank Wall Street Journal collection.
#' 
#' 
#' @name dative
#' @docType data
#' @format A data frame with 3263 observations on the following 15 variables.
#' \describe{ \item{\code{Speaker}}{a factor coding speaker; available only
#' for the subset of spoken English.} \item{\code{Modality}}{a factor with
#' levels \code{spoken}, \code{written}.} \item{\code{Verb}}{a factor with the
#' verbs as levels.} \item{\code{SemanticClass}}{a factor with levels \code{a}
#' (abstract: 'give it some thought'), \code{c} (communication: 'tell, give me
#' your name'), \code{f} (future transfer of possession: 'owe, promise'),
#' \code{p} (prevention of possession: 'cost, deny'), and \code{t} (transfer of
#' possession: 'give an armband, send').} \item{\code{LengthOfRecipient}}{a
#' numeric vector coding the number of words comprising the recipient.}
#' \item{\code{AnimacyOfRec}}{a factor with levels \code{animate} and
#' \code{inanimate} for the animacy of the recipient.}
#' \item{\code{DefinOfRec}}{a factor with levels \code{definite} and
#' \code{indefinite} coding the definiteness of the recipient.}
#' \item{\code{PronomOfRec}}{a factor with levels \code{nonpronominal} and
#' \code{pronominal} coding the pronominality of the recipient.}
#' \item{\code{LengthOfTheme}}{a numeric vector coding the number of words
#' comprising the theme.} \item{\code{AnimacyOfTheme}}{a factor with levels
#' \code{animate} and \code{inanimate} coding the animacy of the theme.}
#' \item{\code{DefinOfTheme}}{a factor with levels \code{definite} and
#' \code{indefinite} coding the definiteness of the theme.}
#' \item{\code{PronomOfTheme}}{a factor with levels \code{nonpronominal} and
#' \code{pronominal} coding the pronominality of the theme.}
#' \item{\code{RealizationOfRecipient}}{a factor with levels \code{NP} and
#' \code{PP} coding the realization of the dative.}
#' \item{\code{AccessOfRec}}{a factor with levels \code{accessible},
#' \code{given}, and \code{new} coding the accessibility of the recipient.}
#' \item{\code{AccessOfTheme}}{a factor with levels \code{accessible},
#' \code{given}, and \code{new} coding the accessibility of the theme.} }
#' @references
#' 
#' Bresnan, J., Cueni, A., Nikitina, T. and Baayen, R. H. (2007) Predicting the
#' dative alternation, in Bouma, G. and Kraemer, I. and Zwarts, J.  (eds.),
#' \emph{Cognitive Foundations of Interpretation}, Royal Netherlands Academy of
#' Sciences, 69-94.
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' data(dative)
#' out <- which(is.element(colnames(dative), c("Speaker","Verb")))
#' dative <- dative[,-out]
#' dative.ndl <- ndlClassify(RealizationOfRecipient ~ ., data=dative)
#' ndlStatistics(dative.ndl)
#' 
#' } 
NULL





#' Lexical example data illustrating the Rescorla-Wagner equations
#' 
#' Ten monomorphemic and inflected English words with fictive frequencies, and
#' meanings.
#' 
#' This example lexicon is used in Baayen et al. (2011) (table 8, figure 4) to
#' illustrate the Rescorla-Wagner equations.
#' 
#' @name lexample
#' @docType data
#' @format A data frame with 10 observations on the following 3 variables:
#' \describe{ \item{\code{Word}}{A character vector specifying word forms}
#' \item{\code{Frequency}}{A numeric vector with the -- fictive -- frequencies
#' of occurrence of the words} \item{\code{Outcomes}}{A character vector
#' specifying the meaning components of the words, separated by underscores} }
#' @seealso \code{\link{RescorlaWagner}, \link{orthoCoding}}
#' @references
#' 
#' Baayen, R. H., Milin, P., Filipovic Durdevic, D., Hendrix, P. and Marelli,
#' M.  (2011), An amorphous model for morphological processing in visual
#' comprehension based on naive discriminative learning. Psychological Review,
#' 118, 438-482.
#' @keywords datasets
#' @examples
#' 
#' data(lexample)
#' lexample$Cues <- orthoCoding(lexample$Word, grams=1)
#' par(mfrow=c(2,2))
#' lexample.rw <- RescorlaWagner(lexample, nruns=25, traceCue="h",traceOutcome="hand")
#' plot(lexample.rw)
#' mtext("h - hand", 3, 1)
#' 
#' lexample.rw <- RescorlaWagner(lexample, nruns=25, traceCue="s",traceOutcome="plural")
#' plot(lexample.rw)
#' mtext("s - plural", 3, 1)
#' 
#' lexample.rw <- RescorlaWagner(lexample, nruns=25, traceCue="a",traceOutcome="as")
#' plot(lexample.rw)
#' mtext("a - as", 3, 1)
#' 
#' lexample.rw <- RescorlaWagner(lexample, nruns=25, traceCue="s",traceOutcome="as")
#' plot(lexample.rw)
#' mtext("s - as", 3, 1)
#' par(mfrow=c(1,1))
#' 
NULL





#' Example data illustrating the Rescorla-Wagner equations as applied to
#' numerical cognition by Ramscar et al. (2011).
#' 
#' The data used in simulation 3 of Ramscar et al. (2011) on numerical
#' cognition.
#' 
#' The cues represent learning trials with objects of the same size, shape and
#' color. The numeric cues represent the presence of at least one subset of the
#' specified size.  The cues \code{exactlyn} represent the presence of exactly
#' \code{n} objects. We are indebted to Michael Ramscar to making this data set
#' available for inclusion in the package.
#' 
#' @name numbers
#' @docType data
#' @format A data frame with 10 observations on the following 3 variables.
#' \describe{ \item{\code{Cues}}{A character vector specifying cues for
#' quantities, separated by underscores.} \item{\code{Frequency}}{The
#' frequencies with which the numbers appear in the COCA corpus.}
#' \item{\code{Outcomes}}{A character vector specifying numerical outcomes
#' associated with the input quantities.} }
#' @references Michael Ramscar, Melody Dye, Hanna Muenke Popick & Fiona
#' O'Donnell-McCarthy (2011), The Right Words or Les Mots Justes? Why Changing
#' the Way We Speak to Children Can Help Them Learn Numbers Faster.
#' Manuscript, Department of Psychology, Stanford University.
#' @keywords datasets
#' @examples
#' 
#' data(numbers)
#' 
#' traceCues=c( "exactly1", "exactly2", "exactly3", "exactly4",
#' "exactly5", "exactly6", "exactly7", "exactly10", "exactly15")
#' traceOutcomes=c("1", "2", "3", "4", "5", "6", "7", "10", "15")
#' 
#' ylimit=c(0,1)
#' par(mfrow=c(3,3),mar=c(4,4,1,1))
#' 
#' for (i in 1:length(traceCues)){
#'   numbers.rw = RescorlaWagner(numbers, nruns=1,
#'     traceCue=traceCues[i],traceOutcome=traceOutcomes[i])
#'   plot(numbers.rw, ylimit=ylimit)
#'   mtext(paste(traceCues[i], " - ", traceOutcomes[i], sep=""), 
#'     side=3, line=-1, cex=0.7)
#' }
#' par(mfrow=c(1,1))
#' 
#' 
NULL





#' Artificial data set used to illustrate the Rescorla-Wagner equations and
#' naive discriminative learning.
#' 
#' Data set with 10 English words of different (ad hoc) frequencies, each with
#' a lexical meaning and a grammatical meaning.
#' 
#' 
#' @name plurals
#' @docType data
#' @format A data frame with 10 observations on the following 3 variables:
#' \describe{ \item{\code{WordForm}}{A character vector of word forms (cues).}
#' \item{\code{Frequency}}{A numeric vector of frequencies.}
#' \item{\code{Outcomes}}{A character vector of meanings (outcomes). Meanings
#' are separated by underscores. The \code{NIL} meaning is ignored.} }
#' @references
#' 
#' Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix, P. and
#' Marelli, M., An amorphous model for morphological processing in visual
#' comprehension based on naive discriminative learning. Psychological Review,
#' 118, 438-482.
#' @source
#' 
#' Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix, P. and
#' Marelli, M., An amorphous model for morphological processing in visual
#' comprehension based on naive discriminative learning. Psychological Review,
#' 118, 438-482.
#' @keywords datasets
#' @examples
#' 
#' data(plurals)
#' plurals$Cues <- orthoCoding(plurals$WordForm, grams=1)
#' estimateWeights(cuesOutcomes=plurals)
#' 
NULL





#' Serbian lexicon with 1187 prime-target pairs.
#' 
#' The 1187 prime-target pairs and their lexical properties used in the
#' simulation study of Experiment 1 of Baayen et al. (2011).
#' 
#' 
#' @name serbianLex
#' @docType data
#' @format A data frame with 1187 observations on the following 14 variables:
#' \describe{ \item{\code{Target}}{A factor specifying the target noun form}
#' \item{\code{Prime}}{A factor specifying the prime noun form}
#' \item{\code{PrimeLemma}}{A factor specifying the lemma of the prime}
#' \item{\code{TargetLemma}}{A factor specifying the target lemma}
#' \item{\code{Length}}{A numeric vector with the length in letters of the
#' target} \item{\code{WeightedRE}}{A numeric vector with the weighted
#' relative entropy of the prime and target inflectional paradigms}
#' \item{\code{NormLevenshteinDist}}{A numeric vector with the normalized
#' Levenshtein distance of prime and target forms}
#' \item{\code{TargetLemmaFreq}}{A numeric vector with log frequency of the
#' target lemma} \item{\code{PrimeSurfFreq}}{A numeric vector with log
#' frequency of the prime form} \item{\code{PrimeCondition}}{A factor with
#' prime conditions, levels: \code{DD}, \code{DSSD}, \code{SS}}
#' \item{\code{CosineSim}}{A numeric vector with the cosine similarity of
#' prime and target vector space semantics} \item{\code{IsMasc}}{A vector of
#' logicals, \code{TRUE} if the noun is masculine.}
#' \item{\code{TargetGender}}{A factor with the gender of the target, levels:
#' \code{f}, \code{m}, and \code{n}} \item{\code{TargetCase}}{A factor
#' specifying the case of the target noun, levels: \code{acc}, \code{dat},
#' \code{nom}} \item{\code{MeanLogObsRT}}{Mean log-transformed observed
#' reaction time} }
#' @references
#' 
#' Baayen, R. H., Milin, P., Filipovic Durdevic, D., Hendrix, P. and Marelli,
#' M. (2011), An amorphous model for morphological processing in visual
#' comprehension based on naive discriminative learning. Psychological Review,
#' 118, 438-482.
#' @keywords datasets
#' @examples
#' 
#' # calculate the weight matrix for the full set of Serbian nouns
#' data(serbian)
#' serbian$Cues <- orthoCoding(serbian$WordForm, grams=2)
#' serbian$Outcomes <- serbian$LemmaCase
#' sw <- estimateWeights(cuesOutcomes=serbian)
#' 
#' # calculate the meaning activations for all unique word forms
#' 
#' desiredItems <- unique(serbian["Cues"])
#' desiredItems$Outcomes <- ""
#' activations <- estimateActivations(desiredItems, sw)$activationMatrix
#' rownames(activations) <- unique(serbian[["WordForm"]])
#' activations <- activations + abs(min(activations))
#' activations[1:5,1:6]
#' 
#' # calculate simulated latencies for the experimental materials
#' 
#' data(serbianLex)
#' syntax <- c("acc", "dat", "gen", "ins", "loc", "nom", "Pl", "Sg")
#' we <- 0.4 # compound cue weight
#' strengths <- rep(0, nrow(serbianLex))
#' for(i in 1:nrow(serbianLex)) {
#'    target <- serbianLex$Target[i]
#'    prime <- serbianLex$Prime[i]
#'    targetLemma <- as.character(serbianLex$TargetLemma[i])
#'    primeLemma <- as.character(serbianLex$PrimeLemma[i])
#'    targetOutcomes <- c(targetLemma, primeLemma, syntax)
#'    primeOutcomes <- c(targetLemma, primeLemma, syntax)
#'    p <- activations[target, targetOutcomes]
#'    q <- activations[prime, primeOutcomes]
#'    strengths[i] <- sum((q^we)*(p^(1-we)))
#' }
#' serbianLex$SimRT <- -strengths
#' lengthPenalty <- 0.3
#' serbianLex$SimRT2 <- serbianLex$SimRT + 
#'   (lengthPenalty * (serbianLex$Length>5))
#' 
#' cor.test(serbianLex$SimRT, serbianLex$MeanLogObsRT)
#' cor.test(serbianLex$SimRT2, serbianLex$MeanLogObsRT)
#' 
#' serbianLex.lm <- lm(SimRT2 ~ Length +  WeightedRE*IsMasc + 
#'       NormLevenshteinDist + TargetLemmaFreq + 
#'       PrimeSurfFreq + PrimeCondition, data=serbianLex)
#' summary(serbianLex.lm)
#' 
NULL





#' Serbian case inflected nouns.
#' 
#' 3240 case-inflected Serbian nouns and their frequencies, for 270 different
#' masculine, feminine and neuter noun lemmas.
#' 
#' Frequencies were taken from the Frequency Dictionary of Contemporary Serbian
#' Language (Kostic, 1999). The 270 lemmas comprise the set of nouns for which
#' each different case form appears at least once in this resource.
#' 
#' @name serbian
#' @docType data
#' @format A data frame with 3240 observations on the following 3 variables:
#' \describe{ \item{\code{WordForm}}{A character vector specifying the
#' inflected word forms.} \item{\code{LemmaCase}}{A character vector
#' specifying lemma (meaning), case, and number.} \item{\code{Frequency}}{A
#' numeric vector specifying the frequency of each word form.} }
#' @seealso See also \code{\link{serbianLex}, \link{estimateActivations}}.
#' @references Kostic, D. (1999). Frekvencijski recnik savremenog srpskog
#' jezika (Frequency Dictionary of Contemporary Serbian Language). Institute
#' for Experimental Phonetics and Speech Pathology & Laboratory of Experimental
#' Psychology, University of Belgrade, Serbia.
#' 
#' Baayen, R. H., Milin, P., Filipovic Durdevic, D., Hendrix, P. and Marelli,
#' M. (2011), An amorphous model for morphological processing in visual
#' comprehension based on naive discriminative learning. Psychological Review,
#' 118, 438-482.
#' @source Kostic, D. (1999). Frekvencijski recnik savremenog srpskog jezika
#' (Frequency Dictionary of Contemporary Serbian Language). Institute for
#' Experimental Phonetics and Speech Pathology & Laboratory of Experimental
#' Psychology, University of Belgrade, Serbia.
#' @keywords datasets
#' @examples
#' 
#' data(serbian)
#' serbian$Cues <- orthoCoding(serbian$WordForm, grams=2)
#' serbian$Outcomes <- serbian$LemmaCase
#' sw <- estimateWeights(cuesOutcomes=serbian)
#' sw[1:5,1:5]
#' desiredItems <- unique(serbian["Cues"])
#' desiredItems$Outcomes=""
#' activations <- estimateActivations(desiredItems, sw)$activationMatrix
#' rownames(activations) <- unique(serbian[["WordForm"]])
#' activations[1:5,1:6]
#' 
NULL





#' Serbian case inflected nouns (in Cyrillic Unicode).
#' 
#' 3240 case-inflected Serbian nouns and their frequencies, for 270 different
#' masculine, feminine and neuter noun lemmas, written using the Cyrillic
#' alphabet and encoded in UTF-8.
#' 
#' Frequencies were taken from the Frequency Dictionary of Contemporary Serbian
#' Language (Kostic, 1999).  The 270 lemmas comprise the set of nouns for which
#' each different case form appears at least once in this resource.
#' 
#' @name serbianUniCyr
#' @docType data
#' @format A data frame with 3240 observations on the following 3 variables:
#' \describe{ \item{\code{WordForm}}{A character vector specifying the
#' inflected word forms encoded in UTF-8.} \item{\code{LemmaCase}}{A character
#' vector specifying lemma (meaning), case, and number.}
#' \item{\code{Frequency}}{A numeric vector specifying the frequency of each
#' word form.} }
#' @seealso See also \code{\link{serbian}, \link{serbianLex},
#' \link{estimateActivations}}.
#' @references Kostic, D. (1999). Frekvencijski recnik savremenog srpskog
#' jezika (Frequency Dictionary of Contemporary Serbian Language). Institute
#' for Experimental Phonetics and Speech Pathology & Laboratory of Experimental
#' Psychology, University of Belgrade, Serbia.
#' 
#' Baayen, R. H., Milin, P., Filipovic Durdevic, D., Hendrix, P. and Marelli,
#' M. (2011), An amorphous model for morphological processing in visual
#' comprehension based on naive discriminative learning. Psychological Review,
#' 118, 438-482.
#' @source Kostic, D. (1999). Frekvencijski recnik savremenog srpskog jezika
#' (Frequency Dictionary of Contemporary Serbian Language). Institute for
#' Experimental Phonetics and Speech Pathology & Laboratory of Experimental
#' Psychology, University of Belgrade, Serbia.
#' @keywords datasets
#' @examples
#' 
#' data(serbianUniCyr)
#' serbianUniCyr$Cues <- orthoCoding(serbianUniCyr$WordForm, grams=2)
#' serbianUniCyr$Outcomes <- serbianUniCyr$LemmaCase
#' sw <- estimateWeights(cuesOutcomes=serbianUniCyr)
#' sw[1:5,1:5]
#' desiredItems <- unique(serbianUniCyr["Cues"])
#' desiredItems$Outcomes=""
#' activations <- estimateActivations(desiredItems, sw)$activationMatrix
#' rownames(activations) <- unique(serbianUniCyr[["WordForm"]])
#' activations[1:5,1:6]
#' 
NULL





#' Serbian case inflected nouns (in Latin-alphabet Unicode).
#' 
#' 3240 case-inflected Serbian nouns and their frequencies, for 270 different
#' masculine, feminine and neuter noun lemmas, written using the Latin alphabet
#' and encoded in UTF-8.
#' 
#' Frequencies were taken from the Frequency Dictionary of Contemporary Serbian
#' Language (Kostic, 1999).  The 270 lemmas comprise the set of nouns for which
#' each different case form appears at least once in this resource.
#' 
#' @name serbianUniLat
#' @docType data
#' @format A data frame with 3240 observations on the following 3 variables:
#' \describe{ \item{\code{WordForm}}{A character vector specifying the
#' inflected word forms encoded in UTF-8.} \item{\code{LemmaCase}}{A character
#' vector specifying lemma (meaning), case, and number.}
#' \item{\code{Frequency}}{A numeric vector specifying the frequency of each
#' word form.} }
#' @seealso See also \code{\link{serbian}, \link{serbianLex},
#' \link{estimateActivations}}.
#' @references Kostic, D. (1999). Frekvencijski recnik savremenog srpskog
#' jezika (Frequency Dictionary of Contemporary Serbian Language). Institute
#' for Experimental Phonetics and Speech Pathology & Laboratory of Experimental
#' Psychology, University of Belgrade, Serbia.
#' 
#' Baayen, R. H., Milin, P., Filipovic Durdevic, D., Hendrix, P. and Marelli,
#' M. (2011), An amorphous model for morphological processing in visual
#' comprehension based on naive discriminative learning. Psychological Review,
#' 118, 438-482.
#' @source Kostic, D. (1999). Frekvencijski recnik savremenog srpskog jezika
#' (Frequency Dictionary of Contemporary Serbian Language). Institute for
#' Experimental Phonetics and Speech Pathology & Laboratory of Experimental
#' Psychology, University of Belgrade, Serbia.
#' @keywords datasets
#' @examples
#' 
#' data(serbianUniLat)
#' serbianUniLat$Cues <- orthoCoding(serbianUniLat$WordForm, grams=2)
#' serbianUniLat$Outcomes <- serbianUniLat$LemmaCase
#' sw <- estimateWeights(cuesOutcomes=serbianUniLat)
#' sw[1:5,1:5]
#' desiredItems <- unique(serbianUniLat["Cues"])
#' desiredItems$Outcomes=""
#' activations <- estimateActivations(desiredItems, sw)$activationMatrix
#' rownames(activations) <- unique(serbianUniLat[["WordForm"]])
#' activations[1:5,1:6]
#' 
NULL





#' Finnish \sQuote{think} verbs.
#' 
#' 3404 occurrences of four synonymous Finnish \sQuote{think} verbs
#' (\sQuote{ajatella}: 1492; \sQuote{mietti\"a}: 812; \sQuote{pohtia}: 713;
#' \sQuote{harkita}: 387) in newspaper and Internet newsgroup discussion texts
#' 
#' 
#' The four most frequent synonyms meaning \sQuote{think, reflect, ponder,
#' consider}, i.e. \sQuote{ajatella, miettia, pohtia, harkita}, were extracted
#' from two months of newspaper text from the 1990s (Helsingin Sanomat 1995)
#' and six months of Internet newsgroup discussion from the early 2000s (SFNET
#' 2002-2003), namely regarding (personal) relationships
#' (sfnet.keskustelu.ihmissuhteet) and politics (sfnet.keskustelu.politiikka).
#' The newspaper corpus consisted of 3,304,512 words of body text (i.e.
#' excluding headers and captions as well as punctuation tokens), and included
#' 1,750 examples of the studied \sQuote{think} verbs. The Internet corpus
#' comprised 1,174,693 words of body text, yielding 1,654 instances of the
#' selected \sQuote{think} verbs. In terms of distinct identifiable authors,
#' the newspaper sub-corpus was the product of just over 500 journalists and
#' other contributors, while the Internet sub-corpus involved well over 1000
#' discussants. The \code{think} dataset contains a selection of 26 contextual
#' features judged as most informative.
#' 
#' For extensive details of the data and its linguistic and statistical
#' analysis, see Arppe (2008). For the full selection of contextual features,
#' see the \code{amph} (2008) microcorpus.
#' 
#' @name think
#' @docType data
#' @format A data frame with 3404 observations on the following 27 variables:
#' 
#' \describe{ \item{\code{Lexeme}}{A factor specifying one of the four
#' \sQuote{think} verb synonyms} \item{\code{Polarity}}{A factor specifying
#' whether the \sQuote{think} verb has negative polarity (\code{Negation}) or
#' not (\code{Other})} \item{\code{Voice}}{A factor specifying whether the
#' \sQuote{think} verb is in the \code{Passive} voice or not (\code{Other})}
#' \item{\code{Mood}}{A factor specifying whether the \sQuote{think} verb is
#' in the \code{Indicative} or \code{Conditional} mood or not (Other)}
#' \item{\code{Person}}{A factor specifying whether the \sQuote{think} verb is
#' in the \code{First}, \code{Second}, \code{Third} person or not
#' (\code{None})} \item{\code{Number}}{A factor specifying whether the
#' \sQuote{think} verb is in the \code{Plural} number or not (\code{Other})}
#' \item{\code{Covert}}{A factor specifying whether the agent/subject of the
#' \sQuote{think} verb is explicitly expressed as a syntactic argument
#' (\code{Overt}), or only as a morphological feature of the \sQuote{think}
#' verb (\code{Covert})} \item{\code{ClauseEquivalent}}{A factor specifying
#' whether the \sQuote{think} verb is used as a non-finite clause equivalent
#' (\code{ClauseEquivalent}) or as a finite verb (\code{FiniteVerbChain})}
#' \item{\code{Agent}}{A factor specifying the occurrence of Agent/Subject of
#' the \sQuote{think} verb as either a Human \code{Individual}, Human
#' \code{Group}, or as absent (\code{None})} \item{\code{Patient}}{A factor
#' specifying the occurrence of the Patient/Object argument among the semantic
#' or structural subclasses as either an Human Individual or Group
#' (\code{IndividualGroup}), \code{Abstraction}, \code{Activity},
#' \code{Communication}, \code{Event}, an \sQuote{etta} (\sQuote{that}) clause
#' (\code{etta_CLAUSE}), \code{DirectQuote}, \code{IndirectQuestion},
#' \code{Infinitive}, \code{Participle}, or as absent (\code{None})}
#' \item{\code{Manner}}{A factor specifying the occurrrence of the Manner
#' argument as any of its subclasses \code{Generic}, \code{Negative}
#' (sufficiency), \code{Positive} (sufficiency), \code{Frame}, \code{Agreement}
#' (Concur or Disagree), \code{Joint} (Alone or Together), or as absent
#' (\code{None})} \item{\code{Time}}{A factor specifying the occurrence of
#' Time argument (as a moment) as either of its subclasses \code{Definite},
#' \code{Indefinite}, or as absent (\code{None})} \item{\code{Modality1}}{A
#' factor specifying the main semantic subclasses of the entire Verb chain as
#' either indicating \code{Possibility}, \code{Necessity}, or their absense
#' (\code{None})} \item{\code{Modality2}}{A factor specifying minor semantic
#' subclasses of the entire Verb chain as indicating either a \code{Temporal}
#' element (begin, end, continuation, etc.), \code{External} (cause),
#' \code{Volition}, \code{Accidental} nature of the thinking process, or their
#' absense (\code{None})} \item{\code{Source}}{A factor specifying the
#' occurrence of a \code{Source} argument or its absense (\code{None})}
#' \item{\code{Goal}}{A factor specifying the occurrence of a \code{Goal}
#' argument or its absence (\code{None})} \item{\code{Quantity}}{A factor
#' specifying the occurrence of a \code{Quantity} argument, or its absence
#' (\code{None})} \item{\code{Location}}{A factor specifying the occurrence of
#' a \code{Location} argument, or its absence (\code{None})}
#' \item{\code{Duration}}{A factor specifying the occurrence of a
#' \code{Duration} argument, or its absence (\code{None})}
#' \item{\code{Frequency}}{A factor specifying the occurrence of a
#' \code{Frequency} arument, or its absence (\code{None})}
#' \item{\code{MetaComment}}{A factor specifying the occurrence of a
#' \code{MetaComment}, or its absence (\code{None})}
#' \item{\code{ReasonPurpose}}{A factor specifying the occurrence of a Reason
#' or Purpose argument (\code{ReasonPurpose}), or their absence (\code{None})}
#' \item{\code{Condition}}{A factor specifying the occurrence of a
#' \code{Condition} argument, or its absence (\code{None})}
#' \item{\code{CoordinatedVerb}}{A factor specifying the occurrence of a
#' Coordinated Verb (in relation to the \sQuote{think} verb:
#' \code{CoordinatedVerb}), or its absence (\code{None})}
#' \item{\code{Register}}{A factor specifying whether the \sQuote{think} verb
#' occurs in the newspaper subcorpus (\code{hs95}) or the Internet newsgroup
#' discussion corpus (\code{sfnet})} \item{\code{Section}}{A factor specifying
#' the subsection in which the \sQuote{think} verb occurs in either of the two
#' subcorpora} \item{\code{Author}}{A factor specifying the author of the text
#' in which the \sQuote{think} verb occurs, if that author is identifiable --
#' authors in the Internet newgroup discussion subcorpus are anonymized;
#' unidentifiable/unknown author designated as (\code{None})} }
#' @references Arppe, A. 2008. Univariate, bivariate and multivariate methods
#' in corpus-based lexicography -- a study of synonymy. Publications of the
#' Department of General Linguistics, University of Helsinki, No. 44. URN:
#' http://urn.fi/URN:ISBN:978-952-10-5175-3.
#' 
#' Arppe, A. 2009. Linguistic choices vs. probabilities -- how much and what
#' can linguistic theory explain? In: Featherston, Sam & Winkler, Susanne
#' (eds.) The Fruits of Empirical Linguistics. Volume 1: Process. Berlin: de
#' Gruyter, pp. 1-24.
#' @source amph 2008. A micro-corpus of 3404 occurrences of the four most
#' common Finnish THINK lexemes, \sQuote{ajatella, miettia, pohtia, and
#' harkita}, in Finnish newspaper and Internet newsgroup discussion texts,
#' containing extracts and linguistic analysis of the relevant context in the
#' original corpus data, scripts for processing this data, R functions for its
#' statistical analysis, as well as a comprehensive set of ensuing results as R
#' data tables. Compiled and analyzed by Antti Arppe. Available on-line at URL:
#' http://www.csc.fi/english/research/software/amph/
#' 
#' Helsingin Sanomat 1995. ~22 million words of Finnish newspaper articles
#' published in Helsingin Sanomat during January--December 1995. Compiled by
#' the Research Institute for the Languages of Finland [KOTUS] and CSC -- IT
#' Center for Science, Finland. Available on-line at URL:
#' http://www.csc.fi/kielipankki/
#' 
#' SFNET 2002-2003. ~100 million words of Finnish internet newsgroup discussion
#' posted during October 2002 -- April 2003. Compiled by Tuuli Tuominen and
#' Panu Kalliokoski, Computing Centre, University of Helsinki, and Antti Arppe,
#' Department of General Linguistics, University of Helsinki, and CSC -- IT
#' Center for Science, Finland. Available on-line at URL:
#' http://www.csc.fi/kielipankki/
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#' data(think)
#' think.ndl = ndlClassify(Lexeme ~ Person + Number + Agent + Patient + Register,
#'    data=think)
#' summary(think.ndl)
#' plot(think.ndl)
#' }
#' 
NULL



