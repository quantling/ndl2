#' Implementation of the Rescorla-Wagner equations.
#' 
#' \code{RescorlaWagner} implements an iterative simulation based on the
#' Rescorla-Wagner equations. Given a data frame specifying cues, outcomes, and
#' frequencies, it calculates, for a given cue-outcome pair, the temporal
#' sequence of developing weights.
#' 
#' The equilibrium weights (Danks, 2003) are also estimated.
#' 
#' @param cuesOutcomes A data frame specifying cues, outcomes, and frequencies
#' of combinations of cues and outcomes. In the data frame, cues and outcomes
#' should be character vectors.
#' @param traceCue A character string specifying the cue to be traced over
#' time.
#' @param traceOutcome A character string specifying the outcome to be traced
#' over time.
#' @param nruns An integer specifying the number of times the data have to be
#' presented for learning.  The total number of learning trials is
#' \code{nruns*sum(cuesOutcomes$Frequency)}.
#' @param random A logical specifying whether the order of the learning trials
#' for a given run should be randomly reordered.  Can be set to \code{FALSE} in
#' case all frequencies are 1, and the sequence of learning trials in
#' \code{cuesOutcomes} is given by the order of the rows.
#' @param randomOrder If not \code{NA}, a vector specifying the (usually
#' random) order of the learning trials.
#' @param compareTo A string specifiying the method to use for
#' comparison of the final weights. The default is the Danks
#' equilibria equations \code{"danks"}. Setting the method to
#' \code{"irw"} will change to the iterative method. See the help for
#' \code{\link{ndl2.package}} for a comparison of the two methods.
#' @param alpha The salience of the trace cue.
#' @param lambda The maximum level of associative strength possible.
#' @param beta1 The salience of the situation in which the outcome occurs.
#' @param beta2 The salience of the situation in which the outcome does not
#' occur.
#' @return An object of the class \code{"RescorlaWagner"}, being a list with
#' the following components:
#' \describe{
#'   \item{\code{weightvector}}{ A numeric vector with the weights for all
#'     \code{nruns*sum(dat[,"Frequency"])} training trials.}
#'   \item{\code{equilibriumWeight}}{ The weight of the cue-outcome link at 
#'     equilibrium.}
#'   \item{\code{traceCue}}{ A character string specifying the trace cue.}
#'   \item{\code{traceOutcome}}{ A character string specifying the trace
#'      outcome.}
#' }
#' @author R. H. Baayen and Antti Arppe
#' @seealso \code{\link{orthoCoding}}, \code{\link{plot.RescorlaWagner}},
#' \code{\link{numbers}}
#' @references Danks, D. (2003). Equilibria of the Rescorla-Wagner model.
#' Journal of Mathematical Psychology, 47 (2), 109-121.
#' 
#' Rescorla, R. A., & Wagner, A. R. (1972). A theory of Pavlovian conditioning:
#' Variations in the effectiveness of reinforcement and nonreinforcement. In
#' Black, A. H., & Prokasy, W. F. (Eds.), Classical conditioning II: Current
#' research and theory (pp. 64-99). New York: Appleton-Century-Crofts.
#' @keywords classif
#' @examples
#' 
#' data(lexample)
#' lexample$Cues <- orthoCoding(lexample$Word, grams=1)
#' lexample.rw <- RescorlaWagner(lexample, nruns=25, 
#'    traceCue="h", traceOutcome="hand")
#' plot(lexample.rw)
#' 
#' data(numbers)
#' traceCues=c( "exactly1", "exactly2", "exactly3", "exactly4",
#'    "exactly5", "exactly6", "exactly7", "exactly10", "exactly15")
#' traceOutcomes=c("1", "2", "3", "4", "5", "6", "7", "10", "15")
#' ylimit=c(0,1)
#' par(mfrow=c(3,3),mar=c(4,4,1,1))
#'      
#' for(i in 1:length(traceCues)) {
#'    numbers.rw <- RescorlaWagner(numbers, nruns=1,
#'       traceCue=traceCues[i], traceOutcome=traceOutcomes[i])
#'     plot(numbers.rw, ylimit=ylimit)
#'     mtext(paste(traceCues[i], " - ", traceOutcomes[i], sep=""), 
#'        side=3, line=-1, cex=0.7)
#'   }
#' par(mfrow=c(1,1))
#' 
#' 
RescorlaWagner = function(
    cuesOutcomes, 
    traceCue = "h",
    traceOutcome = "hand",
    nruns=1, 
    random = TRUE,
    randomOrder = NA,
    compareTo = "danks",
    alpha=0.1, lambda=1, beta1=0.1,beta2=0.1) {

  cues =  unique(unlist(strsplit(as.character(cuesOutcomes$Cues), "_")))
  outcomes = unique(unlist(strsplit(as.character(cuesOutcomes$Outcomes), "_")))

  weightvec = rep(0, length(cues))
  names(weightvec) = cues

  res = vector(mode="numeric")
  # ultimately, res will have nruns * sum(cuesOutcomes$Frequency) elements

  dfr = data.frame(
    cuevector = rep(cuesOutcomes$Cues, cuesOutcomes$Frequency),
    outcomevector = rep(cuesOutcomes$Outcomes, cuesOutcomes$Frequency),
    stringsAsFactors=FALSE
  )

  theOrder = NA
  cnt = 0
  for (run in 1:nruns) {
    if (random) {
      if (is.na(randomOrder[1])) {
        theOrder = sample(1:nrow(dfr))
      } else {
        theOrder = randomOrder
      }
      dfr = dfr[theOrder,]
    }
    for (i in 1:nrow(dfr)) {
      currentCues = unlist(strsplit(dfr$cuevector[i],"_"))
      currentOutcomes = unlist(strsplit(dfr$outcomevector[i],"_"))
      Vtotal = sum(weightvec[currentCues])
      if (traceOutcome %in% currentOutcomes) {
        Lambda = lambda
      } else {
        Lambda = 0
      }
      #for (j in 1:length(weightvec)) {  
      #  if (names(weightvec)[j] %in% currentCues) {
      #    weightvec[j] = weightvec[j] + alpha*beta1*(Lambda-Vtotal)
      #  }
      #}
      weightvec[currentCues] = weightvec[currentCues]+alpha*beta1*(Lambda-Vtotal)
      cnt = cnt + 1 
      res[cnt] = weightvec[traceCue]
    }
  }

  if (compareTo == "danks") { 
    eqw = estimateWeights(cuesOutcomes)[traceCue, traceOutcome]
  } else {
    learner = learnWeights(cuesOutcomes)
    eqw = learner$getWeights(traceCue, traceOutcome)
  }

  result <- (list(weightvector=res, equilibriumWeight=eqw,
          traceCue=traceCue, traceOutcome=traceOutcome, randomOrder=theOrder))
  class(result) <- "RescorlaWagner"

  return(result)
}
