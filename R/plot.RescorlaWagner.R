#' Plot function for the output of \code{RescorlaWagner}.
#' 
#' This function graphs the Rescorla-Wagner weights for a cue-outcome pair
#' against learning time.
#' 
#' 
#' @param x A object of the class \code{"RescorlaWagner"} produced by
#' \code{RescorlaWagner}, consisting of a list including estimated weights for
#' the incremental and equilibrium stages.
#' @param asymptote A logical specifying whether the equilibrium asymptotic
#' weight should be added to the plot.
#' @param xlab Label for x-axis, by default \code{"t"}.
#' @param ylab Label for y-axis, by default \code{"weight"}.
#' @param ylimit The range of values to be displayed on the Y axis. By default,
#' this will be determined from the data itself.
#' @param \dots Arguments to be passed to methods, such as graphical parameters
#' (see \code{link{par}}).
#' @return A plot is produced on the graphics device.
#' @author R. H. Baayen and Antti Arppe
#' @seealso \code{\link{RescorlaWagner}}, \code{\link{orthoCoding}}
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
#' 
#' data(lexample)
#' lexample$Cues <- orthoCoding(lexample$Word, grams=1)
#' lexample.rw <- RescorlaWagner(lexample, nruns=25, 
#'    traceCue="h", traceOutcome="hand")
#' plot(lexample.rw)
#' mtext("h - hand", 3, 1)
#' 
#' # Full example
#' 
#' \dontrun{
#' par(mfrow=c(2,2))
#' lexample.rw <- RescorlaWagner(lexample, nruns=25, 
#'    traceCue="h", traceOutcome="hand")
#' plot(lexample.rw)
#' mtext("h - hand", 3, 1)
#' 
#' lexample.rw <- RescorlaWagner(lexample, nruns=25, 
#'    traceCue="s", traceOutcome="plural")
#' plot(lexample.rw)
#' mtext("s - plural", 3, 1)
#' 
#' lexample.rw <- RescorlaWagner(lexample, nruns=25, 
#'    traceCue="a", traceOutcome="as")
#' plot(lexample.rw)
#' mtext("a - as", 3, 1)
#' 
#' lexample.rw <- RescorlaWagner(lexample, nruns=25, 
#'    traceCue="s", traceOutcome="as")
#' plot(lexample.rw)
#' mtext("s - as", 3, 1)
#' par(mfrow=c(1,1))
#' }
#' 
plot.RescorlaWagner = function(x, asymptote=TRUE, xlab="t", ylab="weight",
    ylimit=NA, ...)
{
  rwWeights = x 
  x = 1:length(rwWeights$weightvector)
  y = rwWeights$weightvector
  eqw = rwWeights$equilibriumWeight
  if (is.na(ylimit[1])) {
    if (asymptote) {
      ylimit = range(y, eqw)
    } else {
      ylimit = range(y)
    }
  } 
  plot.default(x, y, type="l", ylim=ylimit, xlab=xlab, ylab=ylab, ...)
  if (asymptote) abline(h=eqw, lty=2, ...)
}
