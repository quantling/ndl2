#' Calculate statistics for a contingency table
#' 
#' \code{crosstableStatistics} takes a contingency table of observed vs.
#' predicted values for a binary or polytomous response variable as input, and
#' calculates a range of statistics about prediction accuracy.
#' 
#' 
#' @param ctable A contingency table cross-classifying observed and predicted
#' values.
#' @return A list with the following components: \describe{
#' \item{\code{accuracy}}{Overall prediction accuracy}
#' \item{\code{recall.predicted}}{Recall of prediction for each outcome value}
#' \item{\code{precision.predicted}}{Precision of prediction for each outcome
#' value} \item{\code{lambda.prediction}}{lambda for prediction accuracy
#' (improvement over baseline of always predicting mode)}
#' \item{\code{tau.classification}}{tau for classification accuracy
#' (improvement over baseline of homogeneous distribution of predicted
#' outcomes)} \item{\code{d.lambda.prediction}}{d(lambda): used for
#' calculating \code{P(lambda)}} \item{\code{d.tau.classification}}{d(tau):
#' used for calculating \code{P(tau)}}
#' \item{\code{p.lambda.prediction}}{P(lambda): probability of reaching
#' \code{lambda} by chance} \item{\code{p.tau.classification}}{P(tau):
#' probability of reaching \code{tau} by chance} }
#' @author Antti Arppe and Harald Baayen
#' @seealso See also \code{\link{modelStatistics}, \link{ndlStatistics},
#' \link{ndlClassify}}.
#' @references Arppe, A. 2008. Univariate, bivariate and multivariate methods
#' in corpus-based lexicography -- a study of synonymy. Publications of the
#' Department of General Linguistics, University of Helsinki, No. 44. URN:
#' http://urn.fi/URN:ISBN:978-952-10-5175-3.
#' 
#' Arppe, A. and Baayen, R. H. (in prep.). Statistical classification and
#' principles of human learning.
#' 
#' Menard, Scott (1995). Applied Logistic Regression Analysis. Sage University
#' Paper Series on Quantitative Applications in the Social Sciences 07-106.
#' Thousand Oaks: Sage Publications.
#' @keywords discriminative learning
#' @examples
#' 
#' ctable <- matrix(c(30, 10, 5, 60), 2, 2)
#' crosstableStatistics(ctable)
#' 
crosstableStatistics <- function(ctable)
{ N <- sum(ctable);
  # observed as row margins, predicted as column margins
  # according to Menard (1995: 24-32)
  sum.row <- apply(ctable,1,sum);
  sum.col <- apply(ctable,2,sum);
  correct.with.model <- sum(diag(ctable));
  errors.with.model <- N - correct.with.model;
  errors.without.model.prediction <- N - max(sum.row);
  errors.without.model.classification <- sum(sum.row*((N-sum.row)/N));
  lambda.p <- 1-(errors.with.model/errors.without.model.prediction);
  d.lambda.p <- (errors.without.model.prediction/N-errors.with.model/N)/sqrt((errors.without.model.prediction/N)*(1-errors.without.model.prediction/N)/N);
  p.lambda.p <- 1-pnorm(d.lambda.p);
  tau.p <-  1-(errors.with.model/errors.without.model.classification);
  d.tau.p <- (errors.without.model.classification/N-errors.with.model/N)/sqrt((errors.without.model.classification/N)*(1-errors.without.model.classification/N)/N);
  p.tau.p <- 1-pnorm(d.tau.p);
  accuracy <- sum(diag(ctable))/N;
  recall.predicted <- diag(ctable)/sum.row;
  precision.predicted <- diag(ctable)/sum.col;
  statistics <- list(accuracy = accuracy, recall.predicted=recall.predicted, precision.predicted=precision.predicted, lambda.prediction = lambda.p, tau.classification = tau.p, d.lambda.prediction = d.lambda.p, d.tau.classification = d.tau.p, p.lambda.prediction = p.lambda.p, p.tau.classification = p.tau.p);
  return(statistics);
} 
