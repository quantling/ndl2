#' Calculate a range of goodness of fit measures for an object fitted with some
#' multivariate statistical method that yields probability estimates for
#' outcomes.
#' 
#' \code{modelStatistics} calculates a range of goodness of fit measures.
#' 
#' 
#' @param observed observed values of the response variable
#' @param predicted predicted values of the response variable; typically the
#' outcome estimated to have the highest probability
#' @param frequency frequencies of observed and predicted values; if \code{NA},
#' frequencies equal to 1 for all observed and predicted values
#' @param p.values matrix of probabilities for all values of the response
#' variable (i.e outcomes)
#' @param n.data sum frequency of data points in model
#' @param n.predictors number of predictor levels in model
#' @param outcomes a vector with the possible values of the response variable
#' @param p.normalize if \code{TRUE}, probabilities are normalized so that
#' \code{sum(P)} of all outcomes for each datapoint is equal to 1
#' @param cross.tabulation if \code{TRUE}, statistics on the crosstabulation of
#' observed and predicted response values are calculated with
#' \code{crosstableStatistics}
#' @param p.zero.correction a function to adjust slightly
#' response/outcome-specific probability estimates which are exactly P=0;
#' necessary for the proper calculation of pseudo-R-squared statistics; by
#' default calculated on the basis of the dimensions of the matrix of
#' probabilities \code{p.values}.
#' @return A list with the following components: \describe{
#' \item{\code{loglikelihood.null}}{Loglikelihood for null model}
#' \item{\code{loglikelihood.model}}{Loglikelihood for fitted model}
#' \item{\code{deviance.null}}{Null deviance}
#' \item{\code{deviance.model}}{Model deviance}
#' \item{\code{R2.likelihood}}{(McFadden's) R-squared}
#' \item{\code{R2.nagelkerke}}{Nagelkerke's R-squared}
#' \item{\code{AIC.model}}{Akaike's Information Criterion}
#' \item{\code{BIC.model}}{Bayesian Information Criterion}
#' \item{\code{C}}{index of concordance C (for binary response variables
#' only)} \item{\code{crosstable}}{Crosstabulation of observed and predicted
#' outcomes, if \code{cross.tabulation=TRUE}}
#' \item{\code{crosstableStatistics(crosstable)}}{Various statistics
#' calculated on \code{crosstable} with \code{crosstableStatistics}, if
#' \code{cross.tabulation=TRUE}} }
#' @author Antti Arppe and Harald Baayen
#' @seealso See also \code{\link{ndlClassify}}, \code{\link{ndlStatistics}},
#' \code{\link{crosstableStatistics}}.
#' @references Arppe, A. 2008. Univariate, bivariate and multivariate methods
#' in corpus-based lexicography -- a study of synonymy. Publications of the
#' Department of General Linguistics, University of Helsinki, No. 44. URN:
#' http://urn.fi/URN:ISBN:978-952-10-5175-3.
#' 
#' Arppe, A., and Baayen, R. H. (in prep.) Statistical modeling and the
#' principles of human learning.
#' 
#' Hosmer, David W., Jr., and Stanley Lemeshow 2000. Applied Regression
#' Analysis (2nd edition). New York: Wiley.
#' @keywords discriminative learning
#' @examples
#' 
#' data(think)
#' think.ndl <- ndlClassify(Lexeme ~ Agent + Patient, data=think)
#' probs <- acts2probs(think.ndl$activationMatrix)$p
#' preds <- acts2probs(think.ndl$activationMatrix)$predicted
#' n.data <- nrow(think)
#' n.predictors <- nrow(think.ndl$weightMatrix) *
#'    ncol(think.ndl$weightMatrix)
#' modelStatistics(observed=think$Lexeme, predicted=preds, p.values=probs,
#'    n.data=n.data, n.predictors=n.predictors)
#' 
modelStatistics <- function(observed, predicted, frequency=NA, p.values, n.data, n.predictors, outcomes=levels(as.factor(observed)), 
    p.normalize=TRUE, cross.tabulation=TRUE, p.zero.correction=1/(NROW(p.values)*NCOL(p.values))^2)
{ if(p.zero.correction==0) warning("Loglikelihood and related statistics may be inestimable, if P=0 for any observed outcome.");
  N <- length(observed);
#  if(any(!(unique(observed) %in% colnames(p.values))))
#    p.values <- cbind(p.values, matrix(0,NROW(p.values),1,dimnames=list(NULL,setdiff(unique(observed),colnames(p.values)))))
  if(p.normalize)
    p.values <- p.values/apply(p.values,1,sum)
  p.outcomes <- colnames(p.values);
  d <- sapply(1:N, function(i)
    { p <- p.values[i,which(observed[i]==p.outcomes)]
      if(p==0) p = p.zero.correction
      if(all(is.na(frequency)))
        return(log(p))
      else
        return(as.vector(frequency)[i]*log(p))
    });
  loglikelihood.model <- sum(d);
  deviance.model <- -2 * loglikelihood.model;
  if(all(is.na(frequency)))
    n.outcomes <- sapply(outcomes, function(o) length(which(observed==o)))
  else
    n.outcomes <- sapply(outcomes, function(o) sum(as.vector(frequency)[which(observed==o)]))
  loglikelihood.null <- sum(sapply(outcomes, function(o) n.outcomes[o]*log(n.outcomes[o]/n.data)));
  deviance.null <- -2 * loglikelihood.null;
  R2.likelihood <- 1 - deviance.model/deviance.null;
#  R2.nagelkerke <- (1-(exp(loglikelihood.null)/exp(loglikelihood.model))^(2/n.data))/(1-(exp(loglikelihood.null)^(2/n.data)));
  R2.nagelkerke <- (1-exp(-2*(loglikelihood.model-loglikelihood.null)/n.data))/(1-exp(2*loglikelihood.null/n.data))
  AIC.model <- 2 * n.predictors - 2 * loglikelihood.model
  BIC.model <- n.predictors * log(n.data) - 2 * loglikelihood.model

  # calculate index of concordance C if response variable is binary
  if (length(outcomes) == 2)
     { invisible(require(Hmisc, quietly=TRUE))
       if(all(is.na(frequency)))
         { predval = sort(unique(predicted))[1]
           binvec = as.numeric(as.character(observed)==predval)
           C.statistics = somers2(p.values[,predval], binvec)
         }
       else
         { predval = sort(unique(predicted))[1]
           binvec = rep(as.numeric(as.character(observed)==predval), frequency)
           C.statistics = somers2(rep(p.values[,predval],frequency), binvec)
         }

       statistics <- list(loglikelihood.null = loglikelihood.null, loglikelihood.model = loglikelihood.model, deviance.null = deviance.null, deviance.model = deviance.model, R2.likelihood = R2.likelihood, R2.nagelkerke = R2.nagelkerke, AIC.model = AIC.model, BIC.model = BIC.model, C=C.statistics[["C"]]);

     }
  else 

     statistics <- list(loglikelihood.null = loglikelihood.null, loglikelihood.model = loglikelihood.model, deviance.null = deviance.null, deviance.model = deviance.model, R2.likelihood = R2.likelihood, R2.nagelkerke = R2.nagelkerke, AIC.model = AIC.model, BIC.model = BIC.model)

  # Statistics based on crosstabulated of observed vs. predicted outcomes

  if(cross.tabulation)
    if(all(is.na(frequency)))
      { crosstable <- table(factor(observed, levels=outcomes), factor(predicted, levels=outcomes));
        statistics <- c(statistics, list(crosstable = crosstable), crosstableStatistics(crosstable))
      }
    else
      { crosstable <- matrix(0, length(outcomes), length(outcomes), dimnames=list(outcomes,outcomes));
        for(i in 1:N)
           crosstable[observed[i],predicted[i]] = crosstable[observed[i],predicted[i]] + as.vector(frequency)[i];
        statistics <- c(statistics, list(crosstable = crosstable), crosstableStatistics(crosstable))
      }

  return(statistics);

}
