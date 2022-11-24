# Internal Function


#' code a vector of cues as n-grams
#' 
#' \code{cueCoding} codes a vector of cues into unigrams, bigrams, \dots{},
#' n-grams, with unigrams as default.
#' 
#' 
#' @param cues A vector of cues (represented by strings) to be recoded as
#' unigrams, bigrams, \dots{}, ngrams.
#' @param maxn The longest n-gram to be encoded, by default \code{maxn=1}.
#' @param adjacent A logical indicating whether only adjacent bigrams should be
#' included when \code{maxn=2}.  If \code{adjacent=TRUE} and \code{maxn!=2},
#' \code{maxn} is forced to 2.
#' @return A vector of cue n-grams, one for each word in the input vector
#' \code{cues}. Each n-gram vector lists the constituent unigrams, bigrams,
#' etc., separated by underscores.
#' @author Antti Arppe and Harald Baayen
#' @seealso See also \code{\link{ndlClassify}, \link{ndlCuesOutcomes},
#' \link{ndlVarimp}, \link{ndlCrossvalidate}}.
#' @references Arppe, A. and Baayen, R. H. (in prep.). Statistical
#' classification and principles of human learning.
#' @keywords discriminative learning
#' @examples
#' 
#' # Cues from the \code{think} data: Person, Number, Register
#' cues <- c("First", "Plural", "hs95")
#' cueCoding(cues, maxn=1)
#' cueCoding(cues, maxn=2)
#' 
cueCoding = function(cues=c("hello", "world"), maxn=1, adjacent=FALSE)
{
  if(adjacent & maxn!=2)
    { cat("'maxn=2' since 'adjacent=TRUE'.\n")
      maxn=2
    }

  if(length(cues)<maxn) maxn=length(cues)

  ngram.fnc = function(v, n) {
    len = length(v)
    ng = NULL
    combinations <- combn(len,n)
    for (i in 1:ncol(combinations)) {
       ng = c(ng, paste(v[combinations[,i]], collapse=""))
    }
    return(paste(ng, collapse="_"))
  }
  
  grams <- paste(cues, collapse="_")

  if(maxn==1)
    return(grams)
  else
    if(!adjacent)
      for(i in 2:maxn)
        { vv <- ngram.fnc(cues,i)
          grams = paste(grams, vv, sep="_")
        }
    else
      if(maxn==2)
        grams <- paste(c(grams,
                       paste(apply(cbind(cues[1:length(cues)-1],
                                   cues[2:length(cues)]),
                                   1, function(x) paste(x,collapse="")),
                       collapse="_")),
                collapse="_")

  return(grams)
}


