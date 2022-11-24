#' Code a character string (written word form) as letter n-grams
#' 
#' \code{orthoCoding} codes a character string into unigrams, bigrams, ...,
#' n-grams, with as default bigrams as the substring size. If tokenization is
#' not at the letter/character level, a token separator can be provided.
#' 
#' 
#' @param strings A character vector of strings (usually words) to be recoded
#' as n-grams.
#' @param grams A vector of numbers, each one a size of ngram to be produced.
#' For example a vector like grams=c(1,3) will create the unigram and trigram
#' cues from the input.
#' @param tokenized If tokenzied is FALSE (the default), the input strings are
#' split into letters/characters. If it is set to TRUE, the strings will be
#' split up based on the value of sepToken.
#' @param sepToken A string that defines which character will be used to
#' separate tokens when tokenized is TRUE. Defaults to the "." character.
#' @return A vector of grams (joined by underscores), one for each word in the
#' input vector \cite{words}.
#' @author Cyrus Shaoul, Peter Hendrix and Harald Baayen
#' @seealso See also \code{\link{estimateWeights}}, \code{\link{learnWeights}}.
#' @references
#' 
#' Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix, P. and
#' Marelli, M., An amorphous model for morphological processing in visual
#' comprehension based on naive discriminative learning. Psychological Review,
#' 118, 438-482.
#' @keywords discriminative learning
#' @examples
#' 
#' 
#' #Default
#' orthoCoding(tokenize=FALSE)
#' #With tokenizing on a specific character
#' orthoCoding(tokenize=TRUE)
#' 
#' #Comparing different n-gram sizes
#' data(serbian) 
#' serbian$Cues=orthoCoding(serbian$WordForm, grams=2)
#' head(serbian$Cues)
#' serbian$Cues=orthoCoding(serbian$WordForm, grams=c(2,4))
#' head(serbian$Cues)
#' 
orthoCoding = function (strings=c("hel.lo","wor.ld"), grams = c(2), tokenized = F, sepToken = '.') {
    
    if (length(grams) < 1) { stop("This function requires a non-zero length vector of n-gram sizes for the argument 'grams'.")}

    if (! is.numeric(grams)) { stop("This function requires a vector of one or more numbers of n-gram sizes for the argument 'grams'.")}
    
    ngram.fnc = function(s, n) {
        if (n == 1) { # remove hash cues for unigrams
            s = sub(paste('#',sepToken,sep=''),'',s)
            s = sub(paste(sepToken,'#',sep=''),'',s)
        }
        tokens = unlist(strsplit(s, sepToken, fixed=T))
        len = length(tokens)
        ng = NULL
        for (i in 1:(len - n + 1)) {
            ng = c(ng, paste(tokens[i:(i+n-1)],collapse=''))
        }
        return(paste(ng, collapse = "_"))
    }

    if (!tokenized) sepToken = ''
    
    letters = sapply(strings, FUN = function(s) paste('#',sepToken, s,
sepToken,'#', sep = ""))

    for (i in 1:length(grams)) {
        cuesi = unlist(lapply(letters, FUN = ngram.fnc, grams[i]))
        if (exists("mycues") == 0) {
            mycues = cuesi
        } else {
            mycues = paste(mycues, cuesi, sep = "_")
        }
    }
    return(mycues)
}

## Old version... now depreciated.
## orthoCoding <-
## function (words = c("hello", "world"), maxn = 2, inclusive=FALSE) 
## {
##     ngram.fnc = function(s, n) {
##         len = nchar(s)
##         ng = NULL
##         for (i in 1:(len - n + 1)) {
##             ng = c(ng, substr(s, i, i + n - 1))
##         }
##         return(paste(ng, collapse = "_"))
##     }
##     letters = strsplit(words, "")
##     grams = unlist(lapply(letters, FUN = paste, collapse = "_"))
##     letters = sapply(words, FUN = function(s) paste("#", s, "#", sep = ""))
##     if (maxn == 1) {
##         return(grams)
##     }
##     else {
##       if (inclusive) {
##         for (i in 2:maxn) {
##           gramsi = unlist(lapply(letters, FUN = ngram.fnc, 
##             i))
##           grams = paste(grams, gramsi, sep = "_")
##         }
##       } else {
##         grams = unlist(lapply(letters, FUN = ngram.fnc, maxn))
##         return(grams)        
##       }
##     }
##     return(grams)
##   }
