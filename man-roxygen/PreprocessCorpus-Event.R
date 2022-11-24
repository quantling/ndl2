#' @details
#' Each of the created events consists of one or more words as outcomes and
#' their letter n-grams as cues. The n-gram size can be set with 
#' \code{maxNGramSize} and \code{includeSmallerNGrams} is used to specify
#' whether n-grams smaller than \code{maxNGramSize} should be used as well.
#' 
#' @details
#' Only words that appear in the \code{outcomesFiles} are used as outcomes and
#' all other words are ignored. Furthermore, words that contain letters which
#' are not included in the \code{alphabetFile} are not used as outcomes either.

