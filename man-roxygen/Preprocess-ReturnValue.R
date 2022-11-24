#' @return Returns by default a \code{\link[ndl2]{ndlPreprocessingResult}}
#'   object. If, however, \code{returnPath} is set to \code{TRUE}, a string
#'   is returned consisting of the path to the directory in which the newly
#'   created files are located joined with the common beginning of the file
#'   names (hereafter referred to as \code{outputDirWithNamePrefix}).
#'   The path of the output directory is \code{outputDir}, if given, and
#'   the same folder as \code{sourceFile} otherwise. The common beginning
#'   of the file and folder names is the file name of the source file.
#'   
#'   The following files and folders get created:
#'   \describe{
#'     \item{\code{paste0(outputDirWithNamePrefix, ".events/")}}{
#'       Folder containing the binary event files.}
#'     \item{\code{paste0(outputDirWithNamePrefix, ".cues")}}{
#'       Text file listing all cues in the event files and their ids.}
#'     \item{\code{paste0(outputDirWithNamePrefix, ".outcomes")}}{
#'       Text file listing all outcomes in the event files and their ids.}
#'     \item{\code{paste0(outputDirWithNamePrefix, ".error.word.txt")}}{
#'       Text file listing all words in the input file that were not valid
#'       outcomes.}
#'   }
