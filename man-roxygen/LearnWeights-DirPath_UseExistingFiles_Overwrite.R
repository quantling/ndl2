#' @param dirPath The path to the folder where the preprocessed data should get
#'   stored (intermediary binary and text files).
#'   If the folder does not exist yet, a new folder is created.
#'   By default the sourceFile folder is used.
#' @param useExistingFiles Whether temporary preprocessing files should be
#'   used if they already exist with the same preprocessing settings.
#'   Otherwise existing temporary files are either overwritten (if
#'   \code{overwrite} is set to \code{TRUE}) or an error is thrown.
#'   Especially when using big source files setting \code{useExistingFiles} to
#'   \code{TRUE} can save a lot of time (as the preprocessing step might then
#'   bit be needed), but that the input file might have changed. For this to
#'   happen though, the change in the input file needs to be at exactly the
#'   same second as the previous version of the file used for the previous
#'   preprocessing.
#' @param overwrite Whether temporary preprocessing files should be overwritten
#'   if they already exist in the same \code{dirPath} and with the same
#'   \code{sourceName} and either the new preprocessing settings differs from
#'   the existing files or \code{useExistingFiles} is set to
#'   \code{TRUE}.

