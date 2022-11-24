#' @param outputDir The file path to the folder where the binary files and text
#'   files are saved. If the folder does not exist yet, a new folder is created.
#'   By default the \code{sourceFile} folder is used.
#' @param returnPath Whether the path and common prefix of the output files
#'   should be returned as a string instead of returning a
#'   \code{\link[ndl2]{ndlPreprocessingResult}} object.
#' @param comment (Optional) Comment to be included in the settings file.
#'   As this comment also gets compared when data sources are compared,
#'   this parameter can be used to make note of the specifics of the
#'   preprocessed data.
#' @param onlySettingsFile Mostly for internal usage. If set to \code{TRUE},
#'   then no actual preprocessing takes place. Instead, only the settings file
#'   is written with the suffix ".comparison". This can be useful for comparing
#'   the currently existing data source with the one that would be created.
