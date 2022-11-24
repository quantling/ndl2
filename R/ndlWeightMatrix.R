readWeightMatrix <- function(filePath, binary = TRUE) {
  checkMandatoryFileExists <- function(path, name) {
    if (!file.exists(path))
      stop(paste0(name, " file (", path, ") does not exist!"))
  }
  checkMandatoryFileExists(filePath, "Weights")
  checkMandatoryFileExists(paste0(filePath, ".cues"), "Cue index")
  checkMandatoryFileExists(paste0(filePath, ".outcomes"), "Outcome index")
    
  cues <- read.delim(paste0(filePath, ".cues"), header=F, stringsAsFactors=F, sep=" ")[,1]
  outcomes <- read.delim(paste0(filePath, ".outcomes"), header=F, stringsAsFactors=F, sep=" ")[,1]
  
  if ((as.double(length(cues)) * as.double(length(outcomes))) >= 2^31) {
    stop(paste0("Cannot read the requested weight matrix, as it is bigger than the ",
                "maximum allowed size for matrices in R (2^31 elements)"))
  }
  
  if (binary) {
    res <- matrix(readBin(filePath, numeric(), length(cues)*length(outcomes)),
                  length(cues), length(outcomes), byrow=T,
                  dimnames=list(cues, outcomes))
  } else {
    res <- matrix(scan(filePath, numeric(), length(cues)*length(outcomes), quiet=T),
                  length(cues), length(outcomes), byrow=T,
                  dimnames=list(cues, outcomes))
  }
  
  if (file.exists(paste0(filePath, ".info")))
    attr(res, "info") <- structure(readList(paste0(filePath, ".info")), class="ndlWeightMatrixInfo")
  
  return(res)
}