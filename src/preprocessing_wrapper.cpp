#include "preprocessing/ndlpreproc.h"
#include "common/helper.h"
#include "common/serialization.h"
#include <Rcpp.h>


// [[Rcpp::export(".ndlpreprocess")]]
bool ndlpreprocess(const std::string& filename, const int numThreads,
  const std::string& outputDirWithNamePrefix,
  const std::string corpusType = "N", const bool NormCase = 1,
  const std::string& cueFile = "", const std::string& outcomeFile = "",
  const size_t WindowSize = 1, const bool useLetterNGrams = true,
  const size_t maxN = 3, const bool inclusive = false,
  const size_t cueThreshold = 5000, const size_t outcomeThreshold = 60000,
  const std::string& fileInfoComment = "", const bool onlySettingsFile = false,
  const bool verbose = false, const bool debug = false)
{
  bool result = false;

  Verbosity verbosity = Warn;
  if (debug)
    verbosity = Debug;
  else if (verbose)
    verbosity = Info;

  std::string settingsFileSuffix = onlySettingsFile ? ".comparison" : "";

  int returnValue = preprocess(filename, cueFile, outcomeFile,
    outputDirWithNamePrefix, numThreads, corpusType, maxN, WindowSize,
    NormCase, inclusive, useLetterNGrams, cueThreshold, outcomeThreshold,
    true, fileInfoComment, settingsFileSuffix, verbosity);
  
  if (returnValue == 0)
    result = true;
  
  return result;
}


// [[Rcpp::export(".getNumberOfEvents")]]
size_t getNumberOfEvents(const std::string& eventsFolderName) {
  size_t num_events = 0;
  std::vector<std::string> paths = getEventFilePaths(eventsFolderName);
  for (const auto &current_path:paths) {
    num_events += getNumEvents(current_path);
  }
  return(num_events);
}
