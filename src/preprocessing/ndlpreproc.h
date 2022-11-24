#ifndef ndlpreproc_H
#define ndlpreproc_H

#include <string>
#include <unicode/unistr.h>

using U_ICU_NAMESPACE::UnicodeString;

enum Verbosity {
  Quiet,
  Warn,
  Info,
  Debug
};

int preprocess(const std::string& filename,
               const std::string& cueFile,
               const std::string& outcomeFile,
               const std::string& outputDirWithNamePrefix,
               const int numThreads,
               const std::string corpusType = "N",
               const size_t maxN = 3,
               const size_t WindowSize = 1,
               const bool NormCase = 1,
               const bool inclusive = false,
               const bool permute = true,
               const size_t cueThreshold = 5000,
               const size_t outcomeThreshold = 60000,
               const bool sortWithinEvent = true,
               const std::string& fileInfoComment = "",
               const std::string& settingsFileSuffix  = "",
               Verbosity verbosity = Info);

UnicodeString cleanup(std::string word, bool NormCase);

std::string findAndReplacePunct(std::string& source);

#endif // ndlpreproc_H
