#include "../common/testsCommon.h"
#include "ndlpreproc.h"
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

const int IRRELEVANT = 1;
const int NUM_THREADS = 3;

TEST_CASE("Unfiltered tabular preprocessing doesn't change anything",
    "[tabular][files]") {
  const string inputFile = testDataDir + "toy-tabular-with-frequencies.txt";
  const string outputPrefix = "tabular-unfiltered";

  preprocess(
      inputFile, "", "", testOutDir + outputPrefix, NUM_THREADS, "T",
      IRRELEVANT, IRRELEVANT, false, IRRELEVANT, IRRELEVANT,
      std::numeric_limits<size_t>::max(), std::numeric_limits<size_t>::max(),
      false, "", "", Quiet
  );

  const string outputTextFilePath = asTextFile(outputPrefix, true);
  requireFilesEqualOrdered(inputFile, outputTextFilePath);
}
