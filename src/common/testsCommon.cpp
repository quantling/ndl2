#include "testsCommon.h"
#include <stdlib.h> // for system()
#include <sys/stat.h> // for mkdir()
#include <sys/types.h> // for mkdir()
#include <iostream>
#include <fstream>
#include <limits> // numeric limits
#include "view_events.h"

using namespace std;

void createTestDir(string pathDir) {
  if (system(("rm -rf " + pathDir).c_str()) == -1)
    WARN("Failed to delete testing folder '" + pathDir + "'");
  if (mkdir(pathDir.c_str(), 0755) < 0)
    FAIL("Failed to create testing folder '" + pathDir + "'");
}

streambuf* startRedirectCoutTo(ostream& newOutputStream) {
  streambuf *coutBuffer = cout.rdbuf();
  cout.rdbuf(newOutputStream.rdbuf());
  return coutBuffer;
}

void endRedirectCout(streambuf* coutBuffer) {
  cout.rdbuf(coutBuffer);
}

void commandToNewFile(string command, string inputFilePath,
    string outputFilePath) {
  if (system((command + " < " + inputFilePath + " > " + outputFilePath).c_str())
      == -1) {
    FAIL("System command '" + command + "' failed with input file '" +
        inputFilePath + "' and output file '" + outputFilePath);
  }
}

string asTextFile(string inputDataPrefix, bool addHeader) {
  string textOutputPath = testOutDir + inputDataPrefix + "-text";
  ofstream viewEventsStream(textOutputPath);
  streambuf* coutBuffer = startRedirectCoutTo(viewEventsStream);

  if (addHeader) {
    // Includes the header manually, as viewEvents doesn't output one.
    viewEventsStream << "Cues\tOutcomes\tFrequency" << endl;
  }

  viewEvents(0, numeric_limits<long long>::max(), testOutDir, inputDataPrefix,
      true, false, false, "_");
  viewEventsStream.close();
  endRedirectCout(coutBuffer);
  return textOutputPath;
} 

// Compares two streams line by line and tests for equalness
void requireStreamLinesEqual(istream& expectedStream, istream& actualStream) {
  size_t lineNr = 1;
  string lineActual = "";
  string lineExpected = "";
  while(getline(actualStream, lineActual)) {
    INFO("At line number " << lineNr);

    if(getline(expectedStream, lineExpected)) {
      REQUIRE(lineActual == lineExpected);
    } else {
      INFO("The file with the expected content contains less lines ("
          << (lineNr-1) << ") than the file with the actual content.\n"
          << "Next actual line: '" << lineActual << "'");
      // Slight hack, as -i option doesn't seem to work for FAIL
      lineExpected = "";
      REQUIRE(lineActual == lineExpected);
    }

    lineActual = lineExpected = "";
    ++lineNr;
  }
  if(getline(expectedStream, lineExpected)) {
    INFO("At line number " << lineNr);
    INFO("The file with the actual content contains less lines (" << (lineNr-1)
        << ") than the file with the expected content.\n"
        << "Next expected line: '" << lineExpected << "'");
    // Slight hack, as -i option doesn't seem to work for FAIL
    lineActual = "";
    REQUIRE(lineActual == lineExpected);
  }
}

// Compares two files line by line and tests for equalness
void requireFilesEqualOrdered(string pathFileExpected, string pathFileActual) {
  ifstream expectedStream(pathFileExpected);
  if (!expectedStream.good())
    FAIL("Couldn't open file '" + pathFileExpected + "' with expected content");
  ifstream actualStream(pathFileActual);
  if (!actualStream.good())
    FAIL("Couldn't open file '" + pathFileActual + "' with actual content");

  requireStreamLinesEqual(expectedStream, actualStream);
}

// Compares whether two files contain the same lines without regard to the order
void requireFilesEqualUnordered(string pathFileExpected, string pathFileActual){
  vector<string> linesExpected = getLinesFrom(pathFileExpected);
  vector<string> linesActual = getLinesFrom(pathFileActual);

  if (linesExpected.size() != linesActual.size()) {
    WARN("Expected content to have " << linesExpected.size() <<
        " lines, but it actually has " << linesActual.size() << " lines.");
  }

  sort(linesExpected.begin(), linesExpected.end());
  sort(linesActual.begin(), linesActual.end());

  stringstream streamExpected, streamActual;
  copy(linesExpected.begin(), linesExpected.end(),
      ostream_iterator<string>(streamExpected, "\n"));
  copy(linesActual.begin(), linesActual.end(),
      ostream_iterator<string>(streamActual, "\n"));

  requireStreamLinesEqual(streamExpected, streamActual);
}

vector<string> getLinesFrom(string filePath) {
  vector<string> lines;
  ifstream input(filePath);
  
  string newLine = "";
  while(getline(input, newLine)) {
    lines.push_back(newLine);
  }

  return lines;
}

