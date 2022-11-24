#include "../common/testsCommon.h"
#include "../common/helper.h"
#include "../common/Exception.h"
#include <string>
#include <vector>
#include <fstream>

using namespace std;

vector<string> invalidEventFileNames {"", "events_0_0", "events_0_0.da",
  "events_0_1.datx", "events_0_23.bin", "whatever", "run.sh", "xyy.dat",
  "0_0.dat", "events.dat", "events_0.dat", "events_0_.dat", "events__0.dat",
  "events_-1_0.dat", "events_0_-1.dat", "events_x_0.dat", "events_0_x.dat",
  "events.0_0.dat", "events_0.0.dat", "events___.dat", "events_0.0.dat.dat"};

// Create empty files and returns a vector containing of all the files that
// where successfully created .
vector<string> createFiles(vector<string> fileNames, string pathDir) {
  vector<string> pathsCreated {};
  for (auto file : fileNames) {
    string command = "touch " + pathDir + "/" + file;
    if (system(command.c_str()) == -1) {
      WARN("Failed to create file '" + file + "' for testing.");
    } else {
      pathsCreated.push_back(pathDir + "/" + file);
    }
  }
  return pathsCreated;
}

TEST_CASE("all event files are found",
    "[files],[path],[helper]") {
  const string tmpDir = testOutDir + "event_file_finding";
  createTestDir(tmpDir);

  SECTION("in empty folder no files are found") {
    vector<string> pathsFound = getEventFilePaths(tmpDir);
    REQUIRE(pathsFound == vector<string>());
  }

  SECTION("files with valid name are recognized") {
    vector<string> filesValid {"events_0_0.dat", "events_0_1.dat",
      "events_0_23.dat", "events_1_0.dat", "events_5_89.dat",
      "events_69_17.dat", "events_17_32.dat"};
    vector<string> pathsCreated = createFiles(filesValid, tmpDir);
    vector<string> pathsFound = getEventFilePaths(tmpDir);

    // Sort vectors for comparison
    sort(pathsFound.begin(), pathsFound.end());
    sort(pathsCreated.begin(), pathsCreated.end());
    INFO("pathsFound.size() = " << pathsFound.size() <<
	", pathsCreated.size() = " << pathsCreated.size());
    REQUIRE(pathsFound == pathsCreated);
  }

  SECTION("files with invalid name are ignored") {
    const string outputFileName = "event_file_finding-invalid_files_output";
    ofstream invalidFilesOutput(testOutDir + outputFileName);
    streambuf* coutBuffer = startRedirectCoutTo(invalidFilesOutput);

    createFiles(invalidEventFileNames, tmpDir);
    vector<string> pathsFound = getEventFilePaths(tmpDir);
    invalidFilesOutput.close();
    endRedirectCout(coutBuffer);

    REQUIRE(pathsFound == vector<string>());
    requireFilesEqualUnordered(testDataDir + outputFileName,
	testOutDir + outputFileName);
  }
}

void checkEventFileOrder(vector<string> fileNamesOrdered, const string& dir) {
    // To make sure file name are not already sorted we reverse them.
    vector<string> fileNamesReverse = fileNamesOrdered;
    reverse(fileNamesReverse.begin(), fileNamesReverse.end());

    // Sort file names only (without path) given reversed order
    vector<string> fileNamesToSort = fileNamesReverse;
    sort(fileNamesToSort.begin(), fileNamesToSort.end(), compareEventFileNames);
    REQUIRE(fileNamesToSort == fileNamesOrdered);

    // Create (empty) files on disk and check both file detection and sorting.
    // Create files and reverse the file paths to get the correct order again.
    vector<string> pathsCreated = createFiles(fileNamesReverse, dir);
    reverse(pathsCreated.begin(), pathsCreated.end());

    vector<string> pathsFound = getEventFilePaths(dir);
    sort(pathsFound.begin(), pathsFound.end(), compareEventFileNames);

    REQUIRE(pathsFound.size() == pathsCreated.size());
    REQUIRE(pathsFound == pathsCreated);
}

void checkEventFileException(string invalidName, const string& dir) {
  string validPath = dir + "/events_0_0.dat";
  INFO("invalidName = " << invalidName);
  REQUIRE_THROWS_AS(compareEventFileNames(validPath, dir + "/" + invalidName),
      Exception);
  REQUIRE_THROWS_AS(compareEventFileNames(dir + "/" + invalidName, validPath),
      Exception);
}


TEST_CASE("event files are sorted correctly",
    "[files],[helper],[sort]") {
  const string tmpDir = testOutDir + "test-event_file_sorting";
  createTestDir(tmpDir);

  SECTION("sorted according to second number") {
    checkEventFileOrder(vector<string>{"events_0_0.dat", "events_0_1.dat",
      "events_0_2.dat", "events_0_10.dat", "events_0_11.dat",
      "events_0_100.dat", "events_0_1000.dat", "events_0_1099.dat"}, tmpDir);
  }

  SECTION("sorted according to first number") {
    checkEventFileOrder(vector<string>{"events_0_0.dat", "events_1_0.dat",
      "events_2_0.dat", "events_10_0.dat", "events_11_0.dat",
      "events_100_0.dat", "events_1000_0.dat", "events_1099_0.dat"}, tmpDir);
  }

  SECTION("sorted according to both numbers") {
    checkEventFileOrder(vector<string>{"events_0_0.dat", "events_0_1.dat",
      "events_1_0.dat", "events_10_1.dat", "events_100_0.dat",
      "events_100_99.dat", "events_1000_10.dat"}, tmpDir);
  }

  SECTION("invalid event file names throw exception") {
    for (auto invalidName : invalidEventFileNames) {
      checkEventFileException(invalidName, tmpDir);
    }
  }
}

