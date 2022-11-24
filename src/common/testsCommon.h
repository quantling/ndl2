#include "catch.hpp"
#include <string>
#include <unicode/unistr.h>
#include <sstream>

// Constants
const std::string testOutDir = "testingOut/";
const std::string testDataDir = "testingData/";

// Search pattern for columns in view_events output.
// \1 contains the cues, \2 the outcomes and \3 the frequency
const std::string regexColumns = "^\\([^\t]*\\)\t\\([^\t]*\\)\t\\([^t]\\)*$";

// Helper functions
void createTestDir(std::string pathDir);
std::vector<std::string> getLinesFrom(std::string filePath);

// Runs the command with inputFilePath with the input being read from
// inputFilePath (using <) and the output is written to outputFilePath
void commandToNewFile(std::string command, std::string inputFilePath,
    std::string outputFilePath);

// Creates text file from binary preprocessed data with view_events and
// returns the path of the newly created file (inputDataPrefix + "-text").
std::string asTextFile(std::string inputDataPrefix, bool addHeader);
std::streambuf* startRedirectCoutTo(std::ostream &newOutputStream);
void endRedirectCout(std::streambuf* coutBuffer);

void requireStreamLinesEqual(std::istream& expectedStream,
    std::istream& actualStream);
void requireFilesEqualOrdered(std::string pathFileExpected,
    std::string pathFileActual);
void requireFilesEqualUnordered(std::string pathFileExpected,
    std::string pathFileActual);


// Needed for seeing the values of UnicodeStrings when tests fail
namespace Catch {
  using namespace std;

  template<> struct StringMaker<UnicodeString> {
    static string convert(UnicodeString const& value) {
      string utf8String;
      value.toUTF8String(utf8String);

      // Show unicode string as UTF-8 and the unicode values in brackets
      stringstream strOut;
      strOut << utf8String << " (" << hex << setfill('0');
      for (auto i = 0; i < value.length(); i++) {
        strOut << "U+" << setw(4) <<
          static_cast<int>(value.charAt(i)) << setw(1) << ' ';
      }
      long pos = strOut.tellp();
      strOut.seekp(pos-1); // remove final space
      strOut << ')';
      return(strOut.str());
    }
  };
}

