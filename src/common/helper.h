#ifndef helper_H
#define helper_H

#include <string>
#include <vector>
#include <map>
#include <unicode/unistr.h>

using U_ICU_NAMESPACE::UnicodeString;

typedef std::map<UnicodeString, size_t> Dict;
typedef std::map<size_t, std::string> RevDictString;

bool pathExists(std::string path);
bool isDir(std::string path);
bool isFile(std::string path);
std::string getFullPath(std::string path);

// Returns the file paths for all event files in the specified directory.
// (Refactored from IterativeRescorlaWagner's count_event.cpp)
std::vector<std::string> getEventFilePaths(std::string dir_path);

// Sort criterion for event file names.
// (Refactored from IterativeRescorlaWagner's file_management.cpp)
bool compareEventFileNames(const std::string& first_path, 
    const std::string& second_path);

// Lookup strings for ids in dictionary ('idToString') and join the resulting 
// strings together by appending them with 'glue' in between.
std::string lookupAndImplode(const std::vector<size_t> &ids,
    RevDictString &idToString, const std::string glue);

// Read a Dict object (key: UnicodeString, value: size_t) from a file.
Dict readDict(const std::string filename);

// Read a Dict object from a file, but save it in reverse order and
// the cue or outcome name gets stored as string instead of UnicodeString.
RevDictString readRevDictString(const std::string filename, bool verbose=true);


// Split a string on a character delimiter
std::vector<std::string> &split(const std::string &s, char delim, 
  std::vector<std::string> &elems);

// Wrapper for split function
std::vector<std::string> split(const std::string &s, char delim);

#endif // helper_H
