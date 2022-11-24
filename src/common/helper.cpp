#include "helper.h"
#include "Exception.h"
#include <cstdio>
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip> // for setw
#include <sys/stat.h>
#include <dirent.h>
#include <limits.h> // for PATH_MAX
#include <stdlib.h> // for realpath
#include <errno.h>

using namespace std;

using U_ICU_NAMESPACE::UnicodeString;

const string NAME_PREFIX ("events_");
const string KEY_SEPERATOR ("_");
const string NAME_SUFFIX (".dat");

void checkAndRemoveStringPart(stringstream& stream, const string& wantedPart,
    const string& wholePath, bool useRemainingStream = false) {
  string extracted;
  if (!useRemainingStream) {
    stream >> setw(wantedPart.length());
  }
  stream >> extracted;

  if (extracted != wantedPart) {
    throw Exception("Event file path '" + wholePath + "' is invalid. " +
	"File name does not have the required format '" +
	NAME_PREFIX + "X" + KEY_SEPERATOR + "Y" + NAME_SUFFIX + "'."//);
        + " wantedPart == " + wantedPart + ", extracted = " + extracted);
  }
}

unsigned long getNumericPart(stringstream& stream, const string& wholePath) {
  long num;
  stream >> num;
  if (num < 0)
    throw Exception("Negative numbers are not allowed for event file names: '" +
	wholePath + "'");
  return num;
}


// Returns the file paths for all event files in the specified directory.
vector<string> getEventFilePaths(string pathDir)
{
  // Ensure trailing slash
  if (pathDir.substr(pathDir.size()-1,1) != "/") {
    pathDir += "/";
  }

  if (!pathExists(pathDir)) {
    throw Exception("The path '" + pathDir + "' does not exist.");
  } else if (!isDir(pathDir)) {
    throw Exception("The path '" + pathDir + "' is not a directory.");
  }

  DIR *pDirFD = opendir(pathDir.c_str());
  if (pDirFD == NULL) {
    throw Exception("Failed to open folder '" + pathDir + "'.");
  }

  vector<string> paths;
  while (struct dirent *pFD = readdir(pDirFD)) {
    string path = string(pFD->d_name);
    if (path.size() < 5 || path.substr(path.size()-4,4) != ".dat") {
      continue;
    }

    try {
      stringstream stream(path);
      checkAndRemoveStringPart(stream, NAME_PREFIX, path);
      getNumericPart(stream, path);
      checkAndRemoveStringPart(stream, KEY_SEPERATOR, path);
      getNumericPart(stream, path);
      checkAndRemoveStringPart(stream, NAME_SUFFIX, path, true);
    
      paths.push_back(pathDir + path);
    } catch (Exception &ex) {
      cout << ex.what() << "\n";
    }
  }

  return paths;
}

bool pathExists(string path) {
  struct stat statInfo;
  return stat(path.c_str(), &statInfo) == 0;
}

bool isDir(string path) {
  struct stat statInfo;
  if (stat(path.c_str(), &statInfo) != 0) {
    return false;
  }
  return S_ISDIR(statInfo.st_mode);
}

bool isFile(string path) {
  struct stat statInfo;
  if (stat(path.c_str(), &statInfo) != 0) {
    return false;
  }
  return S_ISREG(statInfo.st_mode);
}

// Sort criterion for event file names.
bool compareEventFileNames(const string& firstPath, const string& secondPath)
{
  stringstream stream1(firstPath.substr(firstPath.find_last_of("/\\") + 1));
  stringstream stream2(secondPath.substr(secondPath.find_last_of("/\\") + 1));

  checkAndRemoveStringPart(stream1, NAME_PREFIX, firstPath);
  checkAndRemoveStringPart(stream2, NAME_PREFIX, secondPath);

  unsigned long primaryKey1 = getNumericPart(stream1, firstPath);
  unsigned long primaryKey2 = getNumericPart(stream2, secondPath);

  checkAndRemoveStringPart(stream1, KEY_SEPERATOR, firstPath);
  checkAndRemoveStringPart(stream2, KEY_SEPERATOR, secondPath);

  unsigned long secondaryKey1 = getNumericPart(stream1, firstPath);
  unsigned long secondaryKey2 = getNumericPart(stream2, secondPath);

  checkAndRemoveStringPart(stream1, NAME_SUFFIX, firstPath, true);
  checkAndRemoveStringPart(stream2, NAME_SUFFIX, secondPath, true);

  if (primaryKey1 != primaryKey2) {
    return primaryKey1 < primaryKey2;
  } else {
    return secondaryKey1 < secondaryKey2;
  }
}

string getFullPath(string path) {
  char buffer[PATH_MAX];
  char *res = realpath(path.c_str(), buffer);
  if (res) {
    string fullPath(buffer);
    if (isDir(path) && path.substr(path.size()-1,1) != "/")
      fullPath += "/";
    return fullPath;
  } else {
    if (errno == EACCES) {
      throw Exception("Insufficient permissions for accessing " + path);
    } else if (errno == ENOENT) {
      throw Exception("The path " + path + "does not exist.");
    } else if (errno == EIO) {
      throw Exception("An I/O error occurred while accessing " + path);
    } else if (errno == ELOOP) {
      throw Exception("Too many symbolic links encountered while accessing " +
          path + ". Are there any loops?");
    } else if (errno == ENAMETOOLONG) {
      throw Exception("Absolute path name for " + path + " is too long: " +
          "either a path component is longer than " + to_string(NAME_MAX) +
          " or the entire path name exceeds " + to_string(PATH_MAX) +
          " characters.");
    } else if (errno == ENOTDIR) {
      throw Exception("A path component of " + path + " is not a directory.");
    }
    throw Exception("An unknown error occurred while trying to access " + path);
  }
}

// Lookup strings for ids in dictionary ('idToString') and join the resulting 
// strings together by appending them with 'glue' in between.
string lookupAndImplode(const vector<size_t> &ids, RevDictString &idToString, 
    const string glue)
{
  ostringstream result;
  for(vector<size_t>::const_iterator it = ids.begin(); it != ids.end();) {
    int id = *it;
    result << idToString[id];

    ++it;
    if (it != ids.end())
      result << glue;
  }
  return result.str();  
}

// Read a Dict object from a file.
Dict readDict(const string filename)
{
  cout << "Trying to read " << filename << endl;
  ifstream in(filename.c_str());
  if (in.good()) 
  {
    Dict D;
    char buffer[256];
    while (in.getline(buffer, 256)) {
      istringstream iss(buffer);
      string key;
      iss >> key;
      size_t value;
      iss >> value;
      D[UnicodeString::fromUTF8(key)] = value; 
    }
    return D;
  }
  else {
      throw Exception("Could not read the input file '" + filename + "'. Exiting");
  }
}

// Read a Dict object from a file, but save it in reverse order and
// the cue or outcome name gets stored as string instead of UnicodeString.
RevDictString readRevDictString(const string filename, bool verbose)
{
  !verbose || cout << "Trying to read " << filename << endl;
  ifstream in(filename.c_str());
  if (in.good()) 
  {
    RevDictString result;
    char buffer[256];
    while (in.getline(buffer, 256)) {
      istringstream iss(buffer);
      string name;
      size_t id;

      iss >> name;
      iss >> id;
      result[id] = name;
    }
    return result;
  }
  else {
      throw Exception("Could not read the input file '" + filename + "'. Exiting");
  }
}


// Split a string on a character delimiter
vector<string> &split(const string &s, char delim, vector<string> &elems) {
    stringstream ss(s);
    string item;
    while(getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}

// Wrapper for split function
vector<string> split(const string &s, char delim) {
    vector<string> elems;
    return split(s, delim, elems);
}
