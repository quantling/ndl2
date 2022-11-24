// The NDL preprocessor, copyright 2012,2013 Cyrus Shaoul
//
//
// This program expects a corpus that has already been tokenized by the user. This means
// that any segmentation (ex: "don't" becomes "do n't") has alread taken place and that
// the tokens all exist in the outcome list: "do" and "n't".
//
//

#include <cstdio>
#include <assert.h>
#include <string.h>
#include <math.h>
#include <algorithm>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <cerrno>
#include <map>
#include <set>
#include "ndlpreproc.h"
#include "../common/Exception.h"
#include "../common/serialization.h"
#include "../common/helper.h"
#include <sys/stat.h>
#include <time.h>
#include <omp.h>
#include <unicode/normalizer2.h>
#include <unicode/regex.h>
#include <unicode/unistr.h>

//#include "unicode/utypes.h"
//#include "unicode/stringpiece.h"
//#include "unicode/utf8.h"
//#include "unicode/uchar.h"

#ifdef USING_RCPP
  #include <Rcpp.h>
//  #include "ndlRout.h"
//  std::ostream& xout = ndl::Rcout;
  #include "ndlOutBuffer.h"
  ndlOutBuffer ndlOutBuf(Rcpp::Rcout);
  std::ostream xout(&ndlOutBuf);
#else
  std::ostream& xout = std::cerr;
#endif

using U_ICU_NAMESPACE::UnicodeString;

using namespace std;
using namespace icu;


// Dictionary data structure has keys of words or cues and values of unique WordID or CueID
typedef map<UnicodeString, size_t> Dict;
typedef map<size_t,UnicodeString> RevDict;
typedef map<size_t, size_t> FreqDict;
// List of letters in an alphabet
typedef vector<UChar32> LetterList;
// Lists of words
typedef vector<UnicodeString> WordList;
// set of words
typedef set<string> WordSet;
// Data type for Extended corpus types
typedef vector<WordList> ExtendedWordList;
// Data type for Set of Cues
typedef set<size_t> IDSet;


const double version = 0.07;
static Verbosity v = Debug; // Static to restrict visibility to this file.
auto normalizerErrorCode = U_ZERO_ERROR;
auto nfcNormalizer = icu::Normalizer2::getNFCInstance(normalizerErrorCode);

//==============================================================
// Various Functions
//==============================================================

// Sucks a file off the disk and into a string.
string getFile(string filename)
{
  ifstream in(filename.c_str(), std::ios::in | std::ios::binary);
  if (in)
  {
    std::string contents;
    in.seekg(0, std::ios::end);
    contents.resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(&contents[0], contents.size());
    in.close();
    return(contents);
  } else {
      throw Exception("Error Reading Input File " + filename + " ... Exiting\n");
  //  throw(errno);
  }
}

// Generate a random string of LEN characters
string genRandom(const size_t len) {
  string out (len,' ');
  static const char alphanum[] =
    "0123456789"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz";
  for (size_t i = 0; i < len; ++i) {
    out[i] = alphanum[rand() % (sizeof(alphanum) - 1)];
  }
  return(out);
}

int system(const string& command) { return system(command.c_str()); }

int rmdir(const string& filename)
{
    // Check filename exists and is actually a directory
    struct stat sb;
    if (stat(filename.c_str(), &sb) != 0 || !S_ISDIR(sb.st_mode)) {
        xout << filename << " does not exist or is not a directory" << endl;
        return -1;
    }

    string safefile = filename;
    string::size_type p = 0;
    while (p < safefile.size()) {
        // Don't escape a few safe characters which are common in filenames
        if (!isalnum(safefile[p]) && strchr("/._-", safefile[p]) == NULL) {
            safefile.insert(p, "\\");
            ++p;
        }
        ++p;
    }
    system("rm -rf " + safefile);
    return 0;
}


// Check to see if a directory exists.
bool dirExists(const string& fname)
{
  struct stat sbuf;
  // exists && is a dir
  return stat(fname.c_str(), &sbuf) == 0 && S_ISDIR(sbuf.st_mode);
}

// Create a directory
int createEventsDir (const string& dirName) {
  if (dirExists(dirName)) {
    xout << "Output directory " << dirName << " already exists.\nWould you like to remove this directory, deleting all your previous event data,\nand try again? WARNING: This process is irreversible. \n (y)es or (n)o: "  << flush;
    string answer;
    getline (cin, answer);
    if (answer == "y") {
      if (rmdir(dirName) == 0) {
	v < Info || xout << "Successfully removed directory " << dirName << endl;
      } else {
	ostringstream message;
	message << "Could not remove old output directory " << dirName << " .. Aborting! " << endl;
	throw Exception(message.str());
      }
    } else {
      ostringstream message;
      message << "Cannot continue. Aborting processing until conflict is resolved. " << endl;
      throw Exception(message.str());
    }
  }
  if (mkdir(dirName.c_str(), 0755) >= 0) {
    v < Info || xout << "Created directory " << dirName << endl;
    return 0;
  }
  else {
    ostringstream message;
    message << "Could not create output directory " << dirName << " .. Aborting! " << endl;
    throw Exception(message.str());
  }
}


// Get stl string from UnicodeString
string getString(UnicodeString uniStr) {
  string result;
  uniStr.toUTF8String(result);
  return result;
}

// Convert to Unicode UTF16 and normalize
inline UnicodeString convertAndNormalize(const string& word, const bool NormCase) {
  auto result = UnicodeString::fromUTF8(word);
  UErrorCode errorCode = U_ZERO_ERROR;
  result = nfcNormalizer->normalize(result, errorCode);

  if (U_FAILURE(errorCode)) {
    v < Warn || xout << "Failed to normalize '" << word <<
      ", icu error code = " << errorCode << "'\n";
  }

  if (NormCase) {
    result.toLower();
  }
  return result;
}


// Clean up a string, normalizing it and changing case if necessary.
UnicodeString cleanup(string word, bool NormCase) {
  UnicodeString result = convertAndNormalize(word, NormCase);

  //  remove extra whitespace from Unicode string
  result.trim();
  if (word == "---END.OF.DOCUMENT---" || word == "---end.of.document---") {
    return(UnicodeString());
  }
  return(result);
}


// Save a Dict object to a file.
void writeDict(Dict& D, const string filename)
{
    ofstream out(filename.c_str());
    if (out.good())
    {
      string utf8out="";
      UnicodeString outcome;
      for (Dict::iterator i = D.begin(); i != D.end(); i++) {
	  outcome = i->first;
	  outcome.toUTF8String(utf8out);
	  out << utf8out << "\t" << i->second << endl;
	  utf8out.clear();
        }
        out.close();
    }
    else {
        throw Exception("Could not write to the output file. Exiting");
    }
}

void writeFreqDict(const FreqDict &D, const RevDict &E, const string filename)
{
    ofstream out(filename.c_str());
    if (out.good())
    {
      UnicodeString outcome;
      for (FreqDict::const_iterator i = D.begin(); i != D.end(); i++) {
	outcome = E.at(i->first);
	out << getString(outcome) << "\t" << i->first << "\t" << i->second << endl;
      }
        out.close();
    }
    else {
        throw Exception("Could not write to the output file. Exiting");
    }
}

// Cue or outcome id mapping with old id as key and new id as value.
// The new ids are consecutive numbers from 0 to total number of cues/outcomes.
vector<size_t> getRenumberedIdMapping(Dict& D) {
  size_t maxOldId = 0;
  for (Dict::iterator i = D.begin(); i != D.end(); ++i) {
    if (i->second > maxOldId)
      maxOldId = i->second;
  }

  vector<size_t> mapping(maxOldId + 1);
  int newId = 0;
  // Assign each old id a new id in increasing order (to fill any gaps).
  for (Dict::iterator i = D.begin(); i != D.end(); ++i) {
    assert(i->second < mapping.size());
    mapping[i->second] = newId;
    newId++;
  }
  return mapping;
}

// Update dictionary to new ids
void updateDict(Dict& D, vector<size_t>& mapping) {
  for (Dict::iterator i = D.begin(); i != D.end(); ++i) {
    assert(i->second < mapping.size());
    i->second = mapping[i->second];
  }
}

// Assign new consecutive numbers to cues and outcomes for all cues and
// outcomes that occurred and update all event files and cue, outcome dicts.
void renumberIds(const string eventDirPath, Dict& Cues, Dict& Outcomes,
		const bool sortWithinEvent) {
  // Get new consecutive id's for actually occuring cues and outcomes.
  vector<size_t> old2newCueId = getRenumberedIdMapping(Cues);
  vector<size_t> old2newOutcomeId = getRenumberedIdMapping(Outcomes);

  // Change the cue and outcome ids to the new ids in all event files.
  vector<string> paths = getEventFilePaths(eventDirPath);
  Events events;
  // Hi Samuel,
  // Could you take a look and see if this is parallelizable?
  // thanks,
  //Cyrus
  //#pragma omp for
  for (const auto &path:paths) {
    v < Debug || xout << "Updating ids for " << path << endl << flush;
    readEvents(path, events);

    for (Events::iterator it = events.begin(); it != events.end(); ++it) {
      vector<size_t>::iterator idIt;
      for (idIt = it->Cues.begin(); idIt != it->Cues.end(); ++idIt) {
        assert(*idIt < old2newCueId.size());
        *idIt = old2newCueId[*idIt];
      }
      for (idIt = it->Outcomes.begin(); idIt != it->Outcomes.end(); ++idIt) {
        assert(*idIt < old2newOutcomeId.size());
        *idIt = old2newOutcomeId[*idIt];
      }

      if (sortWithinEvent) {
	sort(it->Cues.begin(), it->Cues.end());
	sort(it->Outcomes.begin(), it->Outcomes.end());
      }
    }
    writeEvents(path, events);
    events.clear();
  }

  updateDict(Cues, old2newCueId);
  updateDict(Outcomes, old2newOutcomeId);
}

// check to see if a letter is contained in a LetterList
bool contains(LetterList list, UChar32 letter) {
  return std::find(list.begin(), list.end(), letter) != list.end();
}

// Read in Letters from Alphabet and return a list.
LetterList LoadAlphabet(const string filename, const bool NormCase) {
  LetterList alphabet;
  alphabet.push_back('#');
  ifstream inStream(filename);
  string normalized = "";
  UnicodeString utemp;
  v < Info || xout << "Loading alphabet." << endl;
  //  string item = "";
  if (inStream.fail()) {
    throw Exception("Error Reading Input File " + filename + " ... Exiting\n");
  }
  for (string item; std::getline(inStream, item); ) {
    //    xout << "Got : " << item << endl;
    if (!item.empty()) {
      utemp = convertAndNormalize(item, NormCase);
      // Check for strings longer than one char
      if (utemp.length() > 1) {
	ostringstream buffer;
	buffer << "An item in your alphabet, '" << item << "', was not a letter but rather a multi-letter string. Please fix this and try again. Exiting." << endl;
	throw Exception(buffer.str());
      }
      // Warn about duplicates.
      if (contains(alphabet,utemp[0])) {
	v < Warn || xout << "Warning: Character '" << item << "' was found twice in your list. It was ignored." << endl;
	continue;
      }
      //    xout << " Item: " << item << endl;
      // add letter to the alphabet
      alphabet.push_back(utemp[0]);
    }
  }
  v < Info || xout << "Loaded Alphabet, alphabet size: " << alphabet.size() << " letters."<<endl;
  inStream.close();
  return alphabet;
}


// Takes a list of letters in a language and returns a list of cues which includes
// all cue n-grams up to maxN
// returns a list of all cues
size_t permuteCues(Dict& D, LetterList letters, const size_t maxN) {
  // add a space to the letter list.
  // for single letters
  size_t i = 0;
  if (maxN == 1) {
    LetterList::iterator it;
    for (it=letters.begin() ; it < letters.end(); it++ ) {
      D[*it] = i;
      i++;
    }
    return 1;
  } else {
    if (maxN ==2) {
      // Permute 2 letter combinations for all  the letters in the alphabet.
      LetterList::iterator it2;
      for (it2=letters.begin() ; it2 < letters.end(); it2++ ) {
	UChar32 l1 = *it2;
	LetterList::iterator it3;
	for (it3=letters.begin() ; it3 < letters.end(); it3++ ) {
	  UChar32 l2 = *it3;
	  UnicodeString bigram;
	  bigram.append(l1);
	  bigram.append(l2);
	  D[bigram] = i;
	i++;
	}
      }
      return 2;
    } else {
      if (maxN == 3) {
	// Permute the 3 letter combinations for all  the letters in the alphabet.
	LetterList::iterator it0;
	for (it0=letters.begin() ; it0 < letters.end(); it0++ ) {
	  UChar32 l0 = *it0;
	  LetterList::iterator it2;
	  for (it2=letters.begin() ; it2 < letters.end(); it2++ ) {
	    UChar32 l1 = *it2;
	    if (l1 != ' ') {              // No spaces in the center of trigrams!
	      LetterList::iterator it3;
	      for (it3=letters.begin() ; it3 < letters.end(); it3++ ) {
		UChar32 l2 = *it3;
		UnicodeString trigram;
		trigram.append(l0);
		trigram.append(l1);
		trigram.append(l2);
		D[trigram] = i;
		i++;
	      }
	    }
	  }
	}
	return 3;
      } else {
	v < Warn || xout << "The maximum value for N is currently 3. You requested MaxN=" << maxN << " Falling back to MaxN=3." << endl;
	return 3;
      }
    }
  }
}


// Takes a list of letters in a language and returns a list of cues which includes
// all cue n-grams up to maxN
// returns a list of all cues
size_t permuteCuesInclusive(Dict& D, LetterList letters, const size_t maxN) {
  // for single letters
  LetterList::iterator it;
  size_t i = 0;
  for (it=letters.begin() ; it < letters.end(); it++ ) {
    D[*it] = i;
    i++;
  }
  if (maxN < 2) {
    return 1;
  }
  // Permute 2 letter combinations for all  the letters in the alphabet.
  LetterList::iterator it2;
  for (it2=letters.begin() ; it2 < letters.end(); it2++ ) {
    UChar32 l1 = *it2;
      LetterList::iterator it3;
      for (it3=letters.begin() ; it3 < letters.end(); it3++ ) {
	UChar32 l2 = *it3;
	UnicodeString bigram;
	bigram.append(l1);
	bigram.append(l2);
	D[bigram] = i;
	i++;
      }
  }
  if (maxN == 2) {
    return 2;
  }
  // Permute the 3 letter combinations for all  the letters in the alphabet.
  LetterList::iterator it0;
  for (it0=letters.begin() ; it0 < letters.end(); it0++ ) {
    UChar32 l0 = *it0;
    LetterList::iterator it2;
    for (it2=letters.begin() ; it2 < letters.end(); it2++ ) {
      UChar32 l1 = *it2;
      if (l1 != ' ') {              // No spaces in the center of trigrams!
	LetterList::iterator it3;
	for (it3=letters.begin() ; it3 < letters.end(); it3++ ) {
	  UChar32 l2 = *it3;
	  UnicodeString trigram;
	    trigram.append(l0);
	    trigram.append(l1);
	    trigram.append(l2);
	    D[trigram] = i;
	    i++;
	}
      }
    }
  }
  if (maxN == 3) {
    return 3;
  } else {
    v < Warn || xout << "The max value is Max N is 3. Falling back to MaxN=3." << endl;
    return 3;
  }
}

// Find if a string contains non-alphabetic characters
//
bool AllValidChars(UnicodeString word, LetterList alphabet) {
  for (int i=0; i < word.length(); i++) {
    if (std::find(alphabet.begin(), alphabet.end(), word[i]) == alphabet.end()) { // Letter Not Found
      //      xout << "This letter number was missing: " << i << endl;
      return 0;
    }
  }
  return 1;
}

// add space before all punctuation...
//
// string addSpace(string &word) {
//   for (size_t i=0; i < word.length(); i++) {
//     if () { // Letter Not Found
//       //      xout << "This letter number was missing: " << i << endl;
//       return 0;
//     }
//   }
//   return 1;
// }

// template<class T>
// size_t inline findAndReplace(T& source, const T& find, const T& replace)
// {
//     size_t num=0;
//     typename T::size_t fLen = find.size();
//     typename T::size_t rLen = replace.size();
//     for (typename T::size_t pos=0; (pos=source.find(find, pos))!=T::npos; pos+=rLen)
//     {
//         num++;
//         source.replace(pos, fLen, replace);
//     }
//     return num;
// }

string findAndReplacePunct(string& source)
{
  UErrorCode errorCode = U_ZERO_ERROR;
  UnicodeString unicodeSource = UnicodeString::fromUTF8(source);
  // need to keep hyphens...
  // Match any chars that are not letters or hyphens.
  RegexMatcher regex("([\\P{Letter}--[\u002d]])", unicodeSource, 0, errorCode);
  if (U_FAILURE(errorCode)) {
    v < Warn || xout << "Failed to normalize '" << source <<
      ", icu error code = " << errorCode << "'\n";
  }

  string result;
  UnicodeString replacement = UnicodeString::fromUTF8(" \1 ");
  UnicodeString unicodeResult = regex.replaceAll(replacement, errorCode);
  unicodeResult.toUTF8String(result);
  return(result);
}


Dict cleanCues(Dict& Cues, IDSet& SeenCues) {
  Dict output;
  size_t CueID;
  for (Dict::iterator i = Cues.begin(); i != Cues.end(); i++) {
    CueID = i->second;
    if (SeenCues.count(CueID) != 0) {
      output[i->first] = CueID;
    }
  }
  //  xout << SeenCues.size() << " " << output.size() << endl;
  return output;
}

Dict cleanOutcomes(Dict& Outcomes, IDSet& SeenOutcomes) {
  Dict output;
  size_t OutcomeID;
  for (Dict::iterator i = Outcomes.begin(); i != Outcomes.end(); i++) {
    OutcomeID = i->second;
    if (SeenOutcomes.count(OutcomeID) != 0) {
      output[i->first] = OutcomeID;
    }
  }
  //  xout << SeenOutcomes.size() << " " << output.size() << endl;
  return output;
}

//
// Reads the tabular data file and creates a list of all unique cues and outcomes.
// returns the number of items read.
//
size_t loadCuesOutcomes(Dict& Cues, Dict& Outcomes, FreqDict& CueFreq,FreqDict& OutcomeFreq, const string filename, const bool NormCase)
{
  ifstream inStream(filename);
  if (!inStream.is_open()) {
    throw Exception("Could not open file: " + filename + ". Please check the file location and try again.\n ... Exiting\n");
  }
  size_t lineCount = 0;
  size_t cueCount = 1;
  size_t outcomeCount = 1;
  string normalized,fileID = "";
  UnicodeString utemp;
  string utf8out = "";
  string item,line = "";
  size_t freq = 0;
  vector<string> elems,cues,outcomes;
  Events localEvents;
  // kill first line
  getline(inStream, line);
  //scan all lines of input file
  v < Info || xout << "Reading in data for the first time, one dot per 100,000 lines." << endl;
  while(getline(inStream, line)) {
    lineCount++;
    if (lineCount % 100000 == 0) {
      v < Info || xout << "." << flush;
    }
    elems = split(line,'\t');
    if ((elems.size() != 3)) {
      throw Exception("Formatting error in Tabular file: " + filename + "\n line was \n" + line + " [It needs to have 3 columns, tab separated]\n ... Exiting\n");
    }
    cues = split(elems[0],'_');
    outcomes = split(elems[1],'_');
    istringstream in(elems[2]);
    in >> freq;
    // add to dictionary of all extant cues and outcomes
    for (vector<string>::iterator it = cues.begin() ; it != cues.end(); ++it) {
      item = *it;
      utemp = cleanup(item,NormCase);
      //      utemp.toUTF8String(utf8out);
      //      xout << utf8out << endl;
      //      utf8out.clear();
      if (utemp != "") {
	if (Cues.find(utemp) == Cues.end()) {
	  Cues[utemp] = cueCount;
	  //	  xout << "First time for " << cueCount << endl;
	  CueFreq[cueCount] = freq;
	  cueCount++;
	} else {
	  //	  xout << "Not first time for " << cueCount << endl;
	  CueFreq[Cues[utemp]] = CueFreq[Cues[utemp]] + freq;
	}
      }
    }
    // add to dictionary of all extant outcomes
    for (vector<string>::iterator it = outcomes.begin() ; it != outcomes.end(); ++it) {
      item = *it;
      utemp = cleanup(item,NormCase);
      if (utemp != "") {
	if (Outcomes.find(utemp) == Outcomes.end()) {
	  Outcomes[utemp] = outcomeCount;
	  OutcomeFreq[outcomeCount] = freq;
	  outcomeCount++;
	} else {
	  OutcomeFreq[Outcomes[utemp]] = OutcomeFreq[Outcomes[utemp]] + freq;
	}
      }
    }
  }
  v < Info || xout << endl;
  //  writeDict(Cues,filename+".crap.cues");
  //  xout << "CueFreq 1 = " <<CueFreq[1] << endl;
  //  xout << "CueFreq 2 = " <<CueFreq[2] << endl;
  return(Cues.size());
}


//
// Reads the contents of the file to the specified dictionary.
// returns the number of items read.
//
void buildOutcomeDict(Dict& D, const string filename, const bool NormCase, const LetterList alphabet, const bool permute)
{
  ifstream inStream(filename);
  size_t count = 0;
  string normalized = "";
  UnicodeString utemp;
  string utf8out = "";
  string item = "";
  size_t dupCount = 0;
  size_t badOutcomes = 0;
  size_t len = 0;
  // Check for stream failure.
  ostringstream buffer;

  if (inStream.fail()) {
    throw Exception(" Error reading input file " + filename + " ... Exiting\n");
  }
  for (string item; std::getline(inStream, item); ) {
    len++;
    // Skip blank lines.
    if (item.empty()) {
      continue;
    }
    // Normalize string
    utemp = convertAndNormalize(item, NormCase);
    // Check for duplicate outcomes. Skip duplicates.
    if(D.find(utemp) != D.end()) {
      dupCount++;
      continue;
    }
    else {
      // Check for alphabet set membership.
      if (permute) {
	if (AllValidChars(utemp,alphabet)) {
	  // OutcomeID set.
	  D[utemp] = count;
	  count++;
	} else {
	  utemp.toUTF8String(utf8out);
	  buffer << utf8out << endl;
	  utf8out.clear();
	  badOutcomes++;
	}
      } else {
	D[utemp] = count;
	count++;
      }
    }
  }
  //  utemp = UnicodeString::fromUTF8("unk");
  //  D[utemp] = count + 1;
  if (v >= Info) {
    xout << "Successfully read " << len << " unique outcomes from the file called "<< filename << endl;
    xout << "Found " << dupCount << " duplicate entries. "<< endl;
    if (permute) {
      xout << "Found " << badOutcomes << " entries with invalid characters.";
      xout << "All rejected entries are in the file" << filename << ".outcomes.with.invalid.characters.txt." << endl;
      xout << "Added only " << count << " items."<< endl;
      ofstream os(filename+".outcomes.with.invalid.characters.txt");
      os << buffer.str();
      os.close();
    } else {
      xout << "Added " << count << " outcomes to the outcome list."<< endl;
    }
  }
  inStream.close();
}


void buildCueDict(Dict& D, const string filename, const bool NormCase)
{
  ifstream inStream(filename.c_str());
  size_t count = 0;
  string normalized = "";
  UnicodeString utemp;
  string utf8out = "";
  string item = "";
  size_t dupCount = 0;
  size_t len = 0;
  // Check for stream failure.
  if (inStream.fail()) {
    throw Exception(" Error Reading Input File " + filename + " ... Exiting\n");
  }
  for (string item; std::getline(inStream, item); ) {
    len++;
    // Normalize string
    if (item.empty()) {
      continue;
    }
    utemp = convertAndNormalize(item, NormCase);
    // Check for duplicate cues. Skip duplicates.
    if(D.find(utemp) != D.end()) {
      dupCount++;
      continue;
    } else {
      // Set Cue ID Number
      D[utemp] = count;
      count++;
    }
  }
  //  utemp = UnicodeString::fromUTF8("unk");
  //  D[utemp] = count + 1;
  if (v >= Info) {
    xout << "Successfully read " << len << " lines from the cue file called "<< filename << endl;
    xout << "Found " << dupCount << " duplicate entries. "<< endl;
    xout << "Added only " << count << " unique items."<< endl;
  }
  inStream.close();
}


// Save list of missing words to a file.
void writeErrors (const string ofilename, WordSet &errors) {
  ofstream os(ofilename.c_str());
  WordSet::iterator it;
  for (it=errors.begin(); it != errors.end(); it++ ) {
    os << *it << endl;
  }
  os.close();
}


// Extract all the cues from the outcomes and build a learning event.
// using permutation of cues
//Event getCues(const UnicodeString& t1, const UnicodeString& t2, const UnicodeString& t3, const Dict& CuesDict, const Dict& Outcomes, const size_t N, const bool inclusive, const size_t WindowSize) {
Event getCues(const WordList& words, const size_t& startPos, const Dict& CuesDict, const Dict& Outcomes, const size_t N, const bool inclusive, const size_t WindowSize, const bool sortWithinEvent) {
  // the event
  Event thisEvent;

  // first add all the outcomes of words in the window
  size_t fullWindow = (WindowSize*2) +1;

  UnicodeString item;
  item.append('#');
  for (size_t i = 0; i < fullWindow; i++) {
    // add all outcomes that are in the list
    if ( Outcomes.find(words[startPos+i]) != Outcomes.end() ) {
      thisEvent.Outcomes.push_back(Outcomes.at(words[startPos+i]));
      // build string of words with word boundary markers
    }
    // add all the text into the cue string, valid outcome or not.
    item.append(words[startPos+i]);
    item.append('#');
  }

  //Then add the cues using the string
  // First add the unigram CueIDs
  if (inclusive || N ==1) {
    for (int i=0; i < item.length(); i++) {
      thisEvent.Cues.push_back(CuesDict.at(item[i]));
      //   xout << "Added Cue " << CuesDict[item[i]] << endl;
    }
  }
  // add the bigram CueIDs
  if ((inclusive && N < 3) || N == 2 ) {
    UnicodeString gram2;
    for (int i=0; i < item.length() - 1; i++) {
      gram2 += item[i];
      gram2 += item[i+1];
      thisEvent.Cues.push_back(CuesDict.at(gram2));
      gram2.remove();
    }
  }
  // add the item CueIDs
  if ((inclusive && N >= 3) || N ==3) {
    UnicodeString gram3;
    for (int i=0; i < item.length() - 2; i++) {
      gram3 += item[i];
      gram3 += item[i+1];
      gram3 += item[i+2];
      thisEvent.Cues.push_back(CuesDict.at(gram3));
      gram3.remove();
    }
  }
  //  Sort event cues and outcomes.
  if (sortWithinEvent) {
    sort(thisEvent.Cues.begin(),thisEvent.Cues.end());
    sort(thisEvent.Outcomes.begin(),thisEvent.Outcomes.end());
  }
  // add frequency, in this case, always 1
  thisEvent.Freq = 1;
  return thisEvent;
}


// Add unpermuted cues and outcomes and build a learning event.
//
// This was the old way.
/*Event getCues2(const UnicodeString& t1, const UnicodeString& t2,
	       const UnicodeString& t3,
	       const Dict& CuesDict, const Dict& Outcomes,
	       const size_t N) {*/
// now the new way.
Event getCues2(const WordList& words, const size_t& startPos,
	       const Dict& CuesDict, const Dict& Outcomes,
	       const size_t N, const size_t WindowSize,
	       const bool sortWithinEvent) {
  // the event
  Event thisEvent;
  // Find position of outcome in full window
  size_t outcomePos = startPos + (N -1);

  // add the outcome, if it is valid. Otherwise, return empty event.
  if ( Outcomes.find(words[outcomePos]) != Outcomes.end() ) {
    thisEvent.Outcomes.push_back(Outcomes.at(words[outcomePos]));
  } else {
    return thisEvent;
  }

  vector<size_t> cuePos;
  size_t fullWindow = (WindowSize*2) +1;

  for (size_t i = 0; i < fullWindow; i++) {
    // If not in the target/outcome position, it is a cue!
    if (i != (N - 1)) {
	cuePos.push_back(startPos +i);
    }
  }

  // add all the Cues
  for (const auto &pos:cuePos) {
    // check if possible cue string is in cue list
    if ( CuesDict.find(words[pos]) != CuesDict.end() ) {
      thisEvent.Cues.push_back(CuesDict.at( words[pos] ));}
  }

  //
  if (sortWithinEvent) {
    sort(thisEvent.Cues.begin(),thisEvent.Cues.end());
  }
  // add frequency, in this case, always 1
  thisEvent.Freq = 1;
  return thisEvent;

  /*

  // Old, bad way!!!!!

  bool t1Good = false;
  bool t2Good = false;
  bool t3Good = false;
  if ( CuesDict.find(t1) != CuesDict.end() ) {
    t1Good = true;
  }
  if ( CuesDict.find(t2) != CuesDict.end() ) {
    t2Good = true;
  }
  if ( CuesDict.find(t3) != CuesDict.end() ) {
    t3Good = true;
  }

  if (N == 3) {
    // first 2 words cue the third word
    thisEvent.Outcomes.push_back(Outcomes.at(t3));
    if (t1Good) {thisEvent.Cues.push_back(CuesDict.at(t1));}
    if (t2Good) {thisEvent.Cues.push_back(CuesDict.at(t2));}
  } else {
    if (N == 2) {
    // first 1st and 3rd word as cues,  the second word as outcome
    thisEvent.Outcomes.push_back(Outcomes.at(t2));
    if (t1Good) { thisEvent.Cues.push_back(CuesDict.at(t1)); }
    if (t3Good) { thisEvent.Cues.push_back(CuesDict.at(t3)); }
    }
    else {
      thisEvent.Outcomes.push_back(Outcomes.at(t1));
      if (t3Good) { thisEvent.Cues.push_back(CuesDict.at(t3));}
      if (t2Good) { thisEvent.Cues.push_back(CuesDict.at(t2));}
    }
  }
  sort(thisEvent.Cues.begin(),thisEvent.Cues.end());
  // add frequency, in this case, always 1
  thisEvent.Freq = 1;
  return thisEvent;

  */
}




void addOutcomes(Event &thisEvent, WordList &item, Dict &OutcomesDict) {
  size_t numItems = item.size();
  UnicodeString outcome;
  for (size_t i = 1; i < numItems; i++) {
    outcome = item[i];
    thisEvent.Outcomes.push_back(OutcomesDict[outcome]);
  }
}

Event getExtCues(WordList &i1, WordList &i2, WordList &i3, Dict &CuesDict, Dict &OutcomesDict, const size_t N, const bool inclusive,const size_t WindowSize, const bool sortWithinEvent) {
  // the event
  Event thisEvent;
  UnicodeString trigram;
  if (WindowSize == 1) {
    UnicodeString t1,t2,t3;
    t1 = i1[0];
    t2 = i2[0];
    t3 = i3[0];
    // the concatenated trigam
    trigram.append('#');
    trigram.append(t1);
    trigram.append('#');
    trigram.append(t2);
    trigram.append('#');
    trigram.append(t3);
    trigram.append('#');
  // add the OutcomeIDs.
  addOutcomes(thisEvent,i1,OutcomesDict);
  addOutcomes(thisEvent,i2,OutcomesDict);
  addOutcomes(thisEvent,i3,OutcomesDict);
// add the unigram CueIDs
  } else {
    UnicodeString t1;
    t1 = i1[0];
    addOutcomes(thisEvent,i1,OutcomesDict);
    trigram.append('#');
    trigram.append(t1);
    trigram.append('#');
  }
  if (inclusive || N == 1) {
    for (int i=0; i < trigram.length(); i++) {
      thisEvent.Cues.push_back(CuesDict[trigram[i]]);
      //   xout << "Added Cue " << CuesDict[trigram[i]] << endl;
    }
  }
  // add the bigram CueIDs
  if ((inclusive && N < 3) || N ==2 ) {
    UnicodeString gram2;
    for (int i=0; i < trigram.length() - 1; i++) {
      gram2 += trigram[i];
      gram2 += trigram[i+1];
      thisEvent.Cues.push_back(CuesDict[gram2]);
      gram2.remove();
    }
  }
  // add the trigram CueIDs
  if ((inclusive && N >= 3) || N == 3) {
    UnicodeString gram3;
    for (int i=0; i < trigram.length() - 2; i++) {
      gram3 += trigram[i];
      gram3 += trigram[i+1];
      gram3 += trigram[i+2];
      thisEvent.Cues.push_back(CuesDict[gram3]);
      gram3.remove();
    }
    thisEvent.Freq = 1;
  }
  if (sortWithinEvent) {
    sort(thisEvent.Cues.begin(),thisEvent.Cues.end());
    sort(thisEvent.Outcomes.begin(),thisEvent.Outcomes.end());
  }
  return thisEvent;
}


size_t addEvents(WordList& words, const Dict& Cues, const Dict& Outcomes, const size_t& realN, const string& pathName, IDSet& SeenCues, IDSet& SeenOutcomes, const bool inclusive, const size_t WindowSize,size_t PortionNumber, const bool permute, const bool sortWithinEvent) {
  Events local_Events;
  Event anEvent;
  string fileID;
  size_t countPara = 0;
  const int id = omp_get_thread_num();
  size_t numWords = words.size();

  if (WindowSize == 0) {
    // Add two dummy words to the end of the word list, as the last two words
    // passed to getCues() get ignored if WindowSize == 0.
    words.push_back("");
    words.push_back("");
    numWords = words.size();
  }

  // numWords is an unsigned integer -> need to make sure that wordList is large enough
  // so as not to have problems with data smaller than the window size
  if (numWords > (WindowSize*2) + 1)
  {
    for (size_t j=0; j < (numWords - 2); j++) {
      if (permute) {
	anEvent = getCues(words,j, Cues, Outcomes, realN, inclusive, WindowSize, sortWithinEvent);
      } else {
	anEvent = getCues2(words,j, Cues, Outcomes, realN, WindowSize, sortWithinEvent);
      }
      if (anEvent.Cues.empty()) {
	continue;
      } else {
	local_Events.push_back(anEvent);
      }
      // Add Cues in each event to the set of seen cues.
      for (size_t i=0; i < anEvent.Cues.size(); i++) {
	SeenCues.insert(anEvent.Cues[i]);
      }
      for (size_t k=0; k < anEvent.Outcomes.size(); k++) {
	SeenOutcomes.insert(anEvent.Outcomes[k]);
      }
      if (id == 0)
      {
	if (countPara % 1000 == 0) {
	  xout.flush(); // Only the master thread is allowed to flush to Rout.
	}
      }
      if (countPara % 100000 == 0) {
	if (v >= Debug) {
	  xout << id << " ";
	}
	else if (v >= Info)
	  xout << ".";
      }
      countPara++;
    }
    ostringstream fileID_stream("");
    fileID_stream<<"events_0_"<<PortionNumber;
    fileID = fileID_stream.str();
    ostringstream outname;
    outname << pathName << "/" << fileID << ".dat";
    v < Debug || xout << "\nThread #" << id <<" writing events to file: " << outname.str() << " ";
    writeEvents(outname.str(),local_Events);
  }
  return(countPara);
}

void addExtendedEvents(ExtendedWordList &words, Dict &Cues, Dict &Outcomes, const size_t &realN, const string &pathName, IDSet& SeenCues, IDSet& SeenOutcomes, const bool inclusive, const size_t WindowSize, const bool sortWithinEvent) {
  Events local_Events;
  IDSet local_SeenCues,local_SeenOutcomes;
  Event anEvent;
  string fileID;
  size_t countPara = 0;
  int id;
  size_t numWords = words.size();
  size_t wordsPerThread = numWords -2;
  size_t bufferCount = 0;
#ifdef _OPENMP
  wordsPerThread = (numWords - 2)/omp_get_num_procs();
#endif
  v < Info || xout << "Starting to extract cues from outcomes. One number per 100,000 events." << endl;
#pragma omp parallel shared(words,realN,Cues,Outcomes,numWords) private(id,anEvent,local_Events,fileID,local_SeenCues,local_SeenOutcomes) firstprivate(countPara,bufferCount)
  {
    #pragma omp for
    for (size_t itr=0; itr < (numWords-2); itr=itr+wordsPerThread) {
	size_t max_j = ((itr + wordsPerThread) > (numWords-2) )? (numWords-2):(itr+ wordsPerThread);
	for(size_t j=itr;; j++) {
	  // create an extended Event.
	  anEvent = getExtCues(words[j], words[j+1], words[j+2], Cues, Outcomes, realN, inclusive, WindowSize, sortWithinEvent);
	  local_Events.push_back(anEvent);
	  for (size_t i=0; i < anEvent.Cues.size(); i++) {
	    local_SeenCues.insert(anEvent.Cues[i]);
	  }
	  for (size_t k=0; k < anEvent.Outcomes.size(); k++) {
	    SeenOutcomes.insert(anEvent.Outcomes[k]);
	  }
	  if (countPara % 100000 == 0) {
	    if (v >= Debug) {
	      id = omp_get_thread_num();
	      xout << id << " ";
	    }
	    else if (v >= Info)
	      xout << ".";
	  }
	  // Save output every million events
	  // This prevents RAM overload.
	  if ((countPara > 1000000) || ((j + 1) == max_j)) {
	    // create random file name.
	    id = omp_get_thread_num();
	    v < Debug || xout << id << "W ";
	    ostringstream fileID_stream("");
	    fileID_stream<<"events_"<<(itr/wordsPerThread)<<"_"<<bufferCount;
	    fileID = fileID_stream.str();
	    ostringstream outname;
	    outname << pathName << "/" << fileID << ".dat";
	    // Save Events to disk.
	    //	    xout << endl << "Write Event to file:" << outname.str() << endl;
	    writeEvents(outname.str(),local_Events);
	    local_Events.clear();
	    countPara = 0;
	    bufferCount++;
	    if((j+1) == max_j)
	      break;
	  }
	  countPara++;
	}
    }
    id = omp_get_thread_num();
#pragma omp critical
    {
      // Join threads and collect all the results of the parallelism
      SeenCues.insert(local_SeenCues.begin(),local_SeenCues.end());
      v < Info || xout << "\nSeenCues = "<< SeenCues.size() <<"\nCompleted Thread #" << id << " ";
      SeenOutcomes.insert(local_SeenOutcomes.begin(),local_SeenOutcomes.end());
      v < Info || xout << "\nSeen outcomes:" << local_SeenOutcomes.size() << endl;
   }
  }
  v < Info || xout << endl;
}

bool badCues(const Event &thisEvent, const FreqDict &BadCues) {
  vector<size_t> c = thisEvent.Cues;
  for (size_t i=0; i < c.size(); i++) {
    if (BadCues.find(c[i]) != BadCues.end()) {
      return(true);
    }
  }
  return(false);
}

template <typename T1, typename T2>
struct less_second {
    typedef pair<T1, T2> type;
    bool operator ()(type const& a, type const& b) const {
        return a.second < b.second;
    }
};

template <typename T1, typename T2>
struct greater_second {
    typedef pair<T1, T2> type;
    bool operator ()(type const& a, type const& b) const {
        return a.second > b.second;
    }
};

RevDict getReversedDict(const Dict &dict) {
  // Create a reverse map with ID mapping to string.
  RevDict reversedDict;

  for (Dict::const_iterator m = dict.begin(); m != dict.end(); ++m)
    reversedDict[m->second] = m->first;

  return reversedDict;
}

Dict getMostFrequentItems(const FreqDict& dictFreq, const RevDict& reversedDict, const size_t threshold) {
  // Copy dictionary freq map to vector, sort it and truncate the vector.
  vector<pair<size_t, size_t> > mapSorted(dictFreq.begin(), dictFreq.end());
  sort(mapSorted.begin(), mapSorted.end(), greater_second<size_t, size_t>());
  mapSorted.erase(mapSorted.begin() + threshold, mapSorted.end());

  // Convert the truncated sorted vector back to a map.
  Dict result;
  size_t id;
  for (vector<pair<size_t, size_t> >::iterator k = mapSorted.begin(); k != mapSorted.end(); k++) {
    id = k->first;
    result[reversedDict.at(id)] = id;
  }
  return result;
}


void processTabularCorpus(const string filename, Dict &Cues, Dict &Outcomes, FreqDict &CueFreq, FreqDict &OutcomeFreq, const string &pathName, size_t cueThreshold, size_t outcomeThreshold, const bool NormCase, const string CorpusType, const bool sortWithinEvent) {
  bool notraw = (CorpusType == "T") ? true : false;
  IDSet SeenCues, SeenOutcomes;

  if (notraw) {
    // Drop low frequency cues if necessary.
    if (Cues.size() > cueThreshold) {
      v < Warn || xout << "Dropping low frequency cues." << endl;
      v < Warn || xout << "Original size of Cues = " << Cues.size() << endl;

      RevDict CuesReversed = getReversedDict(Cues);
      writeFreqDict(CueFreq, CuesReversed, filename+".cuefreq.cues");

      Cues = getMostFrequentItems(CueFreq, CuesReversed, cueThreshold);
      v < Warn || xout << "New set size for Cues (they might not all appear) = " << Cues.size() << endl;
    }

    // Drop low frequency outcomes if necessary.
    if (Outcomes.size() > outcomeThreshold) {
      v < Warn || xout << "Dropping low frequency outcomes." << endl;
      v < Warn || xout << "Original size of Outcomes= " << Outcomes.size() << endl;

      RevDict OutcomesReversed = getReversedDict(Outcomes);
      Outcomes = getMostFrequentItems(OutcomeFreq, OutcomesReversed, outcomeThreshold);
      v < Warn || xout << "New set size for Outcomes (they might not all appear) " << Outcomes.size() << endl;
    }

    //  if (Cues.size() > 15000) {
    //    throw Exception("You had more than 15000 cues. Re-run with a higher threshold. Exiting.\n");
    //  }

    v < Info || xout << "Re-reading in events that had cues above frequency threshold.\nOne dot per 100,000 lines." << endl;
  }

  ifstream inStream(filename);
  size_t goodLines = 0;
  size_t totalCount = 0;
  size_t lineCount = 0;
  size_t eventCount = 0;
  string normalized = "";
  string utf8out = "";
  string item,line = "";
  UnicodeString utemp;
  size_t freq = 0;
  vector<string> elems,cues,outcomes;
  Events localEvents;
  // kill first line
  getline(inStream, line);
  //scan all lines of input file
  size_t fileCount = 0;

  v < Info || xout << "At line number: " << flush;
  while(getline(inStream, line)) {
    lineCount++;
    if (lineCount % 100000 == 0) {
      v < Info || xout << (lineCount/1000) << "k " << flush;
    }
    elems = split(line,'\t');
    cues = split(elems[0],'_');
    // add to dictionary of all extant cues.
    Event anEvent;
    bool goodEvent = true;
    for (vector<string>::iterator it = cues.begin() ; it != cues.end(); ++it) {
      item = *it;
      utemp = cleanup(item,NormCase);
      if (utemp != "") {
	if (Cues.find(utemp) == Cues.end()) {
	  goodEvent=false;
	  break;
	} else {
	    anEvent.Cues.push_back(Cues[utemp]);
	    SeenCues.insert(Cues[utemp]);
	}
      }
    }
    if (!goodEvent) {
      continue;
    } else {
      istringstream in(elems[2]);
      in >> freq;
      outcomes = split(elems[1],'_');
      // add to extant outcomes
      for (vector<string>::iterator it = outcomes.begin() ; it != outcomes.end(); ++it) {
	item = *it;
	utemp = cleanup(item,NormCase);
	if (utemp != "") {
	  // Not all event outcomes need to be valid; one is enough (checked below).
	  if (Outcomes.find(utemp) != Outcomes.end()) {
	    anEvent.Outcomes.push_back(Outcomes[utemp]);
	    SeenOutcomes.insert(Outcomes[utemp]);
	  }
	}
      }
    }
    if (goodEvent && anEvent.Outcomes.size() > 0) {
      goodLines++;
      anEvent.Freq = freq;
      //Sort Cues and Outcomes in Event
      if (sortWithinEvent) {
	sort(anEvent.Cues.begin(),anEvent.Cues.end());
	sort(anEvent.Outcomes.begin(),anEvent.Outcomes.end());
      }
      localEvents.push_back(anEvent);
      eventCount++;
      totalCount++;
      // for (size_t i = 0; i < freq; i++) {
      // 	localEvents.push_back(anEvent);
      // 	eventCount++;
      // 	totalCount++;
      // }
    }
    if (eventCount >= 1000000) {
      fileCount++;
      v < Debug || xout << "\nSaving events at event # " << totalCount << endl;
      ostringstream outname;
      // Old filename convention:
      // outname << pathName << "/events." << setfill('0') << setw(10) << fileCount << ".dat";
      outname << pathName << "/" << "events_0_" << fileCount << ".dat";
      writeEvents(outname.str(),localEvents);
      localEvents.clear();
      eventCount = 0;
    }
  }
  fileCount++;
  v < Debug || xout << "Saving last events at event # " << totalCount << endl;
  ostringstream outname;
  // Old filename convention:
  // outname << pathName << "/events." << setfill('0') << setw(10) << fileCount << ".dat";
  outname << pathName << "/" << "events_0_" << fileCount << ".dat";
  writeEvents(outname.str(),localEvents);
  xout.precision(5);
  double ratioKept = (static_cast<double>(goodLines)/static_cast<double>(lineCount) * 100.0);
  (v < Warn || (v == Warn && ratioKept == 100.0)) || xout << "Read " << lineCount<< " lines and kept " << ratioKept << "% of the events." << endl;
  // remove empty string elements.
  //  writeDict(GoodCues,"shit.cues");
  //  GoodCues.erase(UnicodeString());
  //  GoodOutcomes.erase(UnicodeString());
  if (!notraw) {
    Cues = cleanCues(Cues,SeenCues);
    writeDict(Outcomes,filename+".crap.outcomes");
    v < Info || xout << Outcomes.size() << endl;
    Outcomes = cleanOutcomes(Outcomes,SeenOutcomes);
    v < Info || xout << Outcomes.size() << endl;
  }
}

void processExtendedCorpus(string &contents, const bool NormCase, Dict &Cues, Dict& Outcomes, WordSet & errors, const size_t &realN, const string &pathName, IDSet& SeenCues, IDSet& SeenOutcomes, const bool inclusive, const size_t WindowSize, const bool sortWithinEvent) {
  ExtendedWordList results;
  WordList words;
  istringstream stream(contents);
  string line, item,utf8out;
  UnicodeString cleanword;
  vector<string> elems;
  size_t counter = 0;
  v < Info || xout << "Initializing text processing and Unicode conversion." << endl;
  while(getline(stream, line)) {
    elems = split(line,'\t');
    for (vector<string>::iterator it = elems.begin() ; it != elems.end(); ++it) {
      item = *it;
      cleanword = cleanup(item,NormCase);
      // only accept words that in the outcome list.
      if (Outcomes.find(cleanword) != Outcomes.end()) {
	words.push_back(cleanword);
	} else {
	// add word to errorlist.
	cleanword.toUTF8String(utf8out);
	errors.insert(utf8out);
	utf8out.clear();
      }
      if (counter % 1000000 == 0) {
	  v < Info || xout << ".";
      }
      counter++;
    }
    results.push_back(words);
    words.clear();
  }
  contents.clear();
//  xout << "Added " << counter << " items from the Extended format corpus." << endl;
  addExtendedEvents(results,Cues,Outcomes,realN,pathName,SeenCues,SeenOutcomes,inclusive,WindowSize,sortWithinEvent);
}

// For full text corpora
void processNormalCorpus(string &contents, const int numThreads, const bool NormCase, const Dict &Cues, const Dict &Outcomes, WordSet &errors, const size_t &realN, const string &pathName, IDSet &SeenCues,IDSet &SeenOutcomes, const bool inclusive, const size_t WindowSize, const bool permute, const bool sortWithinEvent) {

  double corpusSize = static_cast<double>(contents.size());
  size_t numChars = contents.size();
  WordList words_local;
  size_t num_words_local=0;
  WordSet errors_local;
  IDSet SeenCues_local, SeenOutcomes_local;
  size_t countLoop = 0;
  size_t eventCount = 0;
  size_t totalEventCount = 0;
  string piece;
  double numPortions;
  double numThreadsReal;
  //  size_t prePostDiff=0;
  numThreadsReal = static_cast<double>(numThreads);
  if (numThreads < 2) {
    numPortions = 1.0;
  } else if (corpusSize/numThreadsReal < 10000000.0) {
    numPortions = numThreadsReal;
    // To make sure there are (at most) numThread portions we use ceiling for portionSize
  } else {
    numPortions = corpusSize / 10000000.0;
  }
  size_t portionSize = static_cast<size_t>(ceil(corpusSize/numPortions));

  if (portionSize > 2000000000) {
    throw Exception("Portion size is too large. Use a smaller corpus or set the number of threads to use to be greater than 1 by specifying OMP_NUM_THREADS.");
  }

  v < Info || xout << "Initializing text processing and Unicode conversion." << endl << flush;
  // turn giant string into a stringstream for token extraction
  // container for cleaned, converted, normalized text
  //  xout << "Adding space before punctuation."<< endl<<flush;
  //  contents = addSpace(contents);
  //  xout << "Done adding space before punctuation."<< endl<<flush;
  v < Debug || xout << "Splitting data into " << floor(numPortions) << " pieces made up of " << portionSize << " chars each.\n" << flush;
#pragma omp parallel shared(portionSize,contents,errors) private(piece,words_local,errors_local,SeenCues_local,SeenOutcomes_local) firstprivate(countLoop,eventCount) reduction(+:num_words_local)
  {
#pragma omp for
    // Loop to take in all data.
    for (size_t i=0; i < numChars; i=i+portionSize) {
      const int id = omp_get_thread_num();
      // As the pieces are just split at an arbitrary position of the string,
      // this split is unlikely to coincide with a word boundary.
      // Therefore we search for where the first word begins and the last ends.

      // Start of the first word to consider in this text piece
      // Which one is the first word depends on the window size setting.
      // If 0, the first word begins at the last space before the piece start.
      // If 1, then word trigrams are considered and we need to go three spaces
      // back. The two words before the actual first word also need to be taken
      // into account, as they fall into the window of the actual first word.
      size_t pieceStart = i+2; // As we start searching at pieceStart-2
      for (size_t counter = 0; counter < (WindowSize*2 + 1); counter++) {
        size_t prevSpace = contents.rfind(" ", pieceStart-2);
        if (prevSpace != std::string::npos) // space found
          pieceStart = prevSpace + 1;
        else { // no space found before pieceStart-2
          pieceStart = i;
          break;
        }
      }

      // End of this text piece (exclusive)
      // Last space in the piece (don't want to include the partial word at the
      // end of the piece) or the end of the text if this is the last piece.
      // The partial word at the piece end is the first word of the next piece.
      size_t pieceEnd = (i+portionSize >= numChars) ? numChars+1 :
                           contents.rfind(" ", i+portionSize);
      if (pieceEnd == std::string::npos) // No space before i+portionSize
        pieceEnd = i + portionSize;

      piece = contents.substr(pieceStart, pieceEnd-pieceStart); // end excl.
      string punctFixed = findAndReplacePunct(piece);
      //      size_t pre = count(piece.begin(), piece.end(), ' ');
      //      size_t post = count(punctFixed.begin(), punctFixed.end(), ' ');
      //      prePostDiff += post -pre;
      piece.clear();
      istringstream iss(punctFixed);
      UnicodeString cleanword,utemp;
      string temp = "";
      string utf8out;
      do {
	//get UTF8 encoded string
	iss >> temp;
	// Clean up the word, getting Unicode
	cleanword = cleanup(temp,NormCase);
	num_words_local++;
	//  accept words that are not on the outcome list.
	if (Outcomes.find(cleanword) != Outcomes.end()) {
	  words_local.push_back(cleanword);
	} else {
	  // add word to errorlist. Insert unknown word marker?
	  cleanword.toUTF8String(utf8out);
	  errors_local.insert(utf8out);
	  utf8out.clear();
	  //	  cleanword.insert(0, UnicodeString::fromUTF8("?"));
	  //	  words_local.push_back(" ");
	  //debugging
	  //	  cleanword.toUTF8String(utf8out);
	  //	  xout << "Found unknown word and inserted: " << utf8out << "\n";
	  //	  utf8out.clear();
	}
	if (id == 0) {
	  if (countLoop % 1000 == 0) {
	    xout.flush(); // Only the master thread is allowed to flush to Rout.
	  }
	}
	if (countLoop % 1000000 == 0) {
	  if (v >= Debug) {
	    xout << id << " ";
	    //	  xout << id << " Size:" << words_local.size() << " ";
	  }
	  else if (v >= Info)
	    xout << ".";
	}
	countLoop++;
	temp.clear();
      } while(iss);
      //      xout << id << "Removing a piece of size:" << piece.size() << "\n";
      piece.clear();

      eventCount += addEvents(words_local, Cues, Outcomes, realN, pathName, SeenCues_local,SeenOutcomes_local, inclusive, WindowSize,i/portionSize, permute, sortWithinEvent);
      //      xout << id << "Removing a WordList of size:" << words_local.size() << "\n";
      //      xout << "\nThread#" << id << " completed a block " << static_cast<double>(i)/corpusSize*100.0 << " of the way through the corpus.\n";
      words_local.clear();
    }
#pragma omp critical
    {
      // Join threads and collect all the results of the parallelism
      const int id = omp_get_thread_num();
      errors.insert(errors_local.begin(), errors_local.end() );
      SeenCues.insert(SeenCues_local.begin(),SeenCues_local.end());
      SeenOutcomes.insert(SeenOutcomes_local.begin(),SeenOutcomes_local.end());
      totalEventCount += eventCount;
      v < Debug || xout << "\nCompleted Thread #" << id << ", found " << errors_local.size() << " non-words, " << SeenCues_local.size() << " unique cues, and "  << SeenOutcomes_local.size() << " unique outcomes.";
      if (id == 0)
      	xout.flush();
    }
  }
  xout.flush(); // Now in master thread: make sure to flush output.
  if (v >= Info) {
    xout<< endl << "\nFinished processing text and removing unknown words." << endl
        << "Found " << num_words_local << " words in total and "
           "saved " << totalEventCount << " events." << endl << flush;
  }
}

string getFormattedTime(tm* ptm) {
    char buffer [20];
    snprintf(buffer, 20, "%4d-%02d-%02d %02d:%02d:%02d", 1900+ptm->tm_year,
	ptm->tm_mon+1, ptm->tm_mday, ptm->tm_hour, ptm->tm_min, ptm->tm_sec);
    return buffer;
}

int preprocess(const string& inputFile, const string& cueFile, const string& outcomeFile,
	       const string& outputDirWithNamePrefix, const int numThreads, const string CorpusType,
	       const size_t maxN, const size_t WindowSize, const bool NormCase, const bool inclusive,
	       const bool permute, const size_t cueThreshold, const size_t outcomeThreshold,
	       const bool sortWithinEvent,
	       const string& fileInfoComment, const string& settingsFileSuffix, Verbosity verbosity)
{
  #ifdef _OPENMP
  /* using conditional compilation to let sequential compilers ignore the omp.h header*/
  // Overrules the environment variable OMP_NUM_THREADS setting.
  omp_set_num_threads(numThreads);
  #endif

  // Start timing execution.
  time_t start = time(NULL);
  v = verbosity; // set global verbosity variable (limited to this file)

  v < Debug || xout << "This is the NDL preprocessor, version " << version << endl;
  // unit test for serialization:
  if (v >= Debug) {
    testIO(v >= Debug);
  }
  //  testDanks();
  //  abort();
  //  xout << "Unit Tests passed." << endl;

  //////////////////////////////////////////////////////////////////////////////
  // Saving the settings to a text file
  ofstream settingsOut((outputDirWithNamePrefix+".settings"+settingsFileSuffix).c_str());
  if (settingsOut.good())
  {
    string typeName;
    switch(CorpusType[0]) {
      case 'N':
	typeName = "corpus"; break;
      case 'E':
	typeName = "extended corpus"; break;
      case 'T':
	typeName = "tabular"; break;
      case 'R':
	typeName = "raw tabular"; break;
      default:
	typeName = "unknown"; break;
    }

    time_t timeNow;
    time(&timeNow);
    string creationTime = getFormattedTime(localtime(&timeNow));

    // Common settings
    settingsOut << std::boolalpha;
    settingsOut << "ndlpreprocess version\t" << version << endl <<
      "creation time\t" << creationTime << endl <<
      "type\t" << typeName << endl;

    if (fileInfoComment != "") {
      settingsOut << "comment\t" << fileInfoComment << endl;
    }

    struct stat fileAttributes;
    stat(inputFile.c_str(), &fileAttributes);
    string modTime = getFormattedTime(localtime(&(fileAttributes.st_mtime)));

    settingsOut << "input file\t" << inputFile << endl <<
      	"file modification time\t" << modTime << endl; // format: yyyy-m-d h:m:
    //	ptm->tm_year<<"-"<<ptm->tm_mon<<"-"<<ptm->tm_day<<
    //	" " << ptm->tm_hour<<":"<<ptm->tm_min<<":"<<ptm->tm_sec << endl <<
    settingsOut << "normalize case\t" << NormCase << endl;

    if (CorpusType == "T") {
      settingsOut << "maximum number cues\t" << cueThreshold << endl <<
	"maximum number outcomes\t" << outcomeThreshold << endl;
    } else {
      settingsOut << "cue file\t" << cueFile << endl <<
	"outcome file\t" << outcomeFile << endl;
      if (CorpusType == "N" || CorpusType == "E") {
	settingsOut << "window size\t" << WindowSize*2+1 << endl << // total window size
	  "use letter ngrams\t" << permute << endl <<
	  "maximum ngram size\t" << maxN << endl <<
	  "include smaller ngrams\t" << inclusive << endl;
      }
    }
    settingsOut.close();
    if (settingsFileSuffix != "") // only writing settings file, no preprocessing
      return 0;
  }
  else {
    v < Warn || xout << "Could not write to the preprocessing information file.\n";
    if (settingsFileSuffix != "") // only writing settings file, no preprocessing
      return 1;
  }

  //////////////////////////////////////////////////////////////////////////////
  // The actual preprocessing starts here
  try
  {
    string pathName;
    pathName = outputDirWithNamePrefix + ".events";
    createEventsDir(pathName);

    // if (cueFile == "") {
    // 	throw Exception("No cue file specified. Exiting.");
    // }

    // Find the current Locale from the environment.
    v < Info || xout << "The language environment has been set to: " <<  endl;//TODO: print //std::use_facet<boost::locale::info>(loc).name() << endl;
    v < Info || xout << "Please make sure that your corpus is encoded in that language!" << endl;

    Dict Cues;
    FreqDict CueFreq;
    FreqDict OutcomeFreq;
    Dict Outcomes;
    IDSet SeenCues;
    IDSet SeenOutcomes;
    size_t realN = maxN;
    LetterList alphabet;

    if (CorpusType != "T") {
      // if permutation is on.
      if (permute) {
	// Load Cues and assign IDs and save IDs
	alphabet = LoadAlphabet(cueFile,NormCase);
	if (inclusive) {
	  realN = permuteCuesInclusive(Cues, alphabet, maxN);
	} else {
	  realN = permuteCues(Cues, alphabet, maxN);
	}
	v < Info || xout << "Permuted Cues: Final number of Possible Cues: " << Cues.size() << endl;
	buildOutcomeDict(Outcomes,outcomeFile,NormCase,alphabet,permute);
      } else {
	// if permutation is off, just load cues and outcomes.
	buildCueDict(Cues,cueFile,NormCase);
	buildOutcomeDict(Outcomes,outcomeFile,NormCase,alphabet,permute);
      }
      // Load Outcomes and assign IDs, save IDs
      v < Debug || xout << "Finished loading Outcomes. Size = " << Outcomes.size() << endl;
    } else {
      if (CorpusType == "T") {
	v < Debug || xout << "Pre-Processing Tabular Data." << endl;
	realN = loadCuesOutcomes(Cues,Outcomes,CueFreq,OutcomeFreq,inputFile,NormCase);
	v < Debug || xout << "Completed pre-processing all Tabular Data." << endl;
      }
    }

    //      ifstream in1 (filename.c_str());
    // set the locale for the input stream.
    //      in1.imbue(loc);

    WordList words;
    ExtendedWordList Extwords;
    WordSet errors;
    if (CorpusType == "N") {
      v < Info || xout << "Reading entire normal corpus into memory." << endl << flush;
      string contents = getFile(inputFile);
      processNormalCorpus(contents,numThreads,NormCase,Cues,Outcomes,errors,realN,pathName,SeenCues,SeenOutcomes,inclusive,WindowSize, permute, sortWithinEvent);
      // Don't need the corpus anymore
      contents.clear();
    } else if (CorpusType == "E") {
      v < Info || xout << "Reading entire extended corpus into memory." << endl << flush;
      string contents = getFile(inputFile);
      processExtendedCorpus(contents,NormCase,Cues,Outcomes,errors,realN,pathName,SeenCues,SeenOutcomes,inclusive,WindowSize,sortWithinEvent);
      // Don't need the corpus anymore
      contents.clear();
    } else {
      processTabularCorpus(inputFile,Cues,Outcomes,CueFreq,OutcomeFreq,pathName,cueThreshold,outcomeThreshold,NormCase,CorpusType, sortWithinEvent);
    }

    if ((CorpusType == "N") | (CorpusType == "E")) {
      // Save list of unknown words.
      v < Info || xout<< "Number of unknown words: " << errors.size() << " words." << endl << flush;

      // After removing non-occurring Cues
      Dict CleanedCues = cleanCues(Cues,SeenCues);
      Dict CleanedOutcomes = cleanOutcomes(Outcomes,SeenOutcomes);
      v < Info || xout << "After cleaning out non-occurring cues, we went from " << Cues.size() << " to " << CleanedCues.size() << " cues." << endl;
      v < Info || xout << "After cleaning out non-occurring outcomes, we went from " << Outcomes.size() << " to " << CleanedOutcomes.size() << " outcomes."<< endl;

      // Use consecutive ids for occuring cues and outcomes.
      renumberIds(pathName, CleanedCues, CleanedOutcomes, sortWithinEvent);
      // Save Cues and Outcomes
      writeDict(CleanedCues,outputDirWithNamePrefix+".cues");
      writeDict(CleanedOutcomes,outputDirWithNamePrefix+".outcomes");
      writeErrors(outputDirWithNamePrefix + ".unknown.words.txt",errors);
      errors.clear();
    } else {
      // If CorpusType == T or R
      // Use ids in alphabetical order starting from zero.
      renumberIds(pathName, Cues, Outcomes, sortWithinEvent);
      // Save Cues and Outcomes
      writeDict(Cues,outputDirWithNamePrefix+".cues");
      writeDict(Outcomes,outputDirWithNamePrefix+".outcomes");
    }
    // Program over.
    v < Debug || xout << endl;
    /* close try block */
  }
  catch (const bad_alloc& x)
    {
      throw(Exception(string("Out of memory error: ") + x.what()));
    }

  v < Debug || xout << "Execution time: " <<  (time(NULL) - start) / 60.0 << " minutes. (Walltime)"<<  endl;
  v < Debug || xout << "Goodbye." << endl;
  return 0;
}




















// Crap from the past.

// const UChar32 apostrophe = 0x0027;
// const UChar32 dash1 = 0x2010;
// const UChar32 dash2 = 0x2011;
// const UChar32 dash3 = 0x2012;
// const UChar32 dash4 = 0x002D;
// const UChar32 dash5 = 0x00AD;

// const UChar32 period = 0x002E;
// const UChar32 comma = 0x002C;
// const UChar32 excl = 0x0021;
// const UChar32 ques = 0x003F;
// const UChar32 invexcl = 0x00A1;
// const UChar32 invques = 0x00BF;


    // Keep letters, apostrophes and hyphens inside words.
    // if ((u_isalnum(letter))
    // 	|| (letter == apostrophe)
    // 	) {
    //   output += letter;
    // }
// else {
//       if ((letter == dash1)
// 	|| (letter == dash2)
// 	|| (letter == dash3)
// 	|| (letter == dash4)
// 	|| (letter == dash5)
// 	) {
//       output += letter;
//     }
    // else {
    //   // put spaces around punctuation
    //   if ((letter == period)
    // 	  || (letter == comma)
    // 	  || (letter == ques)
    // 	  || (letter == excl)
    // 	  || (letter == invexcl)
    // 	  || (letter == invques)
    // 	  )
    // 	{
    // 	output += ' ';
    // 	output += letter;
    // 	output += ' ';
    //   }




// UnicodeString revSearch(size_t ID, Dict D) {
//   Dict::const_iterator it;
//   for (it = D.begin(); it != D.end(); ++it)
//     {
//       if (it->second == ID)
// 	{
// 	  return(it->first);
// 	}
//     }
//   return UnicodeString();
// }

// // Debugging: print event.
// void printEvent(Event event, Dict CuesDict, Dict OutcomesDict) {
//   xout << "Cues: ";
//   string output = "";
//   for (size_t i=0; i < event.Cues.size(); i++) {
//     UnicodeString temp = revSearch(event.Cues[i],CuesDict);
//     temp.toUTF8String(output);
//     xout << output << " ";
//     output.clear();
//   }
//   xout << "Outcomes: ";
//   for (size_t i=0; i < event.Outcomes.size(); i++) {
//     UnicodeString temp = revSearch(event.Outcomes[i],OutcomesDict);
//     temp.toUTF8String(output);
//     xout << output << " ";
//     output.clear();
//   }
//   xout << endl;
// }

// vim: set noexpandtab ts=8 sw=2:
