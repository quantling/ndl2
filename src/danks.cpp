/* 
Copyright (C) 2012,2013,2014,2015  Cyrus Shaoul 

This file is part of the ndl package.

    ndl is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The ndl package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with HiDEx in the COPYING.txt file.
    If not, see <http://www.gnu.org/licenses/>.


*/

//****************************************************************************
//
// danks.cpp
//
// This file contains c++ functions that are exported to the ndl R package.
//
// Currently under development by Cyrus Shaoul. 
// cyrus.shaoul@uni-tuebingen.de
// 
//****************************************************************************

#include <cstdio>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <fstream>
#include <iterator>
#include <set>
#include <vector>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include "common/serialization.h"
#include "common/helper.h"
#include <RcppCommon.h>
#include <Rcpp.h>

//Removed due to issues with MacOsX and Windoze
//#include <sys/sysinfo.h>

// Namespaces to search
using namespace std;
using namespace Rcpp;

// Associative maps
typedef map<string,size_t> DictStr;
//typedef map<size_t,size_t> CMap;

// Cyrus says: This is my own non-portable binary data
// serialization scheme.  I created it to avoid any dependencies on
// external libraries, such as BOOST_SERIALIZATION.
// 
// WARNING: It is completely platform dependent. Please do not attempt
// to read in files created on a different system. It might not work
// due to byte ordering issues or unsigned integer length issues.
//
// To implement this format yourself, here is the what you need:
// The size of an unsigned integer. On most modern systems this is 4 bytes.
//
// The first 4 bytes are the number of events in the file (NumEvents)
// After that, you follow the following steps:
// Read the next 4 bytes. That is the number of Cues in the event (NumCues).
// Then read that many numbers, 4 bytes each. Each number is a CueID
// The next 4-bytes is the number of Outcomes (NumOutcomes).
// Next read that many numbers. Each number is an OutcomeID.
// The final 4 bytes is the frequency of that event.
// The next 4 bytes is the number of cues in the next event, and so on.
// Repeat until you have read all the events (NumEvents of them).
// CueID and OutcomeID files should be stored separately.
//
// As of version 0.2.15 of the ndl package, the binary format is now
// marked with version numbers and a special number to identify the
// type of binary file uniquely.
//

Events readEvents(const string ifilename) {
  // File type and version constants.
  size_t MAGIC_NUMBER=14159265;
  size_t CURRENT_VERSION=215;
  size_t i,j,NumCues,NumOutcomes,NumEvents,num,magic,version;
  // Cretate Events object.
  Events myEvents;
  // Open binary data file.
  ifstream is(ifilename.c_str(), ios::in | ios::binary);
  // Read first two numbers.
  is.read(reinterpret_cast<char *>(&magic), sizeof(magic));
  is.read(reinterpret_cast<char *>(&version), sizeof(version));
  if ((magic != MAGIC_NUMBER) || (version < CURRENT_VERSION)) {
    Rcerr << "Error loading input file " << ifilename << ". Wrong data format or the version is too old. Please update the data to the newest format and try running again. " << endl;
  }
  // Begin reading in data.
  is.read(reinterpret_cast<char *>(&num), sizeof(num));
  NumEvents = num;
  Event myEvent;
  for (i = 0; i < NumEvents; i++) {
    is.read(reinterpret_cast<char *>(&num), sizeof(num));
    NumCues = num;
    for (j = 0; j < NumCues; j++) {
      is.read(reinterpret_cast<char *>(&num), sizeof(num));
      myEvent.Cues.push_back(num);
    }
    is.read(reinterpret_cast<char *>(&num), sizeof(num));
    NumOutcomes = num;
    for (j = 0; j < NumOutcomes; j++) {
      is.read(reinterpret_cast<char *>(&num), sizeof(num));
      myEvent.Outcomes.push_back(num);
    }
    is.read(reinterpret_cast<char *>(&num), sizeof(num));
    myEvent.Freq = num;
    myEvents.push_back(myEvent);
    myEvent.Cues.clear();
    myEvent.Outcomes.clear();
    myEvent.Freq = 0;    
  }
  is.close();
  return(myEvents);
}

// Convert a string to an Interger
static size_t stringToInt(const string& str, const bool verbose)
{
    char * pEnd;
    const char * c_str = str.c_str();
    size_t result = static_cast<size_t>(strtol(c_str, &pEnd, 10));
    // If there are no leftover parts of the string after conversion
    if (pEnd == c_str+str.length()) {
        return result;
    }
    // else
    if (verbose) Rcerr << "While reading input: '"  <<  str  << "' is not an number!\n";
    stop("Ending Exectution");
    return 0;
}

// Extract a vector of ID's from a set of text-based cues
static vector<size_t> extract(string item, DictStr D) { 
  vector<size_t> output;
  vector<string> elems = split(item,'_');
  for (size_t i = 0; i < elems.size(); i++) {
    if (elems[i] != "") {
      output.push_back(D[elems[i]]);
    }
  }
  return(output);
}

// Process Event Data stored in the legacy format (three vectors,
// Cues, Outcomes and Freq)
static void ProcessLegacyData(
			      NumericMatrix& CoMat, 
			      NumericMatrix& CuMat, 
			      DictStr& CueDict, 
			      DictStr& OutcomeDict, 
			      StringVector& Cues, 
			      StringVector& Outcomes, 
			      NumericVector& Frequency, 
			      const bool RemoveDuplicates, 
			      const bool verbose)
{
  vector<size_t> EvCues, EvOutcomes;
  string CueStr, OutcomeStr;
  size_t i,j,lenCues, lenOutcomes;
  int h;
  size_t count = 0;
  size_t skip = 1000;
  //  int id;
  if (verbose) Rcerr << "Starting to Process Events. Number of events processed:\n";
  for (h=0 ; h < Frequency.size(); h++) {
    //    Rcerr << "Working on Event #" << h << endl;
    count++;
    if ((count > 100000) && (skip < 100000)) {
      skip = 100000;
    }
    if ((count +1) % 1000 == 0) {
      if (verbose) Rcerr << count + 1 << " ";
    }
    CueStr = Cues[h];
    EvCues = extract(CueStr,CueDict);
    OutcomeStr= Outcomes[h];
    EvOutcomes = extract(OutcomeStr,OutcomeDict);
    //  remove duplicate outcomes
    sort( EvOutcomes.begin(), EvOutcomes.end() );
    EvOutcomes.erase( unique( EvOutcomes.begin(), EvOutcomes.end() ), EvOutcomes.end() );
    // If requested, remove duplicate cues
    if (RemoveDuplicates) {
      sort( EvCues.begin(), EvCues.end() );
      EvCues.erase( unique( EvCues.begin(), EvCues.end() ), EvCues.end() );
    }
    // Find all cue-cue combinations and add them to the CueMat
    lenCues = EvCues.size();
    for (i = 0; i < lenCues; i++)  {
      for (j = i; j < lenCues; j++)  {
	//Increase count for this cue-cue co-occurrene, including Frequency info
	if ((EvCues[i] == EvCues[j]) && (i!=j)) {
	  continue;
	}
	CuMat(EvCues[i],EvCues[j]) = CuMat(EvCues[i],EvCues[j]) + Frequency[h];
	if (EvCues[i] != EvCues[j]) {
	  CuMat(EvCues[j],EvCues[i]) = CuMat(EvCues[i],EvCues[j]);
	} 
	//	Rcerr << "At: " << i << "," << j <<": Added cooc for cues: " << EvCues[j] << " with " << EvCues[i] << ",new count=" << CuMat(EvCues[i],EvCues[j])  << endl;
      }
    }
    // Find all the cue-outcome combinations and add them to the CoMat
    lenOutcomes = EvOutcomes.size();
    for (i = 0; i < lenCues; i++)	    {
      for (j = 0; j < lenOutcomes; j++)		{
	//Increase count for this cue-outcome co-occurrence, including Frequency info
	CoMat(EvCues[i],EvOutcomes[j]) = CoMat(EvCues[i],EvOutcomes[j]) + Frequency[h];
	//	Rcerr << "Added cooc for cue and outcome: " << EvCues[i] << " with " << EvOutcomes[j] << endl;
      }
    }
    // add background rates to Environ... 
    for (j = 0; j < lenOutcomes; j++) {
      CoMat(CueDict["Environ"],EvOutcomes[j]) += Frequency[h];
      //      Rcerr << "Added background : " << Frequency[h] << " to " << EvOutcomes[j]  << " environ = " << CueDict["Environ"] << endl;
    }
  }
  if (verbose) Rcerr << endl;
}

// Process data stored in the new binary, serialized format.
static void ProcessData( vector<string> files, 
			 NumericMatrix& CoMat, 
			 NumericMatrix& CuMat, 
			 const bool RemoveDuplicates, 
			 const bool verbose, 
			 const size_t MaxEvents, 
			 //			 CMap& CueMap, 
			 //			 CMap& OutcomeMap, 
			 const bool addBackground, 
			 NumericVector& CoFreq ){
  // Local Variables
  vector<size_t> EvCues, EvOutcomes;
  string CueStr, OutcomeStr;
  size_t g,h,i,j,lenCues, lenOutcomes, EvFreq;
  size_t count = 0;
  bool endItAll = false;
  Events theEvents;
  if (verbose) Rcerr <<  "Starting to Process Event Files. Currently processing file number: ";
  // loop over all the event data files
  for (g = 0; g < files.size(); g++) {
    if (endItAll) {
      break;
    }
    // Stop adding events if we are over our event limit.
    if (verbose) Rcerr << g << " ";
    // read the current set of events.
    theEvents = readEvents(files[g]);
    // Loop through a series of events.
    const size_t numEvents = theEvents.size();
    //    Rcerr << "Size of theEvents =  " << numEvents << endl;
    for (h = 0; h < numEvents; h++) {
      count++;
      if (count > MaxEvents) {
	if (verbose) Rcerr << "Hit MaxEvents. Stopping learning at " 
			   << count << " events." << endl << flush;
	endItAll = true;
	break;
      }
      if (count % 1000 == 0) {
      	if (verbose) Rcerr << "." << flush;
      }
      EvCues = theEvents[h].Cues;
      EvOutcomes = theEvents[h].Outcomes;
      EvFreq = theEvents[h].Freq;
      // remove duplicate outcomes.
      sort( EvOutcomes.begin(), EvOutcomes.end() );
      EvOutcomes.erase( unique( EvOutcomes.begin(), EvOutcomes.end() ), 
			EvOutcomes.end() );
      // If requested, remove duplicate cues
      if (RemoveDuplicates) {
	sort( EvCues.begin(), EvCues.end() );
	EvCues.erase( unique( EvCues.begin(), EvCues.end() ), 
		      EvCues.end() );
      }
      lenCues = EvCues.size();      
      //      Rcerr << "Length of Event Cues=" << lenCues << endl;
      lenOutcomes = EvOutcomes.size();
      //      Rcerr << "Length of Outcomes=" << lenOutcomes << endl;
      // Remap Cues
      //      for (i = 0; i < lenCues; i++) {
	//	Rcerr << "Mapping " << EvCues[i] << " to " << CueMap[EvCues[i]] << endl;
      //	EvCues[i] = CueMap[EvCues[i]];
      //      }
      // Remap Outcomes
      //      for (i = 0; i < lenOutcomes; i++) {
	//	Rcerr << "Mapping " << EvOutcomes[i] << " to " << OutcomeMap[EvOutcomes[i]] << endl;
      //	EvOutcomes[i] = OutcomeMap[EvOutcomes[i]];
      //      }
      // Find all cue-cue combinations and add them to the CueMat
      for (i = 0; i < lenCues; i++) {
	for (j = i; j < lenCues; j++) {
	  // Only count identical cues once
	  if ((EvCues[i] == EvCues[j]) && (i != j)) {
	    continue;
	  }
	  CuMat(EvCues[i],EvCues[j]) += EvFreq;
	  if (EvCues[i] != EvCues[j]) {
	    CuMat(EvCues[j],EvCues[i]) += EvFreq;
	  }
	}
      }
      for (i = 0; i < lenCues; i++) {
	  for (j = 0; j < lenOutcomes; j++) {
	    //Increase count for this cue-cue co-occurrence
	    CoMat(EvCues[i],EvOutcomes[j])  += EvFreq;
	  }
      }
      // Add background rates to Environ, if requested
      if (addBackground) {
	for (j = 0; j < lenOutcomes; j++) {
	  CoFreq(EvOutcomes[j]) += EvFreq;
	}
      }
    }
    theEvents.clear();
  }
  if (verbose) Rcerr << endl << "Processed a total of " << count << " discrete events." << endl;
}

// Extract all the unique elements from a vector and return them in a vector.
size_t extractUnique(StringVector &items, DictStr &output, StringVector &Uniq) {
  set<string> uniq;
  vector<string> elems;
  string str;
  // split all strings on the underscore and insert into a set (forcing uniqueness).
  for (int i=0; i < items.size(); i++) {
    str = items[i];
    elems = split(str,'_');
    copy( elems.begin(), elems.end(), inserter( uniq, uniq.end() ) );
  }
  set<string>::iterator theIterator;
  size_t id = 0;
  // return a list of items with sequential id numbers.
  for( theIterator = uniq.begin(); theIterator != uniq.end(); theIterator++ ) {
    output[*theIterator] = id;
    id++;
    Uniq.push_back(*theIterator);
  }
  return(id);
}

StringVector readInput(const string filename,  const bool verbose) {
  //first count the number of lines...
  ifstream ifs(filename.c_str());
  size_t numlines = count(istreambuf_iterator<char>(ifs), istreambuf_iterator<char>(), '\n');
  //allocate vector
  //  Rcerr << "Num Lines = " << numlines << endl;
  StringVector output(numlines);
  // rewind file.
  ifs.seekg(0);
  // other variables.
  size_t ItemID = 0;
  string Item = "";
  string line = "";
  size_t count=0;
  vector<string> elems;
  while ( getline(ifs,line) )  {
    if (ifs.fail()) {
      ostringstream buffer;
      buffer << "Error Reading Input File: " << filename;
      stop(buffer.str());
    }
    elems = split(line,'\t');
    if (elems.size() != 2) {
      ostringstream buffer;
      buffer << "Bad formating in file: " << filename 
	     << ", line # " <<  count  
	     << ". Please check input and try again."<< endl;
      stop(buffer.str());
    }
    Item = elems[0];
    ItemID = stringToInt(elems[1],verbose);
    //
    // push_back is very slow! Do not use it here!!!!
    //
    //    output.push_back(Item);
    //
    // direct assignment is much faster!!!
    output[ItemID] = Item;
    count++;
    //    if (verbose)  {
    //      if ((count - 1) % 1000 == 0) {
    //	Rcerr << ItemID << endl;  
    //      }
    //}
    line.clear();
  }
  ifs.close();
  output.erase(output.begin()+count,output.end());
  return(output);
}


// get all files in a directory
void getDir (string dir, vector<string> &files)
{
    DIR *dp;
    struct dirent *dirp;
    if((dp = opendir(dir.c_str())) == NULL) {
      ostringstream buffer;
      buffer << "Fatal error opening directory " << dir << " . Does the directory exist?";
      stop(buffer.str());
    }
    while ((dirp = readdir(dp)) != NULL) {
      string path = dir + "/" + string(dirp->d_name);
      size_t found = path.find(".dat");
      if (found!=string::npos) {
	files.push_back(path);
      }
    }
    closedir(dp);
}

// Learn the cue-outcome relationships from individual exposure events.

/* This caused the problems!!!!

  List learn(string data, const bool RemoveDuplicates, const bool verbose, const size_t MaxEvents, const bool addBackground) 

*/

//' Count cue-outcome co-occurences needed to run the Danks equations.
//' 
//' An internal function to count cue-outcome co-occurrences.
//' 
//' This function calls an Rcpp function of the same name to process the data in
//' the compact data format.
//' 
//' @param data A directory where the binary event data files are located.
//' @param RemoveDuplicates A logical specifying whether multiple occurrences of
//' a Cue in conjunction with an Outcome shall each be counted as a distinct
//' occurrence of that Cue (\code{FALSE}), or only as a single occurrence
//' (\code{TRUE}: default).
//' @param verbose Display diagnostic messages or not.
//' @param MaxEvents The total number of events to learn from before stopping
//' learning. Checked one time per compact data file.
//' @param addBackground Option to add background rates.
//' @return A list of two matrices with cue-cue coocurrences and cue-outcome
//' cooccurrences and a vector with background rates.
//' @note No temporary files are used.
//' @section Acknowledgements: Thanks to all the testers!
//' @author Cyrus Shaoul
//' @seealso \code{\link{estimateActivations}, \link{ndlCuesOutcomes},
//' \link{estimateWeightsCompact}, \link{danks}, \link{plurals}, \link{serbian}}
//' @references
//' 
//' Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix, P. and
//' Marelli, M., An amorphous model for morphological processing in visual
//' comprehension based on naive discriminative learning. Psychological Review,
//' 118, 438-482.
//' @keywords classif
//' @examples
//' 
//' #None (internal function)
//' 
// [[Rcpp::export]]
SEXP learn(std::string data, const bool RemoveDuplicates, const bool verbose, const size_t MaxEvents, const bool addBackground)
{
  if (verbose) Rcerr << "Entering Rcpp-based learning module." << endl;  

  try
    {
      // Start timing execution.
      //      time_t start = time(NULL);

      StringVector Cues, Outcomes;
      string CueFile = data + ".cues";
      string OutcomeFile = data + ".outcomes";
      string EventDir = data + ".events";

      // load cues
      Cues = readInput(CueFile,verbose);
      size_t NumCues = Cues.size();
      //      if (verbose)  Rcerr << "Loaded "<< NumCues << " cues. " << endl << flush;

      // load outcomes
      Outcomes = readInput(OutcomeFile,verbose);
      size_t NumOutcomes = Outcomes.size();
      //      if (verbose)  Rcerr << "Loaded "<< NumOutcomes << " outcomes. " << endl << flush;
      if (verbose)  Rcerr << "Loaded the data and got " << NumCues 
			  << " unique cues and " << NumOutcomes 
			  << " unique outcomes.\n"; 
      if (NumCues < 2 ) {
	Rcerr << "ERROR: Insufficient number of cues. Cannot continue. Returning Empty Weight Matrix.\n" ;
	return List::create();
      }
      if (NumOutcomes < 2 ) {
	Rcerr << "ERROR: Insufficient number of outcomes. Cannot continue. Returning Empty Weight Matrix.\n" ;
	return List::create();
      }
      if ((NumCues > 20000) && verbose) {
	Rcerr << "NOTICE: Your data has more than 20000 cues. Switching to a lower-rank approximation of the pseudoinverse so that computations are feasible.\n" ;
      }
      // Check for max R matrix size
      if ( (NumCues * NumOutcomes) > 2147483647  ) {
	ostringstream buffer;
	buffer << "You weight matrix is going to be too large! \nYou have requested a network with " << NumCues * NumOutcomes << " weights, \nbut R can only create matrices that contain at most 2147483647 elements.\nPlease reduce the number of cues or outcomes.";
	stop(buffer.str());
      }
      //Initialize matrices
      NumericMatrix CoMat(NumCues,NumOutcomes);
      if (verbose) Rcerr << "Created a Cue-Outcome matrix that is " 
			 << NumCues << " by " << NumOutcomes 
			 << " in size." << endl;
      NumericMatrix CuMat(NumCues,NumCues);
      if (verbose) Rcerr << "Created a Cue-Cue matrix that is " 
			 << NumCues << " by " << NumCues 
			 << " in size." << endl;

      NumericVector CoFreq(NumOutcomes);

      // Get list of event files
      string dir = EventDir;
      vector<string> files = vector<string>();
      getDir(dir,files);
      sort(files.begin(), files.end(), compareEventFileNames);
      // Main loop to process learning events.
      if (verbose)  Rcerr << "Entering ProcessData. " << endl << flush;
      ProcessData(files, CoMat, CuMat, RemoveDuplicates,
 		  verbose, MaxEvents, addBackground, CoFreq);
      //      ProcessData(files, CoMat, CuMat, RemoveDuplicates,
      // 		  verbose, MaxEvents, CueMap,
      // 		  OutcomeMap, addBackground, CoFreq);
      // Create dimname lists
      List CuDims = List::create(Cues, Cues);
      List CoDims = List::create(Cues, Outcomes);

      // and assign it to the matrices
      CuMat.attr("dimnames") = CuDims;
      CoMat.attr("dimnames") = CoDims;

      if (verbose) Rcerr << "Finished processing learning events." << endl;
      if (verbose) Rcerr << "Leaving Rcpp-based learning module." << endl;

      return List::create(Named("CueCue") = CuMat,
       			  Named("CueOutcome") = CoMat,
       			  Named("OutcomeFreq") = CoFreq
       			  );
    }
  catch (const bad_alloc& x)
    {
      Rcerr << " Out of memory error: " << x.what() << endl;
      return List::create(1);
    }
  catch(const std::exception &ex ) 
    {             // or use END_RCPP macro
      forward_exception_to_r( ex );
    } 
  catch(...) 
    { 
      ::Rf_error( "c++ exception (unknown reason)" ); 
    }
  return List::create(1);
}

// Use the legacy format, accept a DataFrame and return the correct Matrices.

//' Count cue-outcome co-occurences needed to run the Danks equations.
//' 
//' An internal function to count cue-outcome co-occurrences.
//' 
//' This function calls an Rcpp function of the same name to process the data in
//' the DFin data frame.
//' 
//' @param DFin A dataframe, as defined in the documentation for
//' \link{estimateWeights}.
//' @param RemoveDuplicates A logical specifying whether multiple occurrences of
//' a Cue in conjunction with an Outcome shall each be counted as a distinct
//' occurrence of that Cue (\code{FALSE}), or only as a single occurrence
//' (\code{TRUE}: default).
//' @param verbose Display diagnostic messages or not.
//' @return A list of two matrices with cue-cue coocurrences and cue-outcome
//' cooccurrences.
//' @note No temporary files are used.
//' @section Acknowledgements: Thanks to all the testers out there! Martijn, you
//' know who you are.
//' @author Cyrus Shaoul
//' @seealso \code{\link{estimateActivations}, \link{ndlCuesOutcomes},
//' \link{estimateWeights}, \link{danks}, \link{plurals}, \link{serbian}}
//' @references
//' 
//' Baayen, R. H. and Milin, P.  and Filipovic Durdevic, D. and Hendrix, P. and
//' Marelli, M., An amorphous model for morphological processing in visual
//' comprehension based on naive discriminative learning. Psychological Review,
//' 118, 438-482.
//' @keywords classif
//' @examples
//' 
//' #None (internal function)
//' 
// [[Rcpp::export]]
SEXP learnLegacy(SEXP DFin, const bool RemoveDuplicates, const bool verbose)
{
  try
    {
      // Start timing execution.
      //      time_t start = time(NULL);

      if (verbose) Rcerr << "Entering Rcpp-based learning module." << endl;
      if (verbose) Rcerr << "First Pass: Making Lists of unique Cue and Outcome types.\n";
      // Get incoming DataFrame
      DataFrame DF = DataFrame(DFin);
      StringVector Cues = DF["Cues"];
      StringVector Outcomes = DF["Outcomes"];
      NumericVector Frequency = DF["Frequency"];
      // Maps to look up strings
      DictStr UniqCues, UniqOutcomes;
      // Vector to hold IDs
      StringVector CueList, OutcomeList;
      size_t max_value = 0;
      //      UniqCues["Environ"] = UniqCues.size() - 1;
      // Extract Outcomes from the dataframe.
      max_value = extractUnique(Outcomes,UniqOutcomes,OutcomeList);
      // Extract Cues from the dataframe.
      max_value = extractUnique(Cues,UniqCues,CueList);

      // // Find maximum Cue Id we can add
      // typedef Dict::iterator iter;
      // iter it = UniqCues.begin();
      // iter end = UniqCues.end();
      // size_t max_value = it->second;
      // string str = it->first;
      // for( ; it != end; ++it) {
      // 	if(*it->second > max_value) {
      // 	  max_value = it->second;
      // 	  str = it->first;
      // 	}
      // }

      // add background rate Cue to Cue List.
      CueList.push_back("Environ");
      // index for Environ Cue....
      UniqCues["Environ"] = max_value;


      size_t NumCues = UniqCues.size();
      size_t NumOutcomes = UniqOutcomes.size();
      if (verbose) Rcerr << "Finished first pass and got " << NumCues << " unique cues and " << NumOutcomes << " unique outcomes.\n";

      if (verbose) Rcerr << "Starting Memory allocation for the Cue-Outcome matrix. "  << endl;
      NumericMatrix CoMat(NumCues,NumOutcomes);
      if (verbose) Rcerr << "Created a Cue-Out matrix that is " << NumCues << " by " << NumOutcomes << " in size." << endl;
      
      if (verbose) Rcerr << "Starting Memory allocation for the Cue-Cue matrix. "  << endl;
      // //Initialize matrix
      NumericMatrix CuMat(NumCues,NumCues);
      if (verbose) Rcerr << "Created a Cue-Cue matrix that is " << NumCues << " by " << NumCues << " in size." << endl;
      if (verbose) Rcerr << "Second Pass: Counting Events.\n";      
      // // Main loop to process learning events.
      ProcessLegacyData(CoMat, CuMat, UniqCues, UniqOutcomes, Cues, Outcomes, Frequency, RemoveDuplicates, verbose);
 
      //add row and column names from these Lists.      
      List CuDims = List::create(CueList, CueList);
      List CoDims = List::create(CueList, OutcomeList);

      // and assign it to the matrix 2x
      CuMat.attr("dimnames") = CuDims;
      CoMat.attr("dimnames") = CoDims;

      if (verbose) Rcerr << "Finished processing all learning events" << endl;
      //      if (verbose) Rcerr << "Processing All events took: " <<  (time(NULL) - start) / 60.0 << " minutes. (Walltime)"<<  endl;
      if (verbose) Rcerr << "Leaving Rcpp-based learning module." << endl;

      return List::create(Named("CueCue") = CuMat,
			  Named("CueOutcome") = CoMat);

    }
  catch (const bad_alloc& x)
    {
      Rcerr << " Out of memory error: " << x.what() << endl;
    }
  catch(const std::exception &ex ) 
    {             // or use END_RCPP macro
      forward_exception_to_r( ex );
    } 
  catch(...) 
    { 
      ::Rf_error( "c++ exception (reason unknown)" ); 
    }
  return List::create();
}



