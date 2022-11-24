#include <unistd.h>
#include <iostream>
#include <sstream>
#include "../common/Exception.h"
#include "ndlpreproc.h"

using namespace std;

// Print usage.
void usage(ostream& out, const char* programName)
{
  out << "This program converts various types of text files into compact, binary" << endl
      << "event files that can be used by the NDL R package (version 0.2.15 and later)." << endl
      << "Two types of corpora are allowed: text corpora or tabluar data." << endl
      << endl
      << "Tabular data must be in tab-separated format, one event per line." << endl
      << "The first element is a _ separated list of cues," << endl
      << "the second is a _ separated list of outcomes, and " << endl
      << "the third is the frequency." << endl
      << endl
      << "Usage: " << programName << endl
      << "  [-f <input filename>]"  << endl
      << "  [-t Corpus Type, (T)abular Events with cue limits, (R)aw tabular events," << endl
      << "                   (N)ormal Corpus or (E)xtended Format, default (N)ormal]" << endl
      << "  [-l Case Normalization, 0 or 1]" << endl
      << "  [-v Verbosity, default 2 (info); 0 (quiet), 1 (warnings) and 3 (debug)]" << endl
      << endl
      << "If the corpus type is (N) or (E), then the following options are also required:" << endl
      << "  [-o Outcome list]" << endl
      << "  [-a Alphabet list (Cue List when permute = 0)]" << endl
      << "  [-n Max N, default 3; 1 or 2 also possible (target position when permute = 0)]" << endl
      << "  [-w WindowSize, currently 0 or 1 (ignored when permute is 0)]" << endl
      << "  [-p Permute, 0 or 1, default is 1, set to 0 for whole word cues]" << endl
      << "  [-k Keep the original cue and outcome order within an event]" << endl
      << endl
      << "If the corpus type is (T), you will need to specify" << endl
      << "  the number of highest frequency cues by setting [-d N] and" << endl
      << "  the number of highest frequency outcomes with [-e N]" << endl
      << endl
      << "If the corpus type is (R), you will need to specify the alphabet and outcome list." << endl
      << "--------------------" << endl;
}

int main(int argc, char *argv[])
{
  // Start timing execution.
  int ch;
  string filename;
  string cueFile;
  string outcomeFile;
  string maxNinput;
  size_t maxN;
  string WindowSizeInput;
  size_t WindowSize;
  string NormCaseInput;
  string inclusiveInput;
  string permuteInput;
  string CorpusType;
  string cueThresholdInput;
  size_t cueThreshold = 0;
  string outcomeThresholdInput;
  size_t outcomeThreshold = 0;
  bool NormCase;
  bool permute;
  bool inclusive;
  bool keepCueOutcomeOrder = false;
  int numThreads;
  string verbosityInput;
  Verbosity v;

  try
  {
      while ((ch = getopt(argc, argv, "w:i:a:o:f:n:l:d:e:t:kv:p:?")) != -1)
        {
	  switch (ch)
            {
            case 'f':
	      filename = optarg;
	      break;
            case 'o':
	      outcomeFile = optarg;
	      break;
            case 'a':
	      cueFile = optarg;
	      break;
            case 'n':
	      maxNinput = optarg;
	      break;
            case 'w':
	      WindowSizeInput = optarg;
	      break;
            case 'l':
	      NormCaseInput = optarg;
	      break;
            case 'i':
	      inclusiveInput = optarg;
	      break;
            case 'p':
	      permuteInput = optarg;
	      break;
            case 'd':
	      cueThresholdInput = optarg;
	      break;
            case 'e':
	      outcomeThresholdInput = optarg;
	      break;
            case 't':
	      CorpusType = optarg;
	      break;
	    case 'k':
	      keepCueOutcomeOrder = true;
	      break;
            case 'v':
	      verbosityInput = optarg;
	      break;
            case '?':
	      usage(cout, argv[0]);
	      return 0;
            default:
	      usage(cerr, argv[0]);
	      throw Exception("No arguments specified. Exiting.");
            }
        }

      if (verbosityInput == "0")
	v = Quiet;
      else if (verbosityInput == "1")
	v = Warn;
      else if (verbosityInput == "3")
	v = Debug;
      else {
	v = Info;
	if (verbosityInput != "2")
	  cerr << "Using default verbosity 2 (info)." << endl;
      }

      if (CorpusType == "" || CorpusType == "N") {
	v < Info || cerr << "Using default Corpus Type (full text)." << endl;
	CorpusType = "N";
      } else {
	if (CorpusType == "E") {
	v < Info || cerr << "Using Extended Corpus Type (One event per line)." << endl;
	CorpusType = "E";
	} else {
	  if (CorpusType == "T") {
	    v < Info || cerr << "Using Tabular Corpus Type (One event per line) with frequency based cue and outcome selection." << endl;
	    CorpusType = "T";
	  } else {
	    if (CorpusType == "R") {
	      v < Info || cerr << "Using Raw Tabular data (One event per line) with no frequency based selection." << endl;
	      CorpusType = "R";
	    }
	  }
	}
      }

      if (filename == "") {
	usage(cerr, argv[0]);
	throw Exception("No input file (-f) specified. Exiting.");
      }

      // Non-Tabular options
      if ((CorpusType == "R") | (CorpusType == "N") | (CorpusType == "E") ) {
	if (outcomeFile == "") {
	  usage(cerr, argv[0]);
	  throw Exception("No outcome file (-o) specified. Exiting.");
	}

	if (cueFile == "") {
	  usage(cerr, argv[0]);
	  throw Exception("No alphabet file (-a) specified. Exiting.");
	}

	if (maxNinput == "") {
	  v < Info || cerr << "Using default MaxN of 3." << endl;
	  maxN = 3;
	} else {
	  istringstream in(maxNinput);
	  in >> maxN;
	  if (maxN < 1) {
	    throw Exception("MaxN must be greater than zero. Exiting.");
	  }
	  v < Debug || cerr << "Using  " << maxN << "-grams" << endl;
	}


	if (inclusiveInput == "") {
	  v < Info || cerr << "Using default inclusiveness (non-inclusive, only one cue size used, currently: " << maxN << " letters)."<< endl;
	  inclusive = 0;
	} else {
	  istringstream inclu(inclusiveInput);
	  inclu >> inclusive;
	}
      }

      if (permuteInput == "") {
	v < Info || cerr << "Using default permute setting (permuting letter cues is on.) "<< endl;
	permute = 1;
      } else {
	istringstream permu(permuteInput);
	permu >> permute;
      }

      if ((CorpusType == "N") | (CorpusType == "E") ) {
	// Left blank: Default to size = 1
	if (WindowSizeInput == "") {
	  v < Info || cerr << "Using default windows size of 1 on either side of the target (word trigrams)." << endl;
	  WindowSize = 1;
	} else {
	  // not left blank: Get arg
	  istringstream in(WindowSizeInput);
	  in >> WindowSize;
	  // If it bigger than 1 and permute is on:
	  //	  if (WindowSize > 1 and permute) {
	  //	    throw Exception("Only Window Sizes of 0 or 1 are currently supported in permutation mode. Exiting.");
	  //	  }
	  if (WindowSize < 1 and not(permute)) {
	    throw Exception("Only Window Sizes of 1 or more are currently supported in non-permutation mode. Exiting.");
	  }
	  // All good. Report window size
	  v < Info || cerr << "Using window size of " << WindowSize <<"  word(s) (on either side of the target)." << endl;
	}
      }

      if (not(permute)) {
	if (maxN > ((WindowSize*2) +1)) {
	  ostringstream out;
	  out << "Target outcome word position " << maxN << " is outsize the max position in the window, " << (WindowSize*2) +1 << "  Exiting.";
	  throw Exception(out.str());
	}
      }

      if (CorpusType == "T") {
	if (cueThresholdInput == "") {
	  v < Info || cerr << "Using default max number of cues of 5000." << endl;
	  cueThreshold = 5000;
	} else {
	  istringstream in(cueThresholdInput);
	  in >> cueThreshold;
	  if (cueThreshold < 1) {
	    throw Exception("Max number of cues must be 1 or greater.");
	  }
	  v < Debug || cerr << "Using max number of highest freq cues of " << cueThreshold << endl;
	}
	if (outcomeThresholdInput == "") {
	  v < Info || cerr << "Using default max number of outcomes of 60000." << endl;
	  outcomeThreshold = 60000;
	} else {
	  istringstream in(outcomeThresholdInput);
	  in >> outcomeThreshold;
	  if (outcomeThreshold < 1) {
	    throw Exception("Max number of outcomes must be 1 or greater.");
	  }
	v < Debug || cerr << "Using max number of highest freq outcomes of " << outcomeThreshold << endl;
      }
    }

    if (NormCaseInput == "") {
      v < Info || cerr << "Using default Case Normalization (All letters normalized to lower case)." << endl;
      NormCase = 1;
    } else {
      istringstream NC(NormCaseInput);
      NC >> NormCase;
    }

    // Test for Open MP settings.
    char * Envnumthreads = getenv("OMP_NUM_THREADS");
    if (Envnumthreads) {
      v < Info || cerr << "Parallel processing enabled. Number of OpenMP threads to be used is: " << Envnumthreads << endl;
      numThreads =  atoi(Envnumthreads);
    } else {
	v < Warn || cerr << "OpenMP not active. Continuing without parallel processing." << endl
	  << "Please set OMP_NUM_THREADS in your environment to use OpenMP." << endl;
	numThreads =  1;
      }
  }
  catch (const exception& e)
  {
      cerr << "!!!!!ERROR: " << e.what() << '\n';
      return 1;
  }
  return preprocess(filename, cueFile, outcomeFile, filename, numThreads,
		    CorpusType, maxN, WindowSize, NormCase, inclusive, permute,
		    cueThreshold, outcomeThreshold, !keepCueOutcomeOrder,
		    "", "", v);
}


// vim: set noexpandtab ts=8 sw=2:
