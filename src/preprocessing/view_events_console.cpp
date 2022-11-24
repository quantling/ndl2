#include "../common/view_events.h"
#include "../common/Exception.h"

#include <cstdio>
#include <cstdlib>
#include <string>
#include <iostream>
#include <unistd.h>

using namespace std;


void showUsage()
{
  cout << "Usage: view_events [OPTION] file_path_to_folder source_name" << endl
    << "Shows the specified events in human readable form." << endl
    << endl
    << "file_path_to_folder needs to be the path to a folder containing:" <<endl
    << "  $(source_name).cues" << endl
    << "  $(source_name).outcomes" << endl
    << "  $(source_name).events/" << endl
    << "Options:" << endl
    << "  -n number of events to consider (defaults to 1)" << endl
    << "  -s starting from event number (defaults to 0)" << endl
    << "  -e ending at event number" << endl
    << "  -f show frequency column" << endl
    << "  -v verbose output" << endl
    << "When -e is used, the value set for -n will be ignored." << endl
    << "If you use specify options, then you should use either" << endl
    << "  -n (with an optional -s) or" << endl
    << "  -s and -e" << endl
    << "Output: " << endl
    << "event_number\\t"
    << "cues_joined_with_underscore\\t"
    << "outcomes_joined_with_underscore" << endl;
}


int main(int argc, char* argv[])
{
    long long num_events = 1;
    long long start = 0;
    long long end = -1;
    bool verbose = false;
    bool show_frequency = false;
    char ch;
    while ((ch = getopt(argc, argv, "n:s:e:vf")) != -1) {
      switch(ch) {
        case 'n': //number of events
          num_events = atoi(optarg);
          break;
        case 's': //starting from event (first event)
          start = atoi(optarg);
          break;
        case 'e': //ending at event (last event)
          end = atoi(optarg);
          break;
        case 'v':
          verbose = true;
          break;
        case 'f':
          show_frequency = true;
          break;
        case '?':
        default:
          showUsage();
          return -1;
      }
    }

    if (end == -1) {
      !verbose || cout << "num_events: " << num_events << endl;
      end = start + num_events - 1;
    }
    else {
      if ((end - start) <= 0) {
        cerr << "End event is before start event!" << endl;
        return -1;
      }
    }
    if (num_events <= 0) {
      cerr << "At least 1 event is needed (-n needs to be > 0)!" << endl;
      return -1;
    }
    if (start < 0) {
      cerr << "The event to start from (-s) needs to be non-negative!" << endl;
      return -1;
    }

    // Apart from the options two arguments are necessary:
    //  filepath to directory containing the needed files
    //  name of the collection (i.e. the string before .cues and .outcomes)
    if (optind != (argc-2)) {
      showUsage();
		  return -1;
    }

    string filepath = argv[optind];
    string name = argv[optind + 1]; 

    try {
      viewEvents(start,end-start+1,filepath,name,show_frequency,true,verbose,"_");
    }
    catch(Exception ex) {
      cerr << ex.what() << endl;
    }
}
