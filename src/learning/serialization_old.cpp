#include <algorithm>
#include <fstream>
#include <vector>
#include <stdio.h>
#include "serialization_old.h"

using namespace std;

	bool oldEventFormat::operator==(const oldEventFormat::Event& lhs, const oldEventFormat::Event& rhs)
	{
	  return lhs.Cues==rhs.Cues && lhs.Outcomes==rhs.Outcomes;
	}

	// My own serialization scheme. Does not depend on BOOST.
	// Note: It is completely non-portable.
	//
	void oldEventFormat::readEvents(const string ifilename, oldEventFormat::Events& myEvents) {
	  myEvents.clear();
	  ifstream is(ifilename.c_str(), ios::in | ios::binary);
	  size_t i,j,NumCues,NumOutcomes,NumEvents,num;
	  is.read(reinterpret_cast<char *>(&num), sizeof(num));
	  NumEvents = num;
	  oldEventFormat::Event myEvent;
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
	    myEvents.push_back(myEvent);
	    myEvent.Cues.clear();
	    myEvent.Outcomes.clear();
	  }
	  is.close();
	}

	// Write out a list to a disk file
	void oldEventFormat::writeEvents (const string ofilename,const oldEventFormat::Events& myEvents) {
	  ofstream os(ofilename.c_str(), ios::binary);
	  size_t num = myEvents.size();
	  // Write the number of Events
	  os.write(reinterpret_cast<char *>(&num), sizeof(num));
	  oldEventFormat::Event myEvent; 
	  // loop through events
	  for (oldEventFormat::Events::const_iterator it = myEvents.begin(); it != myEvents.end(); ++it ) {
	    myEvent = *it;
	    num = myEvent.Cues.size();
	    //Write number of Cues in this event
	    os.write(reinterpret_cast<char *>(&num), sizeof(num));
	    std::vector<size_t>::iterator it2;
	    for (it2=myEvent.Cues.begin(); it2 != myEvent.Cues.end(); it2++ ) {
	      num = *it2;
	      // write the ids for these Cues
	      os.write(reinterpret_cast<char *>(&num), sizeof(num));
	    }
	    num = myEvent.Outcomes.size();
	    //Write number of Outcomes in this event
	    os.write(reinterpret_cast<char *>(&num), sizeof(num));
	    std::vector<size_t>::iterator it3;
	    for (it3=myEvent.Outcomes.begin(); it3 != myEvent.Outcomes.end(); it3++ ) {
	      num = *it3;
	      // write the ids for these Outcomes
	      os.write(reinterpret_cast<char *>(&num), sizeof(num));
	    }
	  }
	  os.close();
	}
