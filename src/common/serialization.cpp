#include <algorithm>
#include <fstream>
#include <vector>
#include <stdio.h>
#include <iostream>
#include "serialization.h"
#include "Exception.h"

using namespace std;

bool operator==(const Event& lhs, const Event& rhs)
{
  return lhs.Cues==rhs.Cues && lhs.Outcomes==rhs.Outcomes;
}


size_t MAGIC_NUMBER=14159265;
size_t CURRENT_VERSION=215;

// My own serialization scheme. Does not depend on BOOST.
// Note: It is completely non-portable.
//
void readEvents(const string ifilename, Events& myEvents) {
  myEvents.clear();
  ifstream is(ifilename.c_str(), ios::in | ios::binary);
  size_t i,j,NumCues,NumOutcomes,NumEvents,num,magic,version;
  is.read(reinterpret_cast<char *>(&magic), sizeof(magic));
  is.read(reinterpret_cast<char *>(&version), sizeof(version));
  if ((magic != MAGIC_NUMBER) | (version < CURRENT_VERSION)) {
    throw Exception("Error in input file " + ifilename + ". Magic was " + to_string(magic) + " and version was " + to_string(version) + "... Exiting\n");
  }
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
}

// Write out a list to a disk file
void writeEvents (const string ofilename, Events& myEvents) {
  ofstream os(ofilename.c_str(), ios::binary);
  // Write magic and version number
  os.write(reinterpret_cast<char *>(&MAGIC_NUMBER), sizeof(MAGIC_NUMBER));
  os.write(reinterpret_cast<char *>(&CURRENT_VERSION), sizeof(CURRENT_VERSION));
  // Write the number of Events
  size_t num = myEvents.size();
  os.write(reinterpret_cast<char *>(&num), sizeof(num));
  Events::iterator it;
  Event myEvent; 
  // loop through events
  for (it = myEvents.begin(); it != myEvents.end(); ++it ) {
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
    num = myEvent.Freq;
    os.write(reinterpret_cast<char *>(&num), sizeof(num));
  }
  os.close();
}

size_t getNumEvents(const string ifilename) {
  ifstream is(ifilename.c_str(), ios::in | ios::binary);
  size_t num,magic,version;
  is.read(reinterpret_cast<char *>(&magic), sizeof(magic));
  is.read(reinterpret_cast<char *>(&version), sizeof(version));
  if ((magic != MAGIC_NUMBER) | (version < CURRENT_VERSION)) {
    throw Exception("Error in input file " + ifilename + ". Magic was " + to_string(magic) + " and version was " + to_string(version) + "... Exiting\n");
  }
  is.read(reinterpret_cast<char *>(&num), sizeof(num));
  is.close();

  return(num);
}

void testIO(bool verbose) {
  Events myEvents;  
  Events yourEvents;
  Event v1,v2;
  v1.Cues.push_back(423);
  v1.Cues.push_back(4998);
  v1.Outcomes.push_back(2343);
  v1.Outcomes.push_back(23343);
  v1.Outcomes.push_back(23443);
  v1.Outcomes.push_back(234423);
  v1.Freq=10;
  v2.Cues.push_back(9000);
  v2.Cues.push_back(334);
  v2.Outcomes.push_back(2234423);
  v2.Freq=20;
  myEvents.push_back(v1);
  myEvents.push_back(v2);
  writeEvents("test.bin",myEvents);
  readEvents("test.bin",yourEvents);
  remove("test.bin");
  if (myEvents == yourEvents) {
    if (verbose)
      cerr << "IO Test Passed"<<endl;
  } else {
    throw Exception("IO Test Failed... Exiting Program.");
  }
};

void testDanks() {
  size_t i;
  Events myEvents;  
  Event v1,v2,v3,v4,v5,v6,v7,v8;
  // Do 5 times!
  v1.Cues.push_back(0);
  v1.Cues.push_back(1);
  v1.Cues.push_back(2);
  v1.Outcomes.push_back(1);
  for (i = 0; i < 5; i++) {
    myEvents.push_back(v1);
  }
  v3.Cues.push_back(0);
  v3.Cues.push_back(1);
  v3.Outcomes.push_back(1);
  for (i = 0; i < 10; i++) {
    myEvents.push_back(v3);
  }
  v4.Cues.push_back(0);
  v4.Cues.push_back(1);
  v4.Outcomes.push_back(0);
  for (i = 0; i < 5; i++) {
    myEvents.push_back(v4);
  }
  v5.Cues.push_back(0);
  v5.Cues.push_back(2);
  v5.Outcomes.push_back(1);
  for (i = 0; i < 5; i++) {
    myEvents.push_back(v5);
  }
  v6.Cues.push_back(0);
  v6.Cues.push_back(2);
  v6.Outcomes.push_back(0);
  for (i = 0; i < 10; i++) {
    myEvents.push_back(v6);
  }
  v8.Cues.push_back(0);
  v8.Outcomes.push_back(0);
  for (i = 0; i < 5; i++) {
    myEvents.push_back(v8);
  }
  writeEvents("XXXXXXXXXXXXXXXXx.dat",myEvents);
};
















// #include <boost/archive/binary_oarchive.hpp>
//#include <boost/archive/binary_iarchive.hpp>
//#include <boost/serialization/vector.hpp>


// namespace boost { namespace serialization {
//     template<class Archive>
//     void serialize(Archive & ar, Event& v, const unsigned int version)
//     {
//       ar & BOOST_SERIALIZATION_NVP(v.Cues); 
//       ar & BOOST_SERIALIZATION_NVP(v.Outcomes);
//     }
//   } 
// }


// Create a list for testing

  

// Write out a list to a disk file
// void writeEvents (const string ofilename, const Events& myEvents) {
//   ofstream os(ofilename.c_str(), ios::binary);
//   boost::archive::binary_oarchive oar(os);
//   oar << myEvents;
//   os.close();
// }

// // Read events back in

// void readEvents(const string ifilename, Events& myEvents) {
//     ifstream is(ifilename.c_str(), ios::binary);
//     boost::archive::binary_iarchive iar(is);
//     iar >> myEvents;
//     is.close();
// }
