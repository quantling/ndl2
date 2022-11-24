#ifndef serialization_H
#define serialization_H

#include <algorithm>
#include <fstream>
#include <vector>
// Obsoleted...
//#include <boost/archive/binary_oarchive.hpp>
//#include <boost/archive/binary_iarchive.hpp>
//#include <boost/serialization/vector.hpp>

// ****************************************************************************
// Defines, typedefs, and macros
// ****************************************************************************




struct Event
{
  std::vector<size_t> Cues;
  std::vector<size_t> Outcomes;
  size_t Freq;
};

//typedef std::pair< vector<size_t>, vector<size_t> > Event;

typedef std::vector<Event> Events;

bool operator==(const Event& lhs, const Event& rhs);

//void writeEvents (const std::string ofilename, const Events& myEvents);

//void readEvents(const std::string ofilename, Events& myEvents);

void writeEvents (const std::string ofilename, Events& myEvents);

void readEvents(const std::string ofilename, Events& myEvents);

size_t getNumEvents(const std::string ifilename);

void testIO(bool verbose = true);
void testDanks();
#endif // serialization_h
