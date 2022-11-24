#ifndef serialization_old_H
#define serialization_old_H

#include <algorithm>
#include <fstream>
#include <vector>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/serialization/vector.hpp>

// ****************************************************************************
// Defines, typedefs, and macros
// ****************************************************************************

namespace oldEventFormat
{
	using namespace std;
	struct Event
	{
	  std::vector<size_t> Cues;
	  std::vector<size_t> Outcomes;
	};

	bool operator==(const oldEventFormat::Event& lhs, const oldEventFormat::Event& rhs);

	using Events = std::vector<oldEventFormat::Event>;

	void writeEvents (const std::string ofilename, const oldEventFormat::Events& myEvents);

	void readEvents(const std::string ofilename, oldEventFormat::Events& myEvents);

}

#endif // serialization_h
