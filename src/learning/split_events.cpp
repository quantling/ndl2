#include "../common/serialization.h"
#include <iostream>
#include <unistd.h>
#include <string>

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/regex.hpp>

using namespace std;

namespace fs = boost::filesystem;

int main(int argc, char* argv[])
{
	if(argc != 3)
	{
		cerr<<"Please give the file name as first argument and the (number of files to split into)/3 as second."<<endl;
		exit(-1);
	}

	fs::path full_path = fs::system_complete(string{argv[1]});

	if(!fs::exists(full_path))
	{
		cerr<<"The path "<<full_path<<" does not exist"<<endl;
		exit(-1);
	}

	int howmany;
	try
	{
		howmany = std::stoi(string{argv[2]});
		if(howmany <= 0)
			throw string{""};

	}catch(...)
	{
		cerr<<"Invalid number given as second argument"<<endl;
		exit(-1);
	}
	Events events_from_file;
	readEvents(string{argv[1]}, events_from_file);	
	size_t basePtr = 0,endPtr = 0;
	for(size_t i = 0; i < 3; i++)
	{
		endPtr += (events_from_file.size()/3);
		if(i == 2)
			endPtr = events_from_file.size();

		Events newEvents;
		size_t counter = 0;
		size_t portionSize = (endPtr - basePtr)/howmany;
		for(size_t j = basePtr;; j++)
		{
			if(!((j - basePtr) %  portionSize) || ( j == endPtr))
			{
				string filename = string{"./events_"} + to_string(i) +  "_" + to_string(counter) + ".dat";
				writeEvents(filename,newEvents);
				counter++;
				newEvents.clear();
				if(j == endPtr)
					break;
			}
			newEvents.push_back(events_from_file[j]);
		}

		basePtr = endPtr;
	}

	cout<<"Done writing events";
	return 0;
}
