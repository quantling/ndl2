#include "../common/serialization.h"
#include <iostream>
#include <unistd.h>

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/regex.hpp>

using namespace std;

namespace fs = boost::filesystem;

int main(int argc, char* argv[])
{
	if(argc != 2)
	{
		cerr<<"Please give the directory containing events files as argument"<<endl;
		exit(-1);
	}

	fs::path full_path = fs::system_complete(string{argv[1]});

	if(!fs::exists(full_path))
	{
		cerr<<"The path "<<full_path<<" does not exist"<<endl;
		exit(-1);
	}else if(!fs::is_directory(full_path))
	{
		cerr<<"The path "<<full_path<<" is not a directory"<<endl;
		exit(-1);
	}
	fs::directory_iterator end_itr;
	vector<string> paths;
	for(fs::directory_iterator i(full_path);  i != end_itr; ++i)
	{
		string current_path = i->path().string();
		if((current_path.size() < 5) || (current_path.substr(current_path.size()-4,4) != ".dat"))
			continue;
		paths.push_back(current_path);
	}

	long long counter = 0;
	for(const auto &i:paths)
	{
		Events current_events{};
		readEvents(i,current_events);
		counter += current_events.size();
	}
	cout<<"There were "<<counter<<" events in the file"<<endl;
	return 0;
}
