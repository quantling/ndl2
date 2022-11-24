#include "serialization.h"
#include "serialization_old.h"
#include <unistd.h>
#include <iostream>

using namespace std;

int main(int argc, char* argv[])
{
	if(argc != 2)
	{
		cerr<<"Please give the event file you wish to convert as first argument"<<endl;
		exit(-1);
	}	
	oldEventFormat::Events old_events;
	oldEventFormat::readEvents(string{argv[1]},old_events);
	
	Events corresponding_new_events;
	for(const auto &current_event : old_events)
	{
		Event corresponding_new_event;
		corresponding_new_event.Freq = 1;
		corresponding_new_event.Cues = current_event.Cues;
		corresponding_new_event.Outcomes = current_event.Outcomes;
		corresponding_new_events.push_back(corresponding_new_event);
	}
	writeEvents(string{argv[1]},corresponding_new_events);
	return 0;
}
