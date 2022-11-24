#ifndef EVENTS_MANAGER_H
#define EVENTS_MANAGER_H

#include <vector>
#include <string>
#include "../common/serialization.h"

using namespace std;

class events_manager
{
	public:
	events_manager() = delete;
	events_manager(const events_manager&) = delete;
	events_manager(const vector<string> &paths,unsigned int num_threads);
	const Events &getEventsNumber(size_t num);
	void doneWithEvents(size_t num, bool quiet_mode, bool debug_output);
	~events_manager();

	private:

	vector<string> event_paths;
	vector<Events*> events;
	vector<size_t> num_done;
	unsigned int num_threads;
	
};

#endif
