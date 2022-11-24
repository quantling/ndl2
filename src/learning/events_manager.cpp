#include "events_manager.h"
#include <iostream>

events_manager::events_manager(const vector<string> &paths,unsigned int num_threads)
{
	#pragma omp critical(events_manager)
	{
		this->event_paths = paths;	
		this->events = vector<Events*>{};
		this->events.resize(event_paths.size());
		for(size_t i = 0; i < this->events.size(); i++)
		{
			this->events[i] = nullptr;
		}
		this->num_threads = num_threads;
		this->num_done = vector<size_t>();
		this->num_done.resize(event_paths.size());
		for(size_t i = 0; i < num_done.size(); i++)
		{
			num_done[i] = 0;
		}
	}
}

const Events & events_manager::getEventsNumber(size_t num)
{
	Events* to_return;
	#pragma omp critical(events_manager)
	{
		if(this->events[num] == nullptr)
		{
			//Load the event.
			Events* current_events = new Events();
			readEvents(this->event_paths[num],*current_events);
			events[num] = current_events;
		}
		to_return = events[num];

	}
	return *to_return;
}

void events_manager::doneWithEvents(size_t num, bool quiet_mode, bool debug_output)
{
	#pragma omp critical(events_manager)
	{
		num_done[num]++;

		if(num_done[num] == num_threads)
		{
			//We'll no longer need it, so free the memory.
			delete events[num];
			events[num] = nullptr;
			if (debug_output) {
				std::cerr << " - F" << num << " finished" << endl;
			} else if (!quiet_mode) {
				std::cerr << num << " ";
				if (num % 10)
					std::cerr << endl;
			}
		}
	}
}

events_manager::~events_manager()
{
	for(size_t i = 0; i < this->events.size(); i++)
	{
		if(this->events[i] != nullptr)
			delete this->events[i];
	}
}


