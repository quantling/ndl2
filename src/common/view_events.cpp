#include "view_events.h"

#ifndef USING_RCPP
  #define USING_RCPP 0
#endif

#include "serialization.h"
#include "Exception.h"
#include "helper.h"

#include <cstdio>
#include <cstdlib>
#include <map>

#include <iostream>
#include <algorithm>
#include <unistd.h>

using namespace std;

RETURN_TYPE viewEvents(long long first_event, long long num_events,
    std::string filepath, std::string name, bool show_frequency,
    bool show_event_number, bool verbose, const std::string glue)
{
    if (filepath.substr(filepath.size()-1,1) != "/")
      filepath = filepath + "/";

    if (num_events <= 0)
        throw Exception("At least one event is needed. Zero and negative numbers are not accepted.");

    long long endExcl = first_event + num_events; // last event exclusive
    RevDictString id2cue = readRevDictString(filepath + name + ".cues", verbose);
    RevDictString id2outcome = readRevDictString(filepath + name + ".outcomes", verbose);

    vector<string> paths = getEventFilePaths(filepath + name + ".events/");
    if (paths.size() == 0)
     throw Exception("No event files found in " + filepath + name + ".events/"); 

    sort(paths.begin(), paths.end(), compareEventFileNames);

    Events current_events{};
    readEvents(paths[0], current_events); //TODO: don't read first file twice

#if USING_RCPP
    long long num_events_available = 0;
    long long num_events_wanted = first_event + num_events; // zero-based ids
    for (const auto &current_path:paths) {
      num_events_available += getNumEvents(current_path);
      if (num_events_available > num_events_wanted)
        break;
    }

    if (num_events_wanted > num_events_available) {
      num_events = num_events_available - first_event;
      if (num_events < 0)
        num_events = 0;
      !verbose || cout << "Only " << num_events << " event(s) available with event number(s) >= " << first_event << endl;
    }

    Rcpp::IntegerVector event_number_vec(num_events);
    Rcpp::CharacterVector cues_vec(num_events);
    Rcpp::CharacterVector outcomes_vec(num_events);
    Rcpp::IntegerVector frequency_vec(show_frequency ? num_events : 0);
#endif

    long long prev_event_count = 0;
    for(const auto &current_path:paths)
    {
      long long num_events_in_file = getNumEvents(current_path);
      !verbose || cout << "File: " << current_path << ", num events = " << num_events_in_file << endl;

      if ((prev_event_count + num_events_in_file) > first_event) {
        readEvents(current_path, current_events);
        !verbose || cout << "Parsing " << current_path << endl;

        long long first_entry_to_use = first_event > prev_event_count ?
          (first_event - prev_event_count) : 0;
        !verbose || cout << "first_entry_to_use: " << first_entry_to_use << endl;

        long long last_entry_to_use = (endExcl - 1) - prev_event_count;
        if (last_entry_to_use >= num_events_in_file) {
          last_entry_to_use = num_events_in_file - 1;
        } 
        !verbose || cout << "last_entry_to_use: " << last_entry_to_use << endl;

        for (unsigned int j = first_entry_to_use; j <= last_entry_to_use; ++j) {
          string cues = lookupAndImplode(current_events[j].Cues, id2cue, glue);
          string outcome = lookupAndImplode(current_events[j].Outcomes, id2outcome, glue);
#if USING_RCPP
          long long idx = prev_event_count + j - first_event;
          if (show_event_number)
            event_number_vec[idx] = prev_event_count + j;
          cues_vec[idx] = cues;
          outcomes_vec[idx] = outcome;
          if (show_frequency)
            frequency_vec[idx] = current_events[j].Freq;
#else
          if (show_event_number)
            cout << prev_event_count + j << ":" << "\t";
          cout << cues << "\t" << outcome;
          if (show_frequency)
            cout << "\t" << current_events[j].Freq;
          cout << endl;
#endif
        }
      }

      prev_event_count += num_events_in_file;
      if (prev_event_count >= endExcl)
        break;
    }

    if (prev_event_count <= (endExcl-1))
        !verbose || cout << "No more events available." << endl;

    !verbose || cout << "Finished reading events." << endl;

#if USING_RCPP
    if (show_frequency) {
      return(DataFrame::create(
        Named("Event") = event_number_vec,
        Named("Cues") = cues_vec,
        Named("Outcomes") = outcomes_vec,
        Named("Frequency") = frequency_vec,
        Named("stringsAsFactors") = false)
      );
    }
    else {
      return(DataFrame::create(
        Named("Event") = event_number_vec,
        Named("Cues") = cues_vec,
        Named("Outcomes") = outcomes_vec,
        Named("stringsAsFactors") = false)
      );
    }
#endif
}
