#ifndef TRACE_SETUP_H
#define TRACE_SETUP_H

#include <map>
#include <vector>
#include <string>
#include "common_type_definitions.h"

using std::map;
using std::vector;
using std::string;
using std::istream;

/*
 * We read in trace directives and return them.
 */
TraceInfo read_traceinfo_from_file(
    istream &infile,
    const map<string,size_t> &cues,
    const map<string,size_t> &outcomes
);

/*
 * This function updates outcomes_to_ignore to have zeroes for
 * outcomes that are output by the trace directives.
 */
void unignore_outcomes_from_traces(
    vector<int> &outcomes_to_ignore,
    const TraceInfo &to_trace
);

#endif //TRACE_SETUP_H
