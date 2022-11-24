//(c) 2013 Nathanael Schilling
//This header file contains the interfaces for using the iterative Rescorla-Wagner model provided by this program.
#ifndef RESCORLA_WAGNER_H
#define RESCORLA_WAGNER_H

#include <vector>
#include <iostream>
#include <utility>

#include <cmath>

#include "common_type_definitions.h"
#include "../common/serialization.h"
#include "../common/Exception.h"
#include "util.h"

using std::vector;
using std::cout;
using std::pair;

void update_association_matrix_in_response_to_event(
		AssociationMatrix &association_matrix,
		const Event &current_event,
		const vector<double> &alphas,
		const vector<double> &betas,
		const double lambda,
		const bool has_duplicates,
		const vector<int> &outcomes_to_ignore,
		const pair<size_t,size_t> &this_threads_region
		);

void output_trace_info(
		AssociationMatrix &association_matrix,
		TraceInfo &to_trace,
		long long int event_count,
		bool suppress_output = false
		);


#endif
