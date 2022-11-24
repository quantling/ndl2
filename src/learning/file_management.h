//(c) 2013 Nathanael Schilling
//This file provides various file management functions.
//For example, it reads in the alpha and beta values from the files provided to the commandline.
//The functions in this file, particularly the read_event_file() function ends up calling the functions in rescorla_wagner.cpp


#ifndef FILE_MANAGEMENT_H
#define FILE_MANAGEMENT_H

#include <map>
#include <vector>
#include <string>
#include <algorithm>
#include <cstdlib>
#include <utility>
#include <csignal>

#include <omp.h>
#include <limits.h>
#include <string.h>

#include "rescorla_wagner.h"
#include "util.h"
#include "../common/Exception.h"
#include "common_type_definitions.h"
#include "matrix_io.h"
#include "events_manager.h"
#include "../common/helper.h"

using std::map;
using std::vector;
using std::string;
using std::pair;
using std::istream;
using std::ostream;
using std::ofstream;
using std::cerr;


//Basically just starts running the model. The arguments should be self-explanatory. This function doesn't do anythign with the events,
//but it initializes traceinfo_filename.
void start_model(
		string events_foldername,
		double default_alpha,
	       	string alphas_filename,
		double default_beta,
		string betas_filename,
		double lambda,
		string outcomes_to_number_filename,
		string cues_to_number_filename,
		bool ignore_duplicate_cues_or_outcomes,
		string matrix_output_file_name,
		vector<long long int> events_at_which_to_output_matrix,
		const string &folder_name,
		const string &corpus_name,
		bool quiet_mode, //If set to true, we output less to stderr
		long long int until,
		unsigned int num_threads
		);

//This function can be used to initialize the first set of parameters (see below) using the second set of parameters.
void bootstrap_model(
		//The first set of parameters:
		//These parameters are all basically return values:
		AssociationMatrix &association_matrix,
		vector<double> &alphas,
		vector<double> &betas,
		TraceInfo &to_trace,
		vector<int> &outcomes_to_ignore,
		size_t &num_rows,
		size_t &num_cols,
		//The second set of parameters.
		//These are the actual "parameters" to the function in the more traditional sense of the word:
		double default_alpha,
		string alphas_filename,
		double default_beta,
		string betas_filename,

		string outcomes_to_number_filename,
		string cues_to_number_filename,

		bool quiet_mode,
		string matrix_output_file_name,

		bool read_trace_info //Whether to initialize to_trace

);

//Learn using the Rescorla-Wagner model.
//Returns the number of events learned from.
int read_events_files(
		string &events_foldername,
		AssociationMatrix &association_matrix,
		const vector<double> &alphas,
		const vector<double> &betas,
		TraceInfo &to_trace,
		double lambda,
		bool has_duplicates,
		vector<int> &outcomes_to_ignore,//We use int instead of bool here too.
		matrixOutputInfo &output_info,
		long long int until,
		bool quiet_mode,
		unsigned int num_threads,
		long long int start_from,
		bool debug_output = true
		);

#endif
