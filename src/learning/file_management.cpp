#include "file_management.h"

#ifndef USING_RCPP
  #include "trace_setup.h"
#endif

//A few static function definitions not in file_management.h

//Takes as an argument the foldername where the events are stored, and calls read_event_file for each .dat file.
//Returns the number of events learned from
int read_events_files(
		string &events_foldername,
		AssociationMatrix &association_matrix,
		const vector<double> &alphas,
		const vector<double> &betas,
		TraceInfo &to_trace,
		double lambda,
		bool has_duplicates,
		vector<int> &outcomes_to_ignore,//We use a vector<int> instead of a vector<bool> here, as doing so is hopefully faster.
		matrixOutputInfo &output_info,
		long long int until,
		bool quiet_mode,
		unsigned int num_threads,
		long long int start_from,
		bool debug_output
		);

//Opens the specified event file and calls both of the functions in rescorla_wagner.h for every event in the file.
static void handle_events(
		AssociationMatrix &association_matrix,
		const vector<double> &alphas,
		const vector<double> &betas,
		TraceInfo &to_trace,
		double lambda,
		long long int &event_count,
		bool has_duplicates,
		vector<int> &outcomes_to_ignore,//This behaves like a vector<bool>
		matrixOutputInfo &output_info,
		long long int &until,
		const Events& current_events,
		size_t num_threads,
		const pair<size_t,size_t> &this_threads_region,
		long long int start_from,
		long long int &skip_count
		);

//We don't need to use namespace std, just include what we need
using std::cin;
using std::cerr;
using std::endl;
using std::istream;
using std::ifstream;
using std::find_if;

void bootstrap_model(
		//These parameters are all basically return values:
		AssociationMatrix &association_matrix,
		vector<double> &alphas,
		vector<double> &betas,
		TraceInfo &to_trace,
		vector<int> &outcomes_to_ignore, 
		size_t &num_rows,
		size_t &num_cols,
		//Here are the actual parameters:
		double default_alpha,
		string alphas_filename,
		double default_beta,
		string betas_filename,

		string outcomes_to_number_filename,
		string cues_to_number_filename,

		bool quiet_mode,
		string matrix_output_file_name, //Pass a non-null string here to model all outcomes in association matrix.

		bool read_trace_info //Whether to initialize to_trace

)
{
	//At first we read in the cues and outcomes.
	quiet_mode || (cerr<<"Reading in outcomes..."<<endl);
	auto outcomes = read_space_separated_file<size_t>(outcomes_to_number_filename);
	quiet_mode || (cerr<<"Done reading in "<<outcomes.first.size()<<" outcomes with maximum outcome value of "<<outcomes.second<<endl);
	outcomes_to_ignore = vector<int>(outcomes.second + 1,(int)(matrix_output_file_name == ""));

	quiet_mode || (cerr<<"Reading in cues..."<<endl);
	auto cues = read_space_separated_file<size_t>(cues_to_number_filename);
	quiet_mode || (cerr<<"Done reading in "<<cues.first.size()<<" cues with maximum cue value of "<<cues.second<<endl);


#ifndef USING_RCPP
	if(!quiet_mode && read_trace_info)
	{
		cerr<<"Please enter a newline-separated list of the values you wish to trace. Each item of the list should look like cue,outcome"<<endl;
		cerr<<"Optionally enter additional parameters in brackets"<<endl;
		cerr<<"\tThese parameters must have the following format: A word, followed by a sequence of integers separated by spaces."<<endl;
		cerr<<"\tThe first word must be one of 'every','at' or 'first'"<<endl;
		cerr<<"\tThese parameters determine when the trace should be output. For details consult the manual in the README file"<<endl;
		cerr<<"\tA well-formed parameter-string could be (every 10)."<<endl;
		cerr<<"\t\tThis which would result in the trace being output on every event number that is a multiple of 10,"<<endl;
		cerr<<"Having entered this newline separated list, press Ctrl+D (or the system's equivalent) to close this program's standard input."<<endl;
	}
	if(read_trace_info)
		to_trace = read_traceinfo_from_file(cin,cues.first,outcomes.first);

	//IMPORTANT: Do not delete the line below, as the web interface depends on it.
	(!read_trace_info || cerr<<"Done reading what to trace. "<<endl);
	//IMPORTANT: See above.
	unignore_outcomes_from_traces(outcomes_to_ignore,to_trace);
#endif

	quiet_mode || (cerr<<"Reading in values for alphas, default alpha value is "<<default_alpha<<endl);

	alphas = vector<double>(cues.second+1,default_alpha);	
	pair<map<string,double>,double> alphas_map = alphas_filename.empty() ?
				pair<map<string,double>,double>() :
				read_space_separated_file<double>(alphas_filename);

	for(auto &current_alpha : alphas_map.first)
	{
		auto cueID = cues.first.find(current_alpha.first);
		if(cueID == cues.first.end())
		{
			cerr<<"Alpha value "<<current_alpha.second<<" for cue \""<<current_alpha.first<<"\" given, but the cue does not exist in the list of cues"<<endl;
			continue;
		}
		if((current_alpha.second <= 0) || (current_alpha.second > 1))
		{
			cerr<<"Alpha value "<<current_alpha.second<<" for cue \""<<current_alpha.first<<"\" is not in the range (0,1] - skipping it."<<endl;
			continue;
		}
		alphas[cueID->second] = current_alpha.second;
	}
	quiet_mode || (cerr<<"Done reading in values for alphas"<<endl);

	quiet_mode || (cerr<<"Reading in values for betas, default beta value is "<<default_beta<<endl);
	betas = vector<double>(outcomes.second+1,default_beta);
	pair<map<string,double>,double> betas_map = betas_filename.empty() ? pair<map<string,double>,double>():read_space_separated_file< double>(betas_filename);
	for(auto &current_beta : betas_map.first)
	{
		auto outcomeID = outcomes.first.find(current_beta.first);
		if(outcomeID == outcomes.first.end())
		{
			cerr<<"Beta value "<<current_beta.second<<" for outcome \""<<current_beta.first<<"\" given, but the outcome does not exist in the list of outcomes"<<endl;
			continue;
		}
		if((current_beta.second <= 0) || (current_beta.second > 1))
		{
			cerr<<"Beta value "<<current_beta.second<<" for outcome \""<<current_beta.second<<"\" is not in the range (0,1] - skipping it"<<endl;
			continue;
		}
		betas[outcomeID->second] = current_beta.second;
	}
	quiet_mode || (cerr<<"Done reading in values for betas"<<endl);

	association_matrix = AssociationMatrix(cues.second+1,outcomes.second+1,outcomes_to_ignore);
	quiet_mode || (cerr<<"Finished creating empty association matrix"<<endl);	
	num_rows = cues.second + 1;
	num_cols = outcomes.second + 1;

}


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
		vector<long long int> events_at_which_to_write_out,
		const string &folder_name,
		const string &corpus_name,
		bool quiet_mode,
		long long int until,
		unsigned int num_threads
		)

{
	//The main job of this function is to initialize the following variables. This is done in the scope created below
	//so that ecess memory (e.g. from the map of outcomes etc) is freed.
	TraceInfo to_trace;
	AssociationMatrix association_matrix(0,0,{}); 
	vector<int> outcomes_to_ignore;
	vector<double> alphas;
	vector<double> betas;
	size_t num_rows = 0;
	size_t num_cols = 0;

	bootstrap_model(
		//These parameters are all basically return values:
		association_matrix,
		alphas,
		betas,
		to_trace,
		outcomes_to_ignore, 
		num_rows,
		num_cols,
		//Here are the actual parameters:
		default_alpha,
		alphas_filename,
		default_beta,
		betas_filename,

		outcomes_to_number_filename,
		cues_to_number_filename,

		quiet_mode,
		matrix_output_file_name,

		true
		);


	bool has_duplicates = !ignore_duplicate_cues_or_outcomes;
	matrixOutputInfo output_info(matrix_output_file_name,
		    {
		    num_rows,
		    num_cols,
		    getFullPath(folder_name),
		    corpus_name,
		    default_alpha,
		    default_beta,
		    events_at_which_to_write_out,
		    has_duplicates
		    }
		    );
	output_info.writeHeader();

	read_events_files(
	    events_foldername,
	    association_matrix,
	    alphas,
	    betas,
	    to_trace,
	    lambda,
	    has_duplicates,
	    outcomes_to_ignore,
	    output_info,
	    until,
	    quiet_mode,
	    num_threads,
	    0
	);

	output_info.writeFooter();

}


bool cancel_processing = false;

void interrupt_handler(int) {
	#pragma omp atomic write
	cancel_processing = true;
}

int read_events_files(
		string &events_foldername,
		AssociationMatrix &association_matrix,
		const vector<double> &alphas,
		const vector<double> &betas,
		TraceInfo &to_trace,
		double lambda,
		bool has_duplicates,
		vector<int> &outcomes_to_ignore,//This behaves like a vector<bool>
		matrixOutputInfo &output_info,
		long long int until,
		bool quiet_mode,
		unsigned int num_threads,
		long long int start_from,
		bool debug_output
		)
{
	quiet_mode || cerr<<"Attempting to learn "<<until<<" events starting from event id "<<start_from<<"."<<endl;
	// Make learning interruptable (e.g. when ctrl+c is pressed in R session):
	//   interrupt_handler() sets cancel_processing to true on interrupts
	cancel_processing = false;
	void (*previous_signal_handler)(int);
	previous_signal_handler = signal(SIGINT, interrupt_handler);

	long long int event_count = 0;
  string full_path = getFullPath(events_foldername);
	vector<string> paths = getEventFilePaths(full_path);
	sort(paths.begin(),paths.end(), compareEventFileNames); 
	quiet_mode || cerr<<"Found "<<paths.size()<<" events file(s)."<<endl;
	if (debug_output) {
		cerr<<"They will be processed in the following order:"<<endl;
		for(auto &current_path : paths)
		cerr<<"\t"<<current_path.substr(current_path.find_last_of("/\\")+1)<<endl;
	}

	// Find out at which event file the first event to be learned occurs.
	size_t starting_file = 0;
	long long int last_event_in_file = getNumEvents(paths[starting_file])-1;
	while(start_from > last_event_in_file && starting_file < (paths.size()-1)) {
	  ++starting_file;
	  last_event_in_file += getNumEvents(paths[starting_file]);
	}
	// skip_count (how many events were already skipped) goes up to inclusive
	// the event id of the event to start from, as the event id is 0 based.
	// First event in file = first event in next file - events in this file
	long long int skip_count = (1+last_event_in_file) - getNumEvents(paths[starting_file]); 
	if (debug_output) {
		cerr<<"Starting at event file "<<starting_file<<" as its first event is "<<skip_count<<
                  " and its last event is "<<last_event_in_file<<"."<<endl;
	}

	events_manager* eventsManager = new events_manager(paths,num_threads);

	vector<pair<size_t,size_t>> regions = split_range(0,association_matrix.cols(),num_threads);

	string exception_message = "";

	quiet_mode || (cerr << "The learning starts now. Event files finished:"<<endl);
	//Not using parallel for below is intentional.
	#pragma omp parallel num_threads(num_threads) shared(association_matrix,regions,cancel_processing,starting_file) firstprivate(until,skip_count)
	{
		long long local_event_count= event_count;
		unsigned int current_thread = omp_get_thread_num();

		pair<size_t,size_t> this_threads_region = regions[current_thread];
		
		for(size_t i = starting_file; i < paths.size() && !cancel_processing && until > 0; i++)
		{
			const Events &current_events = eventsManager->getEventsNumber(i);

			try {
				handle_events(
					association_matrix,
					alphas,
					betas,
					to_trace,
					lambda,
					local_event_count,
					has_duplicates,
					outcomes_to_ignore,
					output_info,
					until,
					current_events,
					num_threads,
					this_threads_region,
					start_from,
					skip_count
				);
			}
			catch(std::exception& e) {
				#pragma omp critical
				{
					if (!cancel_processing) {
						cancel_processing = true;
						exception_message = e.what();
					}
				}
			}
			catch(...) {
				#pragma omp critical
				{
					if (!cancel_processing) {
						cancel_processing = true;
						exception_message = "Unknown error occured in handle_events()";
					}
				}
			}

			if (debug_output)
				cerr << "F" << i << "T" << current_thread << " ";
			eventsManager->doneWithEvents(i, quiet_mode, debug_output);

			if(until <= 0)
				break;
		}
		#pragma omp master
			event_count = local_event_count;
	}
	delete eventsManager;
	// Reset interrupt handler to original signal handler
	signal(SIGINT, previous_signal_handler);
	if (!cancel_processing) {
		quiet_mode || (cerr<<endl<<"Ran Rescorla-Wagner model for "<<event_count<<" events from "<<paths.size()<<" files"<<endl);
		return event_count;
	}
	else {
		cerr << endl; // Above we are printing dots without a newline afterwards.
		if (exception_message != "")
			throw Exception(exception_message);
		else
			return 0; // To signal that no events were (successfully) learned.
	}
}


static void handle_events(
		AssociationMatrix &association_matrix,
		const vector<double> &alphas,
		const vector<double> &betas,
		TraceInfo &to_trace,
		double lambda,
		long long int &event_count,
		bool has_duplicates,
		vector<int> &outcomes_to_ignore,//This is actually a vector<bool>, but we use vector<int> as vector<bool> is probably slower.
		matrixOutputInfo &output_info,
		long long int &until,
		const Events &current_events,
		size_t num_threads,
		const pair<size_t,size_t> &this_threads_region,
		long long int start_from,
		long long int &skip_count
		)
{
	if(!event_count && !skip_count)
	{
		output_info.appendMatrix(association_matrix,event_count);
		if((num_threads == 1) && current_events.size())
			output_trace_info(association_matrix,to_trace,event_count);
	}

	for(const auto &i : current_events)
	{
		if(skip_count < start_from)
		{
			skip_count++;
			continue;
		}
		if(--until < 0)
			break;
		// Check if exception or interrupt has occurred in another thread
		if(cancel_processing)
			return;
		update_association_matrix_in_response_to_event(
				association_matrix,
				i,
				alphas,
				betas,
				lambda,
				has_duplicates,
				outcomes_to_ignore,
				this_threads_region
				);
		event_count++;
		if(output_info.matrix_file_info.events_at_which_to_write.back() == event_count)
		{
			#pragma omp barrier
			#pragma omp flush
			#pragma omp master
			{
				output_info.appendMatrix(association_matrix,event_count);
			}
			#pragma omp flush
			#pragma omp barrier
		}
		if(num_threads == 1)
			output_trace_info(association_matrix,to_trace,event_count);
	}
	return;
}

