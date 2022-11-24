//(c) 2013 Nathanael Schilling

#include <iostream>
#include <string>
#include <sstream>
#include <unistd.h>
#include <boost/filesystem/operations.hpp>

#include "file_management.h"
#include "../common/serialization.h"
using std::cerr;
using std::exit;
using std::endl;
using std::stod;
using std::stoll;

static void print_usage(char* argv[])
{
	cerr<<"Usage:"<<endl<<argv[0]<<" folder_with_everything corpus_name default_alpha default_beta  OPTIONS <what_to_trace >trace_output"<<endl;
	cerr<<"\t"<<"where folder_with_everything is a folder containing the following files:"<<endl;
	cerr<<"\t\t"<<"Mandatory:"<<endl;
	cerr<<"\t\t\t"<<"corpus_name.events - A folder created by ndl.preproc containing .dat files of events."<<endl;
	cerr<<"\t\t\t"<<"corpus_name.outputs - A file created by ndl.preproc containing a map of outputs to their ids."<<endl;
	cerr<<"\t\t\t"<<"corpus_name.cues - A file created by ndl.preproc containing a map of cues to their ids."<<endl;
	cerr<<"\t\t"<<"Optional:"<<endl;
	cerr<<"\t\t\t"<<"corpus_name.alphas - A space separated file of cues to alpha values."<<endl;
	cerr<<"\t\t\t"<<"corpus_name.betas - A space separated file of outcomes to beta values."<<endl;
	cerr<<"\t"<<"and default_alpha or default_beta give default values of alpha and beta."<<endl;
	cerr<<"\t"<<"and OPTIONS is one or many of the following"<<endl;
	cerr<<"\t\t"<<"-l lambda - Set the value used in the model for the maximum possible conditioning for an outcome."<<endl;
	cerr<<"\t\t"<<" -d Add what is being added to weights multiple times for duplicate Cues and Outcomes in a single Event"<<endl;
	cerr<<"\t\t"<<"-u event_number - Learn only using the first event_number of events."<<endl;
	cerr<<"\t\t"<<"-q Quiet mode. Don't output loads of stuff to stderr."<<endl;
	cerr<<"\t\t"<<"-o output_file - Write matrices to output_file"<<endl;
	cerr<<"\t\t"<<"-m a b c.... - Where a b c... are unsigned integers - write the association matrix to output_file at events a,b,c..."<<endl;
	cerr<<"\t\t"<<"-a alphas_file - File containing a space-separated list of cues to alpha values to use"<<endl;
	cerr<<"\t\t"<<"-b betas_file - File containing a space-separated list of outcomes to beta values to use"<<endl;
	cerr<<"\t\t"<<"-t num_threads - How many threads to use. Setting this to a value other than one disables trace outputs"<<endl;
	exit(-1);
}

namespace fs  = boost::filesystem;
int main(int argc, char* argv[])
{
	if(argc < 5)
	{
		cerr<<"Not the right number of arguments."<<endl;
		print_usage(argv);
	}
	string folder_with_everything = argv[1];
	fs::path folder_with_everything_full_path(folder_with_everything);	
	if(!fs::exists(folder_with_everything) || !fs::is_directory(folder_with_everything_full_path) )
	{
		cerr<<folder_with_everything<<" either does not exist or is not a directory."<<endl;
		exit(-1);
	}
	string corpus_name = argv[2];

	fs::path events_folder = folder_with_everything_full_path/(corpus_name + ".events");
	if(!fs::exists(events_folder) || !fs::is_directory(events_folder))
	{
		cerr<<events_folder<<" either does not exist or is not a directory."<<endl;
		exit(-1);
	}
	fs::path cues_file = folder_with_everything_full_path/(corpus_name + ".cues");
	if(!fs::exists(cues_file) || !fs::is_regular_file(cues_file))
	{
		cerr<<cues_file<<" either does not exist or is not a regular file."<<endl;
		exit(-1);
	}

	fs::path outcomes_file = folder_with_everything_full_path/(corpus_name + ".outcomes");
	if(!fs::exists(outcomes_file) || !fs::is_regular_file(outcomes_file))
	{
		cerr<<outcomes_file<<" either does not exist or is not a regular file."<<endl;
		exit(-1);
	}

	double default_alpha;
	double default_beta;

	stringstream default_alpha_stream(argv[3]);
	default_alpha_stream>>default_alpha;

	stringstream default_beta_stream(argv[4]);
	default_beta_stream>>default_beta;

	if(default_alpha_stream.fail() || default_beta_stream.fail() ||  (default_alpha <= 0) || (default_alpha > 1) || (default_beta <= 0) || (default_beta > 1))
	{
		cerr<<"The default values for alpha and beta need to be in the range (0,1]"<<endl;
		exit(-1);
	}
	string alphas_filename("");
	string betas_filename("");
	if(fs::exists(folder_with_everything_full_path/(corpus_name + ".alphas")))
		alphas_filename = (folder_with_everything_full_path/(corpus_name + ".alphas")).string();
	if(fs::exists(folder_with_everything_full_path/(corpus_name + ".betas")))
		betas_filename = (folder_with_everything_full_path/(corpus_name + ".betas")).string();

	string matrix_output_filename;
	vector<long long int> event_numbers_to_output_matrix_at;
	bool quiet_mode = false;
	double lambda = 100.0;
	long long int until = LLONG_MAX;
	bool ignore_duplicate_cues_or_outcomes = true;
	unsigned int num_threads = 1;
	for(int argc_iterator = 5;argc_iterator < argc; argc_iterator++)
	{
		if(string(argv[argc_iterator]) == "-d")
		{
			ignore_duplicate_cues_or_outcomes = false;
		}else if(string(argv[argc_iterator]) == "-o")
		{
			matrix_output_filename = argv[++argc_iterator];
			continue;
		}else if(string(argv[argc_iterator]) == "-m")
		{
			while(++argc_iterator < argc)
			{
				long long int current_event;
				stringstream number(argv[argc_iterator]);
				number>>current_event;
				if(number.fail() || !number.eof())
				{
					argc_iterator--;
					break;
				}else
				{
					event_numbers_to_output_matrix_at.push_back(current_event);
				}
			}
			continue;
		}else if(string(argv[argc_iterator]) == "-a")
		{
			alphas_filename = argv[++argc_iterator];
			if(!fs::exists(alphas_filename))
			{
				cerr<<"The file '"<<alphas_filename<<"' given as alphas filename does not exist."<<endl;
				exit(-1);
			}
		}else if(string(argv[argc_iterator]) == "-b")
		{
			betas_filename = argv[++argc_iterator];
			if(!fs::exists(betas_filename))
			{
				cerr<<"The file '"<<betas_filename<<"'given as betas_filename does not exist."<<endl;
				exit(-1);
			}
		}else if(string(argv[argc_iterator]) == "-q")
		{
			quiet_mode = true;
		}else if(string(argv[argc_iterator]) == "-l")
		{
			try
			{
				lambda = stod(string(argv[++argc_iterator]));
				if(lambda <= 0)
				{
					throw "";
				}
			}catch(...)
			{
				cerr<<"Invalid value for lambda given using flag -l"<<endl;
				exit(-1);
			}
		}else if(string(argv[argc_iterator]) == "-u")
		{
			try
			{
				until = stoll(argv[++argc_iterator]);
				if(until <= 0)
				{
					throw "";
				}
			}catch(...)
			{
				cerr<<"Invalid event number for until given using flag -u"<<endl;	
				exit(-1);
			}
		}else if(string(argv[argc_iterator]) == "-t")
		{
			try
			{
				num_threads = stoll(argv[++argc_iterator]);
				if(num_threads <= 0)
					throw "";
				//Check for power of two num_threads
			}catch(...)
			{
				cerr<<"Invalid number of threads using flag -t"<<endl;
				exit(-1);
			}
		}else
		{
			cerr<<"Invalid option: "<<argv[argc_iterator]<<endl;;
		}
	}
	//Check that either both -o and -m is given or neither:
	if(event_numbers_to_output_matrix_at.size() && (matrix_output_filename == ""))
	{
		cerr<<"Please give -o when giving -m"<<endl;
		exit(-1);
	}

	if((num_threads > 1) && (matrix_output_filename == ""))
	{
		cerr<<"The model does not support outputting traces when running with multiple threads, only matrix output. So please give a matrix output filename"<<endl;
		exit(-1);
	}
	try {
		start_model(
				events_folder.string(),
				default_alpha,
				alphas_filename,
				default_beta,
				betas_filename,
				lambda,
				outcomes_file.string(),
				cues_file.string(),
				ignore_duplicate_cues_or_outcomes,
				matrix_output_filename,
				event_numbers_to_output_matrix_at,
				folder_with_everything,
				corpus_name,
				quiet_mode,
				until,
				num_threads
				);
	}
	catch (Exception& e) {
		cerr << e.what() << endl;
		return -1;
	}

	return 0;
}
