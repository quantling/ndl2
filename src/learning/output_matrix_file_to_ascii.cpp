//(c) 2013 Nathanael Schilling
//This program takes the (binary) matrix output format made by the iterative_rescorla_wagner program 
//and outputs it to ASCII.
//It takes as optional only parameter the filename of the matrix file, and outputs to standard output the matrix in readable form.
//If the first parameter is not given, then it reads the file to read from standard input.

#include <iostream>
#include "common_type_definitions.h"
#include "file_management.h"
#include "rescorla_wagner.h"
#include "util.h"
#include "matrix_io.h"
#include "trace_setup.h"

using namespace std;

int main(int argc, char* argv[])
{
	ifstream maybe_file_to_read;
	if(argc == 1)
	{
		; //Default to using stdin, see below.
	}else if(argc >= 2)
	{
		maybe_file_to_read.open(argv[1]);
		if(!maybe_file_to_read.good())
		{
			cerr<<"Input file, given as first parameter, is not good"<<endl;
			return -1;
		}
	}
	enum {OUTPUT_WHOLE_MATRIX,SIMULATE_TRACE,PRINT_MATRIX_FILE_HEADER} action = OUTPUT_WHOLE_MATRIX;
	if(argc == 3)
	{
		if(string(argv[2]) == "--simulate-trace")
		{
			action = SIMULATE_TRACE;	
		}else if(string(argv[2]) == "--print-matrix-file-header")
		{
			action = PRINT_MATRIX_FILE_HEADER;
		}else
		{
			cerr<<"Invalid second paramter"<<endl;
			return -1;
		}
	}else if(argc > 3)
	{
		cerr<<"Invalid number of parameters given, exiting"<<endl;
		return -1;
	}

	istream &file_to_read = (argc != 1)? maybe_file_to_read : cin;
	if(action == OUTPUT_WHOLE_MATRIX)
	{
		prettyPrintMatrixFileToOstream(cout,file_to_read);
	}else if(action == SIMULATE_TRACE)
	{
		auto matrix_file_info = readMatrixFileHeader(file_to_read);
		//Let's read in the traces first.
		string outcomes_file_name{matrix_file_info.folder_name + "/" + matrix_file_info.corpus_name +".outcomes"};
		string cues_file_name{matrix_file_info.folder_name + "/" + matrix_file_info.corpus_name + ".cues"};
		auto outcomes = read_space_separated_file<size_t>(outcomes_file_name);
		auto cues = read_space_separated_file<size_t>(cues_file_name);
		TraceInfo traces_read_in = read_traceinfo_from_file(cin,cues.first,outcomes.first);
		AssociationMatrix current_association_matrix(matrix_file_info.rows,matrix_file_info.cols,vector<int>(matrix_file_info.cols,0));
		long long int current_association_matrix_event_number = readMatrixFromFile(
				file_to_read,
				current_association_matrix,
				matrix_file_info.rows,
				matrix_file_info.cols
				);
		for(long long int event_number = 0;current_association_matrix_event_number >= 0 ;event_number++)
		{
			output_trace_info(current_association_matrix,traces_read_in,event_number,event_number != current_association_matrix_event_number);
			if(current_association_matrix_event_number == event_number)
			{
				current_association_matrix_event_number  = readMatrixFromFile(
						file_to_read,
						current_association_matrix,
						matrix_file_info.rows,
						matrix_file_info.cols
						);
			}
		}
	}else if(action == PRINT_MATRIX_FILE_HEADER)
	{
		const char* matrixFileHeaderOne = "Iterative Rescorla-Wagner-Matrices output";
		const char* matrixFileHeaderTwo = "Version 0.4";

		auto matrix_file_info = readMatrixFileHeader(file_to_read);
		cout<<matrixFileHeaderOne<<endl;
		cout<<matrixFileHeaderTwo<<endl;
		cout<<"Rows per Column:\t"<<matrix_file_info.rows<<endl;
		cout<<"Columns per Row:\t"<<matrix_file_info.cols<<endl;
		cout<<"Folder Containing Preprocessed Corpus:\t"<<matrix_file_info.folder_name<<endl;
		cout<<"Name of Corpus:\t"<<matrix_file_info.corpus_name<<endl;
		cout<<"Default Alpha:\t"<<matrix_file_info.default_alpha<<endl;
		cout<<"Default Beta:\t"<<matrix_file_info.default_beta<<endl;
		cout<<"Duplicate Cues or Outcomes for Single Event:\t"<<((matrix_file_info.has_duplicates == true) ? "true":"false")<<endl;
		std::function<string(long long int)> my_to_string = [](long long int in) -> string {return std::to_string(in);};
		cout<<"Included Events:\t"<<
			join(
				functional_map(matrix_file_info.events_at_which_to_write,my_to_string)
				,",")
			<<endl;

	}
	return 0;
}
