#include "matrix_io.h"

static const char* matrixFileHeaderOne = "Iterative Rescorla-Wagner-Matrices output";
static const char* matrixFileHeaderTwo = "Version 0.4";

matrixOutputInfo::matrixOutputInfo(string &output_filename, const matrixFileInfo &matrix_file_info)
{
	this->output_filename = output_filename;
	this->matrix_file_info = matrix_file_info;
}
void matrixOutputInfo::writeHeader()
{
	if(!output_filename.empty())
	{
		this->matrix_file_info = matrix_file_info;
		output_file.open(output_filename,std::ios::out);
		if(!output_file)
		{
			throw Exception("Could not open output file");
		}
		writeMatrixFileHeader(this->output_file,matrix_file_info);
	}

	//At this point, we need to prepare for appendMatrix being called.
	//appendMatrix expects matrix_file_info.events_at_which_to_write.back() to exist:
	//We append a dummy value, so that events_to_which to write is never empty despite the fact that we remove events as we reach them.
	std::sort(this->matrix_file_info.events_at_which_to_write.begin(),this->matrix_file_info.events_at_which_to_write.end(),
			[](const long long int &first,const long long int &second) ->bool
			{
				return first > second;
			}
			);

	return;
}
void matrixOutputInfo::appendMatrix(const AssociationMatrix &association_matrix,long long int &event_count)
{
	if(!output_filename.empty())
	{
		if(matrix_file_info.events_at_which_to_write.back() == event_count)
		{
			matrix_file_info.events_at_which_to_write.pop_back();
			appendMatrixToFile(output_file,association_matrix,event_count);
		}
	}
	return;
}
void matrixOutputInfo::writeFooter()
{
	if(!output_filename.empty())
	{
		writeMatrixFileFooter(output_file);
		output_file.close();
	}
}

using std::stringstream;
void writeMatrixFileHeader(
		ofstream &file_to_write,
		const matrixFileInfo &matrix_file_info
		)
{
	AssociationMatrix small_reference_matrix(2,2,{0,0});
	small_reference_matrix(0,0) = 0.1;
	small_reference_matrix(0,1) = 0.2;
	small_reference_matrix(1,0) = 0.3;
	small_reference_matrix(1,1) = 0.4;

	file_to_write<<matrixFileHeaderOne<<std::endl;
	file_to_write<<matrixFileHeaderTwo<<std::endl;
	file_to_write<<"Rows per Column:\t"<<matrix_file_info.rows<<std::endl;
	file_to_write<<"Columns per Row:\t"<<matrix_file_info.cols<<std::endl;
	file_to_write<<"Folder Containing Preprocessed Corpus:\t"<<matrix_file_info.folder_name<<std::endl;
	file_to_write<<"Name of Corpus:\t"<<matrix_file_info.corpus_name<<std::endl;
	file_to_write<<"Default Alpha:\t"<<matrix_file_info.default_alpha<<std::endl;
	file_to_write<<"Default Beta:\t"<<matrix_file_info.default_beta<<std::endl;
	file_to_write<<"Duplicate Cues or Outcomes for Single Event:\t"<<((matrix_file_info.has_duplicates == true) ? "true":"false")<<std::endl;
	std::function<string(long long int)> my_to_string = [](long long int in) -> string {return std::to_string(in);};
	vector<long long int> events = matrix_file_info.events_at_which_to_write;
	events.pop_back();
	file_to_write<<"Included Events:\t"<<
		join(
			functional_map(events,my_to_string)
			,",")
		<<std::endl;
	file_to_write<<"---Begin Reference Matrix for Binary Test---"<<std::endl;
	appendMatrixToFile(file_to_write,small_reference_matrix,0);
	file_to_write<<"---End Reference Matrix for Binary Test---"<<std::endl;
	file_to_write<<"---Begin Matrices---"<<std::endl;
	return;
}

void writeMatrixFileFooter(
		ofstream &file_to_write
		)
{
	file_to_write<<"---End Matrices---"<<std::endl;
}

matrixFileInfo readMatrixFileHeader(istream &file_to_read)
{
	matrixFileInfo result;
	AssociationMatrix small_reference_matrix(2,2,{0,0});
	small_reference_matrix(0,0) = 0.1;
	small_reference_matrix(0,1) = 0.2;
	small_reference_matrix(1,0) = 0.3;
	small_reference_matrix(1,1) = 0.4;

	string current_line;
	if(!getline(file_to_read,current_line))
	{
		throw Exception("Could not read header from the matrix file.");
	}
	if(current_line != matrixFileHeaderOne)
	{
		throw Exception("Matrix File has incorrect header.");
	}
	if(!getline(file_to_read,current_line))
	{
		throw Exception("Matrix file header is too short.");
	}
	if(current_line != matrixFileHeaderTwo)
	{
		throw Exception("Matrix file has incorrect version in header");
	}
	if(!getline(file_to_read,current_line))
	{
		throw Exception("Matrix file does not have rows information");
	}
	stringstream current_line_stream(current_line);
	string before_tab;
	getline(current_line_stream,before_tab,'\t');
	if(before_tab != "Rows per Column:")
	{
		throw Exception("Matrix file has incorrect format, expected information about number of rows per column");
	}
	size_t rows;
	current_line_stream>>rows;
	if(!rows || current_line_stream.fail())
	{
		throw Exception("Expected to find a valid number for number of rows in matrix file");
	}
	current_line_stream.get();
	if(!getline(file_to_read,current_line))
	{
		throw Exception("Matrix file does not have columns information");
	}
	current_line_stream.clear();
	current_line_stream.str(current_line);
	getline(current_line_stream,before_tab,'\t');
	if(before_tab != "Columns per Row:")
	{
		throw Exception("Matrix file has incorrect format, expected information about number of columns per row, got:" + current_line);
	}
	size_t cols;
	current_line_stream>>cols;
	if(!rows || current_line_stream.fail())
	{
		throw Exception("Expected to find a valid number for number of columns in matrix file");
	}
	current_line_stream.get();

	if(!getline(file_to_read,before_tab,'\t'))
	{
		throw Exception("Matrix file ends before giving folder name");
	}
	if(before_tab != "Folder Containing Preprocessed Corpus:")
	{
		throw Exception("Matrix file has incorrect format, expected information about location of preprocessed corpus, got: " + current_line);
	}
	string folder_name;
	if(!getline(file_to_read,folder_name))
	{
		throw Exception("Matrix file has empty folder_name");
	}
	if(!getline(file_to_read,before_tab,'\t'))
	{
		throw Exception("Matrix file ends before giving corpus name");
	}
	if(before_tab != "Name of Corpus:")
	{
		throw Exception("Matrix file has incorrect format, expected information about corpus name, got: " + current_line);
	}
	string corpus_name;
	if(!getline(file_to_read,corpus_name))
	{
		throw Exception("Empty corpus name in matrix file");
	}
	if(!getline(file_to_read,before_tab,'\t'))
	{
		throw Exception("Matrix file ends before giving default value of alpha used");
	}
	if(before_tab != "Default Alpha:")
	{
		throw Exception("Matrix file is incorrectly formatted, expected 'Default Alpha:', got: '" + before_tab + "'");
	}
	double default_alpha;
	if(!(file_to_read >> default_alpha))
	{
		throw Exception("Emtpy or invalid value of default alpha in matrix file");
	}
	file_to_read.get();

	if(!getline(file_to_read,before_tab,'\t'))
	{
		throw Exception("Matrix file ends before giving default value of beta used");
	}
	if(before_tab != "Default Beta:")
	{

		throw Exception("Matrix file is incorrectly formatted, expected 'Default Beta:', got: '" + before_tab + "'");
	}
	double default_beta;
	if(!(file_to_read >> default_beta))
	{
		throw Exception("Empty or invalid value of default beta given in matrix file");
	}
	file_to_read.get(); //Get rid of newline after the beta.

	if(!getline(file_to_read,before_tab,'\t'))
	{
		throw Exception("Matrix file ends before giving info about whether duplicate cues/outcomes were allowed per event.");
	}
	if(before_tab != "Duplicate Cues or Outcomes for Single Event:")
	{
		throw Exception("Matrix file is incorrectly formatted, expected 'Duplicate Cues or Outcomes for Single Event', got: '" + before_tab + "'");
	}

	string has_duplicates_s;
	if(!getline(file_to_read,has_duplicates_s) || !((has_duplicates_s == "true") || (has_duplicates_s == "false")))
	{
		throw Exception("Empty or invalid value given for whether modified Rescorla Wagner Model was used or not");
	}
	bool has_duplicates = (has_duplicates_s == "true");


	if(!getline(file_to_read,before_tab,'\t'))
	{
		throw Exception("Matrix file ends before giving events which it contains");
	}
	if(before_tab != "Included Events:")
	{
		throw Exception("Matrix file has incorrect format, expected 'Included Events:' but got: '" + before_tab + "'");
	}
	string string_of_events;
	if(!getline(file_to_read,string_of_events))
	{
		throw Exception("Matrix file ends before giving list of event numbers included in it");
	}
	std::function<long long int(string)> my_stol = [](string in) -> long long int {return std::stol(in);};
	vector<long long int> events_read_in = functional_map(split_ignoring_empty(string_of_events,','),my_stol);

	getline(file_to_read,current_line);
	if(current_line != "---Begin Reference Matrix for Binary Test---")
	{
		throw Exception("Expected to find metadata about reference Matrix for binary test in matrix file, did not find it");
	}

	//We now read in the sample matrix:
	AssociationMatrix sample_matrix_from_file(2,2,{0,0});
	readMatrixFromFile(file_to_read,sample_matrix_from_file,2,2);
	if(sample_matrix_from_file != small_reference_matrix)
	{
		throw Exception("IO Test for matrix files failed");
	}
	//TODO: possily add tests for whether the output is actually what we are expecting here.
	getline(file_to_read,current_line);//Get rid of end of reference matrix directive.
	getline(file_to_read,current_line);//Get rid of start of actual data directive.
	result.rows = rows;
	result.cols = cols;
	result.folder_name = folder_name;
	result.corpus_name = corpus_name;
	result.default_alpha = default_alpha;
	result.default_beta = default_beta;
	result.events_at_which_to_write = move(events_read_in);
	result.has_duplicates = has_duplicates;
	return result;
}

void appendMatrixToFile(ofstream &file_to_write,const AssociationMatrix &current_matrix,long long int event_number, bool writeSeparator)
{
	if (writeSeparator)
		file_to_write<<"---Begin Matrix at event "<<event_number<<"---"<<std::endl;
	size_t rows = current_matrix.rows();
	size_t columns = current_matrix.cols();
	for(size_t i = 0; i<rows; i++)
	{
		for(size_t j = 0; j < columns; j++)
		{
			//MARKER1
			file_to_write.write(reinterpret_cast<const char*>(&current_matrix(i,j)), sizeof(double));

		}
	}
	if (writeSeparator)
		file_to_write<<"\n---End Matrix at event "<<event_number<<"---"<<std::endl;
	if(file_to_write.fail())
	{
		throw Exception("Failed to write Matrix output");
	}
}

long long int readMatrixFromFile(istream &file_to_read,AssociationMatrix &matrix_to_read, size_t rows, size_t cols)
{
	if(file_to_read.eof())
		return -1;
	//First read in the header line.
	string header_line;
	getline(file_to_read,header_line);
	string::iterator itr = find_if(header_line.begin(), header_line.end(),isdigit);
	if(itr == header_line.end())
	{
		if(header_line == "---End Matrices---")
		{
			return -1;
		}
		throw Exception("Did not find event number when reading matrix");
	}
	stringstream num(string(itr,header_line.end()));
	long long int event_number;
	num>>event_number;
	if(num.fail())
	{
		throw Exception("Could not read a valid event number from matrix, got " + num.str());
	}

	for(size_t i = 0; i < rows; i++)
		{
			for(size_t j =0; j < cols; j++)
			{
			//MARKER1
			double current_value;
			file_to_read.read(reinterpret_cast<char*>(&current_value),sizeof(current_value));
			matrix_to_read(i,j) = current_value;
		}
	}
	file_to_read.get();//Get rid of the newline after the data which exists purely for formatting reasons.
	string footer_line;
	getline(file_to_read,footer_line);
	return event_number;
}

void prettyPrintMatrix(ostream &output_stream,const AssociationMatrix &association_matrix,long long int event_number, bool writeSeparator)
{
	auto previous_precision = output_stream.precision();
	output_stream.precision(12);

	size_t rows = association_matrix.rows();
	size_t cols = association_matrix.cols();
	if (writeSeparator)
		output_stream<<"---Matrix at event "<<event_number<<" ---"<<std::endl;
	for(unsigned long long int i = 0; i < rows; i++)
	{
		for(unsigned long long int j = 0; j < cols; j++)
		{
			output_stream<<std::setw(16)<<association_matrix(i,j);
			output_stream<<' ';
		}
		output_stream<<std::endl;
	}
	output_stream.precision(previous_precision);
}

void prettyPrintMatrixFileToOstream(ostream &output_stream,istream &input_stream)
{
	auto matrix_file_info = readMatrixFileHeader(input_stream);
	vector<int> columns_to_ignore(matrix_file_info.cols,0);
	AssociationMatrix matrix_result{matrix_file_info.rows,matrix_file_info.cols,vector<int>(matrix_file_info.cols,0)};

	long long int event_number;
	while((event_number = readMatrixFromFile(input_stream,matrix_result,matrix_file_info.rows,matrix_file_info.cols)) != -1)
	{
		prettyPrintMatrix(output_stream,matrix_result,event_number);
	}
	return;
}

void printMatrixTabbed(ostream &output_stream,const AssociationMatrix &association_matrix,long long int event_number, bool writeSeparator, int precision)
{
	auto previous_precision = output_stream.precision();
	output_stream.precision(precision);

	size_t rows = association_matrix.rows();
	size_t cols = association_matrix.cols();
	if (writeSeparator)
		output_stream<<"---Matrix at event "<<event_number<<" ---"<<std::endl;
	for(unsigned long long int i = 0; i < rows; i++)
	{
		for(unsigned long long int j = 0; j < cols; j++)
		{
			output_stream<<association_matrix(i,j);
			if (j < (cols - 1))
				output_stream << '\t';
		}
		output_stream<<std::endl;
	}
	output_stream.precision(previous_precision);
}

void printMatrixScientific(ostream &output_stream,const AssociationMatrix &association_matrix,long long int event_number, bool writeSeparator, int precision)
{
	auto previous_precision = output_stream.precision();
	output_stream.precision(precision);

	output_stream << std::scientific;
	const int field_width(1 + 1 + 1 + 1 + precision + 1 + 1 + 3); // (space)-x.xxxxxe-xxx

	size_t rows = association_matrix.rows();
	size_t cols = association_matrix.cols();
	if (writeSeparator)
		output_stream<<"---Matrix at event "<<event_number<<" ---"<<std::endl;
	for(unsigned long long int i = 0; i < rows; i++)
	{
		for(unsigned long long int j = 0; j < cols; j++)
		{
			output_stream<<std::setw(field_width)<<association_matrix(i,j);
		}
		output_stream<<std::endl;
	}
	output_stream.precision(previous_precision);
}
