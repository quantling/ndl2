#ifndef MATRIX_IO_H
#define MATRIX_IO_H

#include <fstream>
#include <iostream>
#include <string>
#include <algorithm>
#include <iomanip>

#include <limits.h>

#include "common_type_definitions.h"
#include "util.h"
#include "../common/Exception.h"

using std::ostream;
using std::ofstream;
using std::istream;
using std::ifstream;
using std::string;

//This class describes a matrix file.
struct matrixFileInfo
{
	matrixFileInfo()
	{
		events_at_which_to_write.push_back(LLONG_MAX);
	};
	matrixFileInfo(
			size_t rows_in,
			size_t cols_in,
			const std::string &folder_name_in,
			const std::string &corpus_name_in,
			double default_alpha_in,
			double default_beta_in,
			std::vector<long long int> events_at_which_to_write_in,
			bool has_duplicates_in
			)
	       	:
			rows(rows_in),
			cols(cols_in),
			folder_name(folder_name_in),
			corpus_name(corpus_name_in),
			default_alpha(default_alpha_in),
			default_beta(default_beta_in),
			events_at_which_to_write(events_at_which_to_write_in),
			has_duplicates(has_duplicates_in)
	{
		events_at_which_to_write.push_back(LLONG_MAX);
	};
	size_t rows;
	size_t cols;
	std::string folder_name;
	std::string corpus_name;
	double default_alpha;
	double default_beta;
	std::vector<long long int> events_at_which_to_write;
	bool has_duplicates;
};

//This class is used to output matrix files. Passing an empty string to the constructor means no matrices should be output.
class matrixOutputInfo
{
	public:
		matrixOutputInfo() = delete;
		matrixOutputInfo(string &output_filename,const matrixFileInfo &matrix_file_info);
		void writeHeader();
		void appendMatrix(const AssociationMatrix &association_matrix,long long int &event_count);
		void writeFooter(void);
		matrixFileInfo matrix_file_info;
	private:
		string output_filename;
		ofstream output_file;

};

//Writes a header to a file to which the matrix output should be written. This contains information such as the number of rows or
//number of columns, and needs to be called before calling appendMatrixToFile();
void writeMatrixFileHeader(
		ofstream &file_to_write,
		const matrixFileInfo &matrix_file_info
		);
//Writes a footer for the matrix, telling whatever is reading it that there are no more matrices.
void writeMatrixFileFooter(
		ofstream &file_to_write
		);	

//Reads the header of the file, returning the number of rows and columns (in that order).
matrixFileInfo readMatrixFileHeader(istream &file_to_read);

//Writes the specified matrix in a binary format to the end of the file.
void appendMatrixToFile(
		ofstream &file_to_write,
		const AssociationMatrix &current_matrix,
		long long int event_count,
		bool writeSeparator = true
		);

//Reads a single matrix from the file at the position that file_to_read is at.
//Returns the event number at which the matrix was output, or -1 if there is no more matrix in the file.
long long int readMatrixFromFile(
		istream &file_to_read,
		AssociationMatrix &matrix_to_read,
		size_t rows,
		size_t cols
		);

//Prints the matrix to the specified output stream in ascii format
void prettyPrintMatrix(
		ostream &output_stream,
		const AssociationMatrix &association_matrix,
		long long int event_number,
		bool writeSeparator = true
		);

void prettyPrintMatrixFileToOstream(
		ostream &output_stream,
		istream &input_stream
		);

void printMatrixTabbed(
		ostream &output_stream,
		const AssociationMatrix &association_matrix,
		long long int event_number,
		bool writeSeparator = true,
		int precision = 12
		);

void printMatrixScientific(
		ostream &output_stream,
		const AssociationMatrix &association_matrix,
		long long int event_number,
		bool writeSeparator = true,
		int precision = 12
		);

#endif
