/*
 * This class serves as a wrapper for R.
 */
#include <Rcpp.h>
#include "common_type_definitions.h"
#include "file_management.h"
#include "../common/helper.h"
#include <string>
#include <vector>
#include <map>
#include <stdexcept>

using std::string;
using std::vector;
using std::invalid_argument;


using namespace Rcpp;


class IterativeRescorlaWagner
{
	public:
	IterativeRescorlaWagner(
			string folder_with_everything,
			string corpus_name
			) :association_matrix{0,0,{}} 
	{
    string folder_with_everything_full_path = getFullPath(folder_with_everything);
		if (!isDir(folder_with_everything_full_path))
			throw std::invalid_argument(folder_with_everything + " does not exist in filesystem or it is not a folder");

		if (pathExists(folder_with_everything_full_path + corpus_name + ".alphas"))
			alphas_filename = folder_with_everything_full_path + corpus_name + ".alphas";

		if (pathExists(folder_with_everything_full_path + corpus_name + ".betas"))
			betas_filename = folder_with_everything_full_path + corpus_name + ".betas";

		string cues_file = folder_with_everything_full_path + corpus_name + ".cues";
		if (!pathExists(cues_file) || !isFile(cues_file))
			throw std::invalid_argument(cues_file + " does not exist or is not a regular file.");
		this->cues_file = cues_file;

		string outcomes_file = folder_with_everything_full_path + corpus_name + ".outcomes";
		if (!pathExists(outcomes_file) || !isFile(outcomes_file))
			throw std::invalid_argument(outcomes_file + " does not exist or is not a regular file.");
		this->outcomes_file = outcomes_file;


		string events_folder = folder_with_everything_full_path + corpus_name + ".events";
		if (!pathExists(events_folder) || !isDir(events_folder))
			throw std::invalid_argument(events_folder + " does not exist or is not a directory.");
		this->events_folder_name = events_folder;

		this->num_rows = 0;
		this->num_cols = 0;
	};

	void init(double default_alpha, double default_beta, bool verbose)
	{
		bootstrap_model(
				association_matrix,
				alphas,
				betas,
				to_trace,
				outcomes_to_ignore,
				num_rows,
				num_cols,
				default_alpha,
				alphas_filename,
				default_beta,
				betas_filename,

				outcomes_file,
				cues_file,

				!verbose,
				string{"Yes please, pretend to output matrices"}, //Passing a non-empty string so that outcomes aren't ignored.

				false
			);
		outcomes = read_space_separated_file<size_t>(outcomes_file).first;
		cues = read_space_separated_file<size_t>(cues_file).first;
		initialized = true;
	};

	size_t getNumActualCols()
	{
		return association_matrix.num_actual_cols();
	}

	size_t learn(size_t first_event, size_t num_events, double lambda,
	bool ignore_duplicates, size_t num_threads, bool verbose, bool debug)
	{
		if(!initialized)
			throw std::invalid_argument("Please call init() first.");

		//We don't really need this, so create a dummy parameter...
		string empty{""}; //Because the next function needs a reference to a string
		matrixOutputInfo output_info(empty,matrixFileInfo{});
		long long int learned = read_events_files(
				events_folder_name,
				association_matrix,
				alphas,
				betas,
				to_trace,
				lambda,
				!ignore_duplicates,
				outcomes_to_ignore,
				output_info,
				num_events,
				!verbose,
				num_threads,
				first_event,
				debug
				);
		// Don't compare the learned count with the num_events requested, as
		// sometimes more events are requested to be learned, than are actually
		// available and we don't want to treat this situation as an error.
		return learned;
	};

	void setMatrix(const NumericMatrix& newMatrix)
	{
		for(size_t i = 0; i < num_cols; i++)
		{
			for(size_t j = 0; j < num_rows; j++)
				association_matrix(j,i) = newMatrix(j,i);
		}
	};

	NumericMatrix getMatrix(const List& learnerInfo, const List& dimNames)
	{
		NumericMatrix result(num_rows,num_cols);
		if (dimNames.length() > 0)
			result.attr("dimnames") = dimNames;
		// We add the attributes here, if any, rather than in R to avoid copying.
		if (learnerInfo.length() > 0)
			result.attr("info") = learnerInfo;

		for(size_t i = 0; i < num_cols; i++)
		{
			for(size_t j = 0; j < num_rows; j++)
				result(j,i) = association_matrix(j,i);
		}
		return result;
	};

	NumericMatrix getCols(const NumericVector& cols, const List& dimNames)
	{
		NumericMatrix result(num_rows,cols.length());
		if (dimNames.length() > 0)
			result.attr("dimnames") = dimNames;
		for(auto k = 0; k < cols.length(); k++)
		{
			auto colIdx = cols[k];
			for(size_t i = 0; i < num_rows; i++) {
				result(i,k) = association_matrix(i,colIdx);
			}
		}
		return result;
	}

	NumericMatrix getRows(const NumericVector& rows, const List& dimNames)
	{
		NumericMatrix result(rows.length(), num_cols);
		if (dimNames.length() > 0)
			result.attr("dimnames") = dimNames;
		for(auto k = 0; k < rows.length(); k++)
		{
			auto rowIdx = rows[k];
			for(size_t j = 0; j < num_cols; j++)
				result(k,j) = association_matrix(rowIdx,j);
		}
		return result;
	}

	NumericMatrix getWeights(const NumericVector& rows, const NumericVector& cols, const List& dimNames)
	{
		auto row_count = rows.length();
		auto col_count = cols.length();
		NumericMatrix result(row_count, col_count);
		if (dimNames.length() > 0)
			result.attr("dimnames") = dimNames;
		for(auto k = 0; k < row_count; k++)
		{
			auto rowIdx = rows[k];
			for(auto l = 0; l < col_count; l++)
				result(k,l) = association_matrix(rowIdx,cols[l]);
		}
		return result;
	}

	void writeMatrixWithoutInfo(const string& filePath, long long int currentEvent, bool binary, int precision)
	{
		ofstream output_stream(filePath);
		if (binary) {
			appendMatrixToFile(output_stream, association_matrix, currentEvent, false);
		} else {
			printMatrixTabbed(output_stream, association_matrix, currentEvent, false, precision);
		}
		output_stream.close();
	}

	size_t num_rows;
	size_t num_cols;
	vector<double> alphas;
	vector<double> betas;
	vector<int> outcomes_to_ignore;
	bool initialized = false;
	private:
	AssociationMatrix association_matrix;
	TraceInfo to_trace;
	string alphas_filename;
	string betas_filename;
	string cues_file;
	string outcomes_file;
	string events_folder_name;
	map<string,size_t> cues;
	map<string,size_t> outcomes;

};


RCPP_MODULE(testle)
{
	class_<IterativeRescorlaWagner>("IterativeRescorlaWagner")
		.constructor<string, string>("Folder name, corpus name")
		.field_readonly("num_cues", &IterativeRescorlaWagner::num_rows)
		.field_readonly("num_outcomes", &IterativeRescorlaWagner::num_cols)
		.field_readonly("alphas", &IterativeRescorlaWagner::alphas)
		.field_readonly("betas", &IterativeRescorlaWagner::betas)
		.field_readonly("ignored_outcomes", &IterativeRescorlaWagner::outcomes_to_ignore)
		.field_readonly("initialized", &IterativeRescorlaWagner::initialized)
		.method("setMatrix",&IterativeRescorlaWagner::setMatrix)
		.method("getMatrix",&IterativeRescorlaWagner::getMatrix)
		.method("getRows", &IterativeRescorlaWagner::getRows)
		.method("getCols", &IterativeRescorlaWagner::getCols)
		.method("getWeights", &IterativeRescorlaWagner::getWeights)
		.method("writeMatrixWithoutInfo", &IterativeRescorlaWagner::writeMatrixWithoutInfo)
		.method("init",&IterativeRescorlaWagner::init)
		.method("learn",&IterativeRescorlaWagner::learn)
		.method("getNumActualCols",&IterativeRescorlaWagner::getNumActualCols);
}
