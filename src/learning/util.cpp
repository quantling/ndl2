#include "util.h"
#include <unicode/unistr.h>
#include <unicode/normalizer2.h>

using U_ICU_NAMESPACE::UnicodeString;

//The function below is used by recursively_remove_directory:
static int callback_function(const char* filepath,const struct stat *ignore_one, int ignore_two,struct FTW* ignore_three)
{
	(void) ignore_one;
	(void) ignore_two;
	(void) ignore_three;

	remove(filepath);
	return 0;
}

void recursively_remove_directory(const char* directory_name)
{
	nftw(directory_name, &callback_function, 2, FTW_DEPTH);
}

vector<string> split_ignoring_empty(const string& input_string,const char delim)
{
	vector<string> result;
	auto itr = input_string.begin();
	auto eow = itr;//End of word
	for(;;itr++)
	{
		if((itr == input_string.end()) || (*itr == delim))
		{
			if(itr - eow)
			{
				result.push_back(string(eow,itr));
			}
			//Assigning eow to itr if itr == input_string.end() is pointless,
			//but (input_string.end() + 1) might be undefined.
			eow = (itr == input_string.end())? itr : itr+1;
		}
		if(itr == input_string.end())
			break;
	}	
	return result;
}
string join(const vector<string> &to_join,const string &delim)
{
	std::ostringstream result{""};
	for(size_t i = 0; i < to_join.size(); i++)
	{
		result<<to_join[i];
		if((i+1) != to_join.size())
		{
			result<<delim;
		}
	}
	return result.str();
}

string normalize_to_nfkc_in_utf8(const string &input_string)
{
  auto errorCode = U_ZERO_ERROR;
  auto nfkcNormalizer = icu::Normalizer2::getNFKCInstance(errorCode);

  auto unicodeString = UnicodeString::fromUTF8(input_string);
  unicodeString = nfkcNormalizer->normalize(unicodeString, errorCode);

  if (U_FAILURE(errorCode)) {
    throw Exception("Failed to normalize '" + input_string +
        ", icu error code = " + std::to_string(errorCode));
  }

  string outputString;
  unicodeString.toUTF8String(outputString);
  return outputString;
}


vector<pair<size_t,size_t>> split_range(size_t begin, size_t end, size_t num_chunks)
{
	if (end < begin)
		throw Exception("split_range() called with 'end' before 'begin'.");
	if (num_chunks < 1)
		throw Exception("At least one chunk is needed for split_range()");

	vector<pair<size_t,size_t>> result{};
	size_t total = end - begin;
	double approximate_chunk_size = (double)total / (double)num_chunks;

	size_t prev_end = begin;
	for (size_t i = 1; i <= num_chunks; i++)
	{
		size_t new_end = round(begin + i * approximate_chunk_size);
		result.push_back({prev_end, new_end});
		prev_end = new_end;
	}

	if (result[num_chunks-1].second != end)
	{
		// For very large input numbers rounding mistakes can occur.
		std::cerr << "split_range() returned as end " << result[num_chunks-1].second <<
		  " instead of " << end << ". Correcting it." << std::endl;
		  result[num_chunks-1].second = end;
	}
	return result;
}
