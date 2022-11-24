/* (c) 2013 Nathanael Schilling * This File defines various utils
 */
#ifndef ITERATIVE_RESCORLA_WAGNER_UTILS_H
#define ITERATIVE_RESCORLA_WAGNER_UTILS_H

#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 500
#endif
#include <ftw.h>
#include <stdio.h>
#include <math.h>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <functional>
#include <map>
#include <sstream>

#include "../common/Exception.h"

using std::string;
using std::vector;
using std::function;
using std::map;
using std::pair;

/* This function does approximately what rm -r does, but only for directories.
 */
void recursively_remove_directory(const char* directory_name);

/* Split a string on delim, but ignoring any empty substrings*/
vector<string> split_ignoring_empty(const string& input_string,const char delim);

//Takes as parameters a vector<T> and mapfunction where mapfunction[T] gives U.
//Returns a vector<U> so that for each element i output[i] =  mapfunction(input[i])
template<class T,class U>
vector<U> functional_map(const vector<T> &input, function<U(T)> mapper)
{
	vector<U> result;
	for(auto &i : input)
	{
		result.push_back(mapper(i));
	}
	return result;
}
//input_string should be in utf-8 encoding, returns the nfkc-normalized form of that string, also in utf-8 encoding.
string normalize_to_nfkc_in_utf8(const string &input_string);

//We read from a file a set of key,value values. 
//Returning a complex type like this should be okay here because of c++11 move semantics.
template<class V> std::pair<std::map<string,V>,V > read_space_separated_file(string &file_name)
{
	V max = 0;
	std::ifstream input_file{file_name};

	if(!input_file.good())
	{
		throw Exception("Could not open input file " + file_name);
	}
	string key;
	V value;
	std::pair<std::map<string,V>,V> result{};

	size_t line_num = 0;
	while(true)
	{
		++line_num;
		input_file>>key;
		if(input_file.eof())
			break;
		input_file>>value;
		if(!input_file.good())
		{
			throw Exception("Input file " + file_name +
					" does not consist of space separated key value pairs like it should, exiting (key = '" +
					key + "', around line " + std::to_string(line_num) + ")");
		}
		input_file.get();
		if(value > max)
			max = value;

		auto out = result.first.insert(typename std::map<string,V>::value_type{normalize_to_nfkc_in_utf8(key),value});	
		if(!out.second)
		{
			throw Exception("Warning: duplicate key '" + key + "' in file " + file_name +
					" around line " + std::to_string(line_num));
		}
	}
	if(!input_file.eof())
	{
		throw Exception("Failed to read in file " + file_name);
	}
	result.second = max;
	return result;
}

//Join elements of to_join using delim.
string join(const vector<string> &to_join,const string &delim);

//Count how many occurances of what are in the sorted vector from.

inline size_t count_sorted(const vector<size_t> &from,const size_t &what)
{
	size_t result = 0;
	for(const auto &i : from)
	{
		if(what == i)
			result++;
		else if (what < i)
			break;
	}
	return result;
}

vector<pair<size_t,size_t>> split_range(size_t begin, size_t end, size_t num_chunks);


#endif

