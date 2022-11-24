#include "trace_setup.h"
#include <iostream>
#include <boost/regex.hpp>
#include "util.h"
#include "../common/Exception.h"

using std::cerr;
using std::endl;
using boost::regex;
using boost::regex_match;
using boost::smatch;

//This small utility function outputs something like Tracing\t(this,the)\tas\t(1,2)
//Note that it does not output anything about when the tracedirective is active.
void say_start_of_what_is_being_traced(const singleTraceDirective &current_trace_directive_without_when)
{
	//IMPORTANT: Something like "Tracing\tfoo\tas\tbar" *must* be output for every trace, as the web interface depends on it.
	if(current_trace_directive_without_when.directiveType == traceDirectiveType::CUE_OUTCOME)
	{
		cerr<<"Tracing\t("<<current_trace_directive_without_when.cueName<<","<<current_trace_directive_without_when.outcomeName<<")\tas\t("<<current_trace_directive_without_when.cueId<<","<<current_trace_directive_without_when.outcomeId
		<<")";
	}else if(current_trace_directive_without_when.directiveType == traceDirectiveType::COSINE)
	{
		cerr<<"Tracing\tcosine("<<current_trace_directive_without_when.outcomeName<<","<<current_trace_directive_without_when.secondOutcomeName<<")\tas\tcosine("<<current_trace_directive_without_when.outcomeId<<","<<current_trace_directive_without_when.secondOutcomeId
		<<")";
	}else if(current_trace_directive_without_when.directiveType == traceDirectiveType::EUCLIDEAN)
	{

		cerr<<"Tracing\teuclidean("<<current_trace_directive_without_when.outcomeName<<","<<current_trace_directive_without_when.secondOutcomeName<<")\tas\teuclidean("<<current_trace_directive_without_when.outcomeId<<","<<current_trace_directive_without_when.secondOutcomeId
		<<")";
	}
	//IMPORANT: See above.
}

//The long-named function below sets those parts of the trace directive related to when it is active.
void set_traceDirectiveWhen_for_singleTraceDirective_and_add_to_TraceInfo(
		TraceInfo &what_we_add_it_to,
		singleTraceDirective &current_trace_directive_without_when,
		const string &when_clause
		)
{
		regex trace_param_regex{R"(^\((every|at|first)\s([\d ]+)\)$)"};
		smatch matches;
		if(!regex_match(when_clause,matches,trace_param_regex))
		{
			throw Exception("Trace parameter '" + when_clause + " does not have correct format.");
		}
		//For some reason my functional_map function doesn't work with the "normal" stol:
		std::function<long long int(string)> my_stol = [](string in)->long long int {return stol(in);};
		vector<long long int> numbers;
		try
		{
			numbers = functional_map(split_ignoring_empty(matches[2],' '), 
					my_stol
					);
		}catch(...)
		{
			throw Exception("Invalid numbers in trace parameter");
		}
		if((matches[1] == "every") || matches[1] == "first")
		{
			if(matches[1] == "every")
			{
				current_trace_directive_without_when.directiveWhen = traceDirectiveWhen::EVERY;
			}else
			{
				current_trace_directive_without_when.directiveWhen = traceDirectiveWhen::FIRST;
			}

			if(numbers.size() != 1)
			{
				throw Exception("Only a single number after 'every' allowed, exiting...");
			}
			current_trace_directive_without_when.directiveEventParam = numbers[0];
			what_we_add_it_to.push_back(current_trace_directive_without_when);
			say_start_of_what_is_being_traced(current_trace_directive_without_when);
			if(matches[1] == "every")
			{
				cerr<<"\tevery "<<numbers[0]<<" iterations."<<endl;
			}else
			{
				cerr<<"\tfor the first "<<numbers[0]<<" iterations."<<endl;
			}
		}else if(matches[1] == "at")
		{
			for(const auto &i : numbers)
			{
				singleTraceDirective this_trace_directive = current_trace_directive_without_when;
				this_trace_directive.directiveWhen = traceDirectiveWhen::AT;
				this_trace_directive.directiveEventParam = i;
				what_we_add_it_to.push_back(std::move(this_trace_directive));
				say_start_of_what_is_being_traced(current_trace_directive_without_when);
				cerr<<"\tat event "<<i<<endl;
			}
		}
}

//This function reads tracedirectives from an input stream.
TraceInfo read_traceinfo_from_file(istream &infile,const map<string,size_t> &cues,const map<string,size_t> &outcomes)
{
	TraceInfo result;
	string current_trace_directive;
	while(std::getline(infile,current_trace_directive))
	{
		if(current_trace_directive == "")
		{
			cerr<<"Received empty line as trace directive, skipping it"<<endl;
			continue;
		}
		//Because boost doesn't seem to give any guarantees for how it treats utf-8 in its regular expressions, we parse cues and outcomes by hand.
		if(current_trace_directive.substr(0,strlen("cosine(")) == "cosine(")
		{
			string::iterator comma = std::find(current_trace_directive.begin() + strlen("cosine("),current_trace_directive.end(),',');
			if(comma == current_trace_directive.end())
			{
				throw Exception("Read invalid trace directive without a comma '" + current_trace_directive + " exiting.");
			}
			string first_outcome(current_trace_directive.begin() + strlen("cosine("),comma);
			string::iterator end_of_outcome = std::find(comma+1,current_trace_directive.end(),')');
			if(end_of_outcome == current_trace_directive.end())
			{
				throw Exception("Please give closing bracket for cosine trace");
			}
			if((end_of_outcome + 1) != current_trace_directive.end() && (*(end_of_outcome + 1) != '\t'))
			{
				throw Exception("Invalid character after closing bracket of cosine trace");
			}
			string second_outcome(comma + 1,end_of_outcome);
			if(!first_outcome.length() || !second_outcome.length())
			{
				throw Exception("There is an empty first or second outcome in cosine trace, exiting");
			}
			auto first_outcome_itr = outcomes.find(normalize_to_nfkc_in_utf8(first_outcome));
			if(first_outcome_itr == outcomes.end())
			{
				throw Exception("Did not find the outcome '" + first_outcome + " in list of outcomes.");
			}
			auto second_outcome_itr = outcomes.find(normalize_to_nfkc_in_utf8(second_outcome));
			if(second_outcome_itr == outcomes.end())
			{
				throw Exception("Did not find the outcome '" + second_outcome + "' in list of outcomes.");
			}

			singleTraceDirective current_trace_directive_without_when;
			current_trace_directive_without_when.directiveType = traceDirectiveType::COSINE;
			current_trace_directive_without_when.outcomeId = first_outcome_itr->second;
			current_trace_directive_without_when.outcomeName = normalize_to_nfkc_in_utf8(first_outcome);
			current_trace_directive_without_when.secondOutcomeId = second_outcome_itr->second;
			current_trace_directive_without_when.secondOutcomeName = normalize_to_nfkc_in_utf8(second_outcome);
			string trace_directive_when;
			if((end_of_outcome +1) == current_trace_directive.end())
				trace_directive_when = "(every 1)";
			else
			{
				if(*(end_of_outcome + 1) != '\t')
				{
					throw Exception("Invalid format, expected tab or nothing after ')' in trace '" + current_trace_directive + "', exiting");
				}
				trace_directive_when= string(end_of_outcome + 2,current_trace_directive.end());
			}

			set_traceDirectiveWhen_for_singleTraceDirective_and_add_to_TraceInfo(result,current_trace_directive_without_when,trace_directive_when);
			
		}else if(current_trace_directive.substr(0,strlen("euclidean(")) == "euclidean(")
		{
			string::iterator comma = std::find(current_trace_directive.begin() + strlen("euclidean("),current_trace_directive.end(),',');
			if(comma == current_trace_directive.end())
			{
				throw Exception("Read invalid trace directive without a comma '" + current_trace_directive + "', exiting.");
			}
			string first_outcome(current_trace_directive.begin() + strlen("euclidean("),comma);
			string::iterator end_of_outcome = std::find(comma+1,current_trace_directive.end(),')');
			if(end_of_outcome == current_trace_directive.end())
			{
				throw Exception("Please give closing bracket for euclidean distance trace");
			}
			if((end_of_outcome + 1) != current_trace_directive.end() && (*(end_of_outcome + 1) != '\t'))
			{
				throw Exception("Invalid character after closing bracket of euclidean distance trace");
			}
			string second_outcome(comma + 1,end_of_outcome);
			if(!first_outcome.length() || !second_outcome.length())
			{
				throw Exception("There is an empty first or second outcome in euclidean distance trace, exiting");
			}
			auto first_outcome_itr = outcomes.find(normalize_to_nfkc_in_utf8(first_outcome));
			if(first_outcome_itr == outcomes.end())
			{
				throw Exception("Did not find the outcome '" + first_outcome + "' in list of outcomes.");
			}
			auto second_outcome_itr = outcomes.find(normalize_to_nfkc_in_utf8(second_outcome));
			if(second_outcome_itr == outcomes.end())
			{
				throw Exception("Did not find the outcome '" + second_outcome + "' in list of outcomes.");
			}

			singleTraceDirective current_trace_directive_without_when;
			current_trace_directive_without_when.directiveType = traceDirectiveType::EUCLIDEAN;
			current_trace_directive_without_when.outcomeId = first_outcome_itr->second;
			current_trace_directive_without_when.outcomeName = normalize_to_nfkc_in_utf8(first_outcome);
			current_trace_directive_without_when.secondOutcomeId = second_outcome_itr->second;
			current_trace_directive_without_when.secondOutcomeName = normalize_to_nfkc_in_utf8(second_outcome);
			string trace_directive_when;
			if((end_of_outcome +1) == current_trace_directive.end())
				trace_directive_when = "(every 1)";
			else
			{
				if(*(end_of_outcome + 1) != '\t')
				{
					throw Exception("Invalid format, expected tab or nothing after ')' in trace '" + current_trace_directive + "', exiting");
				}
				trace_directive_when= string(end_of_outcome + 2,current_trace_directive.end());
			}
			set_traceDirectiveWhen_for_singleTraceDirective_and_add_to_TraceInfo(result,current_trace_directive_without_when,trace_directive_when);
			
		}else
		{
			string::iterator comma = std::find(current_trace_directive.begin(),current_trace_directive.end(),',');
			if(comma == current_trace_directive.end())
			{
				throw Exception("Read invalid trace directive without a comma '" + current_trace_directive + "', exiting.");
			}
			string cue(current_trace_directive.begin(), comma);
			string::iterator end_of_outcome = find(current_trace_directive.begin(),current_trace_directive.end(),'\t');
			string outcome(comma+1,end_of_outcome);
			if(!cue.size() || !outcome.size())
			{
				throw Exception("Read invalid trace directive without cue or outcome '" + current_trace_directive + "', exiting");
			}
	
			auto cueID_itr = cues.find(normalize_to_nfkc_in_utf8(cue));
			if(cueID_itr == cues.end())
			{
				throw Exception("Did not find the cue '" + cue + "' in the list of cues, exiting.");
			}

			auto outcomeID_itr = outcomes.find(normalize_to_nfkc_in_utf8(outcome));
			if(outcomeID_itr == outcomes.end())
			{
				throw Exception("Did not find the outcome '" + outcome + "' in the list of outcomes, exiting.");
			}

			string trace_param;
			if(end_of_outcome == current_trace_directive.end())
			{
				trace_param = "(every 1)";
			}else
			{
				trace_param = string(end_of_outcome+1, current_trace_directive.end());
			}
			singleTraceDirective current_trace_directive_without_when;
			current_trace_directive_without_when.cueId = cueID_itr->second;
			current_trace_directive_without_when.outcomeId = outcomeID_itr->second;
			current_trace_directive_without_when.cueName = normalize_to_nfkc_in_utf8(cue);
			current_trace_directive_without_when.outcomeName = normalize_to_nfkc_in_utf8(outcome);
			current_trace_directive_without_when.directiveType = traceDirectiveType::CUE_OUTCOME;
			set_traceDirectiveWhen_for_singleTraceDirective_and_add_to_TraceInfo(result,current_trace_directive_without_when,trace_param);
		}
	}
	return result;
}

//This function updates outcomes_to_ignore to have zeroes for outcomes that are output by the trace directives.
void unignore_outcomes_from_traces(vector<int> &outcomes_to_ignore, const TraceInfo &to_trace)
{
	for(const auto &i : to_trace)
	{
		outcomes_to_ignore[i.outcomeId] = 0;
		if((i.directiveType == traceDirectiveType::COSINE) || (i.directiveType == traceDirectiveType::EUCLIDEAN))
			outcomes_to_ignore[i.secondOutcomeId] = 0;
	}
	return;
}

