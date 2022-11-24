#include "rescorla_wagner.h"

void update_association_matrix_in_response_to_event(
		AssociationMatrix &association_matrix,
		const Event &current_event,
		const vector<double> &alphas,
		const vector<double> &betas,
		const double lambda,
		const bool has_duplicates,
		const vector<int> &outcomes_to_ignore, //This is actually a vector<bool>, but as that's relatively slow in c++, we avoid using it
		const pair<size_t,size_t> &this_threads_region
		)
{

        if (current_event.Freq > 1)
	{
             throw Exception("You are attempting to learn from events that were not collected sequentially (there was an event frequency greater than 1).  This does not work with the iterative learning method. Please use the Danks equilibria instead. Exiting...");
        }
	// Only check order of events when compiled with debug flag.
	// Otherwise this might really impact performance on big data sets.
//	#ifndef PERFORMANCE
	ssize_t prev = -1;
	for(const auto &j : current_event.Outcomes)
	{
		if(!((ssize_t) j >= prev))
		{
			throw Exception("Outcomes do not seem to be sorted, exiting..");
		}
		prev = j;
	}
	prev = -1;
	for(const auto & i : current_event.Cues)
	{
		if(!((ssize_t) i >= prev))
		{
			throw Exception("Cues do not seem to be sorted, exiting...");
		}
		prev = i;
	}
//	#endif //PERFORMANCE

	//Iterate over all of the outcomes that this thread is responsible for.
	for(size_t j = this_threads_region.first; j < this_threads_region.second; j++)
	{
		//We only calculate the model for outcomes that we need to know in the end.
		if(association_matrix.getZeroCols() && outcomes_to_ignore[j])
			continue;
		//We see how many times this current outcome is present.
		size_t how_many_outcomes = count_sorted(current_event.Outcomes,j);
		//int how_many_outcomes = std::count(current_event.Outcomes.begin(),current_event.Outcomes.end(),j);
	
		
		//We store the current_beta locally, as this might be faster than calling betas[j] every time (maybe).
		const double current_beta = betas[j];
		//We calculate the sum of the associative strengths for the current outcome, 
		//	by summing the associative strengths of all present cues.
		double current_total_associative_strength = 0;
		//As the cues are sorted, we can compare the current cue to the previous cue to ignore duplicate cues.
		ssize_t previous_cue = -1;
		for(const auto &i : current_event.Cues)
		{
			//Check if this cue occured already in the case of not treating duplicate cues differently
			if((!has_duplicates) && (previous_cue == (ssize_t) i))
			{
				continue;
			}
			current_total_associative_strength += association_matrix(i,j);
			previous_cue = i;
		}
		previous_cue = -1;
		for(const auto &i : current_event.Cues)
		{
			if(!has_duplicates && (previous_cue == (ssize_t) i))
				continue;
			double current_alpha = alphas[i];

			//Note that multiplier is zero if the current outcome did not exist in this event.
			int multiplier = has_duplicates ? how_many_outcomes: (how_many_outcomes > 0);
			
			double what_to_add = 0.0;
			what_to_add -= (current_beta * current_alpha * current_total_associative_strength);
			if(multiplier)
			  what_to_add *= multiplier;
			what_to_add += (current_alpha * current_beta * lambda) * multiplier;
			association_matrix(i,j) += what_to_add;

			previous_cue = i;
		}
	}
	return;
}


static void output_trace_directive(const AssociationMatrix &association_matrix,const singleTraceDirective &current_trace_directive,const long long int &event_count)
{
	if(current_trace_directive.directiveType == traceDirectiveType::CUE_OUTCOME)
	{
		cout<<event_count<<"\t"<<"("<<current_trace_directive.cueId<<","<<current_trace_directive.outcomeId<<")\t"
			<<association_matrix(current_trace_directive.cueId,current_trace_directive.outcomeId)
			<<std::endl;
	}else if(current_trace_directive.directiveType == traceDirectiveType::COSINE)
	{
		double result = 0.0;

		double euclidean_length_first = 0.0;
		const size_t first_outcome_id = current_trace_directive.outcomeId;
		const size_t rows = association_matrix.rows();
		for(size_t i = 0; i < rows; i++)
		{
			double value = association_matrix(i,first_outcome_id);
			euclidean_length_first += (value * value);
		}
		euclidean_length_first = std::sqrt(euclidean_length_first);

		double euclidean_length_second = 0.0;
		const size_t second_outcome_id = current_trace_directive.secondOutcomeId;
		for(size_t i = 0; i < rows; i++)
		{
			double value = association_matrix(i,second_outcome_id);
			euclidean_length_second += (value * value);
		}
		euclidean_length_second = std::sqrt(euclidean_length_second);

		if((euclidean_length_first == 0.0) || (euclidean_length_second == 0.0))
		{
			result = std::numeric_limits<double>::quiet_NaN();
		}else
		{
			double dot_product = 0.0;
			for(size_t i = 0; i< rows;i++)
			{
				dot_product += (association_matrix(i,first_outcome_id)*association_matrix(i,second_outcome_id));
			}
			result = dot_product / (euclidean_length_first * euclidean_length_second);
		}


		cout<<event_count<<"\t"<<"cosine("<<current_trace_directive.outcomeId<<","<<current_trace_directive.secondOutcomeId<<")\t"
			<<result
			<<std::endl;
	}else if(current_trace_directive.directiveType == traceDirectiveType::EUCLIDEAN)
	{
		const size_t first_outcome_id = current_trace_directive.outcomeId;
		const size_t second_outcome_id = current_trace_directive.secondOutcomeId;
		const size_t num_rows = association_matrix.rows();
		double euclidean_distance = 0.0;
		for(size_t i = 0; i < num_rows;i++)
		{
			double first = association_matrix(i,first_outcome_id);
			double second = association_matrix(i,second_outcome_id);
			double difference = first - second;
			euclidean_distance += (difference*difference);
		}
		euclidean_distance = std::sqrt(euclidean_distance);
		cout<<event_count<<"\t"<<"euclidean("<<current_trace_directive.outcomeId<<","<<current_trace_directive.secondOutcomeId<<")\t"
			<<euclidean_distance
			<<std::endl;
	}
}

//Set suppress_output to true if nothing should be output on this trace.
void output_trace_info(
		AssociationMatrix &association_matrix,
		TraceInfo &to_trace,
		long long int event_count,
		bool suppress_output
		)
{
	for(auto &current_trace_directive : to_trace)
	{
		switch(current_trace_directive.directiveWhen)
		{

		case traceDirectiveWhen::AT:

			if(current_trace_directive.directiveEventParam == event_count)
			{
				if(!suppress_output)
				{
					output_trace_directive(association_matrix,current_trace_directive,event_count);
				}
			}

		break;

		case traceDirectiveWhen::EVERY:
			if(!event_count || !(event_count % current_trace_directive.directiveEventParam))
			{
				if(!suppress_output)
				{
					output_trace_directive(association_matrix,current_trace_directive,event_count);
				}
			}
		break;

		case traceDirectiveWhen::FIRST:
			if(current_trace_directive.directiveEventParam)
			{
				if(!suppress_output)
				{
					output_trace_directive(association_matrix,current_trace_directive,event_count);
				}
			}else
			{
				current_trace_directive.directiveWhen = traceDirectiveWhen::NEVER;
			}
			--current_trace_directive.directiveEventParam;

		break;

		default:
		break;
		}
	}
	return;
}
