//(c) 2013 Nathanael Schilling
//This file contains type definitions used throughout this model.
#ifndef COMMON_TYPE_DEFINITIONS_H
#define COMMON_TYPE_DEFINITIONS_H

#include <vector>
#include <string>
#include <tuple>
//#include <Rcpp.h>

#include "fast_matrices.h"

//We depend on this type being double at MARKER1
using AssociationMatrix = myMatrix<double>;

enum class traceDirectiveWhen {EVERY,AT,FIRST,NEVER};
enum class traceDirectiveType {CUE_OUTCOME,COSINE,EUCLIDEAN};
struct singleTraceDirective
{
	size_t cueId;
	std::string cueName;

	size_t outcomeId;
	std::string outcomeName;

	size_t secondOutcomeId;
	std::string secondOutcomeName;

	traceDirectiveType directiveType;

	traceDirectiveWhen directiveWhen;

	long long int directiveEventParam;
};

using TraceInfo = std::vector<singleTraceDirective>;

#endif
