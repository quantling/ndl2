#if USING_RCPP
  #include <Rcpp.h>
  using namespace Rcpp;
  #define RETURN_TYPE DataFrame
#else
  #define RETURN_TYPE void
#endif

#include <string>

RETURN_TYPE viewEvents(long long first_event, long long num_events,
    std::string filepath, std::string name, bool show_frequency,
    bool show_event_number, bool verbose, const std::string glue);
