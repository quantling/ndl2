/* 
Copyright (C) 2012,2013,2014,2015  Cyrus Shaoul, Samuel Bitschau

This file is part of the ndl package.

    ndl is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The ndl package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with HiDEx in the COPYING.txt file.
    If not, see <http://www.gnu.org/licenses/>.


*/
#include "common/view_events.h"

// [[Rcpp::export(".viewEvents")]]
Rcpp::DataFrame viewEventsWrapper(long long first_event, long long num_events,
    std::string filepath, std::string name, bool show_frequency, bool verbose,
    const std::string glue) {
  return(viewEvents(first_event, num_events, filepath, name, show_frequency,
        true, verbose, glue));
}
