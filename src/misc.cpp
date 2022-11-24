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

// from http://stackoverflow.com/questions/26666614/how-do-i-check-if-an-externalptr-is-null-from-within-r


#include <Rdefines.h>

// [[Rcpp::export]]
SEXP is_null_externalptr(SEXP pointer)
{
  void *ptr = R_ExternalPtrAddr(pointer);
  SEXP rvalue = PROTECT(NEW_LOGICAL(1));
  if (ptr==NULL) {
    LOGICAL_DATA(rvalue)[0] = (Rboolean)TRUE;
  } else {
    LOGICAL_DATA(rvalue)[0] = (Rboolean)FALSE;
  }
  UNPROTECT(1);
  return(rvalue);
  //return(R_ExternalPtrAddr(pointer) == NULL);
}
