==========
Build ndl2
==========

Dependencies
============
Install the following dependencies.

R development
-------------
* testthat
* roxygen2
* devtools

R ndl2 runtime
--------------
* Rcpp
* MASS
* Hmisc

C++
---
* libboost-filesystem-dev
* libboost-system-dev
* libboost-regex-dev
* libicu-dev
* libcurl4-openssl-dev


Build
=====
In order to build ndl2 from sources use ``make``::

    make
    make test
    make testAll
    make install

