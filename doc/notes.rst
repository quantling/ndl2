=====
Notes
=====

Future plans for the ndl2 package.

Todos
=====
* Do we need ``-g`` as a compiler flag in release mode?


Refactoring / Cleaning
======================
* remove dependency to c++ boost libary
* refactor preprocessor submodule
* (enforce coding standards)
* improve testsuite
* make the ndl2 R package cran compliant
* run tests on Mac
* make a coverage check


How to include Python Code?
===========================
* Cython? https://github.com/cython/cython/wiki/FAQ#how-can-i-make-a-standalone-binary-from-a-python-program-using-cython
* rpy2?


Plans and Ideas
===============
New features and some loose roadmap for the upcoming ndl2 versions.

Short Term
^^^^^^^^^^
* refactor / cleaning
* add a vignette to the ndl2 R package
* run profiler
* test row vs column orientated representation of weights while learning
* tracing (write changes of weights into R / onto disc while learning)
* write change logs the R way
* get rid of libboost
* introduce dependency to ``stringi`` R package in order to use libicu platform
  independent
* do all preprocessing without temp files (optionally)

Mid Term
^^^^^^^^
* start learning with non-zero weights
* add / delete outcomes to existing learner objects
* (write a python3 frontend)


Long Term
^^^^^^^^^
* add / delete cues to existing learner objects (do we want this really?)
* make ndl2 run on Windows
* make it possible to use altered / enhanced Rescorla Wagner models
* GPU implementation
* dynamic / time dependent :math:`\alpha` (learnig rate)
* having individuel :math:`\alpha` and :math:`\beta`


Unsorted
^^^^^^^^
* expose possibility to use different unicode normalizer to R
* include the normalizer settings in the settings and use the same normalizer
  settings while learning and preprocessing
* make it possible to save results after prepreprocessing (before creating the
  binary format)
* set LOCALE within R as an argument



Not possible right now
^^^^^^^^^^^^^^^^^^^^^^
* implement serialization method for learner object (not possible within R)

