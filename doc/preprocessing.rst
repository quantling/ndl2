=============
Preprocessing
=============

The actual learning (iterative or Danks) is done on some binary event file
structure. This structure can be generate out of either

1. a given corpus file and some characters or cues to generate the cues
2. a data.frame containing the cues, outcomes and optionally frequencies
3. a given tabular file containing cues, outcomes and frequencies as tab
   separated text file with a header

.. note::

    All strings are potentially normalized at the moment using the icu library.
    In the c++ code the ``auto nfcNormalizer =
    icu::Normalizer2::getNFCInstance(normalizerErrorCode);`` is used.


Event files
===========
In order to create the binary event file one needs to do the following things:

1. create a list of all cues that should be used (subset of all occurring cues)
2. create a list of all outcomes that should be used (subset of all occurring
   outcomes)

List of Cues
------------
Count all occurring cues, sort them by frequency, and take the first MAX_CUES
cues.

List of Outcomes
----------------
Count all occurring outcomes, sort them by frequency, and take the first
MAX_OUTCOMES cues.

