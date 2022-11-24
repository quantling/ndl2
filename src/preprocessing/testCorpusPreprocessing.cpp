#include "../common/testsCommon.h"
#include "ndlpreproc.h"
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

const string toyCorpusWordsOnly = testDataDir + "toy-corpus-words-only.txt";
const string lettersAll = testDataDir + "english-letters-upper_lower.txt";
const string toyWordsAll = testDataDir + "toy-corpus-words-only-words_all.txt";
const string toyWordsSome = testDataDir +"toy-corpus-words-only-words_some.txt";

const int IRRELEVANT = 1;
const int NUM_THREADS = 3;


TEST_CASE("With all words and letters preprocess corpus creates all trigrams",
    "[corpus][files][word-trigram][letter-trigram]") {
  const string outputPrefix = "corpus-unfiltered-trigrams";
  preprocess(
      toyCorpusWordsOnly, lettersAll, toyWordsAll, testOutDir + outputPrefix,
      NUM_THREADS, "N", 3, 1, false, false, true, IRRELEVANT, IRRELEVANT,
      false, "", "", Quiet
  );

  // Redirect standard output (cout) to file for saving the view events output
  const string outputTextFilePath = asTextFile(outputPrefix, false);

  const string cuesFirstLetters = outputTextFilePath + "-cues_first_letter";
  commandToNewFile("sed -e 's/" + regexColumns + "/\\1/' -e 's/.._//g'",
      outputTextFilePath, cuesFirstLetters);
  
  const string outcomesHashmark = outputTextFilePath + "-outcomes_hashmark";
  commandToNewFile("sed -e 's/" + regexColumns + "/#\\2#/' -e 's/_/#/g'",
      outputTextFilePath, outcomesHashmark);

  requireFilesEqualOrdered(cuesFirstLetters, outcomesHashmark);
}


TEST_CASE("Words not in the word list get filtered out",
    "[corpus][files][word-trigram][letter-trigram]") {
  const string outputPrefix = "corpus-filtered_words-trigrams";
  // In toyWordsSome the following words were removed: stands, paradigm, header
  preprocess(
      toyCorpusWordsOnly, lettersAll, toyWordsSome, testOutDir + outputPrefix,
      NUM_THREADS, "N", 3, 1, false, false, true, IRRELEVANT, IRRELEVANT,
      false, "", "", Quiet
  );

  // Redirect standard output (cout) to file for saving the view events output
  const string outputTextFilePath = asTextFile(outputPrefix, false);
  requireFilesEqualOrdered(testDataDir +
      "toy-corpus-words-only-words_some-reference_output", outputTextFilePath);
}
