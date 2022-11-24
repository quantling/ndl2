#include "../common/testsCommon.h"
#include "ndlpreproc.h"
#include <string>
#include <sstream> // std::stringstream

using namespace std;


TEST_CASE("Cleanup normalizes correctly",
    "[string],[normalization],[unicode]") {
  const string wordsNormal[] = {"test", "Test", "tesT", "TEST"};
  SECTION("with lowercase=false strings that are already normal don't change") {
    for (auto word : wordsNormal) {
      CHECK(cleanup(word, false) == UnicodeString::fromUTF8(word));
    }
  }

  SECTION("with lowercase=false strings are only normalized") {
    // Compare umlaut ä/Ä with a/A and combining diaeresis
    CHECK(cleanup("ä", false) == UnicodeString::fromUTF8(u8"ä"));
    CHECK(cleanup("a\xcc\x88", false) == UnicodeString::fromUTF8(u8"ä"));
    CHECK(cleanup("Ä", false) == UnicodeString::fromUTF8(u8"Ä"));
    CHECK(cleanup("A\xcc\x88", false) == UnicodeString::fromUTF8(u8"Ä"));

    // Å: compare A with ring and Angstrom sign
    const char* const A_RING = "\xC3\x85"; // Å (A with ring)
    CHECK(cleanup(A_RING, false) == UnicodeString::fromUTF8(A_RING));
    CHECK(cleanup("\xE2\x84\xAB", false) == UnicodeString::fromUTF8(A_RING));
  }

  SECTION("with lowercase=true strings that are already normal get lowercase") {
    for (auto word : wordsNormal) {
      CHECK(cleanup(word, true) == UnicodeString::fromUTF8(wordsNormal[0]));
    }
  }

  SECTION("with lowercase=true non-normalized strings get lowercased") {
    // Compare umlaut ä/Ä with a/A and combining diaeresis
    CHECK(cleanup("ä", true) == UnicodeString::fromUTF8(u8"ä"));
    CHECK(cleanup("a\xcc\x88", true) == UnicodeString::fromUTF8(u8"ä"));
    CHECK(cleanup("Ä", true) == UnicodeString::fromUTF8(u8"ä"));
    CHECK(cleanup("A\xcc\x88", true) == UnicodeString::fromUTF8(u8"ä"));

    CHECK(cleanup(u8"å", true) == UnicodeString::fromUTF8(u8"å"));
    // Å (either A with ring or Angstrom sign) become å
    CHECK(cleanup("\xC3\x85", true) == UnicodeString::fromUTF8(u8"å"));
    CHECK(cleanup("\xE2\x84\xAB", true) == UnicodeString::fromUTF8(u8"å"));
  }
}



// Convenience function for testing: accept string literals
string fAndRPunct(string input) {
  return findAndReplacePunct(input);
}

TEST_CASE("Replacing punctuation works",
    "[string],[normalization],[unicode]") {
  CHECK(fAndRPunct("x") == "x");
  CHECK(fAndRPunct(".") == " \x01 ");
  CHECK(fAndRPunct("test") == "test");
  CHECK(fAndRPunct("Test") == "Test");
  CHECK(fAndRPunct("TEST") == "TEST");
  CHECK(fAndRPunct("test.") == "test \x01 ");
  CHECK(fAndRPunct(".test") == " \x01 test");
  CHECK(fAndRPunct("te.st") == "te \x01 st");
  CHECK(fAndRPunct("t?est") == "t \x01 est");
  CHECK(fAndRPunct("tes!t") == "tes \x01 t");
  CHECK(fAndRPunct("test5") == "test \x01 ");
  CHECK(fAndRPunct("test Hallo") == "test \x01 Hallo");
  CHECK(fAndRPunct("test. hallo") == "test \x01  \x01 hallo");
  CHECK(fAndRPunct("test. \"hallo\"") == "test \x01  \x01  \x01 hallo \x01 ");
  CHECK(fAndRPunct("te-st") == "te-st"); // dash should get ignored
  CHECK(fAndRPunct("te_st") == "te \x01 st");
  CHECK(fAndRPunct(u8"täst") == u8"täst");
  CHECK(fAndRPunct(u8"tést") == u8"tést");
}
