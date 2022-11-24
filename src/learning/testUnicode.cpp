#include "../common/testsCommon.h"
#include "util.h"
#include <string>
#include <sstream> // std::stringstream

using namespace std;


TEST_CASE("nfkc normalization works correctly",
  "[string],[normalization],[unicode]") {
  // Compare umlaut ä/Ä with a/A and combining diaeresis
  CHECK(normalize_to_nfkc_in_utf8("ä") == u8"ä");
  CHECK(normalize_to_nfkc_in_utf8("a\xcc\x88") == u8"ä");
  CHECK(normalize_to_nfkc_in_utf8("Ä") == u8"Ä");
  CHECK(normalize_to_nfkc_in_utf8("A\xcc\x88") == u8"Ä");

  // Å: compare A with ring and Angstrom sign
  const char* const A_RING = "\xC3\x85"; // Å (A with ring)
  CHECK(normalize_to_nfkc_in_utf8(A_RING) == A_RING);
  CHECK(normalize_to_nfkc_in_utf8("\xE2\x84\xAB") == A_RING);
}
