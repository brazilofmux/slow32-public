/* Minimal assertion harness for tzlookup. Exits non-zero on first failure. */
#include <cstdio>
#include <cstring>

#include "tzlookup.h"

static int failures = 0;

static void eq_str(const char *what, const char *got, const char *want) {
  bool ok = (got == nullptr && want == nullptr) ||
            (got && want && !std::strcmp(got, want));
  if (!ok) {
    std::printf("FAIL %s: got %s, want %s\n", what,
                got ? got : "(null)", want ? want : "(null)");
    ++failures;
  }
}

static void eq_int(const char *what, int got, int want) {
  if (got != want) {
    std::printf("FAIL %s: got %d, want %d\n", what, got, want);
    ++failures;
  }
}

int main() {
  /* ZIP -> zone (ground truth from gen/tz.sqlite). */
  eq_str("zip 10001", tz_from_zip(10001), "America/New_York");
  eq_str("zip 94089", tz_from_zip(94089), "America/Los_Angeles");
  eq_str("zip 85001", tz_from_zip(85001), "America/Phoenix");
  eq_str("zip 96813", tz_from_zip(96813), "Pacific/Honolulu");
  eq_str("zip 00601 (PR)", tz_from_zip(601), "America/Puerto_Rico");
  eq_str("zip 99501 (AK)", tz_from_zip(99501), "America/Anchorage");
  eq_str("zip 46204 (IN)", tz_from_zip(46204), "America/Indiana/Indianapolis");
  eq_str("zip 09021 (APO)", tz_from_zip(9021), "Europe/Berlin");

  /* Out of range / unknown. */
  eq_str("zip -1", tz_from_zip(-1), nullptr);
  eq_str("zip 100000", tz_from_zip(100000), nullptr);
  eq_str("zip 00000", tz_from_zip(0), nullptr);

  /* Area code -> zone. */
  eq_str("npa 408", tz_from_areacode(408), "America/Los_Angeles");
  eq_str("npa 212", tz_from_areacode(212), "America/New_York");
  eq_str("npa 312", tz_from_areacode(312), "America/Chicago");
  eq_str("npa 808", tz_from_areacode(808), "Pacific/Honolulu");

  /* Phone parsing. */
  eq_int("parse +1 (408) 555-1212", tz_parse_areacode("+1 (408) 555-1212"), 408);
  eq_int("parse 4085551212", tz_parse_areacode("4085551212"), 408);
  eq_int("parse 18085551212", tz_parse_areacode("1-808-555-1212"), 808);
  eq_int("parse 7-digit -> -1", tz_parse_areacode("555-1212"), -1);
  eq_int("parse leading-1-NPA", tz_parse_areacode("1 (212) 555 0000"), 212);

  /* ZIP parsing. */
  eq_int("parse zip 94089-1234", tz_parse_zip("94089-1234"), 94089);
  eq_int("parse zip '  10001 '", tz_parse_zip("  10001 "), 10001);
  eq_int("parse zip too short", tz_parse_zip("123"), -1);

  /* Combined: ZIP wins over a conflicting phone. */
  eq_str("zip+phone, zip wins",
         tz_lookup("10001", "+1 (213) 555-1212"), "America/New_York");
  /* Combined: unknown ZIP falls back to phone. */
  eq_str("bad zip -> phone fallback",
         tz_lookup("00000", "808-555-1212"), "Pacific/Honolulu");
  eq_str("neither resolves", tz_lookup(nullptr, nullptr), nullptr);

  if (failures) {
    std::printf("\n%d test(s) FAILED\n", failures);
    return 1;
  }
  std::printf("all tests passed\n");
  return 0;
}
