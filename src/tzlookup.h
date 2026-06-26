/* tzlookup -- map a US ZIP code and/or NANP phone number to an IANA timezone.
 *
 * Lookups are a single O(1) array load against tables baked into the binary
 * (see gen/tz_data.h). All returned strings are static; never free them.
 */
#ifndef TZLOOKUP_H
#define TZLOOKUP_H

#ifdef __cplusplus
extern "C" {
#endif

/* Return the IANA id for a 5-digit ZIP (0..99999), or NULL if unknown. */
const char *tz_from_zip(int zip);

/* Return the IANA id for a 3-digit NANP area code / NPA (200..999), or NULL.
 * For the handful of area codes spanning >1 zone, this is the majority zone. */
const char *tz_from_areacode(int npa);

/* Combined lookup from integer codes. ZIP is geographically precise, so it
 * wins when known; the area code is the fallback. Returns NULL if neither
 * resolves. Pass -1 (or any out-of-range value) for a missing input. */
const char *tz_lookup_codes(int zip, int npa);

/* Combined lookup from raw strings. Accepts a ZIP like "94089" or "94089-1234"
 * and a phone like "(408) 555-1212", "+1 408 555 1212", or "4085551212".
 * Either may be NULL/empty. Returns NULL if nothing resolves. */
const char *tz_lookup(const char *zip_str, const char *phone_str);

/* Parse helpers (exposed for testing). Return -1 on failure. */
int tz_parse_zip(const char *s);
int tz_parse_areacode(const char *phone);

#ifdef __cplusplus
}
#endif

#endif /* TZLOOKUP_H */
