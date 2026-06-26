# tzlookup

Map a US **ZIP code** and/or **NANP phone number** to an **IANA timezone id**
(e.g. `America/New_York`) — which a caller's tz library expands to the current
offset and DST rule for any instant.

Lookups are a single O(1) array load against tables baked into the binary.
A 5-digit ZIP is already a perfect hash into `[0, 99999]`, so no hashing is
needed: the ZIP *is* the index.

```
$ tz --zip 94089 --phone "+1 (408) 555-1212"
America/Los_Angeles
$ tz --phone 1-808-555-1212
Pacific/Honolulu
```

- **~1.3 ns / lookup** (≈750 M lookups/sec), single-threaded.
- **~101 KB** of static data, compiled into the binary — no runtime data file,
  trivial to drop into a Docker container.
- Covers the 50 states + DC + territories (PR, USVI, Guam, American Samoa,
  CNMI). APO/FPO/DPO ZIPs resolve to their real overseas zone (e.g. a Ramstein
  APO ZIP → `Europe/Berlin`). **105 zones** in use.

## Layout

```
build/        Python data pipeline (run order: download -> build -> emit_c)
  sources.py    pinned source URLs/versions
  download.py   fetch raw data into data/raw/
  build.py      point-in-polygon resolve -> gen/tz.sqlite + resolved.json + report.txt
  emit_c.py     resolved.json -> gen/tz_data.h (embeddable tables)
src/          C++ runtime (tzlookup.h/.cpp) + CLI (tz_cli.cpp)
tests/        assertion-based test suite
gen/          generated: tz_data.h (committed), tz.sqlite/report.txt (inspect)
data/         downloaded raw sources (gitignored, reproducible)
```

## Build

```
make data    # regenerate gen/tz_data.h from public sources (needs .venv + network)
make         # build the `tz` CLI
make test    # build and run tests
```

Python deps (shapely, requests) live in `.venv`; create with
`python3 -m venv .venv && .venv/bin/pip install shapely requests`.

## Data model & sources

Everything reduces to a `(lat, lon)` resolved against the
[timezone-boundary-builder](https://github.com/evansiroky/timezone-boundary-builder)
polygons (release **2026b**, full-granularity zones) via point-in-polygon:

- **ZIP → (lat, lon)** — GeoNames postal codes (US + PR/VI/GU/AS/MP).
  41,706 unique ZIPs; 0 straddle a zone boundary at centroid resolution.
- **Area code → (lat, lon)** — NANP area-code city centroids; multi-city NPAs
  resolve by majority vote. 298 area codes; 10 span >1 zone (see
  `gen/report.txt`). The US-only source omits PR's 787/939 — those are covered
  by ZIP.

Runtime artifacts in `gen/tz_data.h`:

- `TZ_ZONES[]` — zone index → IANA id string (index 0 = unknown).
- `TZ_ZIP_ZONE[100000]` — `uint8` zone index per ZIP.
- `TZ_NPA_ZONE[1000]` — `uint8` zone index per area code.

## Lookup semantics

ZIP is geographically precise, so it wins when known; the area code is the
fallback (and the only signal for the handful of multi-zone NPAs, where it
returns the majority zone). `tz_lookup_codes(zip, npa)` / `tz_lookup(zip_str,
phone_str)` return `NULL` when neither input resolves.
