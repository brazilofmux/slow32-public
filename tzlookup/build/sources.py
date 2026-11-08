"""Authoritative public data sources for the zip/area-code -> timezone build.

Everything reduces to a (lat, lon) point that we resolve against the
timezone-boundary-builder polygons via point-in-polygon. Pinned versions keep
the build reproducible.
"""

from pathlib import Path

# Repo-root-relative paths.
ROOT = Path(__file__).resolve().parent.parent
RAW = ROOT / "data" / "raw"
GEN = ROOT / "gen"

# timezone-boundary-builder: full-granularity zones (keeps currently-distinct
# zones like America/Indiana/Indianapolis, America/Kentucky/Louisville, Arizona
# separate -- exactly where ZIP-level precision matters).
TZB_RELEASE = "2026b"
TZB_URL = (
    "https://github.com/evansiroky/timezone-boundary-builder/releases/"
    f"download/{TZB_RELEASE}/timezones.geojson.zip"
)
TZB_ZIP = RAW / "timezones.geojson.zip"
TZB_GEOJSON = RAW / "combined.json"  # name inside the archive

# GeoNames postal codes -> (zip, place, lat, lon). US plus territories that have
# their own NANP/ZIP footprint.
GEONAMES_COUNTRIES = ["US", "PR", "VI", "GU", "AS", "MP"]
GEONAMES_URL = "https://download.geonames.org/export/zip/{country}.zip"

# Area code (NPA) -> representative city centroid (lat, lon).
AREACODE_URL = (
    "https://raw.githubusercontent.com/ravisorg/"
    "Area-Code-Geolocation-Database/master/us-area-code-cities.csv"
)
AREACODE_CSV = RAW / "us-area-code-cities.csv"
