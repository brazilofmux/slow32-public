"""Resolve every ZIP and area code to an IANA timezone via point-in-polygon.

Inputs  (data/raw, via download.py):
  - combined.json            timezone-boundary-builder polygons
  - <CC>.txt                 GeoNames postal codes (zip -> lat/lon)
  - us-area-code-cities.csv  NPA -> representative city centroids

Outputs (gen/):
  - tz.sqlite                inspectable: zones, zips, areacodes
  - resolved.json            compact tables for the C codegen step
  - report.txt               ambiguities / unresolved / spanning area codes
"""

import csv
import json
import sqlite3
import sys
from collections import Counter, defaultdict

import shapely
from shapely import Point
from shapely.geometry import shape
from shapely import STRtree

from sources import RAW, GEN, GEONAMES_COUNTRIES, AREACODE_CSV, TZB_GEOJSON


def load_zones():
    """Return (names, prepared_geoms, STRtree) for all tz polygons."""
    with open(TZB_GEOJSON) as f:
        gj = json.load(f)
    names, geoms = [], []
    for feat in gj["features"]:
        names.append(feat["properties"]["tzid"])
        geoms.append(shape(feat["geometry"]))
    shapely.prepare(geoms)
    tree = STRtree(geoms)
    print(f"  loaded {len(names)} timezone polygons")
    return names, geoms, tree


class Resolver:
    def __init__(self, names, geoms, tree):
        self.names = names
        self.geoms = geoms
        self.tree = tree
        self.miss_nearest = 0

    def resolve(self, lon, lat):
        """Return zone name for a point, or None. Falls back to nearest polygon."""
        pt = Point(lon, lat)
        for i in self.tree.query(pt):  # bbox candidates
            if self.geoms[i].covers(pt):
                return self.names[i]
        # Point fell outside every polygon (coastline/rounding): nearest zone.
        j = self.tree.nearest(pt)
        if j is not None:
            self.miss_nearest += 1
            return self.names[j]
        return None


def parse_geonames():
    """Yield (zip5:int, place, lat, lon) from all GeoNames country files."""
    for cc in GEONAMES_COUNTRIES:
        path = RAW / f"{cc}.txt"
        if not path.exists():
            continue
        with open(path, encoding="utf-8") as f:
            for line in f:
                parts = line.rstrip("\n").split("\t")
                if len(parts) < 11:
                    continue
                postal = parts[1].strip()
                if not postal.isdigit() or len(postal) > 5:
                    continue
                try:
                    lat = float(parts[9])
                    lon = float(parts[10])
                except ValueError:
                    continue
                yield int(postal), parts[2], lat, lon


def parse_areacodes():
    """Yield (npa:int, lat, lon) from the area-code centroid CSV.

    Columns are sniffed at runtime; the ravisorg file has no header, ordered
    area_code, city, state, country, lat, lon (verified against the data).
    """
    with open(AREACODE_CSV, newline="", encoding="utf-8") as f:
        for row in csv.reader(f):
            if not row:
                continue
            npa = row[0].strip()
            if not npa.isdigit() or len(npa) != 3:
                continue
            try:
                lat = float(row[-2])
                lon = float(row[-1])
            except (ValueError, IndexError):
                continue
            yield int(npa), lat, lon


def main() -> int:
    GEN.mkdir(parents=True, exist_ok=True)
    names, geoms, tree = load_zones()
    R = Resolver(names, geoms, tree)

    # Sanity check: NYC must resolve to America/New_York.
    assert R.resolve(-74.006, 40.7128) == "America/New_York", "resolver sanity failed"

    # --- ZIPs ---------------------------------------------------------------
    print("resolving ZIPs...")
    zip_zone = {}            # zip -> zone name (first/most common wins)
    zip_votes = defaultdict(Counter)
    n = 0
    for zip5, _place, lat, lon in parse_geonames():
        zone = R.resolve(lon, lat)
        if zone:
            zip_votes[zip5][zone] += 1
        n += 1
    for zip5, votes in zip_votes.items():
        zip_zone[zip5] = votes.most_common(1)[0][0]
    zip_conflicts = {z: dict(v) for z, v in zip_votes.items() if len(v) > 1}
    print(f"  {n:,} postal rows -> {len(zip_zone):,} unique ZIPs"
          f" ({len(zip_conflicts)} multi-zone)")

    # --- Area codes ---------------------------------------------------------
    print("resolving area codes...")
    npa_votes = defaultdict(Counter)
    for npa, lat, lon in parse_areacodes():
        zone = R.resolve(lon, lat)
        if zone:
            npa_votes[npa][zone] += 1
    npa_zone = {npa: v.most_common(1)[0][0] for npa, v in npa_votes.items()}
    npa_spanning = {npa: dict(v) for npa, v in npa_votes.items() if len(v) > 1}
    print(f"  {len(npa_zone)} area codes ({len(npa_spanning)} span >1 zone)")
    print(f"  nearest-polygon fallbacks used: {R.miss_nearest}")

    # --- Zone index table (only zones actually used) ------------------------
    used = sorted(set(zip_zone.values()) | set(npa_zone.values()))
    zone_idx = {name: i + 1 for i, name in enumerate(used)}  # 0 = unknown
    print(f"  {len(used)} distinct zones in use")

    # --- Emit SQLite --------------------------------------------------------
    db_path = GEN / "tz.sqlite"
    if db_path.exists():
        db_path.unlink()
    db = sqlite3.connect(db_path)
    db.executescript(
        "CREATE TABLE zones(idx INTEGER PRIMARY KEY, name TEXT);"
        "CREATE TABLE zips(zip INTEGER PRIMARY KEY, zone_idx INTEGER);"
        "CREATE TABLE areacodes(npa INTEGER PRIMARY KEY, zone_idx INTEGER, spanning INTEGER);"
    )
    db.executemany("INSERT INTO zones VALUES(?,?)",
                   [(i, n) for n, i in zone_idx.items()])
    db.executemany("INSERT INTO zips VALUES(?,?)",
                   [(z, zone_idx[zn]) for z, zn in zip_zone.items()])
    db.executemany("INSERT INTO areacodes VALUES(?,?,?)",
                   [(npa, zone_idx[zn], 1 if npa in npa_spanning else 0)
                    for npa, zn in npa_zone.items()])
    db.commit()
    db.close()

    # --- Emit resolved.json (for codegen) -----------------------------------
    resolved = {
        "zones": used,  # index i -> zone i+1
        "zip_to_idx": {str(z): zone_idx[zn] for z, zn in sorted(zip_zone.items())},
        "npa_to_idx": {str(n): zone_idx[zn] for n, zn in sorted(npa_zone.items())},
    }
    with open(GEN / "resolved.json", "w") as f:
        json.dump(resolved, f)

    # --- Report -------------------------------------------------------------
    with open(GEN / "report.txt", "w") as f:
        f.write(f"zones in use: {len(used)}\n")
        for zn in used:
            f.write(f"  {zone_idx[zn]:3d}  {zn}\n")
        f.write(f"\nZIPs: {len(zip_zone)} unique\n")
        f.write(f"multi-zone ZIPs (centroid voted): {len(zip_conflicts)}\n")
        for z, v in sorted(zip_conflicts.items())[:50]:
            f.write(f"  {z:05d}  {v}\n")
        f.write(f"\narea codes: {len(npa_zone)}\n")
        f.write(f"spanning area codes: {len(npa_spanning)}\n")
        for npa, v in sorted(npa_spanning.items()):
            f.write(f"  {npa}  {v}\n")
        f.write(f"\nnearest-polygon fallbacks: {R.miss_nearest}\n")

    print(f"wrote {db_path.name}, resolved.json, report.txt")
    return 0


if __name__ == "__main__":
    sys.exit(main())
