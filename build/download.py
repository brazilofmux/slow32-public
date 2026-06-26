"""Fetch all raw source data into data/raw/. Idempotent: skips existing files."""

import sys
import zipfile

import requests

from sources import (
    RAW,
    TZB_URL,
    TZB_ZIP,
    TZB_GEOJSON,
    GEONAMES_COUNTRIES,
    GEONAMES_URL,
    AREACODE_URL,
    AREACODE_CSV,
)


def fetch(url: str, dest, label: str) -> None:
    if dest.exists() and dest.stat().st_size > 0:
        print(f"  [skip] {label}: {dest.name} ({dest.stat().st_size:,} B)")
        return
    print(f"  [get ] {label}: {url}")
    with requests.get(url, stream=True, timeout=120) as r:
        r.raise_for_status()
        with open(dest, "wb") as f:
            for chunk in r.iter_content(chunk_size=1 << 16):
                f.write(chunk)
    print(f"         -> {dest.name} ({dest.stat().st_size:,} B)")


def main() -> int:
    RAW.mkdir(parents=True, exist_ok=True)

    print("timezone boundaries:")
    fetch(TZB_URL, TZB_ZIP, "tz-boundary-builder")
    if not TZB_GEOJSON.exists():
        with zipfile.ZipFile(TZB_ZIP) as z:
            # Archive contains a single combined.json.
            name = next(n for n in z.namelist() if n.endswith(".json"))
            with z.open(name) as src, open(TZB_GEOJSON, "wb") as dst:
                dst.write(src.read())
        print(f"         unzipped -> {TZB_GEOJSON.name}")

    print("geonames postal codes:")
    for country in GEONAMES_COUNTRIES:
        zpath = RAW / f"{country}.zip"
        fetch(GEONAMES_URL.format(country=country), zpath, f"geonames {country}")
        txt = RAW / f"{country}.txt"
        if not txt.exists():
            with zipfile.ZipFile(zpath) as z:
                with z.open(f"{country}.txt") as src, open(txt, "wb") as dst:
                    dst.write(src.read())

    print("area codes:")
    fetch(AREACODE_URL, AREACODE_CSV, "area-code centroids")

    print("done.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
