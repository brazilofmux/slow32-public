#!/bin/bash
# Regenerate csv_import.c from csv_import.rl (requires ragel)
cd "$(dirname "$0")"
ragel -G2 src/csv_import.rl -o src/csv_import.c
echo "Generated src/csv_import.c"
