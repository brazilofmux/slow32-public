#!/bin/bash
# Generate N CSV files of R rows each into corpus/. All valid (pure parse work).
cd "$(dirname "$0")"
N="${1:-64}"; R="${2:-2000}"
rm -f corpus/*.csv
for i in $(seq 1 "$N"); do
  awk -v r="$R" 'BEGIN{for(n=1;n<=r;n++)print "field1,field2,field3,field4,field5,field6,field7,field8"}' \
    > "corpus/file$(printf '%03d' "$i").csv"
done
echo "generated $N files x $R rows in corpus/"
