#!/usr/bin/env bash
set -euo pipefail

# Generate Ragel -G2 lexer and post-process for s32-cc compatibility.
#
# Transformations applied:
#   1. Strip #line directives (s32-cc doesn't handle them)
#   2. Convert prefix ++var to: var = var + 1;  (s32-cc has no prefix ++)
#      Pattern: "if ( ++VAR == EXPR )" -> "VAR = VAR + 1;\n\tif ( VAR == EXPR )"
#   3. Strip trailing whitespace
#   4. Defuse Duff's device: replace outer switch(cs) with if-goto dispatch,
#      convert bare "case N:" to "_cs_N:" goto labels (s32-cc codegen workaround)

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
INPUT="$SCRIPT_DIR/c_lexer.rl"
RAW="$SCRIPT_DIR/c_lexer_raw.c"
SED_OUT="$SCRIPT_DIR/c_lexer_sed.c"
OUTPUT="$SCRIPT_DIR/c_lexer_gen.c"

echo "Generating Ragel -G2 lexer..."
ragel -G2 -o "$RAW" "$INPUT"
echo "  Raw output: $(wc -l < "$RAW") lines, $(wc -c < "$RAW") bytes"

echo "Post-processing for s32-cc compatibility..."

# Step 1: Strip #line, convert prefix ++, strip trailing whitespace
sed \
    -e '/^#line /d' \
    -e 's/if ( ++\([a-z_]*\) == \([^)]*\) )/\1 = \1 + 1; if ( \1 == \2 )/' \
    -e 's/[[:space:]]*$//' \
    "$RAW" > "$SED_OUT"

# Step 2: Defuse Duff's device pattern
#
# Ragel -G2 places goto labels (trN:) BEFORE the first case in the main
# switch(cs) block ("Duff's device" pattern).  s32-cc generates incorrect
# code for this pattern.  We:
#   a) Replace the outer switch(cs) with an if-goto dispatch chain
#   b) Convert bare "case N:" lines to "_cs_N:" goto labels
#
# Each state has both an "stN:" label (includes p++ advance + eof check)
# and a "case N:" re-entry point (skips advance, correct for fbreak resume).
# After transformation: stN: for inter-state goto, _cs_N: for dispatch resume.
#
# Inner switch(*p) and switch(cs)-for-eof are NOT modified because their
# case lines have code on the same line (not bare "case N:" alone).

echo "  Defusing Duff's device pattern..."

# Pass 1: Collect bare "case N:" values (outer switch cases)
OUTER_CASES=$(awk '/^case [0-9]+:$/ { v=$0; gsub(/[^0-9]/,"",v); printf "%s\n", v }' "$SED_OUT" | sort -n | tr '\n' ' ')
echo "  Outer switch cases: $OUTER_CASES"

# Pass 2: Replace outer switch with dispatch, convert bare case labels
awk -v cases="$OUTER_CASES" '
BEGIN {
    n = split(cases, cv, " ")
    did_switch = 0
    in_fn = 0
}
/void lex_next/ { in_fn = 1 }
# Replace first "switch ( cs )" in lex_next with dispatch chain
in_fn && /switch \( cs \)/ && !did_switch {
    did_switch = 1
    for (i = 1; i <= n; i++) {
        if (cv[i] != "") {
            printf "\tif (cs == %s) goto _cs_%s;\n", cv[i], cv[i]
        }
    }
    printf "\tgoto _out;\n"
    next
}
# Convert bare "case N:" to "_cs_N:" goto label
/^case [0-9]+:$/ {
    v = $0; gsub(/[^0-9]/, "", v)
    printf "_cs_%s:\n", v
    next
}
{ print }
' "$SED_OUT" > "$OUTPUT"

rm -f "$RAW" "$SED_OUT"

LINES="$(wc -l < "$OUTPUT")"
BYTES="$(wc -c < "$OUTPUT")"
GOTOS="$(grep -c 'goto ' "$OUTPUT" || true)"
LABELS="$(grep -cE '^(st|tr|_)[a-zA-Z0-9_]*:' "$OUTPUT" || true)"

echo "  Output: $OUTPUT"
echo "  $LINES lines, $BYTES bytes"
echo "  $GOTOS goto statements, $LABELS labels"
echo "Done."
