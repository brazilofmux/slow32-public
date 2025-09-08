#!/bin/bash
# Fix LLVM-generated assembly where labels are incorrectly output as comments
# Converts "# %bb.N:" to ".LBBF_N:" where F is the function number

if [ $# -ne 2 ]; then
    echo "Usage: $0 input.s output.s"
    exit 1
fi

INPUT="$1"
OUTPUT="$2"

# Process the file - track function numbers from existing labels
awk '
BEGIN { func_num = 0 }
/^\.LBB[0-9]+_[0-9]+:/ {
    # Extract function number from existing label
    if (match($0, /^\.LBB([0-9]+)_/, arr)) {
        func_num = arr[1]
    }
    print
    next
}
/^# %bb\./ {
    # Convert comment to label using current function number
    if (match($0, /^# %bb\.([0-9]+):(.*)$/, arr)) {
        printf ".LBB%d_%s:%s\n", func_num, arr[1], arr[2]
    } else {
        print
    }
    next
}
/^[a-zA-Z_][a-zA-Z0-9_]*:/ {
    # New function starts, increment function counter
    if ($0 !~ /^\.L/) {
        func_num++
    }
    print
    next
}
{ print }
' "$INPUT" > "$OUTPUT"