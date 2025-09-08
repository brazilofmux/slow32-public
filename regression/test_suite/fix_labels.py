#!/usr/bin/env python3
"""
Fix LLVM-generated assembly where labels are incorrectly output as comments
Converts "# %bb.N:" to ".LBBF_N:" where F is the function number
"""

import sys
import re

if len(sys.argv) != 3:
    print(f"Usage: {sys.argv[0]} input.s output.s")
    sys.exit(1)

input_file = sys.argv[1]
output_file = sys.argv[2]

func_num = 0
with open(input_file, 'r') as infile, open(output_file, 'w') as outfile:
    for line in infile:
        # Track function number from existing labels
        match = re.match(r'^\.LBB(\d+)_(\d+):', line)
        if match:
            func_num = int(match.group(1))
            outfile.write(line)
            continue
        
        # Convert comment to label
        match = re.match(r'^# %bb\.(\d+):(.*)$', line)
        if match:
            block_num = match.group(1)
            comment = match.group(2)
            outfile.write(f'.LBB{func_num}_{block_num}:{comment}\n')
            continue
        
        # Track new functions
        if re.match(r'^[a-zA-Z_][a-zA-Z0-9_]*:', line) and not line.startswith('.L'):
            func_num += 1
            
        outfile.write(line)