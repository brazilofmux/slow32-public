#!/bin/bash
# Apply all fixes for SLOW32 backend

echo "Applying SLOW32 backend fixes..."

# Backup originals
cp SLOW32Subtarget.h SLOW32Subtarget.h.bak 2>/dev/null
cp SLOW32Subtarget.cpp SLOW32Subtarget.cpp.bak 2>/dev/null

# Apply Subtarget fixes
if [ -f SLOW32Subtarget_fixed.h ]; then
    cp SLOW32Subtarget_fixed.h SLOW32Subtarget.h
    echo "✓ Fixed SLOW32Subtarget.h"
fi

if [ -f SLOW32Subtarget_fixed.cpp ]; then
    cp SLOW32Subtarget_fixed.cpp SLOW32Subtarget.cpp
    echo "✓ Fixed SLOW32Subtarget.cpp"
fi

echo "Fixes applied! Run 'make -j8 llc' to continue building."