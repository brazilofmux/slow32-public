#!/bin/bash
# Generate patches for SLOW32 integration into Free Pascal
# These patches modify existing FPC files to register the slow32 target.
# The new slow32-specific directories are handled by backup.sh, not patches.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PATCHES_DIR="$(dirname "$SCRIPT_DIR")/patches"
FPC_DIR="$HOME/fpc"

echo "Generating SLOW32 integration patches against upstream FPC..."
echo "============================================================="

# Ensure patches directory exists
mkdir -p "$PATCHES_DIR"

cd "$FPC_DIR"

# Verify we have a clean base to diff against
echo "Using origin/HEAD as base for patch generation..."
echo ""

# 1. Compiler build system (Makefile + Makefile.fpc)
echo "1. Generating compiler build system patch..."
git diff origin/HEAD -- compiler/Makefile compiler/Makefile.fpc > "$PATCHES_DIR/01-compiler-build.patch" 2>/dev/null || true
if [ -s "$PATCHES_DIR/01-compiler-build.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/01-compiler-build.patch")
    echo "   Created: 01-compiler-build.patch ($lines lines)"
else
    echo "   No changes"
fi

# 2. CPU/system registration (systems.inc, systems.pas, entfile.pas)
echo "2. Generating CPU/system registration patch..."
git diff origin/HEAD -- compiler/systems.inc compiler/systems.pas compiler/entfile.pas > "$PATCHES_DIR/02-system-registration.patch" 2>/dev/null || true
if [ -s "$PATCHES_DIR/02-system-registration.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/02-system-registration.patch")
    echo "   Created: 02-system-registration.patch ($lines lines)"
else
    echo "   No changes"
fi

# 3. Compiler defines and options (fpcdefs.inc, globals.pas, options.pas, version.pas, cgbase.pas)
echo "3. Generating compiler defines/options patch..."
git diff origin/HEAD -- compiler/fpcdefs.inc compiler/globals.pas compiler/options.pas compiler/version.pas compiler/cgbase.pas > "$PATCHES_DIR/03-compiler-defines.patch" 2>/dev/null || true
if [ -s "$PATCHES_DIR/03-compiler-defines.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/03-compiler-defines.patch")
    echo "   Created: 03-compiler-defines.patch ($lines lines)"
else
    echo "   No changes"
fi

# 4. Target info and linker registration (i_embed.pas, t_embed.pas)
echo "4. Generating target/linker registration patch..."
git diff origin/HEAD -- compiler/systems/i_embed.pas compiler/systems/t_embed.pas > "$PATCHES_DIR/04-target-registration.patch" 2>/dev/null || true
if [ -s "$PATCHES_DIR/04-target-registration.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/04-target-registration.patch")
    echo "   Created: 04-target-registration.patch ($lines lines)"
else
    echo "   No changes"
fi

# 5. Compiler driver and utilities (pp.pas, psystem.pas, fpc.pp, ppudump.pp, dbgdwarf.pas)
echo "5. Generating compiler driver/utilities patch..."
git diff origin/HEAD -- compiler/pp.pas compiler/psystem.pas compiler/utils/fpc.pp compiler/utils/ppuutils/ppudump.pp compiler/dbgdwarf.pas > "$PATCHES_DIR/05-compiler-driver.patch" 2>/dev/null || true
if [ -s "$PATCHES_DIR/05-compiler-driver.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/05-compiler-driver.patch")
    echo "   Created: 05-compiler-driver.patch ($lines lines)"
else
    echo "   No changes"
fi

# 6. RTL build system (rtl/Makefile, rtl/embedded/Makefile, rtl/embedded/Makefile.fpc)
echo "6. Generating RTL build system patch..."
git diff origin/HEAD -- rtl/Makefile rtl/embedded/Makefile rtl/embedded/Makefile.fpc > "$PATCHES_DIR/06-rtl-build.patch" 2>/dev/null || true
if [ -s "$PATCHES_DIR/06-rtl-build.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/06-rtl-build.patch")
    echo "   Created: 06-rtl-build.patch ($lines lines)"
else
    echo "   No changes"
fi

# 7. RTL system unit includes (rtl/inc/systemh.inc, rtl/inc/system.inc)
echo "7. Generating RTL system includes patch..."
git diff origin/HEAD -- rtl/inc/systemh.inc rtl/inc/system.inc > "$PATCHES_DIR/07-rtl-system.patch" 2>/dev/null || true
if [ -s "$PATCHES_DIR/07-rtl-system.patch" ]; then
    lines=$(wc -l < "$PATCHES_DIR/07-rtl-system.patch")
    echo "   Created: 07-rtl-system.patch ($lines lines)"
else
    echo "   No changes"
fi

# Summary
echo ""
echo "Patch generation complete!"
echo "=========================="
echo "Generated patches in: $PATCHES_DIR"
echo ""
ls -lh "$PATCHES_DIR"/*.patch 2>/dev/null || echo "No patches generated"

# Show which files are modified by the patches
echo ""
echo "Files modified by patches:"
echo "--------------------------"
for patch in "$PATCHES_DIR"/*.patch; do
    if [ -s "$patch" ]; then
        echo "$(basename "$patch"):"
        grep "^+++" "$patch" 2>/dev/null | sed 's/^+++ b\//  /' || true
    fi
done
