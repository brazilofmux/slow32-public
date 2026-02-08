# SLOW-32 floating-point library call stubs
#
# HISTORICAL: All math function implementations have moved to math.c.
# That file provides real soft-float implementations (Taylor series,
# Newton-Raphson, etc.) instead of HALT stubs, so math functions now
# work on slow32 and slow32-fast without interception.
#
# On slow32-dbt and QEMU, the symbols are still intercepted via SYMTAB
# and replaced with host libm calls — the soft-float code never runs.
#
# This file is kept for reference. If you need to add a new stub for
# a function that can't be implemented in C, add it here and include
# float_intrinsics.s32o back in COMPILER_RT_OBJS in the Makefile.
