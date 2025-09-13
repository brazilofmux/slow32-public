# Additional runtime functions for printf support

# memcpy - alias to llvm.memcpy.p0.p0.i32
.global memcpy
memcpy:
    beq r0, r0, llvm.memcpy.p0.p0.i32

# va_start/va_end are in intrinsics.s