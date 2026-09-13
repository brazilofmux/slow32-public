# SLOW-32 COBOL 85 Image: s32-cobc + libcob on slow32:base.
#
#   slow32:base   emulators + as/ld/ar/utilities + runtime
#     -> slow32:cobol   + s32-cobc (host compiler), libcob.s32o (guest
#                       runtime), and `s32cob`, the compile driver.
#
# s32-cobc is a host C program; libcob is guest C and needs a SLOW-32 C
# compiler, so both are built in a stage FROM slow32:toolchain (gcc + clang)
# and only the results are layered onto base.  The final image has no C
# compiler: `.c` inputs to s32cob need slow32:toolchain, not this image.
ARG BASE_IMAGE=slow32:base
ARG TOOLCHAIN_IMAGE=slow32:toolchain

# Stage 1: build the compiler and the runtime with the toolchain image
FROM ${TOOLCHAIN_IMAGE} AS cobol-builder
WORKDIR /build/cobol
COPY cobol/build.sh cobol/cctool.sh cobol/compile.sh /build/cobol/
COPY cobol/src /build/cobol/src
COPY cobol/libcob /build/cobol/libcob
RUN CC=gcc LLVM_BIN=/opt/llvm/bin \
    S32_AS=/opt/slow32/bin/slow32asm S32_RT_INCLUDE=/opt/slow32/include \
    ./build.sh

# Stage 2: layer onto base
FROM ${BASE_IMAGE}
COPY --from=cobol-builder /build/cobol/out/s32-cobc /opt/slow32/bin/
COPY --from=cobol-builder /build/cobol/libcob/libcob.s32o /opt/slow32/lib/
COPY cobol/compile.sh cobol/cctool.sh /opt/slow32/cobol/
COPY docker/bin/s32cob /opt/slow32/bin/
RUN chmod +x /opt/slow32/bin/s32cob /opt/slow32/cobol/compile.sh

# Usage:  s32cob [-free|-fixed] main.cbl [sub.cbl ...] [-I dir] [-o prog.s32x]
#         s32run prog.s32x
WORKDIR /data
