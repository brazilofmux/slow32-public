# SLOW-32 Forth Image: the DTC kernel and forthc, the native Forth compiler,
# on slow32:base.
#
#   slow32:base   emulators + as/ld/ar/utilities + runtime
#     -> slow32:forth   + kernel.s32x (assembled and linked here from
#                       forth/kernel.s), prelude.fth, tube.fth, forthc.fth,
#                       prelude-fc.fth, and two drivers: `s32forth` (the
#                       interactive kernel) and `s32forthc` (compile a
#                       closed-world .fth to a standalone or --hosted .s32x).
#
# Nothing here needs a C compiler: the kernel is assembly, forthc is Forth
# running on the kernel, so the build stage is FROM base itself.
ARG BASE_IMAGE=slow32:base

# Stage 1: assemble and link the kernel with base's tools and runtime
FROM ${BASE_IMAGE} AS forth-builder
WORKDIR /build/forth
COPY forth/build.sh forth/kernel.s /build/forth/
RUN FORTH_RUN=0 S32_AS=/opt/slow32/bin/slow32asm S32_LD=/opt/slow32/bin/s32-ld \
    S32_RT=/opt/slow32/lib ./build.sh

# Stage 2: layer onto base
FROM ${BASE_IMAGE}
COPY --from=forth-builder /build/forth/kernel.s32x /opt/slow32/forth/
COPY forth/prelude.fth forth/tube.fth /opt/slow32/forth/
COPY forthc/compile.sh forthc/forthc.fth forthc/prelude-fc.fth /opt/slow32/forthc/
COPY docker/bin/s32forth docker/bin/s32forthc /opt/slow32/bin/
RUN chmod +x /opt/slow32/bin/s32forth /opt/slow32/bin/s32forthc /opt/slow32/forthc/compile.sh

# Usage:  s32forth [file.fth ...]              (then stdin; -i for a session)
#         s32forthc [--hosted] prog.fth [prog.s32x]; s32run prog.s32x
WORKDIR /data
