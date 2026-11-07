# SLOW-32 Pascal Image: Free Pascal's ppcs32 and its RTL on slow32:base.
#
#   slow32:base   emulators + as/ld/ar/utilities + runtime
#     -> slow32:pascal   + ppcs32 (host compiler), the embedded RTL in two
#                        variants (soft float, and -CfSLOW32 for the f32
#                        FPU), and `s32pc`, the compile driver.
#
# FPC needs no LLVM: the compiler is built by a released host FPC in a stage
# of its own, and programs link the runtime base already carries.  It was in
# slow32:toolchain until 2026-11-07.
ARG BASE_IMAGE=slow32:base

# Stage 1: build the FPC cross-compiler and its RTL for SLOW-32
FROM alpine:3.21 AS fpc-builder

RUN apk add --no-cache build-base git bash tar

# Install host FPC 3.2.2 for the build host's architecture.
# Stable aliases /opt/fpc/bin/ppc-host and /opt/fpc/units/host let the
# subsequent build steps stay arch-agnostic.
#
# Fetched from FPC's own server, not SourceForge. On 2026-10-01 SourceForge
# stopped serving the file at all: .../files/.../download answered 302 to a
# "#!" client-routing URL and then 200 with 859 bytes of text/html, while
# downloads.sourceforge.net and the netix/phoenixnap mirrors returned empty
# bodies or 301s to HTML. `wget -q` exits 0 on a web page, so both arches
# failed three commands later with "tar: This does not look like a tar
# archive" -- a confusing way to be told the download was wrong.
#
# This changes only WHO SERVES the 3.2.2 release tarball; it pins nothing
# less. FPC_VERSION still selects the release, and the FPC *source* pin is
# FPC_COMMIT on the git fetch below, which is untouched. `tar tf` before
# extracting is the guard that was missing: it fails here, loudly, if the
# bytes are not an archive, whoever served them.
WORKDIR /build
ARG FPC_VERSION=3.2.2
RUN MACHINE=$(uname -m) && \
    case "$MACHINE" in \
      x86_64)  FPC_ARCH=x86_64;  FPC_PPC=ppcx64 ;; \
      aarch64) FPC_ARCH=aarch64; FPC_PPC=ppca64 ;; \
      *) echo "Unsupported host arch for FPC bootstrap: $MACHINE" >&2; exit 1 ;; \
    esac && \
    wget -q "https://downloads.freepascal.org/fpc/dist/${FPC_VERSION}/${FPC_ARCH}-linux/fpc-${FPC_VERSION}.${FPC_ARCH}-linux.tar" -O fpc-host.tar && \
    { tar tf fpc-host.tar >/dev/null 2>&1 || { echo "fpc-host.tar is not a tar archive -- the download served something else:" >&2; head -c 200 fpc-host.tar >&2; echo >&2; exit 1; }; } && \
    tar xf fpc-host.tar && \
    cd fpc-${FPC_VERSION}.${FPC_ARCH}-linux && \
    tar xf binary.${FPC_ARCH}-linux.tar && \
    tar xzf base.${FPC_ARCH}-linux.tar.gz && \
    mkdir -p /opt/fpc/bin /opt/fpc/units && \
    cp lib/fpc/${FPC_VERSION}/${FPC_PPC} /opt/fpc/bin/ && \
    chmod +x /opt/fpc/bin/${FPC_PPC} && \
    ln -sf /opt/fpc/bin/${FPC_PPC} /opt/fpc/bin/ppc-host && \
    cp -r lib/fpc/${FPC_VERSION}/units/${FPC_ARCH}-linux /opt/fpc/units/ && \
    ln -sf /opt/fpc/units/${FPC_ARCH}-linux /opt/fpc/units/host && \
    rm -rf /build/fpc-*

# Clone FPC source at pinned commit
# Pin to the upstream merge-base of ~/fpc (same SHA patches are regenerated against).
ARG FPC_COMMIT=3f9ea418a294b249613e3e585f36dd398316bfa9
RUN git init /fpc-src && cd /fpc-src && \
    git remote add origin https://gitlab.com/freepascal.org/fpc/source.git && \
    git fetch --depth=1 origin ${FPC_COMMIT} && \
    git checkout FETCH_HEAD

# Copy SLOW-32 backend and apply patches
COPY fpc-backend/compiler/slow32 /fpc-src/compiler/slow32
COPY fpc-backend/rtl/slow32 /fpc-src/rtl/slow32
COPY fpc-backend/rtl/embedded/slow32 /fpc-src/rtl/embedded/slow32
COPY fpc-backend/patches /build/fpc-patches

WORKDIR /fpc-src
RUN for p in /build/fpc-patches/*.patch; do \
      echo "Applying $(basename $p)..." && \
      git apply "$p" || patch -p1 < "$p"; \
    done

# Generate message include files (msgtxt.inc, msgidx.inc)
WORKDIR /fpc-src/compiler
RUN /opt/fpc/bin/ppc-host -Fu/opt/fpc/units/host/rtl \
      -FE. utils/msg2inc.pp && \
    ./msg2inc msg/errore.msg msg msg

# Build ppcs32 (SLOW-32 cross-compiler)
RUN /opt/fpc/bin/ppc-host -dslow32 -Fuslow32 -Fusystems -Fislow32 \
      -Fu/opt/fpc/units/host/rtl -oppcs32 pp.pas

# Build the embedded RTL via the upstream Makefile-driven path.
# Feature set comes from rtl/embedded/system.cfg's CPUSLOW32 block;
# `-s` keeps the build emitting .s files for slow32asm instead of
# looking for a GNU `slow32-embedded-as`.
#
# Skipped slots (each needs a follow-up before re-enabling):
#   ISO7185UNIT — triggers a ppcs32 EAccessViolation on iso7185.pp.
#   SYSCONSTUNIT — needs TResourceStringRecord in the slow32 system unit.
#   CPU_SPECIFIC_COMMON_UNITS — defaults to the arm-style sysutils/math/
#     classes/etc. list (slow32's Makefile.fpc block leaves it alone);
#     blanking it stops the build pulling in sysutils, which depends on
#     sysconst.  Once sysconst builds, this can shrink to just the
#     individual problem units instead of a wholesale blank.
# Overlay our slow32-specific system.pp onto rtl/embedded/system.pp.
# The Makefile has no hook to redirect SYSTEMUNIT to the CPU-specific
# fork, but it compiles `system.pp` relative to rtl/embedded/, so the
# overlay is enough — the slow32 fork wires in SysInitMemoryManager,
# SysInitStdIO and Slow32InitCmdLine, without which WriteLn silently
# discards output.
RUN cp /fpc-src/rtl/slow32/system.pp /fpc-src/rtl/embedded/system.pp
RUN cd /fpc-src/rtl/embedded && \
    make all \
        FPC=/fpc-src/compiler/ppcs32 PP=/fpc-src/compiler/ppcs32 \
        CPU_TARGET=slow32 OS_TARGET=embedded \
        FPCOPT='-s -n' \
        ISO7185UNIT= SYSCONSTUNIT= CPU_SPECIFIC_COMMON_UNITS=
RUN mkdir -p /opt/fpc/slow32-rtl && \
    cp /fpc-src/rtl/units/slow32-embedded/*.ppu /opt/fpc/slow32-rtl/ && \
    cp /fpc-src/rtl/units/slow32-embedded/*.s   /opt/fpc/slow32-rtl/

# Second RTL variant compiled for the native f32 FPU (-CfSLOW32).
# Units must match the program's -Cf mode, so programs built with
# -CfSLOW32 link against /opt/fpc/slow32-rtl-fpu instead.
RUN cd /fpc-src/rtl/embedded && \
    make clean CPU_TARGET=slow32 OS_TARGET=embedded && \
    make all \
        FPC=/fpc-src/compiler/ppcs32 PP=/fpc-src/compiler/ppcs32 \
        CPU_TARGET=slow32 OS_TARGET=embedded \
        FPCOPT='-s -n -CfSLOW32' \
        ISO7185UNIT= SYSCONSTUNIT= CPU_SPECIFIC_COMMON_UNITS=
RUN mkdir -p /opt/fpc/slow32-rtl-fpu && \
    cp /fpc-src/rtl/units/slow32-embedded/*.ppu /opt/fpc/slow32-rtl-fpu/ && \
    cp /fpc-src/rtl/units/slow32-embedded/*.s   /opt/fpc/slow32-rtl-fpu/

# Stage 2: layer onto base, and assemble each RTL variant into one archive
# so s32pc's link takes only the units a program uses.  fpc_startup.s32o
# stays out of them: the linker always wants it as an explicit object.
FROM ${BASE_IMAGE}
COPY --from=fpc-builder /fpc-src/compiler/ppcs32 /opt/slow32/bin/
COPY --from=fpc-builder /opt/fpc/slow32-rtl/ /opt/slow32/lib/fpc/
COPY --from=fpc-builder /opt/fpc/slow32-rtl-fpu/ /opt/slow32/lib/fpc-fpu/
COPY fpc-backend/runtime/fpc_startup.s /opt/slow32/lib/fpc/
RUN set -e; \
    for d in /opt/slow32/lib/fpc /opt/slow32/lib/fpc-fpu; do \
      cd "$d"; \
      for s in *.s; do /opt/slow32/bin/slow32asm "$s" "${s%.s}.s32o" >/dev/null; done; \
      /opt/slow32/bin/s32-ar rcs librtl.s32a $(ls *.s32o | grep -v '^fpc_startup\.s32o$'); \
      rm -f $(ls *.s32o | grep -v '^fpc_startup\.s32o$'); \
    done; \
    test -f /opt/slow32/lib/fpc/fpc_startup.s32o
COPY docker/bin/s32pc /opt/slow32/bin/
RUN chmod +x /opt/slow32/bin/s32pc /opt/slow32/bin/ppcs32

# Usage:  s32pc [--fpu] [-O2] prog.pas [prog.s32x]
#         s32run prog.s32x
WORKDIR /data
