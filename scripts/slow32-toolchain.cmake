# CMake toolchain file for SLOW-32: clang/clang++ from the llvm-project
# build, slow32asm (which the clang driver calls to assemble), s32-ar for
# archives.  Static libraries only -- there are no shared ones, and the
# compiler checks build an archive instead of linking a program.
#
#   cmake -DCMAKE_TOOLCHAIN_FILE=<slow-32>/scripts/slow32-toolchain.cmake ...
#
# The clang driver finds slow32asm and s32-ld by name, so tools/assembler,
# tools/linker and tools/utilities must be on PATH while building.
# LLVM_BIN (environment) overrides where clang is; default ~/llvm-project/build/bin.

set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_SYSTEM_PROCESSOR slow32)

get_filename_component(SLOW32_ROOT "${CMAKE_CURRENT_LIST_DIR}/.." ABSOLUTE)
if(DEFINED ENV{LLVM_BIN})
  set(SLOW32_LLVM_BIN "$ENV{LLVM_BIN}")
else()
  set(SLOW32_LLVM_BIN "$ENV{HOME}/llvm-project/build/bin")
endif()

set(CMAKE_C_COMPILER "${SLOW32_LLVM_BIN}/clang")
set(CMAKE_CXX_COMPILER "${SLOW32_LLVM_BIN}/clang++")
set(CMAKE_C_COMPILER_TARGET slow32-unknown-none)
set(CMAKE_CXX_COMPILER_TARGET slow32-unknown-none)
set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)

# The runtime's C headers; unwind tables everywhere, since an exception can
# only be unwound through a function that has one (libc++'s throw helpers
# had none, and every throw from them terminated)
set(CMAKE_C_FLAGS_INIT "-isystem ${SLOW32_ROOT}/runtime/include -funwind-tables")
set(CMAKE_CXX_FLAGS_INIT "-isystem ${SLOW32_ROOT}/runtime/include -funwind-tables")

# s32-ar takes c (create) and r (replace); s32-ld reads its archives, not
# ar's, and needs no ranlib.
set(CMAKE_AR "${SLOW32_ROOT}/tools/utilities/s32-ar")
foreach(lang C CXX ASM)
  set(CMAKE_${lang}_ARCHIVE_CREATE "<CMAKE_AR> c <TARGET> <OBJECTS>")
  set(CMAKE_${lang}_ARCHIVE_APPEND "<CMAKE_AR> r <TARGET> <OBJECTS>")
  set(CMAKE_${lang}_ARCHIVE_FINISH "")
endforeach()
set(CMAKE_STATIC_LIBRARY_SUFFIX .s32a)
