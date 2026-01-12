# C++ Support Plan for SLOW32

## Overview

This document outlines the work required to add C++ language support to the
SLOW32 toolchain. The C compiler is complete and stable; this plan builds
incrementally on that foundation.

## Current State

- **C compiler**: Fully working (Clang frontend + SLOW32 LLVM backend)
- **Runtime**: libc with malloc, stdio, string, etc.
- **Startup**: `crt0.s` + `start.c` - calls `main()`, no global ctor/dtor support
- **Emulation**: QEMU TCG backend at ~2B insns/sec

## What Works Today (Untested but Expected)

Most C++ features are handled entirely by the Clang frontend and should
"just work" with `-fno-exceptions -fno-rtti`:

- Classes and structs with methods
- Inheritance (single and multiple)
- Virtual functions and vtables
- Templates (fully instantiated before codegen)
- Operator overloading
- References
- Namespaces
- Lambdas (anonymous struct + operator())
- `constexpr` evaluation
- Range-based for loops
- `auto` type deduction

## Implementation Phases

### Phase 0: Validation (Effort: 1-2 days)

**Goal**: Confirm what already works with minimal flags.

**Tasks**:
1. Create `examples/cpp/` directory with test programs
2. Build simple C++ program with current toolchain:
   ```bash
   clang++ -target slow32-unknown-none -fno-exceptions -fno-rtti \
           -ffreestanding -nostdinc++ -O2 -S -emit-llvm test.cpp
   ```
3. Identify first failures (likely missing runtime symbols)
4. Document which LLVM IR constructs appear that C code never generated

**Test programs** (in order of complexity):
- `class_basic.cpp` - Simple class with constructor/destructor
- `class_virtual.cpp` - Virtual functions, vtables
- `template_basic.cpp` - Function and class templates
- `stl_minimal.cpp` - std::array or hand-rolled container

**Expected blockers**:
- Missing `__cxa_pure_virtual`
- Missing `operator new` / `operator delete`
- No global constructor execution

---

### Phase 1: Minimal C++ Runtime (Effort: 2-3 days)

**Goal**: Programs with classes, virtual functions, and heap allocation work.

#### 1.1 Core Runtime Functions

Add to `runtime/cxxabi.c`:

```c
// Called when pure virtual function is invoked (should never happen)
void __cxa_pure_virtual(void) {
    // Print error and abort
    __builtin_trap();
}

// Called for deleted virtual functions
void __cxa_deleted_virtual(void) {
    __builtin_trap();
}
```

#### 1.2 Operator New/Delete

Add to `runtime/cxxabi.c`:

```c
#include <stdlib.h>

void* operator new(unsigned int size) {
    void* p = malloc(size);
    if (!p) __builtin_trap();  // No exceptions, so trap on OOM
    return p;
}

void* operator new[](unsigned int size) {
    return operator new(size);
}

void operator delete(void* p) noexcept {
    free(p);
}

void operator delete[](void* p) noexcept {
    free(p);
}

void operator delete(void* p, unsigned int) noexcept {
    free(p);
}

void operator delete[](void* p, unsigned int) noexcept {
    free(p);
}
```

Note: Must be compiled as C++ or use `extern "C"` mangled names.

#### 1.3 Update Build

- Add `cxxabi.cpp` to runtime Makefile
- Create `libcxx_runtime.s32a` archive
- Update `slow32cc` wrapper to link C++ runtime for `.cpp` files

---

### Phase 2: Global Constructors/Destructors (Effort: 2-3 days)

**Goal**: Static/global objects are properly initialized before `main()`.

#### 2.1 Linker Script Changes

Ensure `.init_array` and `.fini_array` sections are properly placed:

```
SECTIONS {
    /* ... existing sections ... */

    .init_array : {
        __init_array_start = .;
        KEEP(*(.init_array))
        KEEP(*(SORT(.init_array.*)))
        __init_array_end = .;
    }

    .fini_array : {
        __fini_array_start = .;
        KEEP(*(.fini_array))
        KEEP(*(SORT(.fini_array.*)))
        __fini_array_end = .;
    }
}
```

#### 2.2 Startup Code Changes

Modify `start.c`:

```c
typedef void (*ctor_func)(void);

extern ctor_func __init_array_start[];
extern ctor_func __init_array_end[];
extern ctor_func __fini_array_start[];
extern ctor_func __fini_array_end[];

static void __call_constructors(void) {
    for (ctor_func* p = __init_array_start; p < __init_array_end; p++) {
        (*p)();
    }
}

static void __call_destructors(void) {
    // Call in reverse order
    for (ctor_func* p = __fini_array_end; p > __fini_array_start; ) {
        p--;
        (*p)();
    }
}

int __slow32_start(void) {
    // ... existing argv setup ...

    __call_constructors();

    int rc = main(argc, argv);

    __call_destructors();
    exit(rc);
    return rc;
}
```

#### 2.3 `__cxa_atexit` Support

For objects with `static` storage duration inside functions:

```c
#define ATEXIT_MAX 32

struct atexit_entry {
    void (*destructor)(void*);
    void* arg;
    void* dso_handle;
};

static struct atexit_entry atexit_list[ATEXIT_MAX];
static int atexit_count = 0;

void* __dso_handle = 0;

int __cxa_atexit(void (*destructor)(void*), void* arg, void* dso_handle) {
    if (atexit_count >= ATEXIT_MAX) return -1;
    atexit_list[atexit_count].destructor = destructor;
    atexit_list[atexit_count].arg = arg;
    atexit_list[atexit_count].dso_handle = dso_handle;
    atexit_count++;
    return 0;
}

void __cxa_finalize(void* dso_handle) {
    for (int i = atexit_count - 1; i >= 0; i--) {
        if (dso_handle == 0 || atexit_list[i].dso_handle == dso_handle) {
            atexit_list[i].destructor(atexit_list[i].arg);
        }
    }
}
```

Call `__cxa_finalize(0)` from `exit()` or at end of `__slow32_start`.

---

### Phase 3: Thread-Safe Static Initialization (Effort: 1-2 days)

**Goal**: Function-local static variables initialize correctly.

The compiler emits guard variables and calls to:
- `__cxa_guard_acquire`
- `__cxa_guard_release`
- `__cxa_guard_abort`

#### 3.1 Single-Threaded Implementation

For single-threaded execution (our current model):

```c
// Guard variable is 64 bits; first byte indicates initialization status
int __cxa_guard_acquire(long long* guard) {
    char* guard_byte = (char*)guard;
    if (*guard_byte != 0) return 0;  // Already initialized
    return 1;  // Needs initialization
}

void __cxa_guard_release(long long* guard) {
    char* guard_byte = (char*)guard;
    *guard_byte = 1;  // Mark as initialized
}

void __cxa_guard_abort(long long* guard) {
    // Initialization failed - leave uninitialized
    (void)guard;
}
```

#### 3.2 Multi-Threaded Consideration

If SLOW32 ever gains threading, these need atomic operations and mutex support.
For now, document this limitation.

---

### Phase 4: RTTI Support (Effort: 1-2 weeks)

**Goal**: Enable `-frtti` for `dynamic_cast` and `typeid()`.

This phase is **optional** - many embedded projects use `-fno-rtti` permanently.

#### 4.1 Type Info Structures

The compiler emits `std::type_info` objects. Need:
- `std::type_info` class definition
- Comparison operators for type matching
- Mangled type name strings

#### 4.2 `dynamic_cast` Support

The compiler calls `__dynamic_cast()` from libcxxabi. This requires:
- Understanding class hierarchy at runtime
- Walking vtables to find correct subobject

**Recommendation**: Defer this phase. Most code works without RTTI.

---

### Phase 5: Exception Handling (Effort: 2-3 months)

**Goal**: Full C++ exception support with throw/catch.

This is the largest piece of work and is **optional** for many use cases.

#### 5.1 DWARF Unwind Tables

The compiler emits `.eh_frame` sections with unwind info. Need:
- Linker support for `.eh_frame` and `.eh_frame_hdr`
- DWARF CFI (Call Frame Information) interpretation
- Unwind table format for SLOW32 calling convention

#### 5.2 libunwind Port

Options:
1. Port LLVM's libunwind to SLOW32
2. Write minimal custom unwinder
3. Use a simplified unwinding scheme

Minimum required functions:
- `_Unwind_RaiseException`
- `_Unwind_Resume`
- `_Unwind_GetIP`
- `_Unwind_SetIP`
- `_Unwind_GetGR`
- `_Unwind_SetGR`
- `_Unwind_GetRegionStart`
- `_Unwind_GetLanguageSpecificData`

#### 5.3 C++ ABI Exception Functions

From libcxxabi:
- `__cxa_allocate_exception`
- `__cxa_throw`
- `__cxa_begin_catch`
- `__cxa_end_catch`
- `__cxa_rethrow`
- `__cxa_get_exception_ptr`
- `__gxx_personality_v0` (personality routine)

#### 5.4 Landing Pads

The compiler emits landing pad blocks for catch clauses. The personality
routine must:
1. Parse LSDA (Language-Specific Data Area)
2. Match thrown exception to catch handlers
3. Direct unwinder to correct landing pad

**Recommendation**: Keep `-fno-exceptions` as default. Implement exceptions
only if specific user code requires them.

---

## Minimal C++ Standard Library

For embedded use, a full libstdc++ or libc++ is unnecessary. Consider:

### Option A: No Standard Library

Use `-nostdinc++` and `-nostdlib++`. Provide only:
- `<cstdint>`, `<cstddef>` (just typedefs)
- `<new>` (placement new)
- `<type_traits>` (compile-time only)

### Option B: Freestanding Headers

Many "freestanding" headers require no runtime:
- `<array>`
- `<tuple>`
- `<utility>` (std::move, std::forward)
- `<type_traits>`
- `<limits>`
- `<cstdint>`

### Option C: Minimal libc++ Subset

Build libc++ with `LIBCXX_ENABLE_EXCEPTIONS=OFF` and `LIBCXX_ENABLE_RTTI=OFF`.
Cherry-pick needed components.

---

## Testing Strategy

### Unit Tests

Create `regression/cpp/` with tests for:
- [ ] Basic class instantiation
- [ ] Virtual function dispatch
- [ ] Single inheritance
- [ ] Multiple inheritance
- [ ] Virtual inheritance
- [ ] Templates (function, class, variadic)
- [ ] Global object construction order
- [ ] Static local initialization
- [ ] Operator overloading
- [ ] Lambdas
- [ ] RTTI (when enabled)
- [ ] Exceptions (when enabled)

### Integration Tests

- Compile and run progressively complex C++ programs
- Ensure C and C++ code link together correctly
- Test C++ calling C and vice versa

---

## Compiler Flags Summary

| Phase | Flags |
|-------|-------|
| 0-3 | `-fno-exceptions -fno-rtti -ffreestanding -nostdinc++` |
| 4 | `-fno-exceptions -ffreestanding -nostdinc++` |
| 5+ | `-ffreestanding -nostdinc++` (or use full libc++) |

---

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Unknown LLVM IR constructs in C++ | Medium | Medium | Phase 0 validation |
| Vtable layout issues | Low | High | Test virtual dispatch early |
| Init order problems | Medium | Medium | Careful testing with multiple TUs |
| Exception handling complexity | High | High | Keep as optional phase |
| libc++ build complexity | High | Medium | Use freestanding subset |

---

## Success Criteria

### Minimum Viable C++

- [ ] Classes with constructors/destructors work
- [ ] Virtual functions dispatch correctly
- [ ] Templates instantiate and link
- [ ] Global objects initialize before main
- [ ] Static locals initialize on first use
- [ ] `new`/`delete` work with malloc/free
- [ ] C and C++ interoperate correctly

### Full C++ (Long-term)

- [ ] RTTI works (`dynamic_cast`, `typeid`)
- [ ] Exceptions work (throw/catch)
- [ ] Standard library subset available

---

## Appendix: Key Files to Modify

### Backend (if needed)
- `llvm/lib/Target/SLOW32/SLOW32ISelLowering.cpp` - New SDNode types
- `llvm/lib/Target/SLOW32/SLOW32AsmPrinter.cpp` - Section emission

### Runtime
- `runtime/cxxabi.c` (new) - C++ ABI functions
- `runtime/start.c` - Constructor/destructor calls
- `runtime/Makefile` - Build C++ runtime

### Linker
- Linker script updates for `.init_array`, `.fini_array`, `.eh_frame`

### Toolchain Wrapper
- `slow32cc` - Detect `.cpp` and add appropriate flags/libraries

---

## References

- [Itanium C++ ABI](https://itanium-cxx-abi.github.io/cxx-abi/abi.html)
- [LLVM Exception Handling](https://llvm.org/docs/ExceptionHandling.html)
- [libcxxabi source](https://github.com/llvm/llvm-project/tree/main/libcxxabi)
- [libunwind source](https://github.com/llvm/llvm-project/tree/main/libunwind)
