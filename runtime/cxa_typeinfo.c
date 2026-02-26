/*
 * SLOW-32 Fundamental Type Info Objects
 *
 * Provides std::type_info objects for fundamental C++ types.
 * These are referenced by __cxa_throw and the type tables in
 * .gcc_except_table for catch clause matching.
 *
 * The personality routine (__gxx_personality_v0) uses pointer equality
 * to match types, so the actual content only matters for RTTI name queries.
 * Each typeinfo is a struct { vtable_ptr, name } following the Itanium ABI.
 *
 * Mangled names follow the Itanium ABI:
 *   _ZTIv = void, _ZTIb = bool, _ZTIc = char, _ZTIa = signed char,
 *   _ZTIh = unsigned char, _ZTIs = short, _ZTIt = unsigned short,
 *   _ZTIi = int, _ZTIj = unsigned int, _ZTIl = long, _ZTIm = unsigned long,
 *   _ZTIx = long long, _ZTIy = unsigned long long,
 *   _ZTIf = float, _ZTId = double, _ZTIPv = void*,
 *   _ZTIPc = char*, _ZTIPKc = const char*
 */

#include <stdint.h>

/* Minimal type_info layout: vtable pointer + name.
 * We don't have real RTTI vtables, so vtable_ptr is NULL.
 * This works because our personality routine only compares addresses. */
struct type_info_fundamental {
    void *vtable_ptr;    /* would point to __fundamental_type_info vtable */
    const char *name;
};

struct type_info_pointer {
    void *vtable_ptr;    /* would point to __pointer_type_info vtable */
    const char *name;
    uint32_t flags;      /* pointer flags */
    void *pointee;       /* type_info for pointee type */
};

/* Fundamental type infos */
const struct type_info_fundamental _ZTIv = { 0, "v" };     /* void */
const struct type_info_fundamental _ZTIb = { 0, "b" };     /* bool */
const struct type_info_fundamental _ZTIc = { 0, "c" };     /* char */
const struct type_info_fundamental _ZTIa = { 0, "a" };     /* signed char */
const struct type_info_fundamental _ZTIh = { 0, "h" };     /* unsigned char */
const struct type_info_fundamental _ZTIs = { 0, "s" };     /* short */
const struct type_info_fundamental _ZTIt = { 0, "t" };     /* unsigned short */
const struct type_info_fundamental _ZTIi = { 0, "i" };     /* int */
const struct type_info_fundamental _ZTIj = { 0, "j" };     /* unsigned int */
const struct type_info_fundamental _ZTIl = { 0, "l" };     /* long */
const struct type_info_fundamental _ZTIm = { 0, "m" };     /* unsigned long */
const struct type_info_fundamental _ZTIx = { 0, "x" };     /* long long */
const struct type_info_fundamental _ZTIy = { 0, "y" };     /* unsigned long long */
const struct type_info_fundamental _ZTIf = { 0, "f" };     /* float */
const struct type_info_fundamental _ZTId = { 0, "d" };     /* double */

/* Pointer type infos */
const struct type_info_pointer _ZTIPv  = { 0, "Pv",  0, (void *)&_ZTIv };  /* void* */
const struct type_info_pointer _ZTIPc  = { 0, "Pc",  0, (void *)&_ZTIc };  /* char* */
const struct type_info_pointer _ZTIPKc = { 0, "PKc", 0, (void *)&_ZTIc };  /* const char* */
