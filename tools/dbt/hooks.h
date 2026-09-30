// SLOW-32 DBT: hooks -- native routines the guest opts into
// (docs/dbt-hooks.md).
//
// A guest routine is hookable when its entry carries a second symbol,
// __s32hk_<name>_<tag>, and its first instruction is `jal r0, impl`.  The
// tag is a checksum of the source that defines the routine's contract;
// this DBT hooks a routine only when the tag matches the one it was built
// against.  A hook returns HK_DONE (the stub returns to r31) or
// HK_DECLINE (the stub branches to impl, and the guest code runs).

#ifndef DBT_HOOKS_H
#define DBT_HOOKS_H

#include <stdint.h>
#include "cpu_state.h"

#define HK_DONE    0
#define HK_DECLINE 1

// Called by every hook stub: count, then run hook `idx`.
int dbt_hook_call(dbt_cpu_state_t *cpu, uint8_t *mem, uint32_t idx);

// The hook registered at guest_pc, or -1.
int dbt_hook_at(const dbt_cpu_state_t *cpu, uint32_t guest_pc);

// Fill cpu->hooks from the guest's symbol table.  lookup(user, name)
// returns a symbol's address or 0.
void dbt_hooks_register(dbt_cpu_state_t *cpu,
                        uint32_t (*lookup)(void *user, const char *name),
                        void *user);

// Per-hook calls and declines, for -s.
void dbt_hooks_print_stats(const dbt_cpu_state_t *cpu);

#endif
