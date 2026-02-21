# Selfhost Tool Naming

This document defines stable tool names across stages and the compatibility
policy for legacy names.

## Goal

Use one canonical executable name per tool role within each stage directory,
instead of embedding historical stage numbers in tool names.

## Canonical names

- Compiler: `cc.s32x`
- Assembler: `s32-as.s32x`
- Archiver: `s32-ar.s32x`
- Linker: `s32-ld.s32x`

## Compatibility names (temporary)

- Stage04/05/06 compiler legacy name: `s12cc.s32x`

For now, stage05+stage06 build scripts emit both:
- canonical: `cc.s32x`
- compatibility: `s12cc.s32x`

## Transition rules

1. New scripts and docs should reference canonical names first.
2. Existing scripts may keep compatibility names until migrated.
3. Lookup order in wrappers/build scripts should be:
   - canonical name
   - compatibility fallback
4. Remove compatibility names only after all in-tree references are migrated.

## Current migration status

- Stage03: canonical compiler alias enabled.
- Stage04: canonical compiler alias enabled.
- Stage05: canonical compiler alias enabled.
- Stage06: canonical compiler alias enabled.
- Stage00-stage02: pending (not currently targeted by this policy).
