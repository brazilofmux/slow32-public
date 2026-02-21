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

## Compatibility names

Legacy compiler names (`s32cc.s32x`, `s12cc.s32x`) are retired for active
stage03-stage06 build/script boundaries.

## Transition rules

1. New scripts and docs should reference canonical names first.
2. Stage03-stage06 script/tool boundaries require canonical compiler names (`cc.s32x`).
3. Legacy compiler names should only appear in historical documentation/context.
4. New tooling must not emit or depend on legacy compiler executable names.

## Current migration status

- Stage03: canonical compiler alias enabled.
- Stage04: canonical compiler alias enabled.
- Stage05: canonical compiler alias enabled.
- Stage06: canonical compiler alias enabled.
- Stage00-stage02: pending (not currently targeted by this policy).
