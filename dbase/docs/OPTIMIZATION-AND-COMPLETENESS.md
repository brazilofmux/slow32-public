# Optimization and Completeness Plan

**This document is superseded by [ROADMAP.md](ROADMAP.md).**

The original plan here was written before the implementation was complete.
It identified the right problems (lexer, B-tree, missing features,
Teacher's Pet compatibility) but the priorities and approach have been
refined based on actual experience building Stages 1-4C.

See:
- [STATUS.md](STATUS.md) — What's implemented now
- [ROADMAP.md](ROADMAP.md) — Prioritized work plan (7 phases)
- [TEACHERS-PET-ANALYSIS.md](TEACHERS-PET-ANALYSIS.md) — Feature gap analysis

Key changes from the original plan:
- The lexer was implemented and kept for expression parsing, but command
  dispatch stayed with the simpler str_imatch pattern (see git log).
- B-tree indexing is deferred to Phase 7 — the sorted-array approach
  works fine for Teacher's Pet workloads.
- Phase 1 (8 small gaps) is the critical path to running Teacher's Pet.
