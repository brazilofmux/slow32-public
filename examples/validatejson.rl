// NOTE:
// This file is intentionally a short placeholder.
//
// The production JSON validator for this repository is now implemented in:
//   examples/validatejson.c
//
// Rationale:
// - The C implementation provides stricter RFC 8259 handling for numbers,
//   string escapes, Unicode surrogate pairs, and UTF-8 validation.
// - It is directly buildable on systems without Ragel.
//
// If we decide to maintain a Ragel-driven variant later, this file can be
// expanded back into a full machine description and a generated companion
// source file (e.g. validatejson_ragel.c).
