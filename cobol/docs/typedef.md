# TYPEDEF and TYPE

COBOL 2002's type declarations (docs/standards.md, Stage B). Citations
are to ISO/IEC 1989:2023, 13.18.57 (TYPE) and 13.18.58 (TYPEDEF).

## How it is done

The text defines the TYPE clause by substitution: the entry is "as
though the data description identified by type-name-1 had been coded in
place of the TYPE clause", subordinate level-numbers adjusted (13.18.57.4
rules 1-2). So `expand_types()` does exactly that, over the tokens,
after COPY and REPLACE and before anything is parsed:

- A TYPEDEF entry (level 01 or 77) and its subordinate entries are
  recorded and dropped. A type declaration has no storage (13.18.58.4
  rule 2), and its subordinate names are then not references to
  anything unless a group uses the type (rule 1).
- A `TYPE TO type-name` (TO may be omitted) is replaced by the type's
  clauses: its PICTURE, USAGE, VALUE and the rest, without TYPEDEF and
  GLOBAL. The type's subordinate entries follow the entry's period, their
  levels rebased on the entry's. Level 88 and 66 entries come along.
- A type may use a type declared before it; its TYPE clauses are
  expanded as it is recorded.
- The entry's own clauses stay: OCCURS, VALUE, REDEFINES and so on.

Diagnostics inside an expanded type point at the type's own lines.

## Strong types (ISSUES-80)

`TYPEDEF STRONG` declares a strongly-typed group type (only a group may
be one, 13.18.58.3 rule 1). A group described by it, and each group
inside it, is strongly typed: the expansion puts a marker in each such
entry, carrying a key -- the type's name, or type#n for the n-th group
inside it -- and the same key is the same type.

- MOVE: a strongly-typed group moves only to and from one of the same
  type (8.5.3.3, D.8.3); its elementary items are moved freely.
- Comparison: only with the same type, element by element in order,
  each elementary item by its own rules (8.8.4.2.12) -- a numeric
  element compares as a number, not as its bytes.
- A strong type is used at level 01 or inside a strong type (13.18.57.3
  rule 6).
- Refused on a strongly-typed group: REDEFINES either way and RENAMES
  (rules 3, 4), reference modification (and of a numeric or edited item
  inside one, 8.4.2.4), a class condition, INSPECT, and a STRING
  receiver.

Not checked yet: the CALL rule that an argument and its parameter be of
one strong type (14.9.4), and the rules for FD and SD records.

## Not implemented

- A type declared after its use, or inside a group (only 01 and 77).
- Types across separately compiled programs; a type is visible from its
  declaration to the end of the source file.

## Oracle

None. GnuCOBOL 4 knows no TYPEDEF under `-std=cobol2002`; under its
default dialect it takes simple types but not a type used inside a type
("PICTURE clause required"). Measured on 2002/typedecl.
