# Locale support and STANDARD-COMPARE (queue item 45)

The standard's locale machinery is a late-bound set of cultural
choices: what order strings sort in, what a date looks like, which
letters are upper case, how money is edited.  COBOL does not define
any of it; it names the categories of ISO/IEC 9945 (POSIX) and says the
runtime looks the answers up.  This compiler has refused every door
into it by name: the four LOCALE functions and STANDARD-COMPARE, the
LOCALE phrase on UPPER-CASE and NUMVAL-C, the LOCALE clause of
SPECIAL-NAMES, ALPHABET ... IS LOCALE, CHARACTER CLASSIFICATION, the
LOCALE phrase of PICTURE, and SET LOCALE.  The ruling of 2026-10-07
was that the right way runs through libutf carrying locales itself.
As of libutf dfa04c5 (2026-10-08) it does, for the one category that
is hard: collation.  This plan says what that buys, what it does not,
and the order to take the rest in.

## Authority

ISO/IEC 1989:2023, cited by page of the INCITS copy:

- 8.2 Locales (p. 94): the six categories LC_COLLATE, LC_CTYPE,
  LC_MESSAGES, LC_MONETARY, LC_NUMERIC, LC_TIME and LC_ALL; a run unit
  starts in the user default locale; SET switches categories; a
  missing locale is EC-LOCALE-MISSING, an invalid one
  EC-LOCALE-INVALID.  LC_MESSAGES and LC_NUMERIC "are not used directly
  by COBOL".  8.2.2 (p. 95) lists the fields COBOL reads: the
  LC_MONETARY set (int_curr_symbol, currency_symbol, mon_decimal_point,
  mon_thousands_sep, mon_grouping, positive_sign, negative_sign,
  int_frac_digits, frac_digits, p_cs_precedes, n_cs_precedes) and
  LC_TIME's d_fmt and t_fmt.
- 8.8.4.2.11 Locale-based comparison (p. 190): trailing spaces are
  trimmed from both operands (all spaces -> one space), then the
  LC_COLLATE algorithm decides; two empty operands are equal; a
  character the locale does not order is EC-LOCALE-INCOMPATIBLE.
- 12.3.6 OBJECT-COMPUTER (p. 285): CHARACTER CLASSIFICATION FOR
  ALPHANUMERIC / FOR NATIONAL IS locale-name | LOCALE | SYSTEM-DEFAULT |
  USER-DEFAULT; PROGRAM COLLATING SEQUENCE may name a locale alphabet.
- 12.3.7 SPECIAL-NAMES (p. 288): `LOCALE locale-name-1 IS
  external-locale-name-1 | literal-4`, the implementor defining the
  allowable names (rule 5); `ORDER TABLE ordering-name-1 IS literal-9`
  naming a 14651 Annex A table, default 'ISO_14651_2020_TABLE1' (rule
  17, note 5); `ALPHABET alphabet-name IS LOCALE [locale-name-2]`, the
  collating sequence then being the named locale's, or the current
  locale's at the time of use (rule e).
- 13.18.40 PICTURE (p. 441): the LOCALE phrase, editing by LC_MONETARY.
- 14.6.6 Locale identification (p. 541): the nine rules of which locale
  an operation uses; 9) a called runtime element's switches persist in
  the caller.
- 14.9.39 SET (p. 729): format 11 `SET LOCALE category | USER-DEFAULT
  TO locale-name | identifier (data-pointer) | USER-DEFAULT |
  SYSTEM-DEFAULT`, format 12 `SET identifier (data-pointer) TO LOCALE
  LC_ALL | USER-DEFAULT` (save); rules 21-27 (p. 741).
- 15.51-15.54 (pp. 875-878): LOCALE-COMPARE, LOCALE-DATE, LOCALE-TIME,
  LOCALE-TIME-FROM-SECONDS, each with an optional locale-name argument
  and EC-LOCALE-MISSING.
- 15.57 / 15.97 (pp. 884, 942): LOWER-CASE and UPPER-CASE's `LOCALE
  locale-name` phrase, LC_CTYPE's correspondence, a result that may
  change length when the mapping is not one-to-one.
- 15.68 / 15.94: NUMVAL-C and TEST-NUMVAL-C's LOCALE phrase
  (LC_MONETARY de-editing).
- 15.85 STANDARD-COMPARE (p. 916): the ordering table of ISO/IEC
  14651:2020 Annex A, the optional ordering-name, argument-4 the level
  (default the table's highest), EC-ORDER-NOT-SUPPORTED, the same
  trailing-space rule, '<' '=' '>'.

Outside the text: ISO/IEC 9945 for the categories and field names;
ISO/IEC 14651 for the ordering.  14651's Common Template Table and
Unicode's DUCET (allkeys.txt) are kept in step by their two committees;
libutf's root collator is built from allkeys.txt for Unicode 16.0.
This compiler's default ordering table is therefore the DUCET of
Unicode 16.0 under the Unicode Collation Algorithm, and the name it
answers to is 'ISO_14651_2020_TABLE1'.  The honest statement for
conformance.md: the orders agree except for characters added to
Unicode after 14651:2020 was cut, which 14651 does not order at all.

## What libutf gives, and what it does not

At dfa04c5 (`~/utf`, MIT):

- `utf_collator_find(name)`: BCP 47 or POSIX spellings ("sv-SE",
  "sv_SE", "sv_SE.UTF-8"), case-insensitive, falling back a subtag at
  a time ("de-AT-1996" -> "de-AT" -> "de"); "root", "und", "" and NULL
  are root; NULL for a locale it has no ordering for.  53 locales from
  CLDR 46: az be bg br bs ca cs cy da de de_AT dsb el en eo es et fi fo
  fr fr_CA fy ga gl hr hsb hu is it kl lb lt lv mk mt nb nl nn no pl pt
  ro ru se sk sl smn sq sr sr_Latn sv tr uk.  Some (en, de, fr ...)
  order exactly as root.
- `utf_collate_cmp_l` (three levels and a code-point tiebreak),
  `utf_collate_cmp_ci_l` (two levels), `utf_collate_sortkey_l` /
  `_ci_l` (binary keys, memcmp-comparable: primaries as 16-bit
  big-endian weights, a 0x0000 separator, secondaries, 0x0000,
  tertiaries, an NFC tiebreak).  A level-N comparison for
  STANDARD-COMPARE's argument-4 is the two sort keys compared up to
  their Nth separator; no libutf change is needed.
- Tested against ICU 72 / CLDR 42 (tests/test_collate_icu.c, and the
  `collate_locales` table in test_color_ops.c).  Builds and runs on
  SLOW-32 under stage08 cc: 601/601 identical to the host
  (scripts/build-libutf.sh --check).

Footprint on SLOW-32, as objects: collate 20 KB, the CE table 373 KB,
the contraction and locale DFAs 157 KB, nfc 8 KB, its tables 150 KB;
about 710 KB, nearly all rodata.  A COBOL program today is ~330 KB and
libcob.s32o 509 KB.

Not libutf's, and not planned there: LC_TIME (d_fmt, t_fmt, day and
month names), LC_MONETARY (the 8.2.2 fields), locale-specific case
mapping (Turkish and Azeri dotted i, Lithuanian accents), locale
character classes.  Those are small data, and there is a corpus for
them: glibc's localedata, the POSIX locale definitions the standard's
reference to ISO/IEC 9945 means.

## What we have today

- Every door refused by name: `fn_refuse` (operand_parse.h) for the
  five functions, `fn_phrase_nyi` for the LOCALE phrase, divisions.h
  for ALPHABET IS LOCALE / CHARACTER CLASSIFICATION, data.h for
  PICTURE's LOCALE phrase, goto_set.h for SET LOCALE.
- `cob_locale_word`: a 32-bit word carrying DECIMAL-POINT IS COMMA and
  the currency symbol into the editing routines.  Not a locale, but the
  slot a LC_MONETARY record would replace.
- `cob_collating` and `so->coll`: a 256-byte rank table for PROGRAM
  COLLATING SEQUENCE and SORT/MERGE COLLATING SEQUENCE.  Byte-wise;
  a locale alphabet cannot be a rank table.
- UPPER-CASE / LOWER-CASE: `casemap.h`, Unicode 16 simple mappings
  generated from libutf's UnicodeData.txt (gen_casemap.py); national
  and alphanumeric both.
- National <-> UTF-8: `nat_to_utf8`, `utf8_to_nat`, `fn_narrow` in
  libcob; libutf takes UTF-8, so national operands convert on the way
  in.
- The oracles are thin here.  GnuCOBOL 4.0 implements LOCALE-COMPARE,
  LOCALE-DATE, LOCALE-TIME and LOCALE-TIME-FROM-SECONDS over the C
  library's setlocale, with the locale given as a string (a literal
  for LOCALE-COMPARE, an identifier for the others), not a
  SPECIAL-NAMES locale-name, and it does not accept the LOCALE clause
  our test used; no STANDARD-COMPARE; its container is Alpine with no
  locales installed, so it can only ever answer in the POSIX locale.
  gcobol 15: "sorry, unimplemented: LOCALE syntax".  So the collation
  oracle is ICU, through libutf's own ICU-checked tables, and the
  LC_TIME / LC_MONETARY oracle is glibc on a host with the locale
  installed (`locale -k`, `date`); the tests will say "no oracle" in
  the harness's sense and carry their witness in comments.

## Design

**The runtime's locale.**  A locale record in libcob: the collator
(`const utf_collator *`), the LC_TIME fields (d_fmt, t_fmt,
t_fmt_ampm, am_pm, abday, day, abmon, mon -- what d_fmt/t_fmt can
expand), the LC_MONETARY fields of 8.2.2, and LC_CTYPE's casing
variant (none, Turkic, Lithuanian).  A generated table
`libcob/locale_data.h` holds one record per supported locale, keyed by
its POSIX name; lookup accepts the spellings `utf_collator_find`
accepts and resolves "sv_SE.UTF-8" / "sv-SE" / "sv" alike.  The
supported set is the 53 collators plus the POSIX locale ("C", "POSIX":
root collation, `%m/%d/%y`, `%H:%M:%S`, no currency).  The current
locale is six category slots, each a pointer to a record; the user
default is read once at start from LC_ALL, then LC_<category>, then
LANG in the guest's environment (the MMIO ring carries it), falling
back to POSIX; the system default is POSIX.  SET format 12 copies the
six slots into a heap record and hands back its address as a
data-pointer; format 11 with identifier-10 copies them back
(EC-LOCALE-INVALID-PTR if the pointer is not one format 12 made --
a magic word in the record).

**Where the collation lives.**  Not inside libcob.s32o: 710 KB on
every program for a function most never call is wrong, and the
toolchain image must build cobol/ without `~/utf`.  So: vendor the
seven libutf units collation needs (collate.c, nfc.c, their five
tables, the headers) into `libcob/utf/` by a sync script
(`libcob/sync-libutf.sh`, the casemap.h / s32utf_tables.h precedent:
generated from `~/utf`, committed, regenerated by hand), built by
build.sh into `libcob/collate.s32o` alongside esql.s32o, and linked by
compile.sh only when the program's text references `cob_loc_` -- the
`cob_sql_` grep that pulls esql.s32o in is the model -- with
`--rodata-size 2M` then.  The selfhost-libcob gate builds it with
stage08 cc as it does libcob.c; the vendored copy carries its MIT
notice.  Programs that use a locale function pay the 710 KB; the rest
pay nothing.

**The compiler.**  `LOCALE locale-name IS external-name | literal` in
SPECIAL-NAMES makes a symbol of class locale carrying the external
string; every use (function argument, SET, ALPHABET, CHARACTER
CLASSIFICATION) resolves the name at compile time, and a name the
runtime's table does not know is a compile-time error -- every locale
name in the language is static (SET's dynamic operand is a saved
record, not a name), so EC-LOCALE-MISSING is left for the one dynamic
case, a user default named by the environment that the table does not
have, which falls back to POSIX and sets the condition.  `ORDER TABLE
ordering-name IS literal` accepts 'ISO_14651_2020_TABLE1' and refuses
any other literal (the implementor specifies the allowable content).
Functions take the optional locale-name as a trailing argument
(LOCALE-COMPARE, the three LC_TIME functions, STANDARD-COMPARE's
ordering-name and level) or the `LOCALE locale-name` phrase (UPPER-
and LOWER-CASE, NUMVAL-C, TEST-NUMVAL-C); each lowers to a libcob
entry taking the record's index, -1 for "the current locale".

**Comparison.**  `cob_loc_compare(a, na, nat_a, b, nb, nat_b, loc,
level)`: national operands to UTF-8 (the other operand is converted to
national first when classes differ, rule 1 of 15.51 -- for UCA the two
routes meet in the same UTF-8, so convert each to UTF-8 directly);
trailing spaces trimmed as 8.8.4.2.11 says; level 0 (STANDARD-COMPARE
without argument-4, LOCALE-COMPARE always) is `utf_collate_cmp_l`;
level N in 1..4 compares sort keys through the Nth separator, 4 being
the whole key.  EC-LOCALE-INCOMPATIBLE never arises: UCA orders every
code point (implicit weights).  EC-ORDER-NOT-SUPPORTED for a level
outside 1..4.

**LC_TIME.**  `cob_loc_date(yyyymmdd, loc)` and `cob_loc_time(hhmmss,
loc)` expand d_fmt / t_fmt with a strftime subset covering what glibc's
53 definitions use (%a %A %b %B %d %e %H %I %m %M %p %r %S %T %y %Y %D
%F %Z-less), from the fields in the record; LOCALE-TIME-FROM-SECONDS
takes standard numeric time form (seconds past midnight, a fraction
allowed, 0 <= s < 86400) through the same path.  Arguments are
validated as 15.52 / 15.53 say (hours 00-24, seconds 00-99) and
EC-ARGUMENT-FUNCTION otherwise.  The data generator runs in a Debian
container with locales-all (`locale -k LC_TIME LC_MONETARY` per
locale) and writes locale_data.h; glibc's values are the POSIX
corpus, and what GnuCOBOL prints on a glibc host, so a GnuCOBOL cross-
check becomes possible on any Linux box with the locale installed.

**LC_CTYPE.**  UPPER-CASE / LOWER-CASE with a LOCALE phrase, or under
CHARACTER CLASSIFICATION, use the simple mappings plus the locale's
SpecialCasing exceptions (tr/az: i <-> İ, ı <-> I; lt: the dot above
rules) -- the only locale-conditional mappings Unicode has.  The
class condition ALPHABETIC under a locale classification is Unicode
Alphabetic through a libutf DFA (gen/classify, as item 47 did for
extended letters), ALPHABETIC-UPPER / -LOWER likewise.

**ALPHABET IS LOCALE.**  The alphabet carries a locale (or "current")
instead of a rank table.  A relation condition under a PROGRAM
COLLATING SEQUENCE that is a locale alphabet lowers to
`cob_loc_compare` with the 8.8.4.2.11 trimming; SORT and MERGE with
such a COLLATING SEQUENCE compare records by precomputed sort keys of
each KEY (the collation is fixed for the statement, 14.6.6 rule 5);
HIGH-VALUE and LOW-VALUE under such an alphabet are the native ones
(the standard's figurative constants are defined for the native and
alphabet collating sequences; a locale orders characters, not bytes --
to be stated in conformance.md).

**LC_MONETARY.**  The PICTURE LOCALE phrase, and NUMVAL-C /
TEST-NUMVAL-C's LOCALE phrase: the record's currency_symbol,
mon_decimal_point, mon_thousands_sep, mon_grouping, p_cs_precedes and
the signs drive editing and de-editing, where `cob_locale_word`
drives them today; a LOCALE item's edited picture is laid out at
runtime (the symbol's width is the locale's).  The deepest cut, and
the one no corpus of ours uses; last.

## Steps

0. **Vendoring, the record, the link.**  `libcob/sync-libutf.sh`,
   `libcob/utf/` (MIT notice kept), `libcob/collate.s32o` in build.sh,
   compile.sh's `cob_loc_` grep + rodata size, the locale record and
   `cob_loc_find`, the POSIX locale, the user default from the
   environment.  Gates: selfhost-libcob builds the vendored units with
   stage08 cc; a kern-style host test of `cob_loc_find` and the
   trimming rule.
1. **The SPECIAL-NAMES clauses and the two comparisons.**  LOCALE and
   ORDER TABLE clauses; LOCALE-COMPARE; STANDARD-COMPARE with
   ordering-name and level; SET format 11 (locale-name, USER-DEFAULT,
   SYSTEM-DEFAULT, by category) and format 12 (save, and 11's restore
   from the pointer); EC-LOCALE-MISSING, EC-LOCALE-INVALID-PTR,
   EC-ORDER-NOT-SUPPORTED.  Tests 2002/locale (sv: z < å < ä < ö; cs:
   ch after h; da: aa after z; tr: ı before i; es: ñ before o; the
   trimming rule; national operands), 2002/stdcompare (levels 1-4 on
   "a" "A" "á"; the default level; the ordering-name), 2002/setlocale
   (switch, save, restore, a called program's switch persisting); bad
   tests for an unknown locale name, an unknown ordering table, a
   level of 5.  Witness: ICU's answers as libutf's tables record them.
2. **LC_TIME.**  The glibc generator and locale_data.h; LOCALE-DATE,
   LOCALE-TIME, LOCALE-TIME-FROM-SECONDS; the strftime subset.  Tests
   2002/localedate (sv 2026-10-08, de 08.10.2026, en_US 10/08/2026,
   POSIX 10/08/26; times with %r for en_US), bad arguments
   (EC-ARGUMENT-FUNCTION).  Witness: `date` under the locale on a
   glibc host; GnuCOBOL where a locale is installed.
3. **LC_CTYPE.**  The LOCALE phrase of UPPER-CASE and LOWER-CASE;
   CHARACTER CLASSIFICATION (both FOR phrases, the four locale-phrase
   forms); the Turkic and Lithuanian exceptions; the class condition
   under a classification.  Tests 2002/localecase, 2002/charclass.
4. **ALPHABET IS LOCALE.**  Relation conditions under a locale
   PROGRAM COLLATING SEQUENCE; SORT and MERGE COLLATING SEQUENCE by
   sort keys; the national alphabet form.  Tests 2002/localesort,
   2002/localecoll.  Performance: a sort key per record per key,
   computed once (the kern.h sort hook's compare sees keys, not
   strings).
5. **LC_MONETARY.**  PICTURE's LOCALE phrase; NUMVAL-C and
   TEST-NUMVAL-C's LOCALE phrase; the generator's monetary fields.
   Tests 2002/localepic, 2002/localenumvalc.

Steps 0-2 are the functions and the clauses that feed them: the part
libutf's arrival unlocked, and the natural first batch.  3-5 each
stand alone after 0.

Not planned: LC_MESSAGES and LC_NUMERIC beyond being settable and
saveable (8.2: not used by COBOL); locales outside libutf's 53 plus
POSIX (a locale this build has no collation for is not quietly given
root's -- libutf's rule, kept); the implementor-defined effect of a
non-COBOL module's setlocale (there is none in a SLOW-32 run unit).

## Decisions wanted

1. **LC_TIME / LC_MONETARY data from glibc's localedata** (the POSIX
   corpus; what GnuCOBOL prints on a glibc host), generated in a
   Debian container, committed as locale_data.h.  Recommended over
   CLDR's date formats, which differ from POSIX's and have no oracle
   on hand.
2. **Unknown locale names are compile-time errors**, every name in the
   language being static; the runtime condition is kept for the
   environment's user default.  Recommended (strict by default).
3. **The supported set is libutf's 53 plus POSIX**; USER-DEFAULT is the
   environment (LC_ALL, LC_<cat>, LANG), SYSTEM-DEFAULT is POSIX; the
   harness, with no LANG, runs in POSIX.  Recommended.
4. **The default ordering table is DUCET 16.0 under UCA**, named
   'ISO_14651_2020_TABLE1', the alignment stated as above in
   conformance.md.  Recommended; the alternative is to implement
   14651's own table file, which adds nothing a program can see.
5. **Collation linked on demand** as a separate object (the esql.s32o
   model), vendored from libutf by a sync script.  Recommended over
   linking libutf.s32a from the kit (cobol/ must build in the image
   without `~/utf`) and over folding it into libcob.s32o (710 KB on
   every program).
6. **Tests are "no oracle" with the witness quoted** (ICU through
   libutf for collation, glibc for the formats); GnuCOBOL is cross-
   checked only where its POSIX-only answers apply.
7. **Order: steps 0-2 now; 3, 4, 5 as follow-ups** in that order unless
   a corpus asks otherwise.
