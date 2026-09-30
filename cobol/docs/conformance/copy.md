# COPY and REPLACE: 7.2

Swept 2026-09-30. X3.23-1985: XII (Source Text Manipulation), COPY
XII-2..XII-5 and REPLACE XII-6..XII-8. 2002: 7.1.2, 7.1.3. 2023: 7.2.1
through 7.2.4.

## What changed

Before this sweep, COPY and REPLACE worked on the compiler's own tokens,
after the tokenizer had decided what was a picture, a number or a
hyphenated word. The standard defines them on *text-words* (7.2.2.5),
before any of that, and the token model got the difference wrong in ways
real code meets:

- `COPY x REPLACING ==(5)== BY ==(8)==` did not reach inside `PIC X(5)`:
  the picture was one token, but to the standard it is four text-words,
  since a parenthesis is always a separator.
- IBM's tag idiom, `REPLACING ==:PFX:== BY ==WS==` over a copybook of
  `:PFX:-REC` names, failed: `:PFX:-REC` is four text-words (`:` `PFX`
  `:` `-REC`), and the replacement has to join the `-REC` that followed
  with no space between.
- A pseudo-text ending in `PIC` (`==CS-A PIC==`) confused the picture
  scanner.
- A hexadecimal literal matched the characters it spells (`X"414243"`
  was replaced by `=="ABC"==`).

So text manipulation is now a stage of its own (`text_manipulation` in
s32-cobc.c), run on the lines once the reference format is read:

1. The lines are split into text-words by 7.2.2.5, each remembering
   whether a space stood before it.
2. COPY statements are replaced by their library text, nested COPY
   first, then REPLACING (7.2 step 1 with the replacing of step 2).
3. REPLACE statements are applied (step 3).
4. The words are put back into lines, glued where no space stood, and
   the ordinary tokenizer reads the result.

EXEC SQL text passes through untouched (docs/esql.md), and `EXEC SQL
INCLUDE` members go through steps 1-4 before they are tokenized.

CCVS-85 still passes 8068 of 8175 tests. Majesty, whose copybooks all go
through this path, is byte-identical, and so are the seven papers. One
latent tokenizer bug surfaced on the way: `PICTURE IS` at a line's end,
with the picture on the next line (NC107A), relied on the trailing
spaces of a fixed-form line and failed in free form. Fixed.

## COPY (2023 7.2.3)

| rule | paraphrase | disposition |
|---|---|---|
| SR 1 | anywhere a character-string or separator may appear; not inside another COPY | **test**: free/copybook, fixed/copyupper, CCVS SM |
| SR 2 | preceded by a space | the text-word scan finds COPY only as a word of its own |
| SR 3-5 | text-name, library-name, literal-1/-2 | **test**: fixed/copyupper (a text-name found under its uppercase file name); literal text-names and `OF`/`IN` are **test**ed by CCVS SM; how a name finds a file is the implementor's (GR 3): beside the source, then the `-I` directories, as written, `.cpy`, `.CPY`, `.cbl`, `.CBL`, and upper-cased |
| SR 6 | pseudo-text-1 has a word that is not a comma or semicolon | **refused**: bad/copy-pseudo-commas |
| SR 7 | pseudo-text-2 may be empty | **test**: CCVS SM (deletion) |
| SR 8 | pseudo-text may be continued | the reference-format reader joins continued lines before this stage |
| SR 9 | a text-word up to 65,535 characters (1985: 322) | no limit here |
| SR 10 | no directive line inside pseudo-text | **refused**: "a compiler directive line inside pseudo-text" |
| SR 11-12 | partial-word-1 one text-word; partial-word-2 one or none | **refused**: bad/std2002-copy-partial-two |
| SR 13 | a partial word is not a literal | **refused**: bad/std2002-copy-partial-literal |
| LEADING / TRAILING | 2002 and later | **test**: 2002/replacestack; **refused** under `-std=85`: bad/copy-leading-85 |
| GR 1-3 | locating the library text | as SR 3-5 |
| GR 4-5 | SUPPRESS; LISTING | accepted; there is no listing |
| GR 6-7 | the text replaces the whole statement, period included; without REPLACING, unchanged | **test**: free/copybook |
| GR 8-9 | the matching: leftmost word, each operand in turn, pseudo-text word for word, partial words at a word's start or end; commas, semicolons and runs of spaces are one space; case ignored outside literals; either quotation mark, a doubled quote as one | **test**: fixed/copytext (inside a picture, the `:PFX:` idiom, a separator comma, a hexadecimal literal not matching), free/copyrep (words, literals, identifiers with qualifiers, pseudo-text), free/copyquote (the quotation marks: GnuCOBOL diverges, docs/oracles.md) |
| 1985 identifier, literal and word operands | taken as pseudo-text holding them (1985 GR 4) | **test**: free/copyrep, CCVS SM206A (`BY x IN y IN z (1)`) |
| GR 10 | with REPLACING, the library text has no COPY | **ruling**: accepted; the nested text is expanded first and the outer REPLACING reaches it, as GnuCOBOL does. **test**: fixed/copytext (CNEST). Refusing it would refuse real programs to no one's benefit. Before this sweep the outer REPLACING was applied first and missed the nested text |
| GR 11 | the result in free form; spaces only where there were spaces | **test**: fixed/copytext (`WS` joined to `-REC`) |
| GR 12 | nesting at least five levels; no copybook copies itself | eight levels; **refused**: bad/copy-self |
| GR 13 | replacement introduces no COPY, directive, comment or blank line | a COPY so introduced reaches the parser as a word and is refused there |
| 1985 debugging lines | text for matching, then a comment unless WITH DEBUGGING MODE; a COPY on a debugging line is a comment | **test**: CCVS SM101A, KP008 |

## REPLACE (2023 7.2.4)

| rule | paraphrase | disposition |
|---|---|---|
| SR 1-2 | anywhere a character-string may appear, preceded by a space | **test**: 2002/replacestack (`DISPLAY aa REPLACE ==xx== BY ==bb==.`). The 1985 rule is stricter, a separator period before it or the program's start: **refused** under `-std=85`, bad/replace-midsentence-85. Before this sweep REPLACE was recognised only at a sentence's start, in every edition |
| SR 3-10 | as COPY's SR 6-13 | the same code: the refusals above |
| GR 4-7 | active, inactive and canceled; ALSO queues the active one and extends it; LAST OFF pops; OFF cancels all; a REPLACE without ALSO cancels all | **test**: 2002/replacestack; **refused** under `-std=85`: bad/replace-also-85 |
| GR 8 | the matching, as COPY's rule 9, from the text after the statement | **test**: free/replace, 2002/replacestack, free/copyquote |
| GR 9 | no COPY, REPLACE or directive produced | the result is not rescanned |
| GR 10 | the result in free form | as COPY's rule 11 |
| 1985 GR 4 | REPLACE after COPY | step 3 after steps 1-2: **test**: CCVS SM |
