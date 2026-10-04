# What Micro Focus's ADIS does, key by key

Observed 2026-10-04 with `tests/adischeck.sh`: Microsoft COBOL 5.0
(Micro Focus, 1993) with ADIS in its default configuration, run under
the DOS translator (~/x86) with its key trace.  This is the record
behind docs/plans/screen-input.md: where the Micro Focus reference is
silent, this is what the product does.  Each table is the field (at
line 2, column 6) and the cursor's column within it after each key;
`_` is ADIS's prompt character in an empty or zero-suppressed position;
the last lines are the item after the ACCEPT, as MS COBOL DISPLAYs it
(a numeric item without its point; a negative one with its last digit
overpunched, `p` = -0 ... `y` = -9), and the CRT status as key type /
code 1 / code 2.

Rerun any of them: the line after `$` is the command.

## What the tables show

**Numeric fields are edited as their edited image, not as a value.**

- The cursor starts on the first position that is not zero-suppressed in
  the current image: the first `9` of `ZZZ99.99` when it holds zero; the
  first significant digit of a field that holds a value; the point (or,
  in a picture with no point, the last digit) when everything before it
  is suppressed.
- On a digit position, a digit overtypes and the cursor moves right,
  skipping insertion characters (`,` `/` `B`).  `ZZZ99.99`: `5` Enter
  stores 50.00; `9(5)`: `5` Enter stores 50000.
- With the cursor on the point, a digit is inserted before the point and
  the integer digits move left into the suppressed positions; when the
  integer part is full the cursor goes to the first fraction digit.  So a
  picture with one `9` before the point (`ZZ9.99`, `Z,ZZ9.99`,
  `$$,$$9.99`, `---9.99`, `***9.99`) is entered calculator-style: `1` `2`
  gives 12.00.
- The decimal point key right-aligns what has been typed in the integer
  part and moves to the first fraction digit (`999.99`: `1` `2` `.`
  gives 012.00).  Enter does not align: what is on the screen is what is
  stored.
- In the fraction, digits overtype left to right; on the last one the
  cursor stays and further digits overtype it.
- A non-edited `9(3)V99` is two fields side by side: the point key
  aligns the integer part and moves to the fraction; without it the
  cursor stays on the last integer digit.
- Left and Right move over digit positions only; Home in a single field
  goes to its first position; End to its last.
- Backspace: in the fraction, zero the digit before the cursor and move
  left; from the first fraction digit, move to the point; on the point,
  remove the digit before it and let the others move right (`1234.00`
  gives `123.00`) -- or, when the digits stand only in the picture's `9`
  positions, zero the one to the left and move onto it (`12.00` in
  `ZZZ99.99` gives `10.00`).  (That split is read from two cases; the
  differential test of step 3 is what will pin it.)  Delete removes the digit under the cursor: in the
  integer part the digits to its left move right; in the fraction those
  to its right move left.
- Ctrl-X zeroes the field, Ctrl-Z zeroes from the cursor on, Ctrl-A puts
  back what the field held when the cursor entered it; each puts the
  cursor where the rule above says.
- `-` makes the value negative and `+` positive, wherever the cursor is,
  and the picture's own sign form shows it (leading or trailing `-`, a
  floating `-`, `CR`, a `+` that becomes `-`).  In an unsigned picture,
  and in a non-edited `S9` item, the key does nothing.  Letters and
  other characters do nothing.  Insert does nothing.
- Floating `$`, `+`, `-` and check protection `*` behave as suppression:
  the image is re-edited after every key.
- In a picture with no point (`ZZ9`, `+++9`) the image while typing is
  shown one position to the left with a prompt character in the last
  position (`5` shows `_5_`); the value stored is right.
- BLANK WHEN ZERO: zero shows as `0.00` while the cursor is in the
  field and as blanks after.
- A field that is left shows spaces where the prompt characters were.

**Text fields.**

- A key overtypes and the cursor moves right; on the last position it
  stays, and further keys overtype that position.
- Insert toggles insert mode: a key pushes the rest right and the last
  character falls off.
- Backspace in replace mode puts back the character that was overtyped
  (typing `ab` over `hello` and two Backspaces gives `hello` again); on
  typed text it deletes.  Delete closes up; Ctrl-R puts the deleted
  character back; Ctrl-O inserts a space; Ctrl-F changes the case of
  the character under the cursor.
- Ctrl-X clears the field, Ctrl-Z from the cursor on, Ctrl-A restores
  the field as it was on entry.  Ctrl-Home clears every field;
  Ctrl-End clears from the cursor to the end of the screen.
- `PIC A` refuses digits and punctuation and takes letters and space.
- An alphanumeric-edited picture (`XX/XX/XXXX`, `XXBXX`) is X(n): its
  insertion characters are typed over.
- SECURE shows nothing; the cursor moves.  JUSTIFIED RIGHT is applied
  when the ACCEPT ends.
- Right cannot pass the end of the data (the prompt characters): it
  goes to the next field.  Left from a field's first position goes to
  the end of the data of the field before.

**Between fields, and ending.**

- Tab and Shift-Tab go to the next and previous field; Down and Up
  likewise in a column of fields; Home to the first field and End to
  the last.  Without AUTO a full field keeps the cursor; with AUTO the
  next key position is the next field's first.
- REQUIRED refuses Tab and Enter while the field is empty; FULL while it
  is partly filled (text), or while a digit is still suppressed
  (`ZZ9.99` needs three integer digits).
- Enter ends the ACCEPT from any field: CRT status `0`, 48, 13.
- Esc and the function keys do nothing: user function keys are off by
  default in ADIS.  (The standard has a function key end the ACCEPT
  with status 1xxx, and that is what we keep.)

## The tables

### A zero-suppressed numeric field: the editing keys

```
$ adischeck ZZZ99.99 {BS}12.3{BS}{BS}{BS}{ENTER}
(start)  [___00.00]  4      
BS       [___00.00]  4      
1        [___10.00]  5      
2        [___12.00]  6      
.        [___12.00]  7      
3        [___12.30]  8      
BS       [___12.00]  7      
BS       [___12.00]  6      
BS       [___10.00]  5      
ENTER    [   10.00]  (9,24) 
         ITEM=[0001000]
         F2=[    ] CRT=0/048/013

$ adischeck ZZZ99.99 .5{LEFT}{LEFT}{LEFT}7{ENTER}
(start)  [___00.00]  4      
.        [___00.00]  7      
5        [___00.50]  8      
LEFT     [___00.50]  7      
LEFT     [___00.50]  6      
LEFT     [___00.50]  5      
7        [___07.50]  6      
ENTER    [   07.50]  (9,24) 
         ITEM=[0000750]
         F2=[    ] CRT=0/048/013

$ adischeck ZZZ99.99 123{LEFT}{LEFT}9{RIGHT}{RIGHT}{RIGHT}4{ENTER}
(start)  [___00.00]  4      
1        [___10.00]  5      
2        [___12.00]  6      
3        [__123.00]  6      
LEFT     [__123.00]  5      
LEFT     [__123.00]  4      
9        [__193.00]  5      
RIGHT    [__193.00]  6      
RIGHT    [__193.00]  7      
RIGHT    [__193.00]  8      
4        [__193.04]  8      
ENTER    [  193.04]  (9,24) 
         ITEM=[0019304]
         F2=[    ] CRT=0/048/013

$ adischeck ZZZ99.99 123{DEL}{LEFT}{DEL}.45{LEFT}{DEL}{ENTER}
(start)  [___00.00]  4      
1        [___10.00]  5      
2        [___12.00]  6      
3        [__123.00]  6      
DEL      [__123.00]  6      
LEFT     [__123.00]  5      
DEL      [___12.00]  6      
.        [___12.00]  7      
4        [___12.40]  8      
5        [___12.45]  8      
LEFT     [___12.45]  7      
DEL      [___12.50]  7      
ENTER    [   12.50]  (9,24) 
         ITEM=[0001250]
         F2=[    ] CRT=0/048/013

$ adischeck ZZZ99.99 12.34{^X}5{ENTER}
(start)  [___00.00]  4      
1        [___10.00]  5      
2        [___12.00]  6      
.        [___12.00]  7      
3        [___12.30]  8      
4        [___12.34]  8      
^X       [___00.00]  4      
5        [___50.00]  5      
ENTER    [   50.00]  (9,24) 
         ITEM=[0005000]
         F2=[    ] CRT=0/048/013

$ adischeck ZZZ99.99 12.34{LEFT}{LEFT}{LEFT}{^Z}{ENTER}
(start)  [___00.00]  4      
1        [___10.00]  5      
2        [___12.00]  6      
.        [___12.00]  7      
3        [___12.30]  8      
4        [___12.34]  8      
LEFT     [___12.34]  7      
LEFT     [___12.34]  6      
LEFT     [___12.34]  5      
^Z       [___10.00]  5      
ENTER    [   10.00]  (9,24) 
         ITEM=[0001000]
         F2=[    ] CRT=0/048/013

$ adischeck ZZZ99.99 12.34{^A}{ENTER}
(start)  [___00.00]  4      
1        [___10.00]  5      
2        [___12.00]  6      
.        [___12.00]  7      
3        [___12.30]  8      
4        [___12.34]  8      
^A       [___00.00]  4      
ENTER    [   00.00]  (9,24) 
         ITEM=[0000000]
         F2=[    ] CRT=0/048/013

$ adischeck ZZZ99.99 12{INS}3{HOME}4{END}5{ENTER}
(start)  [___00.00]  4      
1        [___10.00]  5      
2        [___12.00]  6      
INS      [___12.00]  6      
3        [__123.00]  6      
HOME     [__123.00]  3      
4        [__423.00]  4      
END      [__423.00]  8      
5        [__423.05]  8      
ENTER    [  423.05]  (9,24) 
         ITEM=[0042305]
         F2=[    ] CRT=0/048/013

$ adischeck ZZZ99.99 1234567890{ENTER}
(start)  [___00.00]  4      
1        [___10.00]  5      
2        [___12.00]  6      
3        [__123.00]  6      
4        [_1234.00]  6      
5        [12345.00]  7      
6        [12345.60]  8      
7        [12345.67]  8      
8        [12345.68]  8      
9        [12345.69]  8      
0        [12345.60]  8      
ENTER    [12345.60]  (9,24) 
         ITEM=[1234560]
         F2=[    ] CRT=0/048/013

$ adischeck ZZZ99.99 1a-+.x2{ENTER}
(start)  [___00.00]  4      
1        [___10.00]  5      
a        [___10.00]  5      
-        [___10.00]  5      
+        [___10.00]  5      
.        [___01.00]  7      
x        [___01.00]  7      
2        [___01.20]  8      
ENTER    [   01.20]  (9,24) 
         ITEM=[0000120]
         F2=[    ] CRT=0/048/013
```

### Plain numerics, implied decimals, signs

```
$ adischeck ZZZ99.99 5{ENTER}
(start)  [___00.00]  4      
5        [___50.00]  5      
ENTER    [   50.00]  (9,24) 
         ITEM=[0005000]
         F2=[    ] CRT=0/048/013

$ adischeck 9(5) 5{ENTER}
(start)  [00000]  1      
5        [50000]  2      
ENTER    [50000]  (9,24) 
         ITEM=[50000]
         F2=[    ] CRT=0/048/013

$ adischeck 9(5) 12{LEFT}{LEFT}9{END}7{HOME}3{ENTER}
(start)  [00000]  1      
1        [10000]  2      
2        [12000]  3      
LEFT     [12000]  2      
LEFT     [12000]  1      
9        [92000]  2      
END      [92000]  5      
7        [92007]  5      
HOME     [92007]  1      
3        [32007]  2      
ENTER    [32007]  (9,24) 
         ITEM=[32007]
         F2=[    ] CRT=0/048/013

$ adischeck 999.99 12.3{ENTER}
(start)  [000.00]  1      
1        [100.00]  2      
2        [120.00]  3      
.        [012.00]  5      
3        [012.30]  6      
ENTER    [012.30]  (9,24) 
         ITEM=[01230]
         F2=[    ] CRT=0/048/013

$ adischeck 999.99 1234567{ENTER}
(start)  [000.00]  1      
1        [100.00]  2      
2        [120.00]  3      
3        [123.00]  5      
4        [123.40]  6      
5        [123.45]  6      
6        [123.46]  6      
7        [123.47]  6      
ENTER    [123.47]  (9,24) 
         ITEM=[12347]
         F2=[    ] CRT=0/048/013

$ adischeck -i 9(3)V99 9(3)V99 12.3{ENTER}
(start)  [00000]  1      
1        [10000]  2      
2        [12000]  3      
.        [01200]  4      
3        [01230]  5      
ENTER    [01230]  (9,24) 
         ITEM=[01230]
         F2=[    ] CRT=0/048/013

$ adischeck -i 9(3)V99 9(3)V99 1234567{ENTER}
(start)  [00000]  1      
1        [10000]  2      
2        [12000]  3      
3        [12300]  3      
4        [12400]  3      
5        [12500]  3      
6        [12600]  3      
7        [12700]  3      
ENTER    [12700]  (9,24) 
         ITEM=[12700]
         F2=[    ] CRT=0/048/013

$ adischeck -i S9(3)V99 S9(3)V99 12-{ENTER}
(start)  [00000]  1      
1        [10000]  2      
2        [12000]  3      
-        [12000]  3      
ENTER    [12000]  (9,24) 
         ITEM=[12000]
         F2=[    ] CRT=0/048/013

$ adischeck -ZZ9.99 12-.5{ENTER}
(start)  [___0.00]  4      
1        [___1.00]  5      
2        [__12.00]  5      
-        [-_12.00]  5      
.        [-_12.00]  6      
5        [-_12.50]  7      
ENTER    [- 12.50]  (9,24) 
         ITEM=[0125p]
         F2=[    ] CRT=0/048/013

$ adischeck -ZZ9.99 -12+{ENTER}
(start)  [___0.00]  4      
-        [-__0.00]  4      
1        [-__1.00]  5      
2        [-_12.00]  5      
+        [__12.00]  5      
ENTER    [  12.00]  (9,24) 
         ITEM=[01200]
         F2=[    ] CRT=0/048/013

$ adischeck ZZ9.99- 12-{ENTER}
(start)  [__0.00 ]  3      
1        [__1.00 ]  4      
2        [_12.00 ]  4      
-        [_12.00-]  4      
ENTER    [ 12.00-]  (9,24) 
         ITEM=[0120p]
         F2=[    ] CRT=0/048/013

$ adischeck ZZ9.99CR 12-{ENTER}
(start)  [__0.00  ]  3      
1        [__1.00  ]  4      
2        [_12.00  ]  4      
-        [_12.00CR]  4      
ENTER    [ 12.00CR]  (9,24) 
         ITEM=[0120p]
         F2=[    ] CRT=0/048/013

$ adischeck +ZZ9 5-{ENTER}
(start)  [+__0]  4      
5        [+_5_]  4      
-        [-_5_]  4      
ENTER    [-  5]  (9,24) 
         ITEM=[00u]
         F2=[    ] CRT=0/048/013

$ adischeck ZZ9 -5{ENTER}
(start)  [__0]  3      
-        [__0]  3      
5        [_5_]  3      
ENTER    [  5]  (9,24) 
         ITEM=[005]
         F2=[    ] CRT=0/048/013
```

### Commas, floating insertion, check protection, insertion characters, initial values

```
$ adischeck Z,ZZ9.99 12345.6{BS}{BS}{BS}{ENTER}
(start)  [____0.00]  5      
1        [____1.00]  6      
2        [___12.00]  6      
3        [__123.00]  6      
4        [1,234.00]  7      
5        [1,234.50]  8      
.        [1,234.50]  8      
6        [1,234.56]  8      
BS       [1,234.50]  8      
BS       [1,234.00]  7      
BS       [1,234.00]  6      
ENTER    [1,234.00]  (9,24) 
         ITEM=[123400]
         F2=[    ] CRT=0/048/013

$ adischeck ZZ,ZZ9.99 1234{LEFT}{LEFT}{LEFT}{LEFT}{LEFT}9{ENTER}
(start)  [_____0.00]  6      
1        [_____1.00]  7      
2        [____12.00]  7      
3        [___123.00]  7      
4        [_1,234.00]  7      
LEFT     [_1,234.00]  6      
LEFT     [_1,234.00]  5      
LEFT     [_1,234.00]  4      
LEFT     [_1,234.00]  2      
LEFT     [_1,234.00]  2      
9        [_9,234.00]  4      
ENTER    [ 9,234.00]  (9,24) 
         ITEM=[0923400]
         F2=[    ] CRT=0/048/013

$ adischeck $$,$$9.99 1234.5{ENTER}
(start)  [____$0.00]  6      
1        [____$1.00]  7      
2        [___$12.00]  7      
3        [__$123.00]  7      
4        [$1,234.00]  8      
.        [$1,234.00]  8      
5        [$1,234.50]  9      
ENTER    [$1,234.50]  (9,24) 
         ITEM=[123450]
         F2=[    ] CRT=0/048/013

$ adischeck $$,$$9.99 12345{ENTER}
(start)  [____$0.00]  6      
1        [____$1.00]  7      
2        [___$12.00]  7      
3        [__$123.00]  7      
4        [$1,234.00]  8      
5        [$1,234.50]  9      
ENTER    [$1,234.50]  (9,24) 
         ITEM=[123450]
         F2=[    ] CRT=0/048/013

$ adischeck ---9.99 12-3{ENTER}
(start)  [___0.00]  4      
1        [___1.00]  5      
2        [__12.00]  5      
-        [_-12.00]  5      
3        [-123.00]  6      
ENTER    [-123.00]  (9,24) 
         ITEM=[1230p]
         F2=[    ] CRT=0/048/013

$ adischeck +++9 12-{ENTER}
(start)  [__+0]  4      
1        [_+1_]  4      
2        [+12_]  4      
-        [-12_]  4      
ENTER    [ -12]  (9,24) 
         ITEM=[01r]
         F2=[    ] CRT=0/048/013

$ adischeck ***9.99 12.5{ENTER}
(start)  [***0.00]  4      
1        [***1.00]  5      
2        [**12.00]  5      
.        [**12.00]  6      
5        [**12.50]  7      
ENTER    [**12.50]  (9,24) 
         ITEM=[001250]
         F2=[    ] CRT=0/048/013

$ adischeck $ZZ9.99 12{ENTER}
(start)  [$__0.00]  4      
1        [$__1.00]  5      
2        [$_12.00]  5      
ENTER    [$ 12.00]  (9,24) 
         ITEM=[01200]
         F2=[    ] CRT=0/048/013

$ adischeck ZZZ.ZZ 12.5{ENTER}
(start)  [___.00]  4      
1        [__1.00]  4      
2        [_12.00]  4      
.        [_12.00]  5      
5        [_12.50]  6      
ENTER    [ 12.50]  (9,24) 
         ITEM=[01250]
         F2=[    ] CRT=0/048/013

$ adischeck ZZZ.ZZ {ENTER}
(start)  [___.00]  4      
ENTER    [      ]  (9,24) 
         ITEM=[00000]
         F2=[    ] CRT=0/048/013

$ adischeck -c BLANK WHEN ZERO ZZ9.99 1{BS}{ENTER}
(start)  [__0.00]  3      
1        [__1.00]  4      
BS       [__0.00]  3      
ENTER    [      ]  (9,24) 
         ITEM=[00000]
         F2=[    ] CRT=0/048/013

$ adischeck 99/99/99 123456{ENTER}
(start)  [00/00/00]  1      
1        [10/00/00]  2      
2        [12/00/00]  4      
3        [12/30/00]  5      
4        [12/34/00]  7      
5        [12/34/50]  8      
6        [12/34/56]  8      
ENTER    [12/34/56]  (9,24) 
         ITEM=[123456]
         F2=[    ] CRT=0/048/013

$ adischeck 99/99/99 12{LEFT}{LEFT}{LEFT}3{RIGHT}{RIGHT}{RIGHT}{RIGHT}4{ENTER}
(start)  [00/00/00]  1      
1        [10/00/00]  2      
2        [12/00/00]  4      
LEFT     [12/00/00]  2      
LEFT     [12/00/00]  1      
LEFT     [12/00/00]  1      
3        [32/00/00]  2      
RIGHT    [32/00/00]  4      
RIGHT    [32/00/00]  5      
RIGHT    [32/00/00]  7      
RIGHT    [32/00/00]  8      
4        [32/00/04]  8      
ENTER    [32/00/04]  (9,24) 
         ITEM=[320004]
         F2=[    ] CRT=0/048/013

$ adischeck 9(3)B9(3) 123456{ENTER}
(start)  [000 000]  1      
1        [100 000]  2      
2        [120 000]  3      
3        [123 000]  5      
4        [123 400]  6      
5        [123 450]  7      
6        [123 456]  7      
ENTER    [123 456]  (9,24) 
         ITEM=[123456]
         F2=[    ] CRT=0/048/013

$ adischeck -v 12.5 ZZ9.99 7{ENTER}
(start)  [_12.50]  2      
7        [_72.50]  3      
ENTER    [ 72.50]  (9,24) 
         ITEM=[07250]
         F2=[    ] CRT=0/048/013

$ adischeck -v 12.5 ZZ9.99 {RIGHT}{RIGHT}7{ENTER}
(start)  [_12.50]  2      
RIGHT    [_12.50]  3      
RIGHT    [_12.50]  4      
7        [127.50]  5      
ENTER    [127.50]  (9,24) 
         ITEM=[12750]
         F2=[    ] CRT=0/048/013
```

### Text fields

```
$ adischeck X(6) abc{LEFT}{LEFT}Z{HOME}q{END}w{ENTER}
(start)  [______]  1      
a        [a_____]  2      
b        [ab____]  3      
c        [abc___]  4      
LEFT     [abc___]  3      
LEFT     [abc___]  2      
Z        [aZc___]  3      
HOME     [aZc___]  1      
q        [qZc___]  2      
END      [qZc___]  4      
w        [qZcw__]  5      
ENTER    [qZcw  ]  (9,24) 
         ITEM=[qZcw  ]
         F2=[    ] CRT=0/048/013

$ adischeck X(6) abcdefgh{ENTER}
(start)  [______]  1      
a        [a_____]  2      
b        [ab____]  3      
c        [abc___]  4      
d        [abcd__]  5      
e        [abcde_]  6      
f        [abcdef]  6      
g        [abcdeg]  6      
h        [abcdeh]  6      
ENTER    [abcdeh]  (9,24) 
         ITEM=[abcdeh]
         F2=[    ] CRT=0/048/013

$ adischeck X(6) abcd{LEFT}{LEFT}{INS}XY{INS}Q{ENTER}
(start)  [______]  1      
a        [a_____]  2      
b        [ab____]  3      
c        [abc___]  4      
d        [abcd__]  5      
LEFT     [abcd__]  4      
LEFT     [abcd__]  3      
INS      [abcd__]  3      
X        [abXcd_]  4      
Y        [abXYcd]  5      
INS      [abXYcd]  5      
Q        [abXYQd]  6      
ENTER    [abXYQd]  (9,24) 
         ITEM=[abXYQd]
         F2=[    ] CRT=0/048/013

$ adischeck X(6) abcdef{HOME}{INS}XY{ENTER}
(start)  [______]  1      
a        [a_____]  2      
b        [ab____]  3      
c        [abc___]  4      
d        [abcd__]  5      
e        [abcde_]  6      
f        [abcdef]  6      
HOME     [abcdef]  1      
INS      [abcdef]  1      
X        [Xabcde]  2      
Y        [XYabcd]  3      
ENTER    [XYabcd]  (9,24) 
         ITEM=[XYabcd]
         F2=[    ] CRT=0/048/013

$ adischeck X(6) abcd{BS}{BS}xy{BS}{ENTER}
(start)  [______]  1      
a        [a_____]  2      
b        [ab____]  3      
c        [abc___]  4      
d        [abcd__]  5      
BS       [abc___]  4      
BS       [ab____]  3      
x        [abx___]  4      
y        [abxy__]  5      
BS       [abx___]  4      
ENTER    [abx   ]  (9,24) 
         ITEM=[abx   ]
         F2=[    ] CRT=0/048/013

$ adischeck X(6) abcd{LEFT}{LEFT}{LEFT}{DEL}{DEL}{^R}{ENTER}
(start)  [______]  1      
a        [a_____]  2      
b        [ab____]  3      
c        [abc___]  4      
d        [abcd__]  5      
LEFT     [abcd__]  4      
LEFT     [abcd__]  3      
LEFT     [abcd__]  2      
DEL      [acd___]  2      
DEL      [ad____]  2      
^R       [acd___]  2      
ENTER    [acd   ]  (9,24) 
         ITEM=[acd   ]
         F2=[    ] CRT=0/048/013

$ adischeck -v "hello" X(6) {RIGHT}{RIGHT}{^Z}{^A}{^X}Q{ENTER}
(start)  [hello_]  1      
RIGHT    [hello_]  2      
RIGHT    [hello_]  3      
^Z       [he____]  3      
^A       [hello_]  1      
^X       [______]  1      
Q        [Q_____]  2      
ENTER    [Q     ]  (9,24) 
         ITEM=[Q     ]
         F2=[    ] CRT=0/048/013

$ adischeck -v "hello" X(6) ab{BS}{BS}{ENTER}
(start)  [hello_]  1      
a        [aello_]  2      
b        [abllo_]  3      
BS       [aello_]  2      
BS       [hello_]  1      
ENTER    [hello ]  (9,24) 
         ITEM=[hello ]
         F2=[    ] CRT=0/048/013

$ adischeck -v "hello" X(6) {RIGHT}{^F}{^O}{ENTER}
(start)  [hello_]  1      
RIGHT    [hello_]  2      
^F       [hEllo_]  3      
^O       [hE llo]  3      
ENTER    [hE llo]  (9,24) 
         ITEM=[hE llo]
         F2=[    ] CRT=0/048/013

$ adischeck A(4) a1b-c d{ENTER}
(start)  [____]  1      
a        [a___]  2      
1        [a___]  2      
b        [ab__]  3      
-        [ab__]  3      
c        [abc_]  4      
         [abc ]  4      
d        [abcd]  4      
ENTER    [abcd]  (9,24) 
         ITEM=[abcd]
         F2=[    ] CRT=0/048/013

$ adischeck XX/XX/XXXX 12345678{ENTER}
(start)  [  /  /____]  1      
1        [1 /  /____]  2      
2        [12/  /____]  3      
3        [123  /____]  4      
4        [1234 /____]  5      
5        [12345/____]  6      
6        [123456____]  7      
7        [1234567___]  8      
8        [12345678__]  9      
ENTER    [12345678  ]  (9,24) 
         ITEM=[12345678  ]
         F2=[    ] CRT=0/048/013

$ adischeck XXBXX abcde{ENTER}
(start)  [_____]  1      
a        [a____]  2      
b        [ab___]  3      
c        [abc__]  4      
d        [abcd_]  5      
e        [abcde]  5      
ENTER    [abcde]  (9,24) 
         ITEM=[abcde]
         F2=[    ] CRT=0/048/013

$ adischeck X(6) ab{ESC}
(start)  [______]  1      
a        [a_____]  2      
b        [ab____]  3      
ESC      [ab____]  3      
         (keys ran out; the ACCEPT was still waiting)

$ adischeck X(6) ab{F3}
(start)  [______]  1      
a        [a_____]  2      
b        [ab____]  3      
F3       [ab____]  3      
         (keys ran out; the ACCEPT was still waiting)

$ adischeck -c SECURE X(6) abc{ENTER}
(start)  [      ]  1      
a        [      ]  2      
b        [      ]  3      
c        [      ]  4      
ENTER    [      ]  (9,24) 
         ITEM=[abc   ]
         F2=[    ] CRT=0/048/013

$ adischeck -c JUSTIFIED RIGHT X(6) abc{ENTER}
(start)  [______]  1      
a        [a_____]  2      
b        [ab____]  3      
c        [abc___]  4      
ENTER    [   abc]  (9,24) 
         ITEM=[   abc]
         F2=[    ] CRT=0/048/013
```

### Between fields: AUTO, REQUIRED, FULL

```
$ adischeck -2 X(3) ab{RIGHT}{RIGHT}{RIGHT}x{LEFT}{LEFT}y{ENTER}
(start)  [___]  1      
a        [a__]  2      
b        [ab_]  3      
RIGHT    [ab ]  (4,6)     | ____
RIGHT    [ab ]  (4,6)     | ____
RIGHT    [ab ]  (4,6)     | ____
x        [ab ]  (4,7)     | x___
LEFT     [ab ]  (4,6)     | x___
LEFT     [ab_]  3         | x
y        [aby]  3         | x
ENTER    [aby]  (9,24)    | x
         ITEM=[aby]
         F2=[x   ] CRT=0/048/013

$ adischeck -2 X(3) abcd{ENTER}
(start)  [___]  1      
a        [a__]  2      
b        [ab_]  3      
c        [abc]  3      
d        [abd]  3      
ENTER    [abd]  (9,24) 
         ITEM=[abd]
         F2=[    ] CRT=0/048/013

$ adischeck -2 -c AUTO X(3) abcd{ENTER}
(start)  [___]  1      
a        [a__]  2      
b        [ab_]  3      
c        [abc]  (4,6)     | ____
d        [abc]  (4,7)     | d___
ENTER    [abc]  (9,24)    | d
         ITEM=[abc]
         F2=[d   ] CRT=0/048/013

$ adischeck -2 X(3) a{TAB}b{HOME}c{END}d{ENTER}
(start)  [___]  1      
a        [a__]  2      
TAB      [a  ]  (4,6)     | ____
b        [a  ]  (4,7)     | b___
HOME     [a__]  1         | b
c        [c__]  2         | b
END      [c  ]  (4,6)     | b___
d        [c  ]  (4,7)     | d___
ENTER    [c  ]  (9,24)    | d
         ITEM=[c  ]
         F2=[d   ] CRT=0/048/013

$ adischeck -2 -c REQUIRED X(3) {TAB}{ENTER}a{ENTER}
(start)  [___]  1      
TAB      [___]  1      
ENTER    [___]  1      
a        [a__]  2      
ENTER    [a  ]  (9,24) 
         ITEM=[a  ]
         F2=[    ] CRT=0/048/013

$ adischeck -2 -c FULL X(3) a{TAB}{ENTER}bc{ENTER}
(start)  [___]  1      
a        [a__]  2      
TAB      [a__]  2      
ENTER    [a__]  2      
b        [ab_]  3      
c        [abc]  3      
ENTER    [abc]  (9,24) 
         ITEM=[abc]
         F2=[    ] CRT=0/048/013

$ adischeck -2 -c FULL ZZ9.99 1{TAB}{ENTER}23{ENTER}
(start)  [__0.00]  3      
1        [__1.00]  4      
TAB      [__1.00]  4      
ENTER    [__1.00]  4      
2        [_12.00]  4      
3        [123.00]  5      
ENTER    [123.00]  (9,24) 
         ITEM=[12300]
         F2=[    ] CRT=0/048/013

$ adischeck -2 -c AUTO ZZ9.99 12345x{ENTER}
(start)  [__0.00]  3      
1        [__1.00]  4      
2        [_12.00]  4      
3        [123.00]  5      
4        [123.40]  6      
5        [123.45]  (4,6)     | ____
x        [123.45]  (4,7)     | x___
ENTER    [123.45]  (9,24)    | x
         ITEM=[12345]
         F2=[x   ] CRT=0/048/013

$ adischeck -2 -c AUTO 9(3) 123x{ENTER}
(start)  [000]  1      
1        [100]  2      
2        [120]  3      
3        [123]  (4,6)     | ____
x        [123]  (4,7)     | x___
ENTER    [123]  (9,24)    | x
         ITEM=[123]
         F2=[x   ] CRT=0/048/013
```

### Shift-Tab, Up and Down, undo across fields, clear screen

```
$ adischeck -2 X(3) a{TAB}b{BTAB}c{DOWN}d{UP}e{ENTER}
(start)  [___]  1      
a        [a__]  2      
TAB      [a  ]  (4,6)     | ____
b        [a  ]  (4,7)     | b___
BTAB     [a  ]  (4,6)     | b___
c        [a  ]  (4,7)     | c___
DOWN     [a  ]  (4,7)     | c___
d        [a  ]  (4,8)     | cd__
UP       [a__]  2         | cd
e        [ae_]  3         | cd
ENTER    [ae ]  (9,24)    | cd
         ITEM=[ae ]
         F2=[cd  ] CRT=0/048/013

$ adischeck -2 ZZ9.99 5{TAB}{BTAB}7{ENTER}
(start)  [__0.00]  3      
5        [__5.00]  4      
TAB      [  5.00]  (4,6)     | ____
BTAB     [__5.00]  3      
7        [__7.00]  4      
ENTER    [  7.00]  (9,24) 
         ITEM=[00700]
         F2=[    ] CRT=0/048/013

$ adischeck -2 X(3) a{TAB}xyzw{^A}{BTAB}{^A}{ENTER}
(start)  [___]  1      
a        [a__]  2      
TAB      [a  ]  (4,6)     | ____
x        [a  ]  (4,7)     | x___
y        [a  ]  (4,8)     | xy__
z        [a  ]  (4,9)     | xyz_
w        [a  ]  (4,9)     | xyzw
^A       [a  ]  (4,6)     | ____
BTAB     [a__]  1      
^A       [a__]  1      
ENTER    [a  ]  (9,24) 
         ITEM=[a  ]
         F2=[    ] CRT=0/048/013

$ adischeck -2 -v "abc" X(3) {TAB}xy{CHOME}{ENTER}
(start)  [abc]  1      
TAB      [abc]  (4,6)     | ____
x        [abc]  (4,7)     | x___
y        [abc]  (4,8)     | xy__
CHOME    [___]  1      
ENTER    [   ]  (9,24) 
         ITEM=[   ]
         F2=[    ] CRT=0/048/013

$ adischeck -2 -v "abc" X(3) {RIGHT}{CEND}{ENTER}
(start)  [abc]  1      
RIGHT    [abc]  2      
CEND     [a__]  2      
ENTER    [a  ]  (9,24) 
         ITEM=[a  ]
         F2=[    ] CRT=0/048/013
```

