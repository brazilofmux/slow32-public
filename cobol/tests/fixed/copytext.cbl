       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPYTEXT.
      *> COPY ... REPLACING over text-words (2023 7.2.2.5, 7.2.3.4 rule
      *> 9; X3.23-1985 XII COPY general rule 5): '(', ')' and ':' are
      *> separators, so ==(5)== matches inside PIC X(5) and IBM's
      *> ==:PFX:== matches the three text-words of :PFX: in :PFX:-REC, the
      *> replacement joined to the -REC that followed it with no space.
      *> Separator commas match as spaces; a hexadecimal literal is not
      *> the characters it spells; an outer REPLACING reaches the text a
      *> nested COPY brought in (as GnuCOBOL; the 2023 text forbids the
      *> combination, 7.2.3.4 rule 10 -- docs/conformance/copy.md).
      *> Until the COPY sweep (2026-09-30) this compiler matched COBOL
      *> tokens, and the first two did not work.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CTAG REPLACING ==:PFX:== BY ==WS==.
       COPY CTAG REPLACING ==:PFX:== BY ==LK==.
       COPY CWIDTH REPLACING ==(5)== BY ==(8)==.
       COPY CSEP REPLACING ==CS-A PIC== BY ==CS-B PIC==.
       COPY CHEX REPLACING =="ABC"== BY =="xyz"==.
       COPY CNEST REPLACING ==CN-INNER== BY ==CN-DEEP==.
       PROCEDURE DIVISION.
           DISPLAY WS-NAME " " WS-AMT " " LK-NAME.
           DISPLAY "[" CW-TEXT "] " CW-NUM.
           DISPLAY CS-B.
           DISPLAY CH-A " " CH-B.
           DISPLAY CN-OUTER " " CN-DEEP.
           STOP RUN.
