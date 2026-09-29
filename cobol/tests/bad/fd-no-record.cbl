       identification division.
       program-id. fdnorec.
      * An FD is followed by a record description (X3.23-1985 file
      * description syntax rule 3).
       environment division.
       input-output section.
       file-control.
           select f assign to "x.dat".
       data division.
       file section.
       fd f.
       working-storage section.
       01 n pic 9(4).
       01 sn pic s9(2) value 5.
       procedure division.
           stop run.
