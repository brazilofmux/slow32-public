       identification division.
       program-id. float-pic.
      * COMP-1 and COMP-2 (docs/usage.md): COMP-2 takes no PICTURE (ACU's decimal COMP-2 is not implemented).
       data division.
       working-storage section.
       01 i pic 9(5) comp-2.
       procedure division.
           stop run.
