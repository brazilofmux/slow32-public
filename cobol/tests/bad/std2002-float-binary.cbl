       identification division.
       program-id. flbin.
      * USAGE FLOAT-BINARY-32 and the other ISO/IEC 60559 forms are
      * COBOL 2014's (2023 13.18.60.4 rules 14-18): under -std=2002 refused
      * as such, naming the switch that takes them.
       data division.
       working-storage section.
       01 f usage float-binary-32.
       procedure division.
           stop run.
