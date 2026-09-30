       identification division.
       program-id. float-just.
      * COMP-1 and COMP-2 (docs/usage.md): a float takes no editing clauses.
       data division.
       working-storage section.
       01 i comp-2 blank when zero.
       procedure division.
           stop run.
