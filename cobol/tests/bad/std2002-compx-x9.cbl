       identification division.
       program-id. compx-x9.
      * COMP-X (Micro Focus; docs/usage.md): X's past eight bytes are not
      * implemented, even under -std=2002 (cobol ISSUES-124).
       data division.
       working-storage section.
       01 i pic x(9) comp-x.
       procedure division.
           stop run.
