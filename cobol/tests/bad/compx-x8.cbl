       identification division.
       program-id. compx-x8.
      * COMP-X (Micro Focus; docs/usage.md): eight X's hold twenty digits,
      * the wide arithmetic of -std=2002 (cobol ISSUES-124); under 85, refused.
       data division.
       working-storage section.
       01 i pic x(8) comp-x.
       procedure division.
           stop run.
