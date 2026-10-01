       identification division.
       program-id. s02ipic.
      * A PICTURE implied by an alphanumeric VALUE literal (2002
      * 13.13.2 rule 14; 2023 13.16.3 rule 9) is not implemented:
      * refused, naming it.
       data division.
       working-storage section.
       01 a value "abc".
       procedure division.
           stop run.
