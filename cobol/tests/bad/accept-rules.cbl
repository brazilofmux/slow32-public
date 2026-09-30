       identification division.
       program-id. accrul.
      * ACCEPT and DISPLAY (X3.23-1985 ACCEPT and DISPLAY syntax rule
      * 2; 2023 14.9.1.3 rules 2-3): a device is a mnemonic-name of
      * SPECIAL-NAMES; an alphabetic item does not take DATE's digits.
       data division.
       working-storage section.
       01 b pic x(5).
       01 ba pic a(8).
       procedure division.
           accept b from zz.
           display b upon zz.
           accept ba from date.
           stop run.
