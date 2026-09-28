       IDENTIFICATION DIVISION.
       PROGRAM-ID. USEDBG.
      * USE FOR DEBUGGING is the Debug module, obsolete in COBOL 85
      * (item 18) and not implemented: refused with a message naming it.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. SLOW32 WITH DEBUGGING MODE.
       PROCEDURE DIVISION.
       DECLARATIVES.
       DBG SECTION.
           USE FOR DEBUGGING ON ALL PROCEDURES.
       DBG-P.
           DISPLAY DEBUG-NAME.
       END DECLARATIVES.
       MAIN SECTION.
       P1.
           STOP RUN.
