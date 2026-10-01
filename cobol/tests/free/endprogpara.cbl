*> END-PROGRAM is not a reserved word in any COBOL standard, so it may
*> name a paragraph or a section.  It was listed among the scope
*> terminators, and the paragraph pass and the prescan disagreed:
*> "internal: paragraph 'end-program' not prescanned" (X-COBOL's
*> Martinfx_Cobol and phe-sto, both GnuCOBOL programs).
identification division.
program-id. endprogpara.
procedure division.
main-para.
    display "main"
    perform end-program
    go to end-program-exit.
end-program.
    display "in end-program".
end-program-exit.
    display "done"
    stop run.
