identification division.
program-id. rwpara.
*> A reserved word never names a paragraph; cobol ISSUES-43.
procedure division.
main.
    perform limit.
    stop run.
limit.
    display 'x'.
