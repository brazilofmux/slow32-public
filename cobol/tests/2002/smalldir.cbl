*> The small directives (2023 7.3.9, 7.3.17-19; cobol standard-queue
*> item 7), none of which changes what this program does here:
*> >>LISTING (no listing is produced), >>PAGE with its comment-text,
*> >>LEAP-SECOND outside a compilation unit (the clock never reports a
*> 60th second), >>CALL-CONVENTION COBOL (the default, and the one
*> convention).  A contained program sits between the two LEAP-SECONDs'
*> places to show the unit is counted by its nesting.
>>LEAP-SECOND OFF
>>LISTING OFF
identification division.
program-id. smalldir.
>>PAGE the procedure division, on a page of its own -- ( unchecked "text
>>LISTING
procedure division.
>>CALL-CONVENTION COBOL
    call "smalldir2"
    display "done"
    stop run.
identification division.
program-id. smalldir2.
procedure division.
    display "in smalldir2"
    goback.
end program smalldir2.
end program smalldir.
>>LEAP-SECOND ON
