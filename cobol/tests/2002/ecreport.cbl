*> The Report Writer conditions (2023 Table 13): EC-REPORT-NOT-TERMINATED
*> (nonfatal: the file closed with its report active), then the fatal
*> ones, each in a contained program CALLed by the argument
*> (ecreport.args: notterm): EC-REPORT-INACTIVE (GENERATE before
*> INITIATE), EC-REPORT-ACTIVE (INITIATE twice), EC-REPORT-FILE-MODE
*> (INITIATE with the file not open: INPUT is refused at compile time),
*> EC-FLOW-REPORT (a GENERATE in another declarative section, reached
*> from a USE BEFORE REPORTING procedure by PERFORM, which 14.9.49.3
*> rule 4 allows).
*> No oracle: GnuCOBOL 4 does not implement exception declaratives.
identification division.
program-id. ecreport.
data division.
working-storage section.
01  which pic x(8).
procedure division.
    accept which from command-line
    display "case: " which
    evaluate which
      when "notterm" call "ec-notterm"
      when "inactive" call "ec-inactive"
      when "active" call "ec-active"
      when "mode" call "ec-mode"
      when "flow" call "ec-flow"
    end-evaluate
    display "end of " which
    stop run.
identification division.
program-id. ec-notterm.
environment division.
input-output section.
file-control.
    select prt assign to "ecreport.prn" organization line sequential.
data division.
file section.
fd  prt report is rep.
report section.
rd  rep page limit 10 lines heading 1 first detail 2.
01  det type detail line plus 1 column 1 pic x(4) value "item".
procedure division.
declaratives.
d1 section.
    use after exception condition ec-report-not-terminated.
    display "declarative: " function exception-status.
end declaratives.
main section.
>>TURN EC-REPORT-NOT-TERMINATED CHECKING ON
    open output prt
    initiate rep
    generate det
    close prt
    display "after CLOSE".
end program ec-notterm.
identification division.
program-id. ec-inactive.
environment division.
input-output section.
file-control.
    select prt assign to "ecreport.prn" organization line sequential.
data division.
file section.
fd  prt report is rep.
report section.
rd  rep page limit 10 lines heading 1 first detail 2.
01  det type detail line plus 1 column 1 pic x(4) value "item".
procedure division.
declaratives.
d1 section.
    use after exception condition ec-report-inactive.
    display "declarative: " function exception-status.
end declaratives.
main section.
>>TURN EC-REPORT-INACTIVE CHECKING ON
    open output prt
    generate det
    display "after GENERATE (not reached)".
end program ec-inactive.
identification division.
program-id. ec-active.
environment division.
input-output section.
file-control.
    select prt assign to "ecreport.prn" organization line sequential.
data division.
file section.
fd  prt report is rep.
report section.
rd  rep page limit 10 lines heading 1 first detail 2.
01  det type detail line plus 1 column 1 pic x(4) value "item".
procedure division.
declaratives.
d1 section.
    use after exception condition ec-report-active.
    display "declarative: " function exception-status.
end declaratives.
main section.
>>TURN EC-REPORT-ACTIVE CHECKING ON
    open output prt
    initiate rep
    initiate rep
    display "after the second INITIATE (not reached)".
end program ec-active.
identification division.
program-id. ec-mode.
environment division.
input-output section.
file-control.
    select prt assign to "ecreport.prn" organization line sequential.
data division.
file section.
fd  prt report is rep.
report section.
rd  rep page limit 10 lines heading 1 first detail 2.
01  det type detail line plus 1 column 1 pic x(4) value "item".
procedure division.
declaratives.
d1 section.
    use after exception condition ec-report-file-mode.
    display "declarative: " function exception-status.
end declaratives.
main section.
>>TURN EC-REPORT-FILE-MODE CHECKING ON
    initiate rep
    display "after INITIATE (not reached)".
end program ec-mode.
identification division.
program-id. ec-flow.
environment division.
input-output section.
file-control.
    select prt assign to "ecreport.prn" organization line sequential.
data division.
file section.
fd  prt report is rep.
working-storage section.
01  n pic 9 value 0.
report section.
rd  rep page limit 10 lines heading 1 first detail 2.
01  det type detail line plus 1 column 1 pic x(4) value "item".
procedure division.
>>TURN EC-FLOW-REPORT CHECKING ON
declaratives.
d1 section.
    use after exception condition ec-flow-report.
    display "declarative: " function exception-status.
d2 section.
    use before reporting det.
    perform again.
d3 section.
    use after exception condition ec-user-never.
again.
    display "in the USE BEFORE REPORTING procedure"
    generate det.
end declaratives.
main section.
    open output prt
    initiate rep
    generate det
    display "after GENERATE (not reached)".
end program ec-flow.
end program ecreport.
