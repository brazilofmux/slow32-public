\ Regression: kernel compile-time control-flow balance check
\ Valid control structures still compile and execute normally.
: cf-if   7 0 > IF 11 ELSE 22 THEN . CR ;
: cf-loop 4 BEGIN DUP 0 > WHILE DUP . 1- REPEAT DROP CR ;
: cf-case 2 CASE 1 OF 100 ENDOF 2 OF 200 ENDOF 300 ENDCASE . CR ;
cf-if
cf-loop
cf-case
\ Malformed control flow is now rejected at compile time (prints error, aborts).
: cf-bad 1 IF 2 ;
\ Recovery after abort: subsequent definitions still compile and run.
: cf-after 42 . CR ;
cf-after
