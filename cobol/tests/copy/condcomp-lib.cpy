      *> library text for 2002/condcomp: a variable defined here is known
      *> after the COPY, and an >>IF here closes here
       >>DEFINE FROM-LIB AS 7
       >>IF LEVEL > 2
           DISPLAY "copybook: level above 2"
       >>ELSE
           DISPLAY "copybook: level 2 or less"
       >>END-IF
