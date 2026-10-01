*> The environment, X/Open's and Micro Focus's (BP-E31): DISPLAY ... UPON
*> ENVIRONMENT-NAME chooses a variable, ACCEPT ... FROM ENVIRONMENT-VALUE
*> reads it, DISPLAY ... UPON ENVIRONMENT-VALUE sets it; ACCEPT ... FROM
*> ENVIRONMENT name reads one in a step.  No name chosen yet, or no such
*> variable: ON EXCEPTION, the item left as it was.  abrignoli_COBSOFT
*> reads COMPUTERNAME so; debinix_openjensen uses the one-step form.
*> The variables come from envvar.env.  The oracle runs in GnuCOBOL's
*> default dialect: its -std=cobol85 has no environment devices.
identification division.
program-id. envvar.
data division.
working-storage section.
01 v pic x(20) value "unchanged".
procedure division.
    accept v from environment-value
        on exception display "no name chosen yet: " v
    end-accept
    display "S32_ENVTEST" upon environment-name
    accept v from environment-value
        on exception display "missing"
        not on exception display "S32_ENVTEST=" v
    end-accept
    display "changed here" upon environment-value
    accept v from environment-value
    display "after DISPLAY UPON ENVIRONMENT-VALUE: " v
    move spaces to v
    accept v from environment "S32_ENVOTHER"
        on exception display "missing"
    end-accept
    display "S32_ENVOTHER=" v
    accept v from environment "S32_NOT_SET_ANYWHERE"
        on exception display "S32_NOT_SET_ANYWHERE: exception, v still " v
    end-accept
    stop run.
