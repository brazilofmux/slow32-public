\ fly-main.fth — the interactive demo entry: fly until ESC.
: FLY
    TRUE FLYING !
    BEGIN KEYS STEP FRAME 16 MS FLYING @ 0= UNTIL
    ." ok, the ship holds at " >SCREEN SWAP . . CR ;
: MAIN
    TUBE-ON 0= IF EXIT THEN
    FLY ;
