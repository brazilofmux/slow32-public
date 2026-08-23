\ tube primitives smoke test (docs/TUBE.md). Runs headless: with no
\ viewer attached OPEN still succeeds and PRESENT still counts.
TUBE-INIT . CR
1 TUBE-OPEN . CR
TUBE-INFO 65536 / . CR
TUBE-INFO 7 AND . CR
CREATE TLIST HEX 10640640 , 20C80640 , 0 , DECIMAL
TLIST 3 1 TUBE-PRESENT . CR
TUBE-STATUS 16777215 AND . CR
TUBE-CLOSE . CR
2 TUBE-OPEN . CR
TUBE-CLOSE . CR
BYE
