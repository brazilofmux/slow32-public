* DO CASE + early RETURN inside a FUNCTION (util.prg SUB_BEGIN shape)
? lab("13")
? lab("zz")
FUNCTION lab
PARAMETERS cSub
DO CASE
  CASE cSub = "13"
    RETURN "Fixed Assets"
  OTHERWISE
    RETURN "Unknown (" + cSub + ")"
ENDCASE
