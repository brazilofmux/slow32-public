\ PAD is independent of WORD's parse buffer and pictured numeric output

\ PAD stays at HERE+128 after WORD
: T1  WORD DROP  PAD HERE 128 + = . CR ;
T1 dummy

\ PAD stays at HERE+128 after pictured numeric conversion
: T2  0 12345 <# #S #> 2DROP  PAD HERE 128 + = . CR ;
T2

\ Pictured output buffer is not the same address as PAD
: T3
  0 0 <# 48 HOLD #>         \ "0" in pno_buf -> ( addr len )
  DROP                      \ leave addr
  PAD = 0= . CR             \ should differ from user PAD
;
T3

\ Writing to PAD survives a subsequent pictured conversion
: T4
  PAD 5 BLANK
  S" HI" DROP PAD 2 CMOVE
  0 7 <# #S #> 2DROP
  PAD 2 TYPE CR
;
T4

BYE
