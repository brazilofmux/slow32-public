var
 _stack_top: record end; external name '_stack_top';
 _data: record end; external name '_data';
 _edata: record end; external name '_edata';
 _text_start: record end; external name '_text_start';
 _etext: record end; external name '_etext';
 _bss_start: record end; external name '_bss_start';
 _bss_end: record end; external name '_bss_end';

procedure Pascalmain; external name 'PASCALMAIN';

procedure _haltproc; external Name '_haltproc';
procedure HaltProc; noreturn;
begin
  while true do;
end;

procedure InitMemAndStart; noreturn;
var
  pdest, psrc, pend: PLongWord;
begin
  pdest:=@_data;
  psrc:=@_etext;
  pend:=@_bss_start;

  while pdest<pend do
    begin
      pdest^:=psrc^;
      inc(pdest);
      inc(psrc);
    end;

  pend:=@_bss_end;
  while pdest<pend do
    begin
      pdest^:=0;
      inc(pdest);
    end;

  PASCALMAIN;
  HaltProc;
end;


procedure LowlevelStartup; assembler; nostackframe; [public, alias: '_START'];
 asm
  .weak ResetISR
  .set ResetISR, InitMemAndStart;
  .weak _haltproc
  .set _haltproc, HaltProc

  { Initialize Stack Pointer }
  { Use LUI+ORI to load the address of _stack_top into r29 (sp) }
  lui   r29, %hi(_stack_top)
  ori   r29, r29, %lo(_stack_top)

  { Initialize Frame Pointer to zero }
  addi  r30, r0, 0

  { Jump to InitMemAndStart }
  jal r0, InitMemAndStart
end;
