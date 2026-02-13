{
    Does the parsing for the SLOW-32 GNU AS styled inline assembler.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit ras32gas;

{$I fpcdefs.inc}

  interface

    uses
      raatt,ras32,
      cpubase;

    type
      ts32gasreader = class(tattreader)
        function is_register(const s: string): boolean; override;
        function is_asmopcode(const s: string):boolean;override;
        procedure handleopcode;override;
        procedure BuildReference(oper : ts32operand);
        procedure BuildOperand(oper : ts32operand);
        procedure BuildOpCode(instr : ts32instruction);
        procedure ReadAt(oper : ts32operand);
        procedure ReadSym(oper : ts32operand);
      end;

  implementation

    uses
      { helpers }
      cutils,
      { global }
      globtype,globals,verbose,
      systems,
      { aasm }
      aasmbase,aasmtai,aasmdata,aasmcpu,
      { symtable }
      symconst,symsym,symdef,
      { parser }
      procinfo,
      rabase,rautils,
      cgbase,cgobj
      ;


    procedure ts32gasreader.ReadSym(oper : ts32operand);
      var
         tempstr, mangledname : string;
         typesize,l,k : TCGInt;
      begin
        tempstr:=actasmpattern;
        Consume(AS_ID);
        { typecasting? }
        if (actasmtoken=AS_LPAREN) and
           SearchType(tempstr,typesize) then
         begin
           oper.hastype:=true;
           Consume(AS_LPAREN);
           BuildOperand(oper);
           Consume(AS_RPAREN);
           if oper.opr.typ in [OPR_REFERENCE,OPR_LOCAL] then
             oper.SetSize(typesize,true);
         end
        else
         if not oper.SetupVar(tempstr,false) then
          Message1(sym_e_unknown_id,tempstr);
        { record.field ? }
        if actasmtoken=AS_DOT then
         begin
           BuildRecordOffsetSize(tempstr,l,k,mangledname,false);
           if (mangledname<>'') then
             Message(asmr_e_invalid_reference_syntax);
           inc(oper.opr.ref.offset,l);
         end;
      end;


    procedure ts32gasreader.ReadAt(oper : ts32operand);
      begin
        { check for ...@ }
        if actasmtoken=AS_AT then
          begin
            if (oper.opr.ref.symbol=nil) and
               (oper.opr.ref.offset = 0) then
              Message(asmr_e_invalid_reference_syntax);
            Consume(AS_AT);
            if actasmtoken=AS_ID then
              begin
                Message(asmr_e_invalid_reference_syntax);
                Consume(AS_ID);
              end
            else
              Message(asmr_e_invalid_reference_syntax);
          end;
      end;


    procedure ts32gasreader.BuildReference(oper: ts32operand);

      procedure Consume_RParen;
        begin
          if actasmtoken <> AS_RPAREN then
           Begin
             Message(asmr_e_invalid_reference_syntax);
             RecoverConsume(true);
           end
          else
           begin
             Consume(AS_RPAREN);
             if not (actasmtoken in [AS_COMMA,AS_SEPARATOR,AS_END]) then
              Begin
                Message(asmr_e_invalid_reference_syntax);
                RecoverConsume(true);
              end;
           end;
        end;

      var
        l : TCGInt;
        relsym: string;
        asmsymtyp: tasmsymtype;
        isflags: tindsymflags;

      begin
        Consume(AS_LPAREN);
        Case actasmtoken of
          AS_INTNUM,
          AS_MINUS,
          AS_PLUS:
            Begin
              { offset(offset) is invalid }
              If oper.opr.Ref.Offset <> 0 Then
               Begin
                 Message(asmr_e_invalid_reference_syntax);
                 RecoverConsume(true);
               End
              Else
               Begin
                 oper.opr.Ref.Offset:=BuildConstExpression(false,true);
                 Consume(AS_RPAREN);
                 if actasmtoken=AS_AT then
                   ReadAt(oper);
               end;
              exit;
            End;
          AS_REGISTER: { (reg ...  }
            Begin
              if ((oper.opr.typ=OPR_REFERENCE) and (oper.opr.ref.base<>NR_NO)) or
                 ((oper.opr.typ=OPR_LOCAL) and (oper.opr.localsym.localloc.loc<>LOC_REGISTER)) then
                message(asmr_e_cannot_index_relative_var);
              oper.opr.ref.base:=actasmregister;
              Consume(AS_REGISTER);
              Consume_RParen;
            end;
          AS_ID:
            Begin
              ReadSym(oper);
              case actasmtoken of
                AS_PLUS:
                  begin
                    l:=BuildConstExpression(true,true);
                    case oper.opr.typ of
                      OPR_CONSTANT :
                        inc(oper.opr.val,l);
                      OPR_LOCAL :
                        inc(oper.opr.localsymofs,l);
                      OPR_REFERENCE :
                        inc(oper.opr.ref.offset,l);
                      else
                        internalerror(2003092016);
                    end;
                  end;
                AS_MINUS:
                  begin
                    Consume(AS_MINUS);
                    BuildConstSymbolExpression(false,true,false,l,relsym,asmsymtyp);
                    if (relsym<>'') then
                      begin
                        if (oper.opr.typ = OPR_REFERENCE) then
                          oper.opr.ref.relsymbol:=current_asmdata.RefAsmSymbol(relsym,AT_DATA)
                        else
                          begin
                            Message(asmr_e_invalid_reference_syntax);
                            RecoverConsume(false);
                          end
                      end
                    else
                      begin
                        case oper.opr.typ of
                          OPR_CONSTANT :
                            dec(oper.opr.val,l);
                          OPR_LOCAL :
                            dec(oper.opr.localsymofs,l);
                          OPR_REFERENCE :
                            dec(oper.opr.ref.offset,l);
                          else
                            internalerror(2007092601);
                        end;
                      end;
                  end;
                else
                  ;
              end;
              Consume(AS_RPAREN);
              if actasmtoken=AS_AT then
                ReadAt(oper);
            End;
          AS_COMMA: { (, ...  can either be scaling, or index }
            Begin
              Consume(AS_COMMA);
              if (actasmtoken=AS_REGISTER) then
                Begin
                  oper.opr.ref.index:=actasmregister;
                  Consume(AS_REGISTER);
                  Consume_RParen;
                end
              else
                begin
                  Message(asmr_e_invalid_reference_syntax);
                  RecoverConsume(false);
                end;
            end;
        else
          Begin
            Message(asmr_e_invalid_reference_syntax);
            RecoverConsume(false);
          end;
        end;
      end;


    procedure ts32gasreader.BuildOperand(oper: ts32operand);
      var
        expr : string;
        typesize,l : TCGInt;


        procedure AddLabelOperand(hl:tasmlabel);
          begin
            if not(actasmtoken in [AS_PLUS,AS_MINUS,AS_LPAREN]) and
               is_calljmp(actopcode) then
             begin
               oper.opr.typ:=OPR_SYMBOL;
               oper.opr.symbol:=hl;
             end
            else
             begin
               oper.InitRef;
               oper.opr.ref.symbol:=hl;
             end;
          end;


        procedure MaybeRecordOffset;
          var
            mangledname: string;
            hasdot  : boolean;
            l,
            toffset,
            tsize   : TCGInt;
          begin
            if not(actasmtoken in [AS_DOT,AS_PLUS,AS_MINUS]) then
             exit;
            l:=0;
            hasdot:=(actasmtoken=AS_DOT);
            if hasdot then
              begin
                if expr<>'' then
                  begin
                    BuildRecordOffsetSize(expr,toffset,tsize,mangledname,false);
                    if (oper.opr.typ<>OPR_CONSTANT) and
                       (mangledname<>'') then
                      Message(asmr_e_wrong_sym_type);
                    inc(l,toffset);
                    oper.SetSize(tsize,true);
                  end;
              end;
            if actasmtoken in [AS_PLUS,AS_MINUS] then
              inc(l,BuildConstExpression(true,false));
            case oper.opr.typ of
              OPR_LOCAL :
                begin
                  if hasdot and
                     (not oper.hastype) and
                     (tabstractvarsym(oper.opr.localsym).owner.symtabletype=parasymtable) and
                     (current_procinfo.procdef.proccalloption<>pocall_register) then
                    Message(asmr_e_cannot_access_field_directly_for_parameters);
                  inc(oper.opr.localsymofs,l)
                end;
              OPR_CONSTANT :
                if (mangledname<>'') then
                  begin
                    if (oper.opr.val<>0) then
                      Message(asmr_e_wrong_sym_type);
                    oper.opr.typ:=OPR_SYMBOL;
                    oper.opr.symbol:=current_asmdata.DefineAsmSymbol(mangledname,AB_EXTERNAL,AT_FUNCTION,voidcodepointertype);
                  end
                else
                  inc(oper.opr.val,l);
              OPR_REFERENCE :
                inc(oper.opr.ref.offset,l);
              OPR_SYMBOL:
                Message(asmr_e_invalid_symbol_ref);
              else
                internalerror(200309221);
            end;
          end;


        function MaybeBuildReference:boolean;
          begin
            MaybeBuildReference:=true;
            case actasmtoken of
              AS_INTNUM,
              AS_MINUS,
              AS_PLUS:
                Begin
                  oper.opr.ref.offset:=BuildConstExpression(True,False);
                  if actasmtoken<>AS_LPAREN then
                    Message(asmr_e_invalid_reference_syntax)
                  else
                    BuildReference(oper);
                end;
              AS_LPAREN:
                BuildReference(oper);
              AS_ID:
                Begin
                  ReadSym(oper);
                  case actasmtoken of
                    AS_END,
                    AS_SEPARATOR,
                    AS_COMMA: ;
                    AS_LPAREN:
                      BuildReference(oper);
                  else
                    Begin
                      Message(asmr_e_invalid_reference_syntax);
                      Consume(actasmtoken);
                    end;
                  end;
                end;
              else
               MaybeBuildReference:=false;
            end;
          end;


      var
        tempreg : tregister;
        hl : tasmlabel;
        ofs : aint;
        refaddr: trefaddr;
        entered_paren: Boolean;
      Begin
        expr:='';
        entered_paren:=false;

        refaddr:=addr_full;
        if actasmtoken=AS_MOD then
          begin
            consume(AS_MOD);

            if actasmtoken<>AS_ID then
              begin
                Message(asmr_e_invalid_reference_syntax);
                RecoverConsume(false);
              end
            else
              begin
                if lower(actasmpattern)='hi' then
                  refaddr:=addr_hi20
                else if lower(actasmpattern)='lo' then
                  refaddr:=addr_lo12
                else
                  begin
                    Message(asmr_e_invalid_reference_syntax);
                    RecoverConsume(false);
                  end;

                consume(AS_ID);
                consume(AS_LPAREN);
                entered_paren:=true;
              end;
          end;

        case actasmtoken of
          AS_LPAREN: { Memory reference or constant expression }
            Begin
              oper.InitRef;
              BuildReference(oper);
            end;

          AS_INTNUM,
          AS_MINUS,
          AS_PLUS:
            Begin
              { Constant memory offset }
              oper.InitRef;
              oper.opr.ref.offset:=BuildConstExpression(True,False);
              if actasmtoken<>AS_LPAREN then
                begin
                  ofs:=oper.opr.ref.offset;
                  BuildConstantOperand(oper);
                  inc(oper.opr.val,ofs);
                end
              else
                BuildReference(oper);
            end;

          AS_DOT,
          AS_ID: { A constant expression, or a Variable ref.  }
            Begin
              { Local Label ? }
              if is_locallabel(actasmpattern) then
               begin
                 CreateLocalLabel(actasmpattern,hl,false);
                 Consume(AS_ID);
                 AddLabelOperand(hl);
               end
              else
               { Check for label }
               if SearchLabel(actasmpattern,hl,false) then
                begin
                  Consume(AS_ID);
                  AddLabelOperand(hl);
                end
              else
               { probably a variable or normal expression }
               Begin
                 { is it a constant ? }
                 if SearchIConstant(actasmpattern,l) then
                  Begin
                    if not (oper.opr.typ in [OPR_NONE,OPR_CONSTANT]) then
                     Message(asmr_e_invalid_operand_type);
                    BuildConstantOperand(oper);
                  end
                 else
                  begin
                    expr:=actasmpattern;
                    Consume(AS_ID);
                    { typecasting? }
                    if (actasmtoken=AS_LPAREN) and
                       SearchType(expr,typesize) then
                     begin
                       oper.hastype:=true;
                       Consume(AS_LPAREN);
                       BuildOperand(oper);
                       Consume(AS_RPAREN);
                       if oper.opr.typ in [OPR_REFERENCE,OPR_LOCAL] then
                         oper.SetSize(typesize,true);
                     end
                    else
                     begin
                       if oper.SetupVar(expr,false) then
                         ReadAt(oper)
                       else
                        Begin
                          { look for special symbols ... }
                          if expr= '__HIGH' then
                            begin
                              consume(AS_LPAREN);
                              if not oper.setupvar('high'+actasmpattern,false) then
                                Message1(sym_e_unknown_id,'high'+actasmpattern);
                              consume(AS_ID);
                              consume(AS_RPAREN);
                            end
                          else
                           if expr = '__RESULT' then
                            oper.SetUpResult
                          else
                           if expr = '__SELF' then
                            oper.SetupSelf
                          else
                           if expr = '__OLDEBP' then
                            oper.SetupOldEBP
                          else
                            Message1(sym_e_unknown_id,expr);
                        end;
                     end;
                  end;
                  if actasmtoken=AS_DOT then
                    MaybeRecordOffset;
                  { add a constant expression? }
                  if (actasmtoken=AS_PLUS) then
                   begin
                     l:=BuildConstExpression(true,entered_paren);
                     case oper.opr.typ of
                       OPR_CONSTANT :
                         inc(oper.opr.val,l);
                       OPR_LOCAL :
                         inc(oper.opr.localsymofs,l);
                       OPR_REFERENCE :
                         inc(oper.opr.ref.offset,l);
                       else
                         internalerror(2003092017);
                     end;
                   end
               end;
              { Do we have a indexing reference, then parse it also }
              if actasmtoken=AS_LPAREN then
                begin
                  oper.InitRef;
                  BuildReference(oper);
                end;
            end;
          AS_REGISTER: { Register, a variable reference or a constant reference  }
            Begin
              { save the type of register used. }
              tempreg:=actasmregister;
              Consume(AS_REGISTER);
              if (actasmtoken in [AS_END,AS_SEPARATOR,AS_COMMA]) then
                  begin
                    if not (oper.opr.typ in [OPR_NONE,OPR_REGISTER]) then
                      Message(asmr_e_invalid_operand_type);
                    oper.opr.typ:=OPR_REGISTER;
                    oper.opr.reg:=tempreg;
                  end
              else
                Message(asmr_e_syn_operand);
            end;
          AS_END,
          AS_SEPARATOR,
          AS_COMMA: ;
        else
          Begin
            Message(asmr_e_syn_operand);
            Consume(actasmtoken);
          end;
        end; { end case }

        if refaddr<>addr_full then
          begin
            if oper.opr.typ<>OPR_REFERENCE then
              oper.InitRef;

            oper.opr.ref.refaddr:=refaddr;
            Consume(AS_RPAREN);
          end
        else if (oper.opr.typ=OPR_REFERENCE) and
           (oper.opr.ref.refaddr=addr_no) and
           assigned(oper.opr.ref.symbol) then
          oper.opr.ref.refaddr:=addr_full;
      end;


{*****************************************************************************
                                ts32gasreader
*****************************************************************************}

    procedure ts32gasreader.BuildOpCode(instr : ts32instruction);
      var
        operandnum : longint;
      Begin
        { opcode }
        if (actasmtoken<>AS_OPCODE) then
         Begin
           Message(asmr_e_invalid_or_missing_opcode);
           RecoverConsume(true);
           exit;
         end;
        { Fill the instr object with the current state }
        with instr do
          begin
            Opcode:=ActOpcode;
            condition:=ActCondition;
          end;

        { We are reading operands, so opcode will be an AS_ID }
        operandnum:=1;
        Consume(AS_OPCODE);
        { Zero operand opcode ?  }
        if actasmtoken in [AS_SEPARATOR,AS_END] then
         begin
           operandnum:=0;
           exit;
         end;
        { Read the operands }
        repeat
          case actasmtoken of
            AS_COMMA: { Operand delimiter }
              Begin
                if operandnum>Max_Operands then
                  Message(asmr_e_too_many_operands)
                else
                  begin
                    if instr.Operands[operandnum].opr.typ<>OPR_NONE then
                      Inc(operandnum);
                  end;
                Consume(AS_COMMA);
              end;
            AS_SEPARATOR,
            AS_END : { End of asm operands for this opcode  }
              begin
                break;
              end;
          else
            BuildOperand(instr.Operands[operandnum] as ts32operand);
          end; { end case }
        until false;
        if (operandnum=1) and (instr.Operands[operandnum].opr.typ=OPR_NONE) then
          dec(operandnum);
        instr.Ops:=operandnum;
      end;


    function ts32gasreader.is_register(const s: string): boolean;
      var
        reg: TRegister;
      begin
        result:=inherited is_register(s);
        { reg found?
          possible aliases are always 2 to 4 chars
        }
        if not(result) then
          begin
            reg:=is_extra_reg(s);
            if reg<>NR_NO then
              begin
                actasmregister:=reg;
                result:=true;
                actasmtoken:=AS_REGISTER;
              end;
          end;
      end;


    function ts32gasreader.is_asmopcode(const s: string):boolean;
      var
        hs : string;
      Begin
        { making s a value parameter would break other assembler readers }
        hs:=s;
        is_asmopcode:=false;

        { clear op code }
        actopcode:=A_None;
        { clear condition }
        fillchar(actcondition,sizeof(actcondition),0);

        actopcode := tasmop(ptruint(iasmops.find(hs)));
        if actopcode <> A_NONE then
          begin
            actasmtoken:=AS_OPCODE;
            is_asmopcode:=true;
            exit;
          end;
      end;


    procedure ts32gasreader.handleopcode;
      var
        instr : ts32instruction;
      begin
        instr:=ts32instruction.Create(ts32operand);
        BuildOpcode(instr);
        instr.condition := actcondition;
        instr.ConcatInstruction(curlist);
        instr.Free;
      end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

    const
      asmmode_s32_standard_info : tasmmodeinfo =
              (
                id    : asmmode_standard;
                idtxt : 'STANDARD';
                casmreader : ts32gasreader;
              );

initialization
{$ifdef SLOW32}
  RegisterAsmMode(asmmode_s32_standard_info);
{$endif SLOW32}
end.
