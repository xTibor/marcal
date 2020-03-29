{$RANGECHECKS ON}
{$OVERFLOWCHECKS ON}

program Assembler;

uses
  Trit, Words, Arch, Utils, SysUtils;

type
  TInstructionFormat = (
      ifRegister,
      ifImmediate3,
      ifImmediate6
  );

const
  CInstructions: array[TOpcode] of record
    Mnemonic: String;
    Format: TInstructionFormat;
  end = (
    ( Mnemonic: 'UD13'; Format: ifRegister   ),
    ( Mnemonic: 'UD12'; Format: ifRegister   ),
    ( Mnemonic: 'UD11'; Format: ifRegister   ),
    ( Mnemonic: 'UD10'; Format: ifRegister   ),
    ( Mnemonic: 'UD09'; Format: ifRegister   ),
    ( Mnemonic: 'UD08'; Format: ifRegister   ),
    ( Mnemonic: 'UD07'; Format: ifRegister   ),
    ( Mnemonic: 'UD06'; Format: ifRegister   ),
    ( Mnemonic: 'UD05'; Format: ifRegister   ),
    ( Mnemonic: 'ROTR'; Format: ifRegister   ),
    ( Mnemonic: 'LSHR'; Format: ifRegister   ),
    ( Mnemonic: 'NEGR'; Format: ifRegister   ),
    ( Mnemonic: 'DYAD'; Format: ifRegister   ),
    ( Mnemonic: 'ADDR'; Format: ifRegister   ),
    ( Mnemonic: 'ADDQ'; Format: ifImmediate3 ),
    ( Mnemonic: 'ADDH'; Format: ifImmediate6 ),
    ( Mnemonic: 'LDLH'; Format: ifImmediate6 ),
    ( Mnemonic: 'LDHH'; Format: ifImmediate6 ),
    ( Mnemonic: 'LDMR'; Format: ifRegister   ),
    ( Mnemonic: 'STMR'; Format: ifRegister   ),
    ( Mnemonic: 'BREQ'; Format: ifRegister   ),
    ( Mnemonic: 'BRNE'; Format: ifRegister   ),
    ( Mnemonic: 'BRLT'; Format: ifRegister   ),
    ( Mnemonic: 'BRLE'; Format: ifRegister   ),
    ( Mnemonic: 'PUSH'; Format: ifRegister   ),
    ( Mnemonic: 'PULL'; Format: ifRegister   ),
    ( Mnemonic: 'CALL'; Format: ifRegister   )
  );

  CRegisters: array[0..29] of record
    RegName: String;
    Register: TRegister;
  end = (
    ( RegName: 'S0';    Register: regZero           ),
    ( RegName: 'S1';    Register: regProgramCounter ),
    ( RegName: 'S2';    Register: regSystem2        ),
    ( RegName: 'S3';    Register: regSystem3        ),
    ( RegName: 'S4';    Register: regSystem4        ),
    ( RegName: 'S5';    Register: regSystem5        ),
    ( RegName: 'S6';    Register: regSystem6        ),
    ( RegName: 'S7';    Register: regSystem7        ),
    ( RegName: 'S8';    Register: regSystem8        ),
    ( RegName: 'S9';    Register: regSystem9        ),
    ( RegName: 'S10';   Register: regSystem10       ),
    ( RegName: 'S11';   Register: regSystem11       ),
    ( RegName: 'S12';   Register: regSystem12       ),
    ( RegName: 'S13';   Register: regSystem13       ),
    ( RegName: 'U0';    Register: regZero           ),
    ( RegName: 'U1';    Register: regUser1          ),
    ( RegName: 'U2';    Register: regUser2          ),
    ( RegName: 'U3';    Register: regUser3          ),
    ( RegName: 'U4';    Register: regUser4          ),
    ( RegName: 'U5';    Register: regUser5          ),
    ( RegName: 'U6';    Register: regUser6          ),
    ( RegName: 'U7';    Register: regUser7          ),
    ( RegName: 'U8';    Register: regUser8          ),
    ( RegName: 'U9';    Register: regUser9          ),
    ( RegName: 'U10';   Register: regUser10         ),
    ( RegName: 'U11';   Register: regUser11         ),
    ( RegName: 'U12';   Register: regUser12         ),
    ( RegName: 'U13';   Register: regUser13         ),
    ( RegName: 'PC';    Register: regProgramCounter ),
    ( RegName: 'ZERO';  Register: regZero           )
  );

function MnemonicToOpcode(AMnemonic: String): TOpcode;
var
  LIndex: TOpcode;
begin
  for LIndex := Low(CInstructions) to High(CInstructions) do begin
    if CInstructions[LIndex].Mnemonic = AMnemonic then begin
      MnemonicToOpcode := LIndex;
      Exit;
    end;
  end;
  // TODO: Assert here
end;

function StrToRegister(ARegName: String): TRegister;
var
  LIndex: Integer;
begin
  for LIndex := Low(CRegisters) to High(CRegisters) do begin
    if CRegisters[LIndex].RegName = ARegName then begin
      StrToRegister := CRegisters[LIndex].Register;
      Exit;
    end;
  end;
  // TODO: Assert here
end;

function EncodeInstructionRgtr(AOpcode: TOpcode; ARegD: TRegister; ARegA: TRegister; ARegB: TRegister): TWord;
begin
  EncodeInstructionRgtr :=
    (LongInt(AOpcode) * 19683) + { << 9 }
    (LongInt(ARegD)   *   729) + { << 6 }
    (LongInt(ARegA)   *    27) + { << 3 }
    (LongInt(ARegB)   *     1);  { << 0 }
end;

function EncodeInstructionImm3(AOpcode: TOpcode; ARegD: TRegister; ARegA: TRegister; AImmediate: TQuarterWord): TWord;
begin
  EncodeInstructionImm3 :=
    (LongInt(AOpcode)    * 19683) + { << 9 }
    (LongInt(ARegD)      *   729) + { << 6 }
    (LongInt(ARegA)      *    27) + { << 3 }
    (LongInt(AImmediate) *     1);  { << 0 }
end;

function EncodeInstructionImm6(AOpcode: TOpcode; ARegD: TRegister; AImmediate: THalfWord): TWord;
begin
  EncodeInstructionImm6 :=
    (LongInt(AOpcode)    * 19683) + { << 9 }
    (LongInt(ARegD)      *   729) + { << 6 }
    (LongInt(AImmediate) *     1);  { << 0 }
end;

var
  GInputFile: TextFile;
  GOutputFile: TextFile;
  GLine: String;
  GLineSplit: TStringArray;
  GOpcode: TOpcode;
  GProgramCounter: TWord;
  GInstruction: TWord;

begin
  if ParamCount() <> 2 then begin
    WriteLn('Usage: Assembler input.s output.t');
    Exit;
  end;

  Assign(GInputFile, ParamStr(1));
  Assign(GOutputFile, ParamStr(2));
  Reset(GInputFile);
  Rewrite(GOutputFile);

  GProgramCounter := 0;

  while not Eof(GInputFile) do begin
    ReadLn(GInputFile, GLine);
    GLineSplit := Split(GLine);

    GOpcode := MnemonicToOpcode(GLineSplit[0]);
    case CInstructions[GOpcode].Format of
      ifRegister:
        GInstruction := EncodeInstructionRgtr(
          GOpcode,
          StrToRegister(GLineSplit[1]),
          StrToRegister(GLineSplit[2]),
          StrToRegister(GLineSplit[3]));
      ifImmediate3:
        GInstruction := EncodeInstructionImm3(
          GOpcode,
          StrToRegister(GLineSplit[1]),
          StrToRegister(GLineSplit[2]),
          StrToInt(GLineSplit[3]));
      ifImmediate6:
        GInstruction := EncodeInstructionImm6(
          GOpcode,
          StrToRegister(GLineSplit[1]),
          StrToInt(GLineSplit[2]));
    end;

    WriteLn(GOutputFile, GProgramCounter, ' ', GInstruction);
    GProgramCounter += 1;
  end;

  Close(GInputFile);
  Close(GOutputFile);
end.
