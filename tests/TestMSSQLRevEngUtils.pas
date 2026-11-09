program TestMSSQLRevEngUtils;

{ Checks the MS SQL Server reverse engineering conversions with literal inputs.
  Needs no connection. Build from the repository root:
    fpc -Mdelphi -Fusrc tests/TestMSSQLRevEngUtils.pas
  Exit code 0 when all checks pass, 1 otherwise. }

{$mode delphi}
{$H+}

uses
  SysUtils, MSSQLRevEngUtils;

var
  Passed, Failed: Integer;

(** Records one comparison of two strings and prints a failure.
    @param(Name [In] label of the check)
    @param(Got [In] the value produced)
    @param(Expected [In] the value wanted) *)
procedure CheckStr(const Name, Got, Expected: string);
begin
  if Got = Expected then
    Inc(Passed)
  else
  begin
    Inc(Failed);
    WriteLn('[FAIL] ', Name, ': got <', Got, '>, expected <', Expected, '>');
  end;
end;

(** Records one comparison of two booleans.
    @param(Name [In] label of the check)
    @param(Got [In] the value produced)
    @param(Expected [In] the value wanted) *)
procedure CheckBool(const Name: string; Got, Expected: Boolean);
begin
  CheckStr(Name, BoolToStr(Got, True), BoolToStr(Expected, True));
end;

(** Checks MSSQLSplitTypeName for one type name.
    @param(TypeName [In] the sp_columns type name)
    @param(ExpectedBase [In] the wanted base name)
    @param(ExpectedIdentity [In] the wanted identity flag)
    @param(ExpectedBinary [In] the wanted binary flag) *)
procedure CheckType(const TypeName, ExpectedBase: string; ExpectedIdentity, ExpectedBinary: Boolean);
var
  BaseName: string;
  IsIdentity, IsBinary: Boolean;
begin
  MSSQLSplitTypeName(TypeName, BaseName, IsIdentity, IsBinary);
  CheckStr('base of ' + TypeName, BaseName, ExpectedBase);
  CheckBool('identity of ' + TypeName, IsIdentity, ExpectedIdentity);
  CheckBool('binary of ' + TypeName, IsBinary, ExpectedBinary);
end;

begin
  Passed := 0;
  Failed := 0;

  CheckType('int', 'int', False, False);
  CheckType('int identity', 'int', True, False);
  CheckType('tinyint identity', 'tinyint', True, False);
  CheckType('smallint identity', 'smallint', True, False);
  CheckType('bigint identity', 'bigint', True, False);
  CheckType('numeric() identity', 'numeric', True, False);
  CheckType('decimal() identity', 'decimal', True, False);
  CheckType(' NUMERIC() IDENTITY ', 'NUMERIC', True, False);
  CheckType('numeric', 'numeric', False, False);
  CheckType('varbinary', 'varbinary', False, True);
  CheckType('timestamp', 'timestamp', False, True);

  CheckStr('map float', MSSQLBuiltinModelType('float'), 'DOUBLE');
  CheckStr('map int', MSSQLBuiltinModelType('INT'), 'INTEGER');
  CheckStr('map numeric keeps own name', MSSQLBuiltinModelType('numeric'), 'numeric');
  CheckStr('map decimal keeps own name', MSSQLBuiltinModelType('decimal'), 'decimal');

  CheckStr('default no value', MSSQLDefaultValue(''), '');
  CheckStr('default number', MSSQLDefaultValue('((0))'), '0');
  CheckStr('default function', MSSQLDefaultValue('(getdate())'), 'getdate()');
  CheckStr('default expression', MSSQLDefaultValue('((1)+(2))'), '(1)+(2)');
  CheckStr('default unbalanced outer', MSSQLDefaultValue('(1)+(2)'), '(1)+(2)');
  CheckStr('default string', MSSQLDefaultValue('(''abc'')'), 'abc');
  CheckStr('default unicode string', MSSQLDefaultValue('(N''abc'')'), 'abc');
  CheckStr('default empty string', MSSQLDefaultValue('('''')'), '''''');
  CheckStr('default empty unicode string', MSSQLDefaultValue('(N'''')'), '''''');
  CheckStr('default concatenation', MSSQLDefaultValue('(''a''+''b'')'), '''a''+''b''');
  CheckStr('default doubled quote', MSSQLDefaultValue('(''it''''s'')'), 'it''s');
  CheckStr('default only a quote', MSSQLDefaultValue('('''''''')'), '''');
  CheckStr('default paren in literal', MSSQLDefaultValue('(''(a)'')'), '(a)');

  CheckStr('action 0', MSSQLRefActionName(0), 'NO ACTION');
  CheckStr('action 1', MSSQLRefActionName(1), 'CASCADE');
  CheckStr('action 2', MSSQLRefActionName(2), 'SET NULL');
  CheckStr('action 3', MSSQLRefActionName(3), 'SET DEFAULT');
  CheckStr('action unknown', MSSQLRefActionName(9), 'NO ACTION');

  CheckStr('fk rule cascade', IntToStr(MSSQLActionFromFKRule(0)), '1');
  CheckStr('fk rule other', IntToStr(MSSQLActionFromFKRule(1)), '0');

  WriteLn('PASS: ', Passed, '  FAIL: ', Failed);
  if Failed > 0 then
    ExitCode := 1;
end.
