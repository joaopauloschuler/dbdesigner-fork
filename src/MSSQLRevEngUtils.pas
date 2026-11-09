unit MSSQLRevEngUtils;

//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of DBDesigner Fork which is forked from DBDesigner 4.
//
// DBDesigner Fork is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// DBDesigner Fork is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with DBDesigner Fork; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------------------------------------------------
//
// Unit MSSQLRevEngUtils
// ---------------------
// Description
//   Conversions used by the MS SQL Server reverse engineering that need neither
//   a connection nor a model, so that they can be tested on their own.
//
//----------------------------------------------------------------------------------------------------------------------

{$mode delphi}
{$H+}

interface

uses
  SysUtils;

(** Splits a TYPE_NAME value of sp_columns into its base type and the identity property.
    @param(TypeName [In] the type name, e.g. "int identity" or "numeric() identity")
    @param(BaseName [Out] the type without identity suffix and without trailing "()")
    @param(IsIdentity [Out] True when the type carries the identity property)
    @param(IsBinary [Out] True when the column holds raw bytes (binary, varbinary, timestamp)) *)
procedure MSSQLSplitTypeName(const TypeName: string; out BaseName: string; out IsIdentity, IsBinary: Boolean);

(** Escapes a name for a catalog procedure parameter that is matched as a LIKE pattern.
    @param(Name [In] the literal name)
    @returns(the name with [, % and _ wrapped in brackets) *)
function MSSQLEscapeLikePattern(const Name: string): string;

(** Returns the model datatype for a SQL Server base type without a model type of its name.
    @param(BaseName [In] the base type name from MSSQLSplitTypeName)
    @returns(the model datatype name, or BaseName when the type maps by its own name) *)
function MSSQLBuiltinModelType(const BaseName: string): string;

(** Turns a COLUMN_DEF value into the text stored in the model: outer parentheses go, and
    the quotes of one complete string literal go with its doubled quotes reduced. An empty
    string literal stays as two quotes, because an empty DefaultValue means no default.
    @param(DefVal [In] the COLUMN_DEF value of sp_columns)
    @returns(the default value to store in the model) *)
function MSSQLDefaultValue(DefVal: string): string;

(** Names a referential action of sys.foreign_keys.
    @param(Action [In] 0 = no action, 1 = cascade, 2 = set null, 3 = set default)
    @returns(the action name; unknown values give NO ACTION) *)
function MSSQLRefActionName(Action: integer): string;

(** Converts an UPDATE_RULE or DELETE_RULE value of sp_fkeys to the action numbering of
    sys.foreign_keys. The procedure only tells cascade from everything else.
    @param(Rule [In] the rule value of sp_fkeys)
    @returns(1 for cascade, otherwise 0 for no action) *)
function MSSQLActionFromFKRule(Rule: integer): integer;

implementation

procedure MSSQLSplitTypeName(const TypeName: string; out BaseName: string; out IsIdentity, IsBinary: Boolean);
var IdentityPos: integer;
begin
  BaseName:=Trim(TypeName);

  IdentityPos:=Pos(' identity', LowerCase(BaseName));
  IsIdentity:=(IdentityPos>0);
  if(IsIdentity)then
    BaseName:=Trim(Copy(BaseName, 1, IdentityPos-1));

  //sp_columns writes numeric() and decimal() for the identity forms
  if(Copy(BaseName, Length(BaseName)-1, 2)='()')then
    BaseName:=Copy(BaseName, 1, Length(BaseName)-2);

  IsBinary:=(CompareText(BaseName, 'binary')=0)or(CompareText(BaseName, 'varbinary')=0)or
    (CompareText(BaseName, 'timestamp')=0)or(CompareText(BaseName, 'rowversion')=0);
end;

function MSSQLEscapeLikePattern(const Name: string): string;
var s: string;
begin
  //The bracket goes first, the other two replacements add brackets
  s:=StringReplace(Name, '[', '[[]', [rfReplaceAll]);
  s:=StringReplace(s, '%', '[%]', [rfReplaceAll]);
  s:=StringReplace(s, '_', '[_]', [rfReplaceAll]);
  MSSQLEscapeLikePattern:=s;
end;

function MSSQLBuiltinModelType(const BaseName: string): string;
const
  //SQL Server types without a model datatype of the same name
  TypeMapCount = 20;
  TypeMap: array[0..TypeMapCount-1, 0..1] of string = (
    ('float', 'DOUBLE'), ('real', 'FLOAT'), ('money', 'DECIMAL'), ('smallmoney', 'DECIMAL'),
    ('smalldatetime', 'DATETIME'), ('datetime2', 'DATETIME'), ('datetimeoffset', 'DATETIME'),
    ('int', 'INTEGER'), ('nvarchar', 'VARCHAR'), ('nchar', 'CHAR'), ('sysname', 'VARCHAR'),
    ('ntext', 'TEXT'), ('xml', 'TEXT'), ('image', 'LONGBLOB'), ('binary', 'CHAR'),
    ('varbinary', 'VARCHAR'), ('timestamp', 'CHAR'), ('rowversion', 'CHAR'),
    ('uniqueidentifier', 'CHAR'), ('sql_variant', 'BLOB'));
var i: integer;
begin
  MSSQLBuiltinModelType:=BaseName;
  for i:=0 to TypeMapCount-1 do
    if(CompareText(BaseName, TypeMap[i, 0])=0)then
    begin
      MSSQLBuiltinModelType:=TypeMap[i, 1];
      break;
    end;
end;

(** Tells whether a text is exactly one string literal, with or without an N prefix.
    @param(S [In] the text to examine)
    @param(Inner [Out] the literal's value with doubled quotes reduced; undefined on False)
    @returns(True when S is one complete literal) *)
function IsSingleStringLiteral(const S: string; out Inner: string): Boolean;
var i, first: integer;
begin
  IsSingleStringLiteral:=False;
  Inner:='';

  first:=1;
  if(Length(S)>=2)and(S[1]='N')and(S[2]='''')then
    first:=2;
  if(Length(S)<first+1)or(S[first]<>'''')or(S[Length(S)]<>'''')then
    Exit;

  i:=first+1;
  while(i<=Length(S)-1)do
  begin
    if(S[i]='''')then
    begin
      //Inside a literal a quote is only valid doubled
      if(i+1>Length(S)-1)or(S[i+1]<>'''')then
        Exit;
      inc(i);
    end;
    Inner:=Inner+S[i];
    inc(i);
  end;

  IsSingleStringLiteral:=True;
end;

function MSSQLDefaultValue(DefVal: string): string;
var i, depth: integer;
  enclosed, inQuote: Boolean;
  Inner: string;
begin
  DefVal:=Trim(DefVal);

  //Remove outer parentheses as long as they enclose the whole expression
  while(Length(DefVal)>=2)and(DefVal[1]='(')and(DefVal[Length(DefVal)]=')')do
  begin
    depth:=0;
    enclosed:=True;
    inQuote:=False;
    for i:=1 to Length(DefVal)-1 do
    begin
      if(DefVal[i]='''')then
        inQuote:=Not(inQuote)
      else if(Not(inQuote))and(DefVal[i]='(')then
        inc(depth)
      else if(Not(inQuote))and(DefVal[i]=')')then
        dec(depth);

      if(depth=0)then
      begin
        enclosed:=False;
        break;
      end;
    end;

    if(Not(enclosed))then
      break;

    DefVal:=Trim(Copy(DefVal, 2, Length(DefVal)-2));
  end;

  if(IsSingleStringLiteral(DefVal, Inner))then
  begin
    if(Inner='')then
      DefVal:=''''''
    else
      DefVal:=Inner;
  end;

  MSSQLDefaultValue:=DefVal;
end;

function MSSQLRefActionName(Action: integer): string;
begin
  case Action of
    1: MSSQLRefActionName:='CASCADE';
    2: MSSQLRefActionName:='SET NULL';
    3: MSSQLRefActionName:='SET DEFAULT';
  else
    MSSQLRefActionName:='NO ACTION';
  end;
end;

function MSSQLActionFromFKRule(Rule: integer): integer;
begin
  if(Rule=0)then
    MSSQLActionFromFKRule:=1
  else
    MSSQLActionFromFKRule:=0;
end;

end.
