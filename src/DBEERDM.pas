unit DBEERDM;

//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of fabFORCE DBDesigner4.
// Copyright (C) 2002 Michael G. Zinner, www.fabFORCE.net
//
// DBDesigner4 is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// DBDesigner4 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with DBDesigner4; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//----------------------------------------------------------------------------------------------------------------------
//
// Unit DBEERDM.pas
// ----------------
// Version 2.1, 03.05.2003, Mike
// Description
//   Contains the reverse engineering and database syncronisation functions
//
// Changes:
//   Version Fork 1.5, 05.4.2010, JP: "NOT Null" is reversed engineered in MySQL 5.
//   Version Fork 1.5, 23.3.2010, JP: Better support for ORACLE reverse engineering using ORACLE connection/functions.
//   Version Fork 1.5, 15.3.2010, JP: Better support for SQLite reverse engineering using ODBC.
//   Version Fork 1.5, 13.3.2010, JP: Better support for FireBird reverse engineering using ODBC.
//   Version 2.1, 03.05.2003, Mike
//     introduced GetidDatatype
//   Version 2.0, 18.04.2003, Mike
//     Changed all Records to TObjects.
//   Version 1.3, 07.04.2003, Mike
//     Fixed bug when rev eng ORCL DB, "schema"."tablename", removed all quotes
//   Version 1.2, 01.04.2003, Mike
//     Fixed bug in ReverseEngineering Functions, IndexParams were
//       not assigned and caused an access violation when saving
//     DBSync: BIGINT(20) = BIGINT
//             Ignore LengthParam when the index is FULLTEXT
//   Version 1.1, 20.03.2003, Mike
//     added support for IndexColumn length parameter
//     added support for PrevTableName and generate SQL RENAME command
//   Version 1.0, 13.03.2003, Mike
//     initial version
//
//----------------------------------------------------------------------------------------------------------------------


{$I DBDesigner4.inc}

interface

uses
  SysUtils, Classes, LCLType, SqlExpr, DB, Forms, StdCtrls, Dialogs,
  EERModel, DBXpress, RegExpr;

type
  CreateTableSyntax = (MySQL, SQLite, Oracle, MSSQL);

  TDMDBEER = class(TDataModule)

    //Do the Reverse Engeneering
    procedure EERReverseEngineer(theModel: Pointer; DBConn: Pointer; theTables: TStringList; XCount: integer; BuildRelations: Boolean; BuildRelUsingPrimKey: Boolean; DatatypeSubst: TStringList; StatusLbl: TLabel = nil; CreateStdInserts: Boolean = False; limitStdIns: integer = 0);
    procedure EERMySQLReverseEngineer(theModel: Pointer; DBConn: Pointer; theTables: TStringList; XCount: integer; BuildRelations: Boolean; BuildRelUsingPrimKey: Boolean; DatatypeSubst: TStringList; StatusLbl: TLabel = nil; CreateStdInserts: Boolean = False; limitStdIns: integer = 0);
    procedure EERORCLReverseEngineer(theModel: Pointer; DBConn: Pointer; theTables: TStringList; XCount: integer; BuildRelations: Boolean; BuildRelUsingPrimKey: Boolean; DatatypeSubst: TStringList; StatusLbl: TLabel = nil; CreateStdInserts: Boolean = False; limitStdIns: integer = 0; ImportSchema: Boolean = False; PutDefaultValuesInQuotes: Boolean = False);
    procedure EERSQLiteReverseEngineer(theModel: Pointer; DBConn: Pointer; theTables: TStringList; XCount: integer; BuildRelations: Boolean; BuildRelUsingPrimKey: Boolean; DatatypeSubst: TStringList; StatusLbl: TLabel = nil; CreateStdInserts: Boolean = False; limitStdIns: integer = 0);
    procedure EERMSSQLReverseEngineer(theModel: Pointer; DBConn: Pointer; theTables: TStringList; XCount: integer; BuildRelations: Boolean; BuildRelUsingPrimKey: Boolean; DatatypeSubst: TStringList; StatusLbl: TLabel = nil; CreateStdInserts: Boolean = False; limitStdIns: integer = 0; CollapseTables: Boolean = False);
    procedure EERMySQLReverseEngineer2(theModel: Pointer; DBConn: Pointer; theTables: TStringList; XCount: integer; BuildRelations: Boolean; BuildRelUsingPrimKey: Boolean; DatatypeSubst: TStringList; StatusLbl: TLabel = nil; CreateStdInserts: Boolean = False; limitStdIns: integer = 0);
    procedure BuildTableFromCreateStatement(theTable: TEERTable; theCreateStatement: string; Syntax: CreateTableSyntax);

    procedure EERReverseEngineerMakeRelations(theModel: Pointer; theTables: TList; BuildRelUsingPrimKey: Boolean; SkipExisting: Boolean = False);
    procedure EERReverseEngineerCreateStdInserts(theModel: Pointer; theTables: TList; limit: integer;
      StatusLbl: TLabel = nil; ImportSchema: Boolean = False);

    procedure EERMySQLSyncDB(theModel: Pointer; DBConn: Pointer; Log: TStrings;
      KeepExTbls, StdInsertsOnCreate, StdInsertsSync: Boolean);
    procedure EERMySQLSyncStdInserts(EERTable: Pointer; var Log: TStrings);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);

    function RemoveCommentsFromSQLCmd(cmd: string): string;
    function GetSQLiteDatatype(theModel: Pointer; DeclType: string; DatatypeSubst: TStringList; var DatatypeParams: string): Pointer;
    function SQLiteRefActionCode(action: string): string;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DMDBEER: TDMDBEER;
  //Number of tables skipped by the last reverse engineering run because a
  //table of that name was already in the model (sqlite-bug-catalog #14).
  //A unit variable: DMDBEER is never instantiated, its methods run on nil.
  RevEngSkippedTables: integer = 0;

implementation

uses MainDM, DBDM, EERDM;

{$R *.lfm}



procedure TDMDBEER.DataModuleCreate(Sender: TObject);
begin
  //
end;

procedure TDMDBEER.DataModuleDestroy(Sender: TObject);
begin
  //
end;

procedure TDMDBEER.EERReverseEngineer(theModel: Pointer; DBConn: Pointer; theTables: TStringList; XCount: integer; BuildRelations: Boolean; BuildRelUsingPrimKey: Boolean; DatatypeSubst: TStringList; StatusLbl: TLabel = nil; CreateStdInserts: Boolean = False; limitStdIns: integer = 0);
var DbTables: TList;
  i, j, xpos, ypos, xanz, defwidth, defheight, iddatatype: integer;
  DatatypeName, prevIndex, tablename, indexname: string;
  EERModel: TEERModel;
  theTable, tmpTbl: TEERTable;
  theColumn: TEERColumn;
  theIndex: TEERIndex;
  tblAtPos: Boolean;
  fldColumnName :TField; // Addition by Vadim
  //s: string;
  index11: string;
  theQuoteChar: String;
begin
  EERModel:=theModel;

  defwidth:=220;
  defheight:=160;
  xanz:=XCount;
  xpos:=0;
  ypos:=0;

  //GetTables
  //Col 0: RECNO
  //1: CATALOG_NAME
  //2: SCHEMA_NAME
  //3: TABLE_NAME
  //4: TABLE_TYPE

  if(StatusLbl<>nil)then
  begin
    StatusLbl.Caption:=DMMain.GetTranslatedMessage('Fetching Tables', 147);
    StatusLbl.Refresh;
    Application.ProcessMessages;
  end;

  DbTables:=TList.Create;
  try
    DMDB.SchemaSQLQuery.SetSchemaInfo(stTables, '', '');
    DMDB.SchemaSQLQuery.Open;
    theQuoteChar:=DMDB.SchemaSQLQuery.GetQuoteChar;

    while(Not(DMDB.SchemaSQLQuery.EOF))do
    begin
      tablename:=DMDB.SchemaSQLQuery.Fields[3].AsString;
      tablename := SysUtils.StringReplace(tablename,'"','',[SysUtils.rfReplaceAll]);
      tablename := SysUtils.StringReplace(tablename,'`','',[SysUtils.rfReplaceAll]);

      //get only selected tables
      if(theTables.IndexOf(tablename)<>-1)then
      begin
        //Place at temporary position and reposition after Columns and
        //indices have been added
        theTable:=EERModel.NewTable(EERModel.EERModel_Width-250, 0, False);


        if(Pos(theQuoteChar, tablename)>0)then
          theTable.ObjName:=DMMain.ReplaceText(tablename, theQuoteChar, '')
        else
          theTable.ObjName:=tablename;
        //theTable.RefreshObj;

        DbTables.Add(theTable);
      end;

      DMDB.SchemaSQLQuery.Next;
    end;
    DMDB.SchemaSQLQuery.Close;

    //Get the columns
    for i:=0 to DbTables.Count-1 do
    begin
      if(StatusLbl<>nil)then
      begin
        StatusLbl.Caption:=DMMain.GetTranslatedMessage('Fetching Table Columns/Indices (%s)', 148,
          TEERTable(DbTables[i]).ObjName);
        StatusLbl.Refresh;
        Application.ProcessMessages;
      end;

      //Get Columns
      //Col 0: RECNO
      //1: CATALOG_NAME
      //2: SCHEMA_NAME
      //3: TABLE_NAME
      //4: COLUMN_NAME
      //5: COLUMN_POSITION
      //6: COLUMN_TYPE
      //7: COLUMN_DATATYPE
      //8: COLUMN_TYPENAME
      //9: COLUMN_SUBTYPE
      //10: COLUMN_LENGTH
      //11: COLUMN_PRECISION
      //12: COLUMN_SCALE
      //13: COLUMN_NULLABLE

      DMDB.SchemaSQLQuery.SetSchemaInfo(stColumns, TEERTable(DbTables[i]).ObjName, '');

      try
        DMDB.SchemaSQLQuery.Open;
      except
        if DMDB.SchemaSQLQuery.RecordCount=0 then
        begin
          raise;
        end;
      end;

      while(Not(DMDB.SchemaSQLQuery.EOF))do
      begin
        //new(theColumn);
        theColumn:=TEERColumn.Create(TEERTable(DbTables[i]));
        TEERTable(DbTables[i]).Columns.Add(theColumn);

        DatatypeName:=DMDB.SchemaSQLQuery.Fields[8].AsString;

        // JP: MySQL ODBC returns varcha
        if(CompareText(DatatypeName, 'varcha')=0) then DatatypeName := 'varchar';

        theColumn.DatatypeParams:='';
        if(CompareText(DatatypeName, 'varchar')=0) then
        begin
          //The openodbc Driver returns Columns Params at Pos 10
          if(DMDB.CurrentDBConn.DriverName='openodbc')then
            theColumn.DatatypeParams:='('+DMDB.SchemaSQLQuery.Fields[10].AsString+')'
          else
            theColumn.DatatypeParams:='('+DMDB.SchemaSQLQuery.Fields[11].AsString+')';
        end;

        // is a varchar that contains length (like SQLite result set)
        if (CompareText(Copy(DatatypeName, 1, 8), 'VARCHAR(')=0) then
        begin
          theColumn.DatatypeParams := Copy(DatatypeName, 8, length(DatatypeName)-7);
          DatatypeName := 'varchar';
          try
            theColumn.Width := strtoint( Copy(theColumn.DatatypeParams,2,length(theColumn.DatatypeParams)-2) );
          except
            //nothing can be done
          end;
        end else
        begin
          theColumn.Width:=DMDB.SchemaSQLQuery.Fields[10].AsInteger;
        end;

        theColumn.ColName:=DMDB.SchemaSQLQuery.Fields[4].AsString;
        theColumn.Obj_id:=DMMain.GetNextGlobalID;
        theColumn.Pos:=TEERTable(DbTables[i]).Columns.Count;
        theColumn.idDatatype:=TEERDatatype(EERModel.GetDataTypeByNameSubst(DatatypeName, DatatypeSubst)).id;

        theColumn.Prec:=DMDB.SchemaSQLQuery.Fields[11].AsInteger;
        theColumn.PrimaryKey:=False;
        theColumn.NotNull:=(DMDB.SchemaSQLQuery.Fields[13].AsString='1');
        theColumn.AutoInc:=False;
        theColumn.IsForeignKey:=False;

        {if(CompareText(SchemaSQLQuery.Fields[8].AsString,
          'float')=0)then
          theColumn.DatatypeParams:='('+SchemaSQLQuery.Fields[12].AsString+', '+SchemaSQLQuery.Fields[11].AsString+')';}

        DMDB.SchemaSQLQuery.Next;
      end;
      DMDB.SchemaSQLQuery.Close;

      TEERTable(DbTables[i]).RefreshObj;

      //Get Indices
      //Col 0: RECNO
      //1: CATALOG_NAME
      //2: SCHEMA_NAME
      //3: TABLE_NAME
      //4: INDEX_NAME
      //5: COLUMN_NAME
      //6: COLUMN_POSITION
      //7: PKEY_NAME
      //8: INDEX_TYPE
      //9: SORT_ORDER
      //10: FILTER

      prevIndex:='';

      DMDB.SchemaSQLQuery.SetSchemaInfo(stIndexes, TEERTable(DbTables[i]).ObjName, '');
      DMDB.SchemaSQLQuery.Open;
      DMDB.SchemaSQLQuery.First;

      {s:='';
      for j:=0 to DMMain.SchemaSQLQuery.FieldCount-1 do
        s:=s+IntToStr(j)+': '+DMMain.SchemaSQLQuery.Fields[j].DisplayName+#13#10;
      ShowMessage(s);}

      theIndex:=nil;

      // Addition by Vadim
      fldColumnName := DMDB.SchemaSQLQuery.FindField('COLUMN_NAME');
      if Assigned(fldColumnName) then
        while(Not(DMDB.SchemaSQLQuery.EOF))do
        begin
          indexname:=DMDB.SchemaSQLQuery.Fields[4].AsString;
          index11 := Copy(indexname, 1, 11);

          if
            (CompareText(index11, 'RDB$PRIMARY')=0) or        // JP: is FireBird PK?
            (CompareText(index11, 'sqlite_auto')=0) or        // JP: is SQLite PK?
            (CompareText(Copy(indexname, 1, 7), 'PRIMARY')=0) // JP: is PK?
          then
          begin
            indexname := 'PRIMARY';
          end;

          //Don't add the same Index a second time
          if(prevIndex<>indexname)then
          begin
            //new(theIndex);
            theIndex:=TEERIndex.Create(TEERTable(DbTables[i]));
            theIndex.Obj_id:=DMMain.GetNextGlobalID;
            theIndex.IndexName:=indexname;
            if
              (CompareText(Copy(indexname, 1, 7), 'PRIMARY')=0)
            then
            begin
              theIndex.IndexKind:=ik_PRIMARY;
              theColumn := TEERTable(DbTables[i]).GetColumnByName(fldColumnName.AsString);
              // JP: only add an index that has columns
              if Assigned(theColumn) then
              begin
                TEERTable(DbTables[i]).Indices.Add(theIndex);
                theIndex.Pos:=TEERTable(DbTables[i]).Indices.Count-1;
                prevIndex:=indexname;
              end;
            end
            else
            // JP: is FireBird FK?
            if (CompareText(index11, 'RDB$FOREIGN')=0) then
            begin
              // JP: nothing can be done here
            end else
            begin
              theIndex.IndexKind:=ik_INDEX;
              theColumn := TEERTable(DbTables[i]).GetColumnByName(fldColumnName.AsString);
              // JP: only add an index that has columns
              if Assigned(theColumn) then
              begin
                TEERTable(DbTables[i]).Indices.Add(theIndex);
                theIndex.Pos:=TEERTable(DbTables[i]).Indices.Count-1;
                prevIndex:=indexname;
              end;
            end;

          end;

          {//The openodbc Driver returns Column Name at Pos 6
          if(DMDB.CurrentDBConn.DriverName='openodbc')then
            theColumn:=TEERTable(DbTables[i]).GetColumnByName(DMDB.SchemaSQLQuery.Fields[6].AsString)
          else
            theColumn:=TEERTable(DbTables[i]).GetColumnByName(DMDB.SchemaSQLQuery.Fields[5].AsString);}

          // Addition by Vadim
          theColumn := TEERTable(DbTables[i]).GetColumnByName(fldColumnName.AsString);

          // JP: only add a column that exists
          if Assigned(theColumn) and Assigned(theIndex) then
          begin
            if
              (CompareText(Copy(indexname, 1, 7), 'PRIMARY')=0) then
              theColumn.PrimaryKey:=True;

            // JP: does not make sense add an index for FK in Firebird
            if
              (CompareText(index11, 'RDB$FOREIGN')<>0) then
              theIndex.Columns.Add(IntToStr(theColumn.Obj_id));
          end;

          DMDB.SchemaSQLQuery.Next;
        end;
        DMDB.SchemaSQLQuery.Close;

        TEERTable(DbTables[i]).RefreshObj;
      end;

    //Order table positions
    for j:=0 to EERModel.ComponentCount-1 do
    begin
      if(EERModel.Components[j].ClassNameIs('TEERTable'))then
      begin
        theTable:=TEERTable(EERModel.Components[j]);

        theTable.Obj_X:=80+xpos*defwidth;
        theTable.Obj_Y:=40+ypos*defheight;

        //Remove quotations
        theTable.ObjName:=DMMain.ReplaceText(theTable.ObjName, theQuoteChar, '');
        {theTable.ObjName:=DMMain.ReplaceText(theTable.ObjName, '"', '');
        theTable.ObjName:=DMMain.ReplaceText(theTable.ObjName, '''', '');
        theTable.ObjName:=DMMain.ReplaceText(theTable.ObjName, '`', '');
        theTable.ObjName:=DMMain.ReplaceText(theTable.ObjName, '´', '');}

        //Simply remove table prefix, scott.test
        if(Pos('.', theTable.ObjName)>0)then
          theTable.ObjName:=Copy(theTable.ObjName, Pos('.', theTable.ObjName)+1, Length(theTable.ObjName));

        theTable.RefreshObj;

        //Check, if there is already a table at this position
        tblAtPos:=True;
        while(tblAtPos)do
        begin
          tblAtPos:=False;
          for i:=0 to EERModel.ComponentCount-1 do
          begin
            if(EERModel.Components[i].ClassNameIs('TEERTable'))and
              (EERModel.Components[i]<>EERModel.Components[j])then
            begin
              tmpTbl:=TEERTable(EERModel.Components[i]);

              if((theTable.Obj_X>=tmpTbl.Obj_X)and
                (theTable.Obj_X<=tmpTbl.Obj_X+tmpTbl.Obj_W))and
                ((theTable.Obj_Y>=tmpTbl.Obj_Y)and
                (theTable.Obj_Y<=tmpTbl.Obj_Y+tmpTbl.Obj_H))then
              begin
                inc(xpos);
                if(xpos>=xanz)then
                begin
                  xpos:=0;
                  inc(ypos);
                end;

                //get next free pos
                theTable.Obj_X:=80+xpos*defwidth;
                theTable.Obj_Y:=40+ypos*defheight;

                theTable.RefreshObj;

                tblAtPos:=True;
                break;
              end;
            end;
          end;
        end;
      end;
    end;

    if(StatusLbl<>nil)then
    begin
      StatusLbl.Caption:=DMMain.GetTranslatedMessage('Building Relations...', 149);
      StatusLbl.Refresh;
      Application.ProcessMessages;
    end;
    if(BuildRelations)then
      EERReverseEngineerMakeRelations(EERModel, DbTables, BuildRelUsingPrimKey);

    if(StatusLbl<>nil)then
    begin
      StatusLbl.Caption:=DMMain.GetTranslatedMessage('Creating Standard Inserts...', 150);
      StatusLbl.Refresh;
      Application.ProcessMessages;
    end;
    if(CreateStdInserts)then
      EERReverseEngineerCreateStdInserts(EERModel, DbTables, limitStdIns);

    StatusLbl.Caption:=DMMain.GetTranslatedMessage('Finished.', 151);
  finally
    DbTables.Free;
  end;
end;

procedure TDMDBEER.EERMySQLReverseEngineer(theModel: Pointer; DBConn: Pointer; theTables: TStringList; XCount: integer; BuildRelations: Boolean; BuildRelUsingPrimKey: Boolean; DatatypeSubst: TStringList; StatusLbl: TLabel = nil; CreateStdInserts: Boolean = False; limitStdIns: integer = 0);
var EERModel: TEERModel;
  i, j, xpos, ypos, xanz, defwidth, defheight: integer;
  DbTables: TList;
  theTable, tmpTbl: TEERTable;
  theColumn: TEERColumn;
  theIndex: TEERIndex;
  theDatatype: TEERDatatype;
  DatatypeName, DatatypeParams, prevIndex: string;
  tblAtPos: Boolean;
  parentTbl: TEERTable;
  theRel: TEERRel;
  fkName, fkColName, pkColName: string;
  AllFKColsArePK: Boolean;
begin
  EERModel:=theModel;

  defwidth:=250;
  defheight:=160;
  xanz:=XCount;
  xpos:=0;
  ypos:=0;

  if(StatusLbl<>nil)then
  begin
    StatusLbl.Caption:=DMMain.GetTranslatedMessage('Fetching Tables', 147);
    StatusLbl.Refresh;
    Application.ProcessMessages;
  end;

  RevEngSkippedTables:=0;

  //Get Tables
  DbTables:=TList.Create;
  try
    DMDB.SchemaSQLQuery.SetSchemaInfo(stNoSchema, '', '');
    DMDB.SchemaSQLQuery.SQL.Text:='show tables';
    DMDB.SchemaSQLQuery.Open;

    while(Not(DMDB.SchemaSQLQuery.EOF))do
    begin
      //get only selected tables
      if(theTables.IndexOf(DMDB.SchemaSQLQuery.Fields[0].AsString)<>-1)then
      begin
        //Skip tables that are already in the model (sqlite-bug-catalog #14)
        if(EERModel.GetEERObjectByName(EERTable, DMDB.SchemaSQLQuery.Fields[0].AsString)<>nil)then
          inc(RevEngSkippedTables)
        else
        begin
          theTable:=EERModel.NewTable(EERModel.EERModel_Width-250, 0, False);
          theTable.ObjName:=DMDB.SchemaSQLQuery.Fields[0].AsString;

          //theTable.RefreshObj;

          DbTables.Add(theTable);
        end;
      end;

      DMDB.SchemaSQLQuery.Next;
    end;
    DMDB.SchemaSQLQuery.Close;

    //Get the columns
    for i:=0 to DbTables.Count-1 do
    begin
      if(StatusLbl<>nil)then
      begin
        StatusLbl.Caption:=DMMain.GetTranslatedMessage('Fetching Table Columns/Indices (%s)', 148,
          TEERTable(DbTables[i]).ObjName);
        StatusLbl.Refresh;
        Application.ProcessMessages;
      end;

      //SHOW FIELDS: Field, Type, Null, Key, Default, Extra (MySQL 4 .. 8).
      //Read by name: NULL-typed columns can vanish from the field list
      //(mysql-bug-catalog #2), which would shift positional reads.
      DMDB.SchemaSQLQuery.SQL.Text:='show fields from '+TEERTable(DbTables[i]).GetSQLTableName;
      DMDB.SchemaSQLQuery.Open;
      while(Not(DMDB.SchemaSQLQuery.EOF))do
      begin
        //new(theColumn);
        theColumn:=TEERColumn.Create(TEERTable(DbTables[i]));
        TEERTable(DbTables[i]).Columns.Add(theColumn);

        theColumn.ColName:=DMDB.SchemaSQLQuery.FieldByName('Field').AsString;
        theColumn.Obj_id:=DMMain.GetNextGlobalID;
        theColumn.Pos:=TEERTable(DbTables[i]).Columns.Count;
        //theColumn.idDatatype:=-1;
        theColumn.DatatypeParams:='';
        {theColumn.Width:=SchemaSQLQuery.Fields[10].AsInteger;
        theColumn.Prec:=SchemaSQLQuery.Fields[11].AsInteger;}
        theColumn.PrimaryKey:=(DMDB.SchemaSQLQuery.FieldByName('Key').AsString='PRI');
        theColumn.NotNull:=
          (DMDB.SchemaSQLQuery.FieldByName('Null').AsString<>'Y') and
          (DMDB.SchemaSQLQuery.FieldByName('Null').AsString<>'YES');
        //Extra = 'auto_increment' (mysql-bug-catalog #4)
        theColumn.AutoInc:=(Pos('auto_increment',
          LowerCase(DMDB.SchemaSQLQuery.FieldByName('Extra').AsString))>0);
        theColumn.IsForeignKey:=False;
        if(DMDB.SchemaSQLQuery.FieldByName('Default').AsString<>'0')then
          theColumn.DefaultValue:=DMDB.SchemaSQLQuery.FieldByName('Default').AsString;


        //Get Datatype
        DatatypeName:=DMDB.SchemaSQLQuery.FieldByName('Type').AsString;

        DatatypeParams:='';
        if(Pos('(', datatypename)>0)then
        begin
          DatatypeName:=Copy(DatatypeName, 1, Pos('(', DatatypeName)-1);
          DatatypeParams:=Copy(DMDB.SchemaSQLQuery.FieldByName('Type').AsString,
            Pos('(', DMDB.SchemaSQLQuery.FieldByName('Type').AsString),
            Pos(')', DMDB.SchemaSQLQuery.FieldByName('Type').AsString)-Pos('(', DMDB.SchemaSQLQuery.FieldByName('Type').AsString)+1);
        end;
        //int unsigned -> just get int
        if(Pos(' ', datatypename)>0)then
          DatatypeName:=Copy(DatatypeName, 1, Pos(' ', DatatypeName)-1);

        theDatatype:=EERModel.GetDataTypeByNameSubst(DatatypeName, DatatypeSubst);
        theColumn.idDatatype:=theDatatype.id;

        //Get Options
        if(Assigned(theDatatype))then
          for j:=0 to theDatatype.OptionCount-1 do
            if(Pos(UpperCase(theDatatype.Options[j]),
              UpperCase(DMDB.SchemaSQLQuery.FieldByName('Type').AsString))>0)then
              theColumn.OptionSelected[j]:=True
            else
              theColumn.OptionSelected[j]:=False;

        theColumn.DatatypeParams:=DatatypeParams;

        DMDB.SchemaSQLQuery.Next;
      end;
      DMDB.SchemaSQLQuery.Close;

      TEERTable(DbTables[i]).RefreshObj;


      //Indices
      //SHOW KEYS: Table, Non_unique, Key_name, Seq_in_index, Column_name,
      //Collation, Cardinality, Sub_part, Packed, Null, Index_type, Comment
      //(MySQL 4); MySQL 8 adds Index_comment, Visible, Expression and its
      //Packed is NULL-typed, so it is missing from the field list (#2):
      //read by name. One row per index column, ordered by index and Seq.

      prevIndex:='';

      DMDB.SchemaSQLQuery.SQL.Text:='show keys from '+TEERTable(DbTables[i]).GetSQLTableName;
      DMDB.SchemaSQLQuery.Open;

      theIndex:=nil;

      while(Not(DMDB.SchemaSQLQuery.EOF))do
      begin
        //Don't add the same Index a second time
        if(prevIndex<>DMDB.SchemaSQLQuery.FieldByName('Key_name').AsString)then
        begin
          //new(theIndex);
          theIndex:=TEERIndex.Create(TEERTable(DbTables[i]));
          theIndex.Obj_id:=DMMain.GetNextGlobalID;
          theIndex.IndexName:=DMDB.SchemaSQLQuery.FieldByName('Key_name').AsString;
          if(CompareText(theIndex.IndexName, 'PRIMARY')=0)then
            theIndex.IndexKind:=ik_PRIMARY
          //Non_unique = 0: UNIQUE index (mysql-bug-catalog #5)
          else if(DMDB.SchemaSQLQuery.FieldByName('Non_unique').AsString='0')then
            theIndex.IndexKind:=ik_UNIQUE_INDEX
          else
            theIndex.IndexKind:=ik_INDEX;
          TEERTable(DbTables[i]).Indices.Add(theIndex);
          theIndex.Pos:=TEERTable(DbTables[i]).Indices.Count-1;

        end;

        //Functional key parts (MySQL 8) have no column name
        theColumn:=nil;
        if(Not(DMDB.SchemaSQLQuery.FieldByName('Column_name').IsNull))then
          theColumn:=TEERColumn(TEERTable(DbTables[i]).GetColumnByName(
            DMDB.SchemaSQLQuery.FieldByName('Column_name').AsString));
        if(theColumn<>nil)then
        begin
          theIndex.Columns.Add(IntToStr(theColumn.Obj_id));
          //Sub_part: index prefix length, e.g. info(100) (#5)
          if(Not(DMDB.SchemaSQLQuery.FieldByName('Sub_part').IsNull))and
            (DMDB.SchemaSQLQuery.FieldByName('Sub_part').AsString<>'')then
            theIndex.ColumnParams.Values[IntToStr(theColumn.Obj_id)]:=
              DMDB.SchemaSQLQuery.FieldByName('Sub_part').AsString;
        end;

        prevIndex:=DMDB.SchemaSQLQuery.FieldByName('Key_name').AsString;
        DMDB.SchemaSQLQuery.Next;
      end;
      DMDB.SchemaSQLQuery.Close;

      TEERTable(DbTables[i]).RefreshObj;
    end;

    //Order table positions
    for j:=0 to EERModel.ComponentCount-1 do
    begin
      if(EERModel.Components[j].ClassNameIs('TEERTable'))then
      begin
        theTable:=TEERTable(EERModel.Components[j]);

        theTable.Obj_X:=80+xpos*defwidth;
        theTable.Obj_Y:=40+ypos*defheight;

        theTable.RefreshObj;

        //Check, if there is already a table at this position
        tblAtPos:=True;
        while(tblAtPos)do
        begin
          tblAtPos:=False;
          for i:=0 to EERModel.ComponentCount-1 do
          begin
            if(EERModel.Components[i].ClassNameIs('TEERTable'))and
              (EERModel.Components[i]<>EERModel.Components[j])then
            begin
              tmpTbl:=TEERTable(EERModel.Components[i]);

              if((theTable.Obj_X>=tmpTbl.Obj_X)and
                (theTable.Obj_X<=tmpTbl.Obj_X+tmpTbl.Obj_W))and
                ((theTable.Obj_Y>=tmpTbl.Obj_Y)and
                (theTable.Obj_Y<=tmpTbl.Obj_Y+tmpTbl.Obj_H))then
              begin
                inc(xpos);
                if(xpos>=xanz)then
                begin
                  xpos:=0;
                  inc(ypos);
                end;

                //get next free pos
                theTable.Obj_X:=80+xpos*defwidth;
                theTable.Obj_Y:=40+ypos*defheight;

                theTable.RefreshObj;

                tblAtPos:=True;
                break;
              end;
            end;
          end;
        end;
      end;
    end;

    if(StatusLbl<>nil)then
    begin
      StatusLbl.Caption:=DMMain.GetTranslatedMessage('Building Relations...', 149);
      StatusLbl.Refresh;
      Application.ProcessMessages;
    end;
    if(BuildRelations)then
    begin
      //1. Native relations from the FOREIGN KEY constraints
      //   (mysql-bug-catalog #7): information_schema.KEY_COLUMN_USAGE gives
      //   the column pairs, REFERENTIAL_CONSTRAINTS the ON UPDATE/DELETE
      //   rules. One row per column pair, grouped by table + constraint.
      //   MySQL < 5.0 has no information_schema: the query fails and only
      //   the name/PK heuristic below runs (as before).
      try
        DMDB.SchemaSQLQuery.SQL.Text:=
          'SELECT k.TABLE_NAME AS tblname, k.CONSTRAINT_NAME AS fkname, '+
          'k.ORDINAL_POSITION AS fkseq, k.COLUMN_NAME AS fkcol, '+
          'k.REFERENCED_TABLE_NAME AS reftable, k.REFERENCED_COLUMN_NAME AS refcol, '+
          'r.UPDATE_RULE AS on_update, r.DELETE_RULE AS on_delete '+
          'FROM information_schema.KEY_COLUMN_USAGE k '+
          'JOIN information_schema.REFERENTIAL_CONSTRAINTS r '+
          'ON r.CONSTRAINT_SCHEMA=k.CONSTRAINT_SCHEMA '+
          'AND r.CONSTRAINT_NAME=k.CONSTRAINT_NAME AND r.TABLE_NAME=k.TABLE_NAME '+
          'WHERE k.TABLE_SCHEMA=DATABASE() AND k.REFERENCED_TABLE_NAME IS NOT NULL '+
          'ORDER BY k.TABLE_NAME, k.CONSTRAINT_NAME, k.ORDINAL_POSITION';
        DMDB.SchemaSQLQuery.Open;
      except
        DMDB.SchemaSQLQuery.Close;
      end;
      if(DMDB.SchemaSQLQuery.Active)then
      begin
        while(Not(DMDB.SchemaSQLQuery.EOF))do
        begin
          fkName:=DMDB.SchemaSQLQuery.FieldByName('tblname').AsString+'.'+
            DMDB.SchemaSQLQuery.FieldByName('fkname').AsString;

          //Child = the table holding the FK (one of the new tables),
          //parent = the referenced table (anywhere in the model)
          theTable:=nil;
          for i:=0 to DbTables.Count-1 do
            if(CompareText(TEERTable(DbTables[i]).ObjName,
              DMDB.SchemaSQLQuery.FieldByName('tblname').AsString)=0)then
            begin
              theTable:=TEERTable(DbTables[i]);
              break;
            end;
          parentTbl:=TEERTable(EERModel.GetEERObjectByName(EERTable,
            DMDB.SchemaSQLQuery.FieldByName('reftable').AsString));
          if(theTable=nil)or(parentTbl=nil)then
          begin
            while(Not(DMDB.SchemaSQLQuery.EOF))and
              (DMDB.SchemaSQLQuery.FieldByName('tblname').AsString+'.'+
              DMDB.SchemaSQLQuery.FieldByName('fkname').AsString=fkName)do
              DMDB.SchemaSQLQuery.Next;
            continue;
          end;

          //Self references (forumpost.idforumpost_parent -> forumpost) are
          //ordinary relations for the model
          theRel:=TEERRel(EERModel.NewRelation(rk_1nNonId, parentTbl, theTable, False));
          theRel.FKFields.Clear;
          theRel.FKFieldsComments.Clear;
          theRel.CreateRefDef:=True;
          //MySQL uses the same rule names as SQLite
          theRel.RefDef.Values['OnDelete']:=
            SQLiteRefActionCode(DMDB.SchemaSQLQuery.FieldByName('on_delete').AsString);
          theRel.RefDef.Values['OnUpdate']:=
            SQLiteRefActionCode(DMDB.SchemaSQLQuery.FieldByName('on_update').AsString);

          //Build PK - FK Mapping
          AllFKColsArePK:=True;
          while(Not(DMDB.SchemaSQLQuery.EOF))and
            (DMDB.SchemaSQLQuery.FieldByName('tblname').AsString+'.'+
            DMDB.SchemaSQLQuery.FieldByName('fkname').AsString=fkName)do
          begin
            fkColName:=DMDB.SchemaSQLQuery.FieldByName('fkcol').AsString;
            pkColName:=DMDB.SchemaSQLQuery.FieldByName('refcol').AsString;

            theRel.FKFields.Add(pkColName+'='+fkColName);
            theRel.FKFieldsComments.Add('');

            theColumn:=TEERColumn(theTable.GetColumnByName(fkColName));
            if(theColumn<>nil)then
            begin
              theColumn.IsForeignKey:=True;
              if(Not(theColumn.PrimaryKey))then
                AllFKColsArePK:=False;
            end
            else
              AllFKColsArePK:=False;

            DMDB.SchemaSQLQuery.Next;
          end;

          //FK columns that are all part of the child's PK: identifying relation
          if(AllFKColsArePK)then
            theRel.RelKind:=rk_1n;

          theRel.SrcTbl.RefreshRelations;
          theRel.DestTbl.RefreshRelations;
        end;
        DMDB.SchemaSQLQuery.Close;
      end;

      //2. Guess the remaining ones by name / primary key like the other
      //   drivers do, without duplicating the native ones
      EERReverseEngineerMakeRelations(EERModel, DbTables, BuildRelUsingPrimKey, True);
    end;

    if(StatusLbl<>nil)then
    begin
      StatusLbl.Caption:=DMMain.GetTranslatedMessage('Creating Standard Inserts...', 150);
      StatusLbl.Refresh;
      Application.ProcessMessages;
    end;
    if(CreateStdInserts)then
      EERReverseEngineerCreateStdInserts(EERModel, DbTables, limitStdIns);

    StatusLbl.Caption:=DMMain.GetTranslatedMessage('Finished.', 151);
    if(RevEngSkippedTables>0)then
      StatusLbl.Caption:=StatusLbl.Caption+' '+IntToStr(RevEngSkippedTables)+' existing table(s) skipped.';
  finally
    DbTables.Free;
  end;
end;

procedure TDMDBEER.EERORCLReverseEngineer(theModel: Pointer; DBConn: Pointer;
  theTables: TStringList; XCount: integer; BuildRelations: Boolean;
  BuildRelUsingPrimKey: Boolean; DatatypeSubst: TStringList;
  StatusLbl: TLabel = nil; CreateStdInserts: Boolean = False;
  limitStdIns: integer = 0; ImportSchema: Boolean = False;
  PutDefaultValuesInQuotes: Boolean = False);
var EERModel: TEERModel;
  i, j, xpos, ypos, xanz, defwidth, defheight: integer;
  DbTables: TList;
  theTable, tmpTbl: TEERTable;
  theColumn: TEERColumn;
  theIndex: TEERIndex;
  theDatatype: TEERDatatype;
  theRel: TEERRel;
  DatatypeName, DatatypeParams, prevIndex: string;
  tblAtPos: Boolean;
  NewRelCounter: integer;
  fkname: string;
  theQuoteChar: string;
begin
  EERModel:=theModel;

  defwidth:=250;
  defheight:=160;
  xanz:=XCount;
  xpos:=0;
  ypos:=0;

  if(StatusLbl<>nil)then
  begin
    StatusLbl.Caption:=DMMain.GetTranslatedMessage('Fetching Tables', 147);
    StatusLbl.Refresh;
    Application.ProcessMessages;
  end;

  //Get Tables
  DbTables:=TList.Create;
  try
    DMDB.SchemaSQLQuery.SetSchemaInfo(stNoSchema, '', '');
    DMDB.SchemaSQLQuery.SQL.Text:='SELECT OWNER, TABLE_NAME FROM ALL_TABLES '+
      'ORDER BY OWNER, TABLE_NAME';
    DMDB.SchemaSQLQuery.Open;

    theQuoteChar:=DMDB.SchemaSQLQuery.GetQuoteChar;

    while(Not(DMDB.SchemaSQLQuery.EOF))do
    begin
      //get only selected tables
      if(theTables.IndexOf(DMDB.SchemaSQLQuery.FieldByName('OWNER').AsString+
        '.'+DMDB.SchemaSQLQuery.FieldByName('TABLE_NAME').AsString)<>-1)or
        (theTables.IndexOf(theQuoteChar+DMDB.SchemaSQLQuery.FieldByName('OWNER').AsString+theQuoteChar+
        '.'+theQuoteChar+DMDB.SchemaSQLQuery.FieldByName('TABLE_NAME').AsString+theQuoteChar)<>-1)then
      begin
        //Create Table Prefix
        i:=TEERModel(theModel).TablePrefix.IndexOf(DMDB.SchemaSQLQuery.FieldByName('OWNER').AsString);
        if(i=-1)then
          i:=TEERModel(theModel).TablePrefix.Add(DMDB.SchemaSQLQuery.FieldByName('OWNER').AsString);

        theTable:=EERModel.NewTable(EERModel.EERModel_Width-250, 0, False);
        theTable.ObjName:=DMDB.SchemaSQLQuery.FieldByName('TABLE_NAME').AsString;
        theTable.TablePrefix:=i;

        //theTable.RefreshObj;

        DbTables.Add(theTable);
      end;

      DMDB.SchemaSQLQuery.Next;
    end;
    DMDB.SchemaSQLQuery.Close;

    //Get the columns
    for i:=0 to DbTables.Count-1 do
    begin
      if(StatusLbl<>nil)then
      begin
        StatusLbl.Caption:=DMMain.GetTranslatedMessage('Fetching Table Columns/Indices (%s)', 148,
          TEERTable(DbTables[i]).ObjName);
        StatusLbl.Refresh;
        Application.ProcessMessages;
      end;

      DMDB.SchemaSQLQuery.SQL.Text:='SELECT DATA_TYPE, COLUMN_NAME, '+
        ' DATA_LENGTH, DATA_PRECISION, NULLABLE, DATA_DEFAULT '+
        'FROM ALL_TAB_COLUMNS '+
        'WHERE OWNER='''+TEERTable(DbTables[i]).GetTablePrefix+''' AND '+
        ' TABLE_NAME='''+TEERTable(DbTables[i]).ObjName+'''';
      DMDB.SchemaSQLQuery.Open;
      while(Not(DMDB.SchemaSQLQuery.EOF))do
      begin
        //Get Datatype
        DatatypeName:=DMDB.SchemaSQLQuery.FieldByName('DATA_TYPE').AsString;
        if(CompareText(DatatypeName, 'VARCHAR2')=0) then DatatypeName := 'VARCHAR';
        theDatatype:=EERModel.GetDataTypeByNameSubst(DatatypeName, DatatypeSubst);
        DatatypeName:=theDatatype.GetPhysicalTypeName;
        DatatypeParams:='';

        //Take special care of DATETIME datatype with SYSDATE as DEFAULT
        if(CompareText(DMDB.SchemaSQLQuery.FieldByName('DATA_TYPE').AsString,
          'DATE')=0)and(Pos('SYSDATE',
          UpperCase(DMDB.SchemaSQLQuery.FieldByName('DATA_DEFAULT').AsString))>0)then
        begin
          theDatatype:=EERModel.GetDataTypeByName('TIMESTAMP');
          DatatypeName:=theDatatype.GetPhysicalTypeName;
        end;

        //Create Column
        theColumn:=TEERColumn.Create(TEERTable(DbTables[i]));
        TEERTable(DbTables[i]).Columns.Add(theColumn);

        theColumn.ColName:=DMDB.SchemaSQLQuery.FieldByName('COLUMN_NAME').AsString;
        theColumn.Obj_id:=DMMain.GetNextGlobalID;
        theColumn.Pos:=TEERTable(DbTables[i]).Columns.Count;
        theColumn.idDatatype:=theDatatype.id;
        theColumn.DatatypeParams:='';
        theColumn.Width:=DMDB.SchemaSQLQuery.FieldByName('DATA_LENGTH').AsInteger;
        theColumn.Prec:=DMDB.SchemaSQLQuery.FieldByName('DATA_PRECISION').AsInteger;
        theColumn.PrimaryKey:=False; //(DMDB.SchemaSQLQuery.Fields[3].AsString='PRI');
        theColumn.NotNull:=(DMDB.SchemaSQLQuery.FieldByName('NULLABLE').AsString='1');
        theColumn.AutoInc:=False;
        theColumn.IsForeignKey:=False;
        if(DMDB.SchemaSQLQuery.FieldByName('DATA_DEFAULT').AsString<>'')then
        begin
          if(CompareText(DMDB.SchemaSQLQuery.FieldByName('DATA_DEFAULT').AsString,
            'NULL')=0)then
          begin
            theColumn.DefaultValue:=DMDB.SchemaSQLQuery.FieldByName('DATA_DEFAULT').AsString
          end
          else if(CompareText('TIMESTAMP', DatatypeName)=0)then
          begin
            theColumn.DefaultValue:='';
          end
          else
          begin
            if(PutDefaultValuesInQuotes)then
            begin
              theColumn.DefaultValue:=''''+
                DMMain.ReplaceText(
                  DMMain.ReplaceText(
                    DMDB.SchemaSQLQuery.FieldByName('DATA_DEFAULT').AsString,
                    '"', '\"'),
                  '''', '\''')+'''';
            end
            else
            begin
              //If the default value is in the form 'xxxxx', keep outer '
              if(Copy(DMDB.SchemaSQLQuery.FieldByName('DATA_DEFAULT').AsString, 1, 1)='''')then
                theColumn.DefaultValue:=''''+
                  DMMain.ReplaceText(
                    DMMain.ReplaceText(
                      Copy(DMDB.SchemaSQLQuery.FieldByName('DATA_DEFAULT').AsString, 2,
                        Length(DMDB.SchemaSQLQuery.FieldByName('DATA_DEFAULT').AsString)-2),
                      '"', '\"'),
                    '''', '\''')+''''
              //if the default value is in the form ('xxxxx'), keep outer '
              else if(Copy(DMDB.SchemaSQLQuery.FieldByName('DATA_DEFAULT').AsString, 1, 2)='(''')then
                theColumn.DefaultValue:=''''+
                  DMMain.ReplaceText(
                    DMMain.ReplaceText(
                      Copy(DMDB.SchemaSQLQuery.FieldByName('DATA_DEFAULT').AsString, 3,
                        Length(DMDB.SchemaSQLQuery.FieldByName('DATA_DEFAULT').AsString)-4),
                      '"', '\"'),
                    '''', '\''')+''''
              //if the default value is in the form (xxxxx), remove outer ()
              else if(Copy(DMDB.SchemaSQLQuery.FieldByName('DATA_DEFAULT').AsString, 1, 1)='(')then
                theColumn.DefaultValue:=
                  DMMain.ReplaceText(
                    DMMain.ReplaceText(
                      Copy(DMDB.SchemaSQLQuery.FieldByName('DATA_DEFAULT').AsString, 2,
                        Length(DMDB.SchemaSQLQuery.FieldByName('DATA_DEFAULT').AsString)-2),
                      '"', '\"'),
                    '''', '\''')
              else
                theColumn.DefaultValue:=
                  DMMain.ReplaceText(
                    DMMain.ReplaceText(
                      DMDB.SchemaSQLQuery.FieldByName('DATA_DEFAULT').AsString,
                      '"', '\"'),
                    '''', '\''');
            end;
          end;
        end;

        //Check DatatypeParams
        if(DMDB.SchemaSQLQuery.FieldByName('DATA_LENGTH').AsString<>'')and
          (CompareText('TIMESTAMP', DatatypeName)<>0)then
        begin
          DatatypeParams:='('+DMDB.SchemaSQLQuery.FieldByName('DATA_LENGTH').AsString;
          //if there is no precision
          if(DMDB.SchemaSQLQuery.FieldByName('DATA_PRECISION').AsString<>'')then
            DatatypeParams:=DatatypeParams+', '+DMDB.SchemaSQLQuery.FieldByName('DATA_PRECISION').AsString;

          DatatypeParams:=DatatypeParams+')';

          //Change DECIMAL to bigint or integer
          //if there is no precision
          if(CompareText(DatatypeName, 'decimal')=0)and
            (DMDB.SchemaSQLQuery.FieldByName('DATA_PRECISION').AsString='')then
          begin
            if(DMDB.SchemaSQLQuery.FieldByName('DATA_LENGTH').AsInteger>8)then
              theColumn.idDatatype:=TEERDatatype(EERModel.GetDataTypeByName('BIGINT')).id
            else
              theColumn.idDatatype:=TEERDatatype(EERModel.GetDataTypeByName('INTEGER')).id;

            //Clear DatatypeParams when using bigint(22)
            if(DMDB.SchemaSQLQuery.FieldByName('DATA_LENGTH').AsInteger=22)then
              DatatypeParams:='';
            //Clear DatatypeParams when using integer(8)
            if(DMDB.SchemaSQLQuery.FieldByName('DATA_LENGTH').AsInteger=8)then
              DatatypeParams:='';
          end
          else if(CompareText(DatatypeName, 'varchar')=0)or
            (CompareText(DatatypeName, 'char')=0)then
          begin
            if(DMDB.SchemaSQLQuery.FieldByName('DATA_LENGTH').AsInteger>255)then
            begin
              theColumn.idDatatype:=TEERDatatype(EERModel.GetDataTypeByName('BLOB')).id;
              DatatypeParams:='';
            end;

          end;
        end;

        theColumn.DatatypeParams:=DatatypeParams;


        //Get Options
        if(Assigned(theDatatype))then
          for j:=0 to theDatatype.OptionCount-1 do
            if(Pos(UpperCase(theDatatype.Options[j]),
              UpperCase(DMDB.SchemaSQLQuery.Fields[1].AsString))>0)then
              theColumn.OptionSelected[j]:=True
            else
              theColumn.OptionSelected[j]:=False;

        DMDB.SchemaSQLQuery.Next;
      end;
      DMDB.SchemaSQLQuery.Close;

      //PK Columns
      DMDB.SchemaSQLQuery.SQL.Text:='SELECT i.COLUMN_NAME FROM ALL_CONSTRAINTS c, '+
        'ALL_IND_COLUMNS i '+
        'WHERE c.OWNER='''+TEERTable(DbTables[i]).GetTablePrefix+''' AND '+
        ' c.TABLE_NAME='''+TEERTable(DbTables[i]).ObjName+''' AND '+
        ' c.CONSTRAINT_TYPE=''P'' AND '+
        ' i.TABLE_NAME=c.TABLE_NAME AND '+
        ' i.TABLE_OWNER=c.OWNER AND '+
        ' c.CONSTRAINT_NAME=i.INDEX_NAME';
      DMDB.SchemaSQLQuery.Open;
      if(Not(DMDB.SchemaSQLQuery.EOF))then
      begin
        theIndex:=TEERIndex.Create(TEERTable(DbTables[i]));
        theIndex.Obj_id:=DMMain.GetNextGlobalID;
        theIndex.IndexName:='PRIMARY';
        theIndex.IndexKind:=ik_PRIMARY;
        theIndex.Pos:=0;
        TEERTable(DbTables[i]).Indices.Add(theIndex);

        while(Not(DMDB.SchemaSQLQuery.EOF))do
        begin
          theColumn:=TEERColumn(TEERTable(DbTables[i]).GetColumnByName(DMDB.SchemaSQLQuery.FieldByName('COLUMN_NAME').AsString));
          if(theColumn<>nil)then
          begin
            theColumn.PrimaryKey:=True;
            theIndex.Columns.Add(IntToStr(theColumn.Obj_id));
          end;

          DMDB.SchemaSQLQuery.Next;
        end;
      end;
      DMDB.SchemaSQLQuery.Close;


      //Indices
      prevIndex:='';
      theIndex:=nil;

      //ORCL 9i:
      //DMDB.SchemaSQLQuery.SQL.Text:='SELECT i.INDEX_NAME, i.UNIQUENESS, ic.COLUMN_NAME, ic.DESCEND, ic.COLUMN_LENGTH '+'FROM ALL_INDEXES i, DBA_IND_COLUMNS ic, DBA_CONSTRAINTS c '+'WHERE i.TABLE_OWNER='''+TEERTable(DbTables[i]).GetTablePrefix+''' AND '+' i.TABLE_NAME='''+TEERTable(DbTables[i]).ObjName+''' AND '+' ic.TABLE_OWNER=i.TABLE_OWNER AND ic.TABLE_NAME=i.TABLE_NAME AND '+' ic.INDEX_NAME=i.INDEX_NAME AND c.INDEX_OWNER(+)=i.OWNER AND '+' c.INDEX_NAME(+)=i.INDEX_NAME AND '+' (c.CONSTRAINT_TYPE is null OR c.CONSTRAINT_TYPE<>''P'') '+'ORDER BY i.TABLE_NAME, ic.INDEX_NAME, ic.COLUMN_POSITION';
      //ORCL 8:
      DMDB.SchemaSQLQuery.SQL.Text:='SELECT i.INDEX_NAME, '+
        'i.UNIQUENESS, ic.COLUMN_NAME, ic.COLUMN_LENGTH '+
        'FROM ALL_INDEXES i, ALL_IND_COLUMNS ic, ALL_CONSTRAINTS c '+
        'WHERE i.TABLE_OWNER='''+TEERTable(DbTables[i]).GetTablePrefix+''' AND '+
        'ic.TABLE_OWNER=i.TABLE_OWNER AND '+
        'i.TABLE_NAME='''+TEERTable(DbTables[i]).ObjName+''' AND '+
        'ic.TABLE_NAME=i.TABLE_NAME AND '+
        'ic.INDEX_NAME=i.INDEX_NAME AND '+
        'c.OWNER(+)=i.OWNER AND '+
        'c.CONSTRAINT_NAME(+)=i.INDEX_NAME AND '+
        '(c.CONSTRAINT_TYPE is null OR c.CONSTRAINT_TYPE<>''P'') '+
        'ORDER BY i.TABLE_NAME, ic.INDEX_NAME, ic.COLUMN_POSITION';
      DMDB.SchemaSQLQuery.Open;
      while(Not(DMDB.SchemaSQLQuery.EOF))do
      begin
        if(prevIndex<>DMDB.SchemaSQLQuery.FieldByName('INDEX_NAME').AsString)then
        begin
          prevIndex:=DMDB.SchemaSQLQuery.FieldByName('INDEX_NAME').AsString;

          theIndex:=TEERIndex.Create(TEERTable(DbTables[i]));
          theIndex.Obj_id:=DMMain.GetNextGlobalID;
          theIndex.IndexName:=DMDB.SchemaSQLQuery.FieldByName('INDEX_NAME').AsString;
          if(Copy(DMDB.SchemaSQLQuery.FieldByName('UNIQUENESS').AsString, 1, 1)='U')then
            theIndex.IndexKind:=ik_UNIQUE_INDEX
          else
            theIndex.IndexKind:=ik_INDEX;
          TEERTable(DbTables[i]).Indices.Add(theIndex);
          theIndex.Pos:=TEERTable(DbTables[i]).Indices.Count-1;
        end;

        theIndex.Columns.Add(IntToStr(
          TEERColumn(TEERTable(DbTables[i]).GetColumnByName(DMDB.SchemaSQLQuery.FieldByName('COLUMN_NAME').AsString)).Obj_id));

        DMDB.SchemaSQLQuery.Next;
      end;
      DMDB.SchemaSQLQuery.Close;
    end;

    //Order table positions
    for j:=0 to EERModel.ComponentCount-1 do
    begin
      if(EERModel.Components[j].ClassNameIs('TEERTable'))then
      begin
        theTable:=TEERTable(EERModel.Components[j]);

        theTable.Obj_X:=80+xpos*defwidth;
        theTable.Obj_Y:=40+ypos*defheight;

        theTable.RefreshObj;

        //Check, if there is already a table at this position
        tblAtPos:=True;
        while(tblAtPos)do
        begin
          tblAtPos:=False;
          for i:=0 to EERModel.ComponentCount-1 do
          begin
            if(EERModel.Components[i].ClassNameIs('TEERTable'))and
              (EERModel.Components[i]<>EERModel.Components[j])then
            begin
              tmpTbl:=TEERTable(EERModel.Components[i]);

              if((theTable.Obj_X>=tmpTbl.Obj_X)and
                (theTable.Obj_X<=tmpTbl.Obj_X+tmpTbl.Obj_W))and
                ((theTable.Obj_Y>=tmpTbl.Obj_Y)and
                (theTable.Obj_Y<=tmpTbl.Obj_Y+tmpTbl.Obj_H))then
              begin
                inc(xpos);
                if(xpos>=xanz)then
                begin
                  xpos:=0;
                  inc(ypos);
                end;

                //get next free pos
                theTable.Obj_X:=80+xpos*defwidth;
                theTable.Obj_Y:=40+ypos*defheight;

                theTable.RefreshObj;

                tblAtPos:=True;
                break;
              end;
            end;
          end;
        end;
      end;
    end;

    if(StatusLbl<>nil)then
    begin
      StatusLbl.Caption:=DMMain.GetTranslatedMessage('Building Relations...', 149);
      StatusLbl.Refresh;
      Application.ProcessMessages;
    end;
    if(BuildRelations)then
    begin
      for i:=1 to TEERModel(theModel).TablePrefix.Count-1 do
      begin
        //Relations

        DMDB.SchemaSQLQuery.SQL.Text:=
          'select c.CONSTRAINT_NAME, '+
          'r.TABLE_NAME as source_table_name, '+
          'rc.COLUMN_NAME as source_column_name, '+
          'c.TABLE_NAME as dest_table_name, '+
          'cc.COLUMN_NAME as dest_column_name, '+
          'cc.POSITION '+
          'from all_constraints c, '+
          'all_constraints r, '+
          'all_cons_columns cc, '+
          'all_cons_columns rc '+
          'where 	c.CONSTRAINT_TYPE = ''R'' '+
          'and c.OWNER='''+TEERModel(theModel).TablePrefix[i]+''' '+
          'and c.R_OWNER = r.OWNER '+
          'and c.R_CONSTRAINT_NAME = r.CONSTRAINT_NAME '+
          'and c.CONSTRAINT_NAME = cc.CONSTRAINT_NAME '+
          'and c.OWNER = cc.OWNER '+
          'and r.CONSTRAINT_NAME = rc.CONSTRAINT_NAME '+
          'and r.OWNER = rc.OWNER '+
          'and cc.POSITION = rc.POSITION '+
          'order by c.OWNER, c.TABLE_NAME, c.CONSTRAINT_NAME, cc.POSITION';

        DMDB.SchemaSQLQuery.Open;
        NewRelCounter:=0;
        while(Not(DMDB.SchemaSQLQuery.EOF))do
        begin
          theTable:=EERModel.GetEERObjectByName(EERTable,
            DMDB.SchemaSQLQuery.FieldByName('dest_table_name').AsString);

          tmpTbl:=EERModel.GetEERObjectByName(EERTable,
            DMDB.SchemaSQLQuery.FieldByName('source_table_name').AsString);

          if(theTable<>nil)and(tmpTbl<>nil)then
          begin
            inc(NewRelCounter);

            theRel:=TEERRel.Create(EERModel, DMDB.SchemaSQLQuery.FieldByName('CONSTRAINT_NAME').AsString+
              FormatFloat('#00', NewRelCounter));

            //Build PK - FK Mapping
            theRel.FKFields.Clear;
            theRel.FKFields.Add(DMDB.SchemaSQLQuery.FieldByName('source_column_name').AsString+'='+
              DMDB.SchemaSQLQuery.FieldByName('dest_column_name').AsString);
            theRel.FKFieldsComments.Add('');

            fkname:=DMDB.SchemaSQLQuery.FieldByName('CONSTRAINT_NAME').AsString;
            DMDB.SchemaSQLQuery.Next;
            while(Not(DMDB.SchemaSQLQuery.EOF))do
            begin
              if(DMDB.SchemaSQLQuery.FieldByName('dest_table_name').AsString=theTable.ObjName)and
                (DMDB.SchemaSQLQuery.FieldByName('CONSTRAINT_NAME').AsString=fkname)then
              begin
                theRel.FKFields.Add(DMDB.SchemaSQLQuery.FieldByName('source_column_name').AsString+'='+
                  DMDB.SchemaSQLQuery.FieldByName('dest_column_name').AsString);
                theRel.FKFieldsComments.Add('');

                DMDB.SchemaSQLQuery.Next;
              end
              else
                break;
            end;


            //Set Kind of Relation
            theRel.RelKind:=rk_1nNonId;

            //Assign Tables to the relation
            theRel.SrcTbl:=tmpTbl;
            theRel.DestTbl:=theTable;

            //Add relation to Tables
            theRel.SrcTbl.RelStart.Add(theRel);
            theRel.DestTbl.RelEnd.Add(theRel);

            //Display at the right pos and size
            theRel.SrcTbl.RefreshRelations;
            theRel.DestTbl.RefreshRelations;
          end
          else
            DMDB.SchemaSQLQuery.Next;
        end;
        DMDB.SchemaSQLQuery.Close;
      end;

      EERModel.CheckAllRelations;
    end;

    if(StatusLbl<>nil)then
    begin
      StatusLbl.Caption:=DMMain.GetTranslatedMessage('Creating Standard Inserts...', 150);
      StatusLbl.Refresh;
      Application.ProcessMessages;
    end;
    if(CreateStdInserts)then
      EERReverseEngineerCreateStdInserts(EERModel, DbTables, limitStdIns,
        StatusLbl, ImportSchema);

    //Clear Table Prefixes
    if(Not(ImportSchema))then
    begin
      EERModel.GetEERObjectList([EERTable], DbTables);
      for i:=0 to DbTables.Count-1 do
        TEERTable(DbTables[i]).TablePrefix:=0;

      TEERModel(theModel).TablePrefix.Clear;
      TEERModel(theModel).TablePrefix.Add(DMMain.GetTranslatedMessage('Default (no prefix)', 208));
    end;

    StatusLbl.Caption:=DMMain.GetTranslatedMessage('Finished.', 151);
  finally
    DbTables.Free;
  end;
end;

procedure TDMDBEER.EERSQLiteReverseEngineer(theModel: Pointer; DBConn: Pointer; theTables: TStringList; XCount: integer; BuildRelations: Boolean; BuildRelUsingPrimKey: Boolean; DatatypeSubst: TStringList; StatusLbl: TLabel = nil; CreateStdInserts: Boolean = False; limitStdIns: integer = 0);
var EERModel: TEERModel;
  i, j, xpos, ypos, xanz, defwidth, defheight: integer;
  tblAtPos, AllFKColsArePK, HasAutoInc: Boolean;
  DbTables: TList;
  theTable, tmpTbl, parentTbl: TEERTable;
  theColumn: TEERColumn;
  theIndex: TEERIndex;
  theDatatype: TEERDatatype;
  theRel: TEERRel;
  sqlcmd, quotedName, DeclType, DatatypeParams, DefVal,
    prevIndex, fkId, pkColName, fkColName: string;
  pkCount, pkColCount: integer;
begin
  EERModel:=theModel;

  defwidth:=250;
  defheight:=160;
  xanz:=XCount;
  xpos:=0;
  ypos:=0;

  if(StatusLbl<>nil)then
  begin
    StatusLbl.Caption:=DMMain.GetTranslatedMessage('Fetching Tables', 147);
    StatusLbl.Refresh;
    Application.ProcessMessages;
  end;

  DMDB.SchemaSQLQuery.ParamCheck:=False;

  RevEngSkippedTables:=0;

  //Get Tables
  DbTables:=TList.Create;
  try
    //---------------------------------------------------

    DMDB.SchemaSQLQuery.SetSchemaInfo(stNoSchema, '', '');
    DMDB.SchemaSQLQuery.SQL.Text:='SELECT name FROM sqlite_master '+
      'WHERE type=''table'' AND name NOT LIKE ''sqlite_%'' '+
      'ORDER BY name';
    DMDB.SchemaSQLQuery.Open;

    while(Not(DMDB.SchemaSQLQuery.EOF))do
    begin
      //get only selected tables
      if(theTables.IndexOf(DMDB.SchemaSQLQuery.FieldByName('name').AsString)<>-1)then
      begin
        //Skip tables that are already in the model (sqlite-bug-catalog #14)
        if(EERModel.GetEERObjectByName(EERTable, DMDB.SchemaSQLQuery.FieldByName('name').AsString)<>nil)then
          inc(RevEngSkippedTables)
        else
        begin
          theTable:=EERModel.NewTable(EERModel.EERModel_Width-250, 0, False);
          theTable.ObjName:=DMDB.SchemaSQLQuery.FieldByName('name').AsString;
          //theTable.TablePrefix:=0;

          DbTables.Add(theTable);
        end;
      end;

      DMDB.SchemaSQLQuery.Next;
    end;
    DMDB.SchemaSQLQuery.Close;

    //Get the columns and indices (SQLite's own metadata via the
    //pragma_* table-valued functions; the CREATE TABLE text is only
    //used to detect the AUTOINCREMENT keyword)
    for i:=0 to DbTables.Count-1 do
    begin
      theTable:=TEERTable(DbTables[i]);

      if(StatusLbl<>nil)then
      begin
        StatusLbl.Caption:=DMMain.GetTranslatedMessage('Fetching Table Columns/Indices (%s)', 148,
          theTable.ObjName);
        StatusLbl.Refresh;
        Application.ProcessMessages;
      end;

      quotedName:=StringReplace(theTable.ObjName, '''', '''''', [rfReplaceAll]);

      //Get SQL Create cmd (AUTOINCREMENT is not visible through the pragmas)
      sqlcmd:='';
      DMDB.SchemaSQLQuery.SQL.Text:='SELECT sql FROM sqlite_master '+
        'WHERE type=''table'' AND name='''+quotedName+'''';
      DMDB.SchemaSQLQuery.Open;
      if(Not(DMDB.SchemaSQLQuery.EOF))then
        sqlcmd:=DMDB.SchemaSQLQuery.Fields[0].AsString;
      DMDB.SchemaSQLQuery.Close;
      HasAutoInc:=(Pos('AUTOINCREMENT', UpperCase(RemoveCommentsFromSQLCmd(sqlcmd)))>0);

      //Columns
      pkColCount:=0;
      DMDB.SchemaSQLQuery.SQL.Text:='SELECT cid, name, type, "notnull" AS colnotnull, '+
        'dflt_value, pk FROM pragma_table_info('''+quotedName+''') ORDER BY cid';
      DMDB.SchemaSQLQuery.Open;
      while(Not(DMDB.SchemaSQLQuery.EOF))do
      begin
        theColumn:=TEERColumn.Create(theTable);
        theTable.Columns.Add(theColumn);

        theColumn.ColName:=DMDB.SchemaSQLQuery.FieldByName('name').AsString;
        theColumn.Obj_id:=DMMain.GetNextGlobalID;
        theColumn.Pos:=theTable.Columns.Count;
        theColumn.PrimaryKey:=(DMDB.SchemaSQLQuery.FieldByName('pk').AsInteger>0);
        if(theColumn.PrimaryKey)then
          inc(pkColCount);
        theColumn.NotNull:=(DMDB.SchemaSQLQuery.FieldByName('colnotnull').AsInteger=1)or
          (theColumn.PrimaryKey);
        theColumn.AutoInc:=False;
        theColumn.IsForeignKey:=False;

        //Default value: SQLite returns it as written in the DDL,
        //strip one pair of enclosing quotes like MySQL's SHOW FIELDS does
        DefVal:='';
        if(Not(DMDB.SchemaSQLQuery.FieldByName('dflt_value').IsNull))then
          DefVal:=DMDB.SchemaSQLQuery.FieldByName('dflt_value').AsString;
        if(Length(DefVal)>=2)and(DefVal[1]='''')and(DefVal[Length(DefVal)]='''')then
          DefVal:=Copy(DefVal, 2, Length(DefVal)-2);
        if(CompareText(DefVal, 'NULL')<>0)then
          theColumn.DefaultValue:=DefVal;

        //Datatype
        DeclType:=DMDB.SchemaSQLQuery.FieldByName('type').AsString;
        theDatatype:=TEERDatatype(GetSQLiteDatatype(EERModel, DeclType, DatatypeSubst, DatatypeParams));
        theColumn.idDatatype:=theDatatype.id;
        theColumn.DatatypeParams:=DatatypeParams;

        //Get Options (UNSIGNED, ZEROFILL, ...)
        for j:=0 to theDatatype.OptionCount-1 do
          theColumn.OptionSelected[j]:=
            (Pos(UpperCase(theDatatype.Options[j]), UpperCase(DeclType))>0);

        DMDB.SchemaSQLQuery.Next;
      end;
      DMDB.SchemaSQLQuery.Close;

      //AUTOINCREMENT: only valid for a single INTEGER PRIMARY KEY column
      if(HasAutoInc)and(pkColCount=1)then
        for j:=0 to theTable.Columns.Count-1 do
          if(TEERColumn(theTable.Columns[j]).PrimaryKey)then
            TEERColumn(theTable.Columns[j]).AutoInc:=True;

      //Build the PRIMARY index from the PK columns (an INTEGER PRIMARY KEY
      //has no entry in pragma_index_list)
      theTable.CheckPrimaryIndex;

      //Indices (skip the PK autoindex, handled above)
      prevIndex:='';
      theIndex:=nil;
      DMDB.SchemaSQLQuery.SQL.Text:='SELECT il.seq AS idxseq, il.name AS idxname, '+
        'il."unique" AS idxunique, il.origin AS idxorigin, ii.seqno AS colseq, '+
        'ii.name AS colname '+
        'FROM pragma_index_list('''+quotedName+''') il '+
        'JOIN pragma_index_info(il.name) ii '+
        'WHERE il.origin<>''pk'' ORDER BY il.seq, ii.seqno';
      DMDB.SchemaSQLQuery.Open;
      while(Not(DMDB.SchemaSQLQuery.EOF))do
      begin
        if(prevIndex<>DMDB.SchemaSQLQuery.FieldByName('idxname').AsString)then
        begin
          theIndex:=TEERIndex.Create(theTable);
          theIndex.Obj_id:=DMMain.GetNextGlobalID;
          theIndex.IndexName:=DMDB.SchemaSQLQuery.FieldByName('idxname').AsString;
          //autoindexes of UNIQUE constraints have reserved names
          if(Pos('sqlite_autoindex_', theIndex.IndexName)=1)then
            theIndex.IndexName:=theTable.ObjName+'_unique_'+
              DMDB.SchemaSQLQuery.FieldByName('idxseq').AsString;
          if(DMDB.SchemaSQLQuery.FieldByName('idxunique').AsInteger=1)then
            theIndex.IndexKind:=ik_UNIQUE_INDEX
          else
            theIndex.IndexKind:=ik_INDEX;
          theTable.Indices.Add(theIndex);
          theIndex.Pos:=theTable.Indices.Count-1;
        end;

        //expression indexes have no column name
        if(Not(DMDB.SchemaSQLQuery.FieldByName('colname').IsNull))then
        begin
          theColumn:=TEERColumn(theTable.GetColumnByName(
            DMDB.SchemaSQLQuery.FieldByName('colname').AsString));
          if(theColumn<>nil)then
            theIndex.Columns.Add(IntToStr(theColumn.Obj_id));
        end;

        prevIndex:=DMDB.SchemaSQLQuery.FieldByName('idxname').AsString;
        DMDB.SchemaSQLQuery.Next;
      end;
      DMDB.SchemaSQLQuery.Close;

      theTable.RefreshObj;
    end;


    //Order table positions
    for j:=0 to EERModel.ComponentCount-1 do
    begin
      if(EERModel.Components[j].ClassNameIs('TEERTable'))then
      begin
        theTable:=TEERTable(EERModel.Components[j]);

        theTable.Obj_X:=80+xpos*defwidth;
        theTable.Obj_Y:=40+ypos*defheight;

        theTable.RefreshObj;

        //Check, if there is already a table at this position
        tblAtPos:=True;
        while(tblAtPos)do
        begin
          tblAtPos:=False;
          for i:=0 to EERModel.ComponentCount-1 do
          begin
            if(EERModel.Components[i].ClassNameIs('TEERTable'))and
              (EERModel.Components[i]<>EERModel.Components[j])then
            begin
              tmpTbl:=TEERTable(EERModel.Components[i]);

              if((theTable.Obj_X>=tmpTbl.Obj_X)and
                (theTable.Obj_X<=tmpTbl.Obj_X+tmpTbl.Obj_W))and
                ((theTable.Obj_Y>=tmpTbl.Obj_Y)and
                (theTable.Obj_Y<=tmpTbl.Obj_Y+tmpTbl.Obj_H))then
              begin
                inc(xpos);
                if(xpos>=xanz)then
                begin
                  xpos:=0;
                  inc(ypos);
                end;

                //get next free pos
                theTable.Obj_X:=80+xpos*defwidth;
                theTable.Obj_Y:=40+ypos*defheight;

                theTable.RefreshObj;

                tblAtPos:=True;
                break;
              end;
            end;
          end;
        end;
      end;
    end;


    if(StatusLbl<>nil)then
    begin
      StatusLbl.Caption:=DMMain.GetTranslatedMessage('Building Relations...', 149);
      StatusLbl.Refresh;
      Application.ProcessMessages;
    end;
    if(BuildRelations)then
    begin
      //1. Native relations from the FOREIGN KEY constraints
      //   (PRAGMA foreign_key_list: one row per column pair, grouped by id)
      for i:=0 to DbTables.Count-1 do
      begin
        theTable:=TEERTable(DbTables[i]);
        quotedName:=StringReplace(theTable.ObjName, '''', '''''', [rfReplaceAll]);

        DMDB.SchemaSQLQuery.SQL.Text:='SELECT id AS fkid, seq AS fkseq, '+
          '"table" AS reftable, "from" AS fromcol, "to" AS tocol, '+
          'on_update, on_delete FROM pragma_foreign_key_list('''+quotedName+''') '+
          'ORDER BY id, seq';
        DMDB.SchemaSQLQuery.Open;
        while(Not(DMDB.SchemaSQLQuery.EOF))do
        begin
          fkId:=DMDB.SchemaSQLQuery.FieldByName('fkid').AsString;
          parentTbl:=TEERTable(EERModel.GetEERObjectByName(EERTable,
            DMDB.SchemaSQLQuery.FieldByName('reftable').AsString));

          //Referenced table not in the model (or self reference): skip this FK
          if(parentTbl=nil)or(parentTbl=theTable)then
          begin
            while(Not(DMDB.SchemaSQLQuery.EOF))and
              (DMDB.SchemaSQLQuery.FieldByName('fkid').AsString=fkId)do
              DMDB.SchemaSQLQuery.Next;
            continue;
          end;

          theRel:=TEERRel(EERModel.NewRelation(rk_1nNonId, parentTbl, theTable, False));
          theRel.FKFields.Clear;
          theRel.FKFieldsComments.Clear;
          theRel.CreateRefDef:=True;
          theRel.RefDef.Values['OnDelete']:=
            SQLiteRefActionCode(DMDB.SchemaSQLQuery.FieldByName('on_delete').AsString);
          theRel.RefDef.Values['OnUpdate']:=
            SQLiteRefActionCode(DMDB.SchemaSQLQuery.FieldByName('on_update').AsString);

          //Build PK - FK Mapping
          AllFKColsArePK:=True;
          pkCount:=0;
          while(Not(DMDB.SchemaSQLQuery.EOF))and
            (DMDB.SchemaSQLQuery.FieldByName('fkid').AsString=fkId)do
          begin
            fkColName:=DMDB.SchemaSQLQuery.FieldByName('fromcol').AsString;
            pkColName:='';
            if(Not(DMDB.SchemaSQLQuery.FieldByName('tocol').IsNull))then
              pkColName:=DMDB.SchemaSQLQuery.FieldByName('tocol').AsString;
            //"to" is NULL when the FK references the parent's PK implicitly:
            //take the parent's PK columns in order
            if(pkColName='')then
              for j:=0 to parentTbl.Columns.Count-1 do
                if(TEERColumn(parentTbl.Columns[j]).PrimaryKey)then
                begin
                  if(pkCount=DMDB.SchemaSQLQuery.FieldByName('fkseq').AsInteger)then
                  begin
                    pkColName:=TEERColumn(parentTbl.Columns[j]).ColName;
                    break;
                  end;
                  inc(pkCount);
                end;

            theRel.FKFields.Add(pkColName+'='+fkColName);
            theRel.FKFieldsComments.Add('');

            theColumn:=TEERColumn(theTable.GetColumnByName(fkColName));
            if(theColumn<>nil)then
            begin
              theColumn.IsForeignKey:=True;
              if(Not(theColumn.PrimaryKey))then
                AllFKColsArePK:=False;
            end
            else
              AllFKColsArePK:=False;

            DMDB.SchemaSQLQuery.Next;
          end;

          //FK columns that are all part of the child's PK: identifying relation
          if(AllFKColsArePK)then
            theRel.RelKind:=rk_1n;

          theRel.SrcTbl.RefreshRelations;
          theRel.DestTbl.RefreshRelations;
        end;
        DMDB.SchemaSQLQuery.Close;
      end;

      //2. Guess the remaining ones by name / primary key like the other
      //   drivers do, without duplicating the native ones
      EERReverseEngineerMakeRelations(EERModel, DbTables, BuildRelUsingPrimKey, True);
    end;

    if(StatusLbl<>nil)then
    begin
      StatusLbl.Caption:=DMMain.GetTranslatedMessage('Creating Standard Inserts...', 150);
      StatusLbl.Refresh;
      Application.ProcessMessages;
    end;
    if(CreateStdInserts)then
      EERReverseEngineerCreateStdInserts(EERModel, DbTables, limitStdIns);

    StatusLbl.Caption:=DMMain.GetTranslatedMessage('Finished.', 151);
    if(RevEngSkippedTables>0)then
      StatusLbl.Caption:=StatusLbl.Caption+' '+IntToStr(RevEngSkippedTables)+' existing table(s) skipped.';
  finally
    DbTables.Free;
  end;
end;

function TDMDBEER.GetSQLiteDatatype(theModel: Pointer; DeclType: string; DatatypeSubst: TStringList; var DatatypeParams: string): Pointer;
var EERModel: TEERModel;
  DatatypeName, s: string;
  theDatatype: TEERDatatype;
begin
  EERModel:=theModel;

  //Split "Varchar(45) UNSIGNED" into name "Varchar", params "(45)"
  DatatypeName:=Trim(DeclType);
  DatatypeParams:='';
  if(Pos('(', DatatypeName)>0)then
  begin
    if(Pos(')', DatatypeName)>Pos('(', DatatypeName))then
      DatatypeParams:=Copy(DatatypeName, Pos('(', DatatypeName),
        Pos(')', DatatypeName)-Pos('(', DatatypeName)+1);
    DatatypeName:=Trim(Copy(DatatypeName, 1, Pos('(', DatatypeName)-1));
  end;

  //Exact name (with substitution list), e.g. DOUBLE PRECISION
  s:=DatatypeName;
  if(Assigned(DatatypeSubst))then
    if(DatatypeSubst.Values[DatatypeName]<>'')then
      s:=DatatypeSubst.Values[DatatypeName];
  theDatatype:=EERModel.GetDataTypeByName(s);

  //First word only, e.g. "INT UNSIGNED" -> INT
  if(theDatatype=nil)and(Pos(' ', DatatypeName)>0)then
  begin
    s:=Copy(DatatypeName, 1, Pos(' ', DatatypeName)-1);
    if(Assigned(DatatypeSubst))then
      if(DatatypeSubst.Values[s]<>'')then
        s:=DatatypeSubst.Values[s];
    theDatatype:=EERModel.GetDataTypeByName(s);
  end;

  //Unknown declared type: follow SQLite's type affinity rules
  if(theDatatype=nil)then
  begin
    s:=UpperCase(DatatypeName);
    if(s='')or(Pos('BLOB', s)>0)then
      theDatatype:=EERModel.GetDataTypeByName('BLOB')
    else if(Pos('INT', s)>0)then
      theDatatype:=EERModel.GetDataTypeByName('INTEGER')
    else if(Pos('CHAR', s)>0)or(Pos('CLOB', s)>0)or(Pos('TEXT', s)>0)or(Pos('STRING', s)>0)then
    begin
      if(DatatypeParams<>'')then
        theDatatype:=EERModel.GetDataTypeByName('VARCHAR')
      else
        theDatatype:=EERModel.GetDataTypeByName('TEXT');
    end
    else if(Pos('REAL', s)>0)or(Pos('FLOA', s)>0)or(Pos('DOUB', s)>0)then
      theDatatype:=EERModel.GetDataTypeByName('FLOAT')
    else if(Pos('BOOL', s)>0)then
      theDatatype:=EERModel.GetDataTypeByName('BOOL')
    else if(Pos('DATE', s)>0)or(Pos('TIME', s)>0)then
      theDatatype:=EERModel.GetDataTypeByName('DATETIME')
    else
      theDatatype:=EERModel.GetDataTypeByName('DECIMAL');
  end;

  if(theDatatype=nil)then
    theDatatype:=TEERDatatype(EERModel.GetDataType(EERModel.DefaultDataType));

  GetSQLiteDatatype:=theDatatype;
end;

function TDMDBEER.SQLiteRefActionCode(action: string): string;
begin
  //Codes as used by TEERRel.RefDef (see TEERTable.GetSQLCreateCode)
  action:=UpperCase(Trim(action));
  if(action='RESTRICT')then
    SQLiteRefActionCode:='0'
  else if(action='CASCADE')then
    SQLiteRefActionCode:='1'
  else if(action='SET NULL')then
    SQLiteRefActionCode:='2'
  else if(action='SET DEFAULT')then
    SQLiteRefActionCode:='4'
  else
    SQLiteRefActionCode:='3'; //NO ACTION
end;

procedure TDMDBEER.EERMSSQLReverseEngineer(theModel: Pointer; DBConn: Pointer; theTables: TStringList; XCount: integer; BuildRelations: Boolean; BuildRelUsingPrimKey: Boolean; DatatypeSubst: TStringList; StatusLbl: TLabel = nil; CreateStdInserts: Boolean = False; limitStdIns: integer = 0; CollapseTables: Boolean = False);
var EERModel: TEERModel;
  i, j, xpos, ypos, xanz, defwidth, defheight: integer;
  DbTables: TList;
  theTable, tmpTbl: TEERTable;
  theColumn: TEERColumn;
  theIndex: TEERIndex;
  theDatatype: TEERDatatype;
  theRel: TEERRel;
  DatatypeName, DatatypeParams, prevIndex: string;
  tblAtPos: Boolean;
  fkname: string;
  theQuoteChar: string;
begin
  EERModel:=theModel;

  defwidth:=250;
  defheight:=160;
  xanz:=XCount;
  xpos:=0;
  ypos:=0;

  if(StatusLbl<>nil)then
  begin
    StatusLbl.Caption:=DMMain.GetTranslatedMessage('Fetching Tables', 147);
    StatusLbl.Refresh;
    Application.ProcessMessages;
  end;

  //Get Tables
  DbTables:=TList.Create;
  try
    DMDB.SchemaSQLQuery.SetSchemaInfo(stNoSchema, '', '');
    DMDB.SchemaSQLQuery.SQL.Text:='sp_tables @table_type="''TABLE''"';
    DMDB.SchemaSQLQuery.Open;

    theQuoteChar:=DMDB.SchemaSQLQuery.GetQuoteChar;

    i:=0;
    while(Not(DMDB.SchemaSQLQuery.EOF))do
    begin
      //get only selected tables
      //Check for Tables retrived with ODBC as well and use theQuoteChar
      if(theTables.IndexOf(DMDB.SchemaSQLQuery.FieldByName('TABLE_QUALIFIER').AsString+
        '.'+DMDB.SchemaSQLQuery.FieldByName('TABLE_NAME').AsString)<>-1)or
        (theTables.IndexOf(theQuoteChar+DMDB.SchemaSQLQuery.FieldByName('TABLE_OWNER').AsString+theQuoteChar+
        '.'+theQuoteChar+DMDB.SchemaSQLQuery.FieldByName('TABLE_NAME').AsString+theQuoteChar)<>-1)then
      begin
        theTable:=EERModel.NewTable(EERModel.EERModel_Width-250, 0, False);
        theTable.ObjName:=DMDB.SchemaSQLQuery.FieldByName('TABLE_NAME').AsString;
        theTable.TablePrefix:=i;

        theTable.Collapsed:=CollapseTables;

        //theTable.RefreshObj;

        DbTables.Add(theTable);
      end;

      DMDB.SchemaSQLQuery.Next;
    end;
    DMDB.SchemaSQLQuery.Close;

    //Get the columns
    for i:=0 to DbTables.Count-1 do
    begin
      if(StatusLbl<>nil)then
      begin
        StatusLbl.Caption:=DMMain.GetTranslatedMessage('Fetching Table Columns/Indices (%s)', 148,
          TEERTable(DbTables[i]).ObjName);
        StatusLbl.Refresh;
        Application.ProcessMessages;
      end;

      DMDB.SchemaSQLQuery.SQL.Text:='sp_columns '''+TEERTable(DbTables[i]).ObjName+'''';
      DMDB.SchemaSQLQuery.Open;
      while(Not(DMDB.SchemaSQLQuery.EOF))do
      begin
        //Get Datatype
        theDatatype:=EERModel.GetDataTypeByNameSubst(DMDB.SchemaSQLQuery.FieldByName('TYPE_NAME').AsString, DatatypeSubst);
        DatatypeName:=theDatatype.GetPhysicalTypeName;
        DatatypeParams:='';

        //Create Column
        theColumn:=TEERColumn.Create(TEERTable(DbTables[i]));
        TEERTable(DbTables[i]).Columns.Add(theColumn);

        theColumn.ColName:=DMDB.SchemaSQLQuery.FieldByName('COLUMN_NAME').AsString;
        theColumn.Obj_id:=DMMain.GetNextGlobalID;
        theColumn.Pos:=TEERTable(DbTables[i]).Columns.Count;
        theColumn.idDatatype:=theDatatype.id;
        theColumn.DatatypeParams:='';
        theColumn.Width:=DMDB.SchemaSQLQuery.FieldByName('LENGTH').AsInteger;
        theColumn.Prec:=DMDB.SchemaSQLQuery.FieldByName('PRECISION').AsInteger;
        theColumn.PrimaryKey:=False;
        theColumn.NotNull:=(DMDB.SchemaSQLQuery.FieldByName('NULLABLE').AsString='1');
        theColumn.AutoInc:=False;
        theColumn.IsForeignKey:=False;
        if(DMDB.SchemaSQLQuery.FieldByName('COLUMN_DEF').AsString<>'')then
          theColumn.DefaultValue:=DMDB.SchemaSQLQuery.FieldByName('COLUMN_DEF').AsString;

        //Check DatatypeParams
        if(DMDB.SchemaSQLQuery.FieldByName('PRECISION').AsString<>'')and
          ((CompareText(DatatypeName, 'TINYINT')=0)or
          (CompareText(DatatypeName, 'SMALLINT')=0)or
          (CompareText(DatatypeName, 'MEDIUMINT')=0)or
          (CompareText(DatatypeName, 'INTEGER')=0)or
          (CompareText(DatatypeName, 'BIGINT')=0)or
          (CompareText(DatatypeName, 'FLOAT')=0)or
          (CompareText(DatatypeName, 'DOUBLE')=0)or
          (CompareText(DatatypeName, 'DOUBLE_PRECISION')=0)or
          (CompareText(DatatypeName, 'REAL')=0)or
          (CompareText(DatatypeName, 'DECIMAL')=0)or
          (CompareText(DatatypeName, 'NUMERIC')=0))then
        begin
          DatatypeParams:='('+DMDB.SchemaSQLQuery.FieldByName('PRECISION').AsString;
          //if there is no precision
          if(DMDB.SchemaSQLQuery.FieldByName('SCALE').AsString<>'')and
            (DMDB.SchemaSQLQuery.FieldByName('SCALE').AsString<>'0')then
            DatatypeParams:=DatatypeParams+', '+DMDB.SchemaSQLQuery.FieldByName('SCALE').AsString;

          DatatypeParams:=DatatypeParams+')';

          //Change DECIMAL to bigint or integer
          //if there is no precision
          if(CompareText(DatatypeName, 'INTEGER')=0)then
          begin
            if(DMDB.SchemaSQLQuery.FieldByName('PRECISION').AsInteger>10)then
              theColumn.idDatatype:=TEERDatatype(EERModel.GetDataTypeByName('BIGINT')).id;

            //Clear DatatypeParams when using bigint(22)
            if(DMDB.SchemaSQLQuery.FieldByName('PRECISION').AsInteger=22)then
              DatatypeParams:='';
            //Clear DatatypeParams when using integer(10)
            if(DMDB.SchemaSQLQuery.FieldByName('PRECISION').AsInteger=10)then
              DatatypeParams:='';
          end;
        end
        else if(DMDB.SchemaSQLQuery.FieldByName('PRECISION').AsString<>'')and
          ((CompareText(DatatypeName, 'VARCHAR')=0)or
          (CompareText(DatatypeName, 'CHAR')=0))then
        begin
          //Replace VARCHAR(>255) with BLOB
          if(DMDB.SchemaSQLQuery.FieldByName('PRECISION').AsInteger<=255)then
            DatatypeParams:='('+DMDB.SchemaSQLQuery.FieldByName('PRECISION').AsString+')'
          else
          begin
            theColumn.idDatatype:=TEERDatatype(EERModel.GetDataTypeByName('BLOB')).id;
            DatatypeParams:='';
          end;
        end;

        theColumn.DatatypeParams:=DatatypeParams;


        //Get Options
        if(Assigned(theDatatype))then
          for j:=0 to theDatatype.OptionCount-1 do
            if(Pos(UpperCase(theDatatype.Options[j]),
              UpperCase(DMDB.SchemaSQLQuery.Fields[1].AsString))>0)then
              theColumn.OptionSelected[j]:=True
            else
              theColumn.OptionSelected[j]:=False;

        DMDB.SchemaSQLQuery.Next;
      end;
      DMDB.SchemaSQLQuery.Close;

      //PK Columns
      DMDB.SchemaSQLQuery.SQL.Text:='sp_pkeys '''+TEERTable(DbTables[i]).ObjName+'''';
      DMDB.SchemaSQLQuery.Open;
      if(Not(DMDB.SchemaSQLQuery.EOF))then
      begin
        theIndex:=TEERIndex.Create(TEERTable(DbTables[i]));
        theIndex.Obj_id:=DMMain.GetNextGlobalID;
        theIndex.IndexName:='PRIMARY';
        theIndex.IndexKind:=ik_PRIMARY;
        theIndex.Pos:=0;
        TEERTable(DbTables[i]).Indices.Add(theIndex);

        while(Not(DMDB.SchemaSQLQuery.EOF))do
        begin
          theColumn:=TEERColumn(TEERTable(DbTables[i]).GetColumnByName(DMDB.SchemaSQLQuery.FieldByName('COLUMN_NAME').AsString));
          if(theColumn<>nil)then
          begin
            theColumn.PrimaryKey:=True;
            theIndex.Columns.Add(IntToStr(theColumn.Obj_id));
          end;

          DMDB.SchemaSQLQuery.Next;
        end;
      end;
      DMDB.SchemaSQLQuery.Close;


      //Indices
      prevIndex:='';
      {theIndex:=nil;

      //ORCL 9i:
      //DMDB.SchemaSQLQuery.SQL.Text:='SELECT i.INDEX_NAME, i.UNIQUENESS, ic.COLUMN_NAME, ic.DESCEND, ic.COLUMN_LENGTH '+'FROM ALL_INDEXES i, DBA_IND_COLUMNS ic, DBA_CONSTRAINTS c '+'WHERE i.TABLE_OWNER='''+TEERTable(DbTables[i]).GetTablePrefix+''' AND '+' i.TABLE_NAME='''+TEERTable(DbTables[i]).ObjName+''' AND '+' ic.TABLE_OWNER=i.TABLE_OWNER AND ic.TABLE_NAME=i.TABLE_NAME AND '+' ic.INDEX_NAME=i.INDEX_NAME AND c.INDEX_OWNER(+)=i.OWNER AND '+' c.INDEX_NAME(+)=i.INDEX_NAME AND '+' (c.CONSTRAINT_TYPE is null OR c.CONSTRAINT_TYPE<>''P'') '+'ORDER BY i.TABLE_NAME, ic.INDEX_NAME, ic.COLUMN_POSITION';
      //ORCL 8:
      DMDB.SchemaSQLQuery.SQL.Text:='SELECT i.INDEX_NAME, '+
        'i.UNIQUENESS, ic.COLUMN_NAME, ic.COLUMN_LENGTH '+
        'FROM ALL_INDEXES i, DBA_IND_COLUMNS ic, DBA_CONSTRAINTS c '+
        'WHERE i.TABLE_OWNER='''+TEERTable(DbTables[i]).GetTablePrefix+''' AND '+
        'ic.TABLE_OWNER=i.TABLE_OWNER AND '+
        'i.TABLE_NAME='''+TEERTable(DbTables[i]).ObjName+''' AND '+
        'ic.TABLE_NAME=i.TABLE_NAME AND '+
        'ic.INDEX_NAME=i.INDEX_NAME AND '+
        'c.OWNER(+)=i.OWNER AND '+
        'c.CONSTRAINT_NAME(+)=i.INDEX_NAME AND '+
        '(c.CONSTRAINT_TYPE is null OR c.CONSTRAINT_TYPE<>''P'') '+
        'ORDER BY i.TABLE_NAME, ic.INDEX_NAME, ic.COLUMN_POSITION';
      DMDB.SchemaSQLQuery.Open;
      while(Not(DMDB.SchemaSQLQuery.EOF))do
      begin
        if(prevIndex<>DMDB.SchemaSQLQuery.FieldByName('INDEX_NAME').AsString)then
        begin
          prevIndex:=DMDB.SchemaSQLQuery.FieldByName('INDEX_NAME').AsString;

          theIndex:=TEERIndex.Create(TEERTable(DbTables[i]));
          theIndex.Obj_id:=DMMain.GetNextGlobalID;
          theIndex.IndexName:=DMDB.SchemaSQLQuery.FieldByName('INDEX_NAME').AsString;
          if(Copy(DMDB.SchemaSQLQuery.FieldByName('UNIQUENESS').AsString, 1, 1)='U')then
            theIndex.IndexKind:=ik_UNIQUE_INDEX
          else
            theIndex.IndexKind:=ik_INDEX;
          TEERTable(DbTables[i]).Indices.Add(theIndex);
          theIndex.Pos:=TEERTable(DbTables[i]).Indices.Count-1;
        end;

        theIndex.Columns.Add(IntToStr(
          TEERColumn(TEERTable(DbTables[i]).GetColumnByName(DMDB.SchemaSQLQuery.FieldByName('COLUMN_NAME').AsString)).Obj_id));

        DMDB.SchemaSQLQuery.Next;
      end;
      DMDB.SchemaSQLQuery.Close;}
    end;

    //Order table positions
    for j:=0 to EERModel.ComponentCount-1 do
    begin
      if(EERModel.Components[j].ClassNameIs('TEERTable'))then
      begin
        theTable:=TEERTable(EERModel.Components[j]);

        theTable.Obj_X:=80+xpos*defwidth;
        theTable.Obj_Y:=40+ypos*defheight;

        theTable.RefreshObj;

        //Check, if there is already a table at this position
        tblAtPos:=True;
        while(tblAtPos)do
        begin
          tblAtPos:=False;
          for i:=0 to EERModel.ComponentCount-1 do
          begin
            if(EERModel.Components[i].ClassNameIs('TEERTable'))and
              (EERModel.Components[i]<>EERModel.Components[j])then
            begin
              tmpTbl:=TEERTable(EERModel.Components[i]);

              if((theTable.Obj_X>=tmpTbl.Obj_X)and
                (theTable.Obj_X<=tmpTbl.Obj_X+tmpTbl.Obj_W))and
                ((theTable.Obj_Y>=tmpTbl.Obj_Y)and
                (theTable.Obj_Y<=tmpTbl.Obj_Y+tmpTbl.Obj_H))then
              begin
                inc(xpos);
                if(xpos>=xanz)then
                begin
                  xpos:=0;
                  inc(ypos);
                end;

                //get next free pos
                theTable.Obj_X:=80+xpos*defwidth;
                theTable.Obj_Y:=40+ypos*defheight;

                theTable.RefreshObj;

                tblAtPos:=True;
                break;
              end;
            end;
          end;
        end;
      end;
    end;

    if(StatusLbl<>nil)then
    begin
      StatusLbl.Caption:=DMMain.GetTranslatedMessage('Building Relations...', 149);
      StatusLbl.Refresh;
      Application.ProcessMessages;
    end;
    if(BuildRelations)or(1=2)then
    begin
      for i:=0 to DbTables.Count-1 do
      begin
        //Relations
        DMDB.SchemaSQLQuery.SQL.Text:='sp_fkeys '''+TEERTable(DbTables[i]).ObjName+'''';
        DMDB.SchemaSQLQuery.Open;

        while(Not(DMDB.SchemaSQLQuery.EOF))do
        begin
          theTable:=EERModel.GetEERObjectByName(EERTable,
            DMDB.SchemaSQLQuery.FieldByName('FKTABLE_NAME').AsString);

          if(theTable<>nil)then
          begin
            theRel:=TEERRel.Create(EERModel, DMDB.SchemaSQLQuery.FieldByName('FK_NAME').AsString);

            //Build PK - FK Mapping
            theRel.FKFields.Clear;
            theRel.FKFields.Add(DMDB.SchemaSQLQuery.FieldByName('PKCOLUMN_NAME').AsString+'='+
              DMDB.SchemaSQLQuery.FieldByName('FKCOLUMN_NAME').AsString);
            theRel.FKFieldsComments.Add('');

            fkname:=DMDB.SchemaSQLQuery.FieldByName('FK_NAME').AsString;
            DMDB.SchemaSQLQuery.Next;
            while(Not(DMDB.SchemaSQLQuery.EOF))do
            begin
              if(DMDB.SchemaSQLQuery.FieldByName('FKTABLE_NAME').AsString=theTable.ObjName)and
                (DMDB.SchemaSQLQuery.FieldByName('FK_NAME').AsString=fkname)then
              begin
                theRel.FKFields.Add(DMDB.SchemaSQLQuery.FieldByName('PKCOLUMN_NAME').AsString+'='+
                  DMDB.SchemaSQLQuery.FieldByName('FKCOLUMN_NAME').AsString);
                theRel.FKFieldsComments.Add('');

                DMDB.SchemaSQLQuery.Next;
              end
              else
                break;
            end;

            //Set Kind of Relation
            theRel.RelKind:=rk_1nNonId;

            //Assign Tables to the relation
            theRel.SrcTbl:=TEERTable(DbTables[i]);
            theRel.DestTbl:=theTable;

            //Add relation to Tables
            theRel.SrcTbl.RelStart.Add(theRel);
            theRel.DestTbl.RelEnd.Add(theRel);

            //Display at the right pos and size
            theRel.SrcTbl.RefreshRelations;
            theRel.DestTbl.RefreshRelations;
          end
          else
            DMDB.SchemaSQLQuery.Next;
        end;
        DMDB.SchemaSQLQuery.Close;
      end;

      EERModel.CheckAllRelations;
    end;

    if(StatusLbl<>nil)then
    begin
      StatusLbl.Caption:=DMMain.GetTranslatedMessage('Creating Standard Inserts...', 150);
      StatusLbl.Refresh;
      Application.ProcessMessages;
    end;
    if(CreateStdInserts)then
      EERReverseEngineerCreateStdInserts(EERModel, DbTables, limitStdIns);

    StatusLbl.Caption:=DMMain.GetTranslatedMessage('Finished.', 151);
  finally
    DbTables.Free;
  end;
end;


procedure TDMDBEER.EERReverseEngineerMakeRelations(theModel: Pointer; theTables: TList; BuildRelUsingPrimKey: Boolean; SkipExisting: Boolean = False);
var i, j, k, l: integer;
  EERModel: TEERModel;
  srcPKIndex: TEERIndex;
  FieldFound, AllFieldsFound, AllFieldsPKs: Boolean;

  //SkipExisting: don't add a second relation between two tables that
  //already got one (e.g. from the database's own FOREIGN KEY constraints)
  function RelationExists(SrcTbl, DestTbl: TEERTable): Boolean;
  var m: integer;
  begin
    RelationExists:=False;
    if(Not(SkipExisting))then
      exit;
    for m:=0 to SrcTbl.RelStart.Count-1 do
      if(TEERRel(SrcTbl.RelStart[m]).DestTbl=DestTbl)then
      begin
        RelationExists:=True;
        exit;
      end;
    for m:=0 to SrcTbl.RelEnd.Count-1 do
      if(TEERRel(SrcTbl.RelEnd[m]).SrcTbl=DestTbl)then
      begin
        RelationExists:=True;
        exit;
      end;
  end;
begin
  EERModel:=theModel;

  for i:=0 to theTables.Count-1 do
  begin
    //Primary Keys up
    TEERTable(theTables[i]).PrimaryColumnsFirst;

    //Get Primary Index of first table
    srcPKIndex:=nil;
    if(BuildRelUsingPrimKey)then
    begin
      for k:=0 to TEERTable(theTables[i]).Indices.Count-1 do
        if(CompareText(TEERIndex(TEERTable(theTables[i]).Indices[k]).IndexName,
          'PRIMARY')=0)then
        begin
          srcPKIndex:=TEERIndex(TEERTable(theTables[i]).Indices[k]);
          break;
        end;

      //If this table has no PK, check next
      if(srcPKIndex=nil)then
        continue;
    end;

    for j:=0 to theTables.Count-1 do
    begin
      //No relation to itself
      if(i=j)then
        continue;

      if(Not(BuildRelUsingPrimKey))then
      begin
        //Build Rel based on Tablenames and ID-Fieldnames
        for k:=0 to TEERTable(theTables[j]).Columns.Count-1 do
        begin
          if(CompareText('id'+TEERTable(theTables[i]).ObjName,
            TEERColumn(TEERTable(theTables[j]).Columns[k]).ColName)=0)and
            (Not(RelationExists(TEERTable(theTables[i]), TEERTable(theTables[j]))))then
          begin
            //New Relation
            EERModel.NewRelation(rk_1nNonId, theTables[i], theTables[j], False);
          end;
        end;
      end
      else
      begin
        //Build Rel based on PK

        //Check if each PKField is found in the Dest-Table
        AllFieldsFound:=True;
        AllFieldsPKs:=True;
        for k:=0 to srcPKIndex.Columns.Count-1 do
        begin
          FieldFound:=False;
          for l:=0 to TEERTable(theTables[j]).Columns.Count-1 do
            if(CompareText(TEERColumn(TEERTable(theTables[i]).GetColumnByID(StrToInt(srcPKIndex.Columns[k]))).ColName,
              TEERColumn(TEERTable(theTables[j]).Columns[l]).ColName)=0)then
            begin
              if(Not(TEERColumn(TEERTable(theTables[j]).Columns[l]).PrimaryKey))then
                AllFieldsPKs:=False;

              FieldFound:=True;
              break;
            end;

          if(Not(FieldFound))then
          begin
            AllFieldsFound:=False;
            break;
          end;
        end;

        //New Relation
        if(AllFieldsFound)and
          (Not(RelationExists(TEERTable(theTables[i]), TEERTable(theTables[j]))))then
        begin
          if(AllFieldsPKs)then
            //New 1n Relation
            EERModel.NewRelation(rk_1n, theTables[i], theTables[j], False)
          else
            //New 1n-Sub Relation
            EERModel.NewRelation(rk_1nNonId, theTables[i], theTables[j], False);
        end;
      end;
    end;
  end;

  EERModel.CheckAllRelations;
end;

procedure TDMDBEER.EERReverseEngineerCreateStdInserts(theModel: Pointer; theTables: TList; limit: integer;
  StatusLbl: TLabel; ImportSchema: Boolean);
var i: integer;
  theTable: TEERTable;
  s: string;
begin
  for i:=0 to theTables.Count-1 do
  begin
    theTable:=TEERTable(theTables[i]);

    if(StatusLbl<>nil)then
    begin
      StatusLbl.Caption:=DMMain.GetTranslatedMessage('Creating Standard Inserts...', 150)+
        ' '+theTable.GetSQLTableName;
      StatusLbl.Refresh;
      Application.ProcessMessages;
    end;

    DMDB.OutputQry.SQL.Text:='SELECT * FROM '+theTable.GetSQLTableName;
    try
      DMDB.OutputQry.Open;
      //Force no prefix
      if(Not(ImportSchema))then
        s:=DMDB.GetRecordsAsInsertsTSQLQuery(
          'SELECT * FROM '+theTable.GetSQLTableName(True), DMDB.OutputQry, limit)
      else
        s:=DMDB.GetRecordsAsInsertsTSQLQuery(
          'SELECT * FROM '+theTable.GetSQLTableName, DMDB.OutputQry, limit);
    finally
      DMDB.OutputQry.Close;
    end;

    theTable.StandardInserts.Text:=s;
  end;
end;

procedure TDMDBEER.EERMySQLSyncDB(theModel: Pointer; DBConn: Pointer;
  Log: TStrings; KeepExTbls, StdInsertsOnCreate, StdInsertsSync: Boolean);
var EERModel: TEERModel;
  i, j, k, position,
  ColNr: integer;
  DbTables, DbColumns,
  SQLStr, SQL2Str, IndexColumns, IndexColumnParams: TStringList;
  ModelTables,
  IndexRecreate,
  DBIndices: TList;
  theTable: TEERTable;
  theColumn: TEERColumn;
  theIndex: TEERIndex;
  theDatatype, theModelDatatype: TEERDatatype;
  s, DatatypeName, DatatypeParams, prevIndex: string;
  NewPrimaryKey, ColumnChanged, ChangeColumnName,
  checkIndex, PKFieldIsChecked: Boolean;
  //IndexComment: string;
  ColumnCompCounter, ColumnModCounter, ColumnDelCounter,
  ColumnAddCounter,
  TableCreateCounter, TableRenameCounter, TableDropCounter,
  PKChangedCounter,
  IndexDropCounter, IndexCreateCounter, IndexUpdateCounter: integer;
  oldDecSep: Char;
  FieldOnGeneratorOrSequence:string;
begin
  EERModel:=theModel;

  Log.Add(DMMain.GetTranslatedMessage('Syncronisation started.', 152));

  ColumnCompCounter:=0;
  ColumnModCounter:=0;
  ColumnDelCounter:=0;
  ColumnAddCounter:=0;
  TableCreateCounter:=0;
  TableRenameCounter:=0;
  TableDropCounter:=0;
  PKChangedCounter:=0;
  IndexDropCounter:=0;
  IndexCreateCounter:=0;
  IndexUpdateCounter:=0;

  ModelTables:=TList.Create;
  DbTables:=TStringList.Create;
  DbColumns:=TStringList.Create;
  SQLStr:=TStringList.Create;
  SQL2Str:=TStringList.Create;
  IndexColumns:=TStringList.Create;
  IndexColumnParams:=TStringList.Create;
  IndexRecreate:=TList.Create;
  DBIndices:=TList.Create;
  try
    //Get Tables from Model
    EERModel.GetEERObjectList([EERTable], ModelTables);

    //Remove Linked Tables if CreateSQLforLinkedObjects is deactivated
    if(Not(EERModel.CreateSQLforLinkedObjects))then
    begin
      i:=0;
      while(i<ModelTables.Count)do
        if(TEERTable(ModelTables[i]).IsLinkedObject)then
          ModelTables.Delete(i)
        else
          inc(i);
    end;

    //Sort tables in FK order, if they are created all
    EERModel.SortEERTableListByForeignKeyReferences(ModelTables);

    //Disable Foreign Key checks
    DMDB.ExecSQL('SET FOREIGN_KEY_CHECKS=0');
    try
      //Get Tables from DB
      Log.Add('Get Tables from DB');
      DMDB.SchemaSQLQuery.ParamCheck:=False;
      DMDB.SchemaSQLQuery.SetSchemaInfo(stNoSchema, '', '');
      DMDB.SchemaSQLQuery.SQL.Text:='show tables';
      DMDB.SchemaSQLQuery.Open;

      while(Not(DMDB.SchemaSQLQuery.EOF))do
      begin
        //Ignore DBDesigner4 table
        if(CompareText(DMDB.SchemaSQLQuery.Fields[0].AsString,
          'DBDesigner4')<>0)then
          DbTables.Add(DMDB.SchemaSQLQuery.Fields[0].AsString);

        DMDB.SchemaSQLQuery.Next;
      end;
      DMDB.SchemaSQLQuery.Close;

      //---------------------------------------------------------------
      //Compare Tables

      Log.Add(DMMain.GetTranslatedMessage('Compare tables', 153));

      //---------------------
      //Create non existing
      for i:=0 to ModelTables.Count-1 do
      begin
        theTable:=TEERTable(ModelTables[i]);

        //if the table is not in DB-Table List, create it
        if(DbTables.IndexOf(theTable.ObjName)=-1)then
        begin
          //Check if table was renamed
          if(DbTables.IndexOf(theTable.PrevTableName)=-1)then
          begin
            Log.Add('Create non existing table '+theTable.ObjName);
            inc(TableCreateCounter);

  //          DMDB.SchemaSQLQuery.SQL.Text:=
  //          DMDB.SchemaSQLQuery.ExecSQL;
            DMDB.ExecuteSQLCmdScript(theTable.GetSQLCreateCode(True, //Define PKs
              True, //CreateIndices
              True, //DefineFK
              True, //TblOptions
              False)); //StdInserts are executed seperatly


            //Execute Standard Inserts also if user whishes
            if(StdInsertsOnCreate)and(Trim(theTable.StandardInserts.Text)<>'')then
              DMDB.ExecuteSQLCmdScript(theTable.StandardInserts.Text);

            //Clear previous colname for DB-Sync when table has just been created
            for j:=0 to theTable.Columns.Count-1 do
              TEERColumn(theTable.Columns[j]).PrevColName:='';
          end
          else
          begin
            //Table was renamed
            Log.Add(DMMain.GetTranslatedMessage('Rename existing table %s to %s', 154,
              theTable.PrevTableName, theTable.ObjName));
            inc(TableRenameCounter);

            //Rename in DB
            DMDB.SchemaSQLQuery.SQL.Text:='RENAME TABLE '+theTable.PrevTableName+
              ' TO '+theTable.ObjName;
            DMDB.SchemaSQLQuery.ExecSQL;

            //Set renamed TableName in DBList
            DbTables[DbTables.IndexOf(theTable.PrevTableName)]:=theTable.ObjName;

            theTable.PrevTableName:='';
          end;
        end;
      end;


      //---------------------
      //Drop tables not longer in model in reverse order
      DMMain.ReverseList(ModelTables);

      for i:=0 to DbTables.Count-1 do
      begin
        position:=-1;
        for j:=0 to ModelTables.Count-1 do
          if(CompareText(DbTables[i], TEERTable(ModelTables[j]).ObjName)=0)then
            position:=j;

        //if the table is not in DB-Table List, create it
        if(position=-1)and(Not(KeepExTbls))then
        begin
          Log.Add(DMMain.GetTranslatedMessage('Drop table %s', 155,
            DbTables[i]));
          inc(TableDropCounter);

          DMDB.SchemaSQLQuery.SQL.Text:='drop table '+DbTables[i];
          DMDB.SchemaSQLQuery.ExecSQL;
        end;
      end;

      //Re-reverse order
      DMMain.ReverseList(ModelTables);

      //---------------------
      //Compare columns

      for i:=0 to ModelTables.Count-1 do
      begin
        theTable:=TEERTable(ModelTables[i]);
        Log.Add(DMMain.GetTranslatedMessage('Compare columns from table %s', 156,
          theTable.ObjName));

        SQLStr.Clear;
        NewPrimaryKey:=False;
        DbColumns.Clear;
        IndexColumns.Clear;

        DMDB.SchemaSQLQuery.SQL.Text:='show fields from '+theTable.ObjName;
        DMDB.SchemaSQLQuery.Open;
        while(Not(DMDB.SchemaSQLQuery.EOF))do
        begin
          inc(ColumnCompCounter);
          DbColumns.Add(DMDB.SchemaSQLQuery.Fields[0].AsString);

          //find column
          theColumn:=nil;
          ColNr:=-1;
          ChangeColumnName:=False;
          for j:=0 to theTable.Columns.Count-1 do
            if(CompareText(DMDB.SchemaSQLQuery.Fields[0].AsString, TEERColumn(theTable.Columns[j]).ColName)=0)or
              (CompareText(DMDB.SchemaSQLQuery.Fields[0].AsString, TEERColumn(theTable.Columns[j]).PrevColName)=0)then
            begin
              if(CompareText(DMDB.SchemaSQLQuery.Fields[0].AsString, TEERColumn(theTable.Columns[j]).PrevColName)=0)then
                ChangeColumnName:=True;

              ColNr:=j;
              theColumn:=TEERColumn(theTable.Columns[ColNr]);
              break;
            end;

          //drop column
          if(theColumn=nil)then
          begin
            Log.Add(DMMain.GetTranslatedMessage('Dropping column %s from table %s', 157,
              DMDB.SchemaSQLQuery.Fields[0].AsString, theTable.ObjName));
            inc(ColumnDelCounter);

            SQLStr.Add('ALTER TABLE '+theTable.ObjName+' DROP COLUMN '+DMDB.SchemaSQLQuery.Fields[0].AsString);
          end
          else
          begin
            //Check if column has to be altered
            ColumnChanged:=False;


            //Add to IndexColumns List
            if(DMDB.SchemaSQLQuery.Fields[3].AsString='PRI')then
              IndexColumns.Add(DMDB.SchemaSQLQuery.Fields[0].AsString);

            //check primary key change
            if(theColumn.PrimaryKey=True)and
              (Not(DMDB.SchemaSQLQuery.Fields[3].AsString='PRI'))then
              NewPrimaryKey:=True;

            if(theColumn.PrimaryKey=False)and(DMDB.SchemaSQLQuery.Fields[3].AsString='PRI')then
              NewPrimaryKey:=True;



            //check not null
            if(theColumn.NotNull<>(DMDB.SchemaSQLQuery.Fields[2].AsString<>'Y'))then
              ColumnChanged:=True;

            //theColumn.AutoInc:=False;

            //-------------------------------
            //Get Datatype name from DB
            DatatypeName:=DMDB.SchemaSQLQuery.Fields[1].AsString;
            DatatypeParams:='';
            if(Pos('(', datatypename)>0)then
            begin
              DatatypeName:=Copy(DatatypeName, 1, Pos('(', DatatypeName)-1);
              DatatypeParams:=Copy(DMDB.SchemaSQLQuery.Fields[1].AsString,
                Pos('(', DMDB.SchemaSQLQuery.Fields[1].AsString),
                Pos(')', DMDB.SchemaSQLQuery.Fields[1].AsString)-Pos('(', DMDB.SchemaSQLQuery.Fields[1].AsString)+1);
            end;
            //int unsigned -> just get int
            if(Pos(' ', datatypename)>0)then
              DatatypeName:=Copy(DatatypeName, 1, Pos(' ', DatatypeName)-1);


            //-------------------------------
            //Check DefaultValue

            //if there is no default value
            if(theColumn.DefaultValue='')then
            begin
              //and the defval in db is not 0 and not ''
              if((DMDB.SchemaSQLQuery.Fields[4].AsString<>'0')and(DMDB.SchemaSQLQuery.Fields[4].AsString<>'')and
                (DMDB.SchemaSQLQuery.Fields[4].AsString<>'0000-00-00 00:00:00')and
                (DMDB.SchemaSQLQuery.Fields[4].AsString<>'0000-00-00'))then
                ColumnChanged:=True;
            end
            else
            //if there IS a default value
            begin
              //and it is not equal to the db defval
              if(theColumn.DefaultValue<>DMDB.SchemaSQLQuery.Fields[4].AsString)then
              begin
                if(Comparetext(DatatypeName, 'tinyint')=0)and
                  (((DMDB.SchemaSQLQuery.Fields[4].AsString='0')and(Comparetext(theColumn.DefaultValue, 'False')=0))or
                  ((DMDB.SchemaSQLQuery.Fields[4].AsString='1')and(Comparetext(theColumn.DefaultValue, 'True')=0)))then
                  //Ignore BOOLEAN / tinyint: 0=False, 1=True
                else
                begin
                  //Ignore 0 = 0.00
                  oldDecSep:=DefaultFormatSettings.DecimalSeparator;
                  try
                    DefaultFormatSettings.DecimalSeparator:='.';
                    try
                      if(StrToFloat(theColumn.DefaultValue)<>
                        StrToFloat(DMDB.SchemaSQLQuery.Fields[4].AsString))then
                        ColumnChanged:=True;

                    except
                      ColumnChanged:=True;
                    end;
                  finally
                    DefaultFormatSettings.DecimalSeparator:=oldDecSep;
                  end;
                end;
              end;
            end;

            //-------------------------------
            //Get Model Datatype

            theDatatype:=EERModel.GetDatatypeByName(DatatypeName);
            if(theDatatype<>nil)then
            begin
              theModelDatatype:=TEERDatatype(EERModel.GetDataType(theColumn.idDatatype));

              //if the datatype doesn't match
              if(theColumn.idDatatype<>theDatatype.id)then
              begin
                //Check the SynonymGroup also
                if(Not((theModelDatatype.SynonymGroup=theDatatype.SynonymGroup)and
                  (theDatatype.SynonymGroup>0)))then
                begin
                  //Check if there is an user defined datatype
                  if(Comparetext(theModelDatatype.GetPhysicalTypeName+theColumn.DatatypeParams,
                    theDatatype.GetPhysicalTypeName+DatatypeParams)<>0)then
                  begin
                    try
                      //Ignore the MySQL silent column changes Varchar(<4) -> char(<4)
                      if(Comparetext(theModelDatatype.GetPhysicalTypeName, 'VARCHAR')=0)and
                        (Comparetext(theDatatype.GetPhysicalTypeName, 'CHAR')=0)then
                      begin
                        if(not(StrToInt(Copy(DatatypeParams, 2, Length(DatatypeParams)-2))<4))then
                          ColumnChanged:=True
                      end
                      else
                        //Ignore tinyint(1)=BOOL
                        if(Comparetext(DatatypeName, 'tinyint')=0)and
                          (DatatypeParams='(1)')and
                          (Comparetext(theModelDatatype.GetPhysicalTypeName, 'BOOL')=0)then
                        begin
                          //This is ok.
                        end
                        else
                          ColumnChanged:=True;

                        //!!! Open, Char -> varchar
                    except
                      ColumnChanged:=True;
                    end;
                  end;
                end;
              end;

              //if the Parameter don't match
              if(theColumn.DatatypeParams<>DatatypeParams)then
              begin
                //Tolerate int without params
                //and usertypes
                //int(10) unsigned = INTEGER oder int
                //bigint(20) = BIGINT
                if((DatatypeName='int')and(DatatypeParams='(10)')and((CompareText(theModelDatatype.GetPhysicalTypeName, 'INTEGER')=0)or(CompareText(theModelDatatype.GetPhysicalTypeName, 'INT')=0)))then
                begin
                  //Check unsigned
                  if((Pos(UpperCase(theDatatype.Options[0]),
                    UpperCase(DMDB.SchemaSQLQuery.Fields[1].AsString))>0)<>theColumn.OptionSelected[0])then
                    ColumnChanged:=True;

                  //ShowMessage('Tolerate int without params: '+DatatypeName+DatatypeParams+'='+theModelDatatype.GetPhysicalTypeName)
                end
                else if((DatatypeName='int')and(DatatypeParams='(11)')and((CompareText(theModelDatatype.GetPhysicalTypeName, 'INTEGER')=0)or(CompareText(theModelDatatype.GetPhysicalTypeName, 'INT')=0)))then
                  //tolerate int(11) = INTEGER or int
                  //ShowMessage('tolerate int(11) = INTEGER or int: '+DatatypeName+DatatypeParams+'='+theModelDatatype.GetPhysicalTypeName)
                else if(Comparetext(theModelDatatype.GetPhysicalTypeName+theColumn.DatatypeParams, theDatatype.GetPhysicalTypeName+DatatypeParams)=0)then
                  //tolerate dataTypeName+DatatypeParams match
                  //ShowMessage('tolerate dataTypeName+DatatypeParams: '+DatatypeName+DatatypeParams+'='+theModelDatatype.GetPhysicalTypeName)
                else if((DatatypeName='bigint')and(DatatypeParams='(20)')and(CompareText(theModelDatatype.GetPhysicalTypeName, 'BIGINT')=0))then
                  //tolerate bigint(20) = bigint
                  //ShowMessage('tolerate bigint(20) = bigint: '+DatatypeName+DatatypeParams+'='+theModelDatatype.GetPhysicalTypeName)
                else if((Comparetext(DatatypeName, 'tinyint')=0)and
                  (DatatypeParams='(1)')and
                  (Comparetext(theModelDatatype.GetPhysicalTypeName, 'BOOL')=0))then
                  //tolerate tinyint(1)=BOOL
                else
                  ColumnChanged:=True;
              end;

              //Check Options
              if(Assigned(theDatatype))then
                for j:=0 to theDatatype.OptionCount-1 do
                  if((Pos(UpperCase(theDatatype.Options[j]),
                    UpperCase(DMDB.SchemaSQLQuery.Fields[1].AsString))>0)<>theColumn.OptionSelected[j])then
                    ColumnChanged:=True;
            end
            else
              ColumnChanged:=True;

            if(ColumnChanged)or(ChangeColumnName)then
            begin
              Log.Add(DMMain.GetTranslatedMessage('Modifying column %s from table %s', 158,
                DMDB.SchemaSQLQuery.Fields[0].AsString, theTable.ObjName));
              inc(ColumnModCounter);

              //New name
              if(ChangeColumnName)then
              begin
                SQLStr.Add('ALTER TABLE '+theTable.ObjName+' CHANGE COLUMN '+
                  theColumn.PrevColName+' '+theTable.GetSQLColumnCreateDefCode(ColNr,FieldOnGeneratorOrSequence));

                DbColumns.Delete(DbColumns.IndexOf(theColumn.PrevColName));
                DbColumns.Add(theColumn.ColName);
              end
              //Just change type
              else
                SQLStr.Add('ALTER TABLE '+theTable.ObjName+' MODIFY COLUMN '+
                  theTable.GetSQLColumnCreateDefCode(ColNr,FieldOnGeneratorOrSequence));
            end;
          end;

          DMDB.SchemaSQLQuery.Next;
        end;
        DMDB.SchemaSQLQuery.Close;

        //Check Model columns against DB Columns
        for j:=0 to theTable.Columns.Count-1 do
        begin
          //find column
          theColumn:=TEERColumn(theTable.Columns[j]);
          ColNr:=-1;
          for k:=0 to DbColumns.Count-1 do
            if(Comparetext(theColumn.ColName, DbColumns[k])=0)then
            begin
              ColNr:=k;
              break;
            end;

          //Create new column
          if(ColNr=-1)then
          begin
            Log.Add(DMMain.GetTranslatedMessage('Add Column %s to table %s', 159,
              theColumn.ColName, theTable.ObjName));
            inc(ColumnAddCounter);

            if(j=0)then
              SQLStr.Add('ALTER TABLE '+theTable.ObjName+' ADD COLUMN '+
                theTable.GetSQLColumnCreateDefCode(j,FieldOnGeneratorOrSequence)+' FIRST')
            else
              SQLStr.Add('ALTER TABLE '+theTable.ObjName+' ADD COLUMN '+
                theTable.GetSQLColumnCreateDefCode(j,FieldOnGeneratorOrSequence)+' AFTER '+TEERColumn(theTable.Columns[j-1]).ColName);
          end;
        end;

        //Check count of DB primary key cols against Model pk cols
        if(theTable.Indices.Count>0)then
          if(CompareText(TEERIndex(theTable.Indices[0]).IndexName, 'PRIMARY')=0)then
            //Check if count of PK fields does match
            if(IndexColumns.Count<>TEERIndex(theTable.Indices[0]).Columns.Count)then
              NewPrimaryKey:=True;
            //This is checked above already
            {else
            begin
              //Check if Model PK fields are in DB PK
              for j:=0 to IndexColumns.Count-1 do
              begin
                PKFieldIsChecked:=False;
                for k:=0 to TEERIndex(theTable.Indices[0]).Columns.Count-1 do
                  if(CompareText(TEERColumn(theTable.GetColumnByID(StrToInt(TEERIndex(theTable.Indices[0]).Columns[k]))).ColName, IndexColumns[j])=0)then
                  begin
                    PKFieldIsChecked:=True;
                    break;
                  end;

                if(PKFieldIsChecked=False)then
                  NewPrimaryKey:=True;
              end;
            end;}


        //-------------------------------------
        //Primary Key change
        if(NewPrimaryKey)then
        begin
          SQL2Str.Clear;

          Log.Add(DMMain.GetTranslatedMessage('Change primary key on table %s', 160, theTable.ObjName));
          inc(PKChangedCounter);

          //Drop old Key
          SQL2Str.Add('ALTER TABLE '+theTable.ObjName+' DROP PRIMARY KEY');

          //find primary index
          theIndex:=nil;
          for j:=0 to theTable.Indices.Count-1 do
            if(TEERIndex(theTable.Indices[j]).IndexName='PRIMARY')then
            begin
              theIndex:=TEERIndex(theTable.Indices[j]);
              break;
            end;

          //Create new key
          if(Assigned(theIndex))then
          begin
            s:='ALTER TABLE '+theTable.ObjName+' ADD PRIMARY KEY (';

            for j:=0 to theIndex.Columns.Count-1 do
            begin
              s:=s+TEERColumn(theTable.GetColumnByID(StrToInt(theIndex.Columns[j]))).ColName;

              if(j<theIndex.Columns.Count-1)then
                s:=s+', ';
            end;

            s:=s+')';

            SQL2Str.Add(s);
          end;
        end;

        //Execute the SQL2 Commandos (used for indices)
        //Ignore Errors
        for j:=0 to SQL2Str.Count-1 do
        begin
          DMDB.SchemaSQLQuery.SQL.Text:=SQL2Str[j];
          try
            DMDB.SchemaSQLQuery.ExecSQL(True);
          except
          end;
        end;


        //Execute the SQL Commandos
        for j:=0 to SQLStr.Count-1 do
        begin
          DMDB.SchemaSQLQuery.SQL.Text:=SQLStr[j];
          DMDB.SchemaSQLQuery.ExecSQL(True);
        end;

        //Execute the SQL2 Commandos, again (used for indices)
        //Ignore Errors
        for j:=0 to SQL2Str.Count-1 do
        begin
          DMDB.SchemaSQLQuery.SQL.Text:=SQL2Str[j];
          try
            DMDB.SchemaSQLQuery.ExecSQL(True);
          except
            {on x: Exception do
              ShowMessage(x.Message);}
          end;
        end;


        //Indices

        SQLStr.Clear;
        IndexColumns.Clear;
        IndexColumnParams.Clear;
        IndexRecreate.Clear;
        DBIndices.Clear;
        prevIndex:='';
        theIndex:=nil;
        //IndexComment:='';

        DMDB.SchemaSQLQuery.SQL.Text:='show keys from '+theTable.ObjName;
        DMDB.SchemaSQLQuery.Open;

        while(Not(DMDB.SchemaSQLQuery.EOF))do
        begin
          //ignore primary key
          if(DMDB.SchemaSQLQuery.Fields[2].AsString='PRIMARY')then
          begin
            DMDB.SchemaSQLQuery.Next;
            continue;
          end;

          //collect all columns of the index
          if(prevIndex<>DMDB.SchemaSQLQuery.Fields[2].AsString)then
          begin
            //IndexComment:=DMDB.SchemaSQLQuery.Fields[9].AsString;

            IndexColumns.Clear;

            //find the right index
            theIndex:=nil;

            for j:=0 to theTable.Indices.Count-1 do
              if(CompareText(DMDB.SchemaSQLQuery.Fields[2].AsString,
                TEERIndex(theTable.Indices[j]).IndexName)=0)then
              theIndex:=TEERIndex(theTable.Indices[j]);

            //If the db-index is not found in the model, drop it
            if(theIndex=nil)then
            begin
              Log.Add(DMMain.GetTranslatedMessage('Drop obsolete index %s on table %s', 161,
                DMDB.SchemaSQLQuery.Fields[2].AsString, theTable.ObjName));
              inc(IndexDropCounter);

              SQLStr.Add('ALTER TABLE '+theTable.ObjName+' drop index '+DMDB.SchemaSQLQuery.Fields[2].AsString);
            end
            else
            begin
              //Add index to DBIndex List
              DBIndices.Add(theIndex);

              //if it is found, check params

              //Check unique
              if((DMDB.SchemaSQLQuery.Fields[1].AsInteger=0)and
                (theIndex.IndexKind<>ik_PRIMARY)and
                (theIndex.IndexKind<>ik_UNIQUE_INDEX))or
                ((DMDB.SchemaSQLQuery.Fields[1].AsInteger=1)and
                (theIndex.IndexKind<>ik_INDEX)and
                (theIndex.IndexKind<>ik_FULLTEXT_INDEX))then
              begin
                Log.Add(DMMain.GetTranslatedMessage('Update index %s on table %s', 162,
                  theIndex.IndexName, theTable.ObjName));
                inc(IndexUpdateCounter);

                IndexRecreate.Add(theIndex);
              end;
            end;

          end;

          //Add column-ID
          IndexColumns.Add(IntToStr(
            TEERColumn(theTable.GetColumnByName(DMDB.SchemaSQLQuery.Fields[4].AsString)).Obj_id));
          //Add length parameter if not null
          if(Not(DMDB.SchemaSQLQuery.Fields[7].IsNull))then
            IndexColumnParams.Add(IntToStr(
              TEERColumn(theTable.GetColumnByName(DMDB.SchemaSQLQuery.Fields[4].AsString)).Obj_id)+'='+
              DMDB.SchemaSQLQuery.Fields[7].AsString);

          prevIndex:=DMDB.SchemaSQLQuery.Fields[2].AsString;
          DMDB.SchemaSQLQuery.Next;

          checkIndex:=False;

          //Do the check after EOF or all columns of the index have been listed
          if(DMDB.SchemaSQLQuery.EOF)then
            checkIndex:=True
          else if(prevIndex<>DMDB.SchemaSQLQuery.Fields[2].AsString)then
            checkIndex:=True;

          if(checkIndex)and(theIndex<>nil)then
          begin
            //Check the columns

            //if count of index doesn't match
            if(IndexColumns.Count<>theIndex.Columns.Count)then
            begin
              Log.Add(DMMain.GetTranslatedMessage('Update index %s on table %s', 162,
                theIndex.IndexName, theTable.ObjName));
              inc(IndexUpdateCounter);

              IndexRecreate.Add(theIndex);
            end
            else
            begin
              //check all columns
              for j:=0 to IndexColumns.Count-1 do
              begin
                //if a column doesn't match
                //or length parameter is different (Not when the index is a FULLTEXT index)
                if(IndexColumns[j]<>theIndex.Columns[j])or
                  ((theIndex.IndexKind<>ik_FULLTEXT_INDEX)and
                  (IndexColumnParams.Values[IndexColumns[j]]<>theIndex.ColumnParams.Values[theIndex.Columns[j]]))then
                begin
                  Log.Add(DMMain.GetTranslatedMessage('Update index %s on table %s', 162,
                    theIndex.IndexName, theTable.ObjName));
                  inc(IndexUpdateCounter);

                  IndexRecreate.Add(theIndex);

                  break;
                end;
              end;
            end;
          end;
        end;
        DMDB.SchemaSQLQuery.Close;

        //Recreate indices
        for j:=0 to IndexRecreate.Count-1 do
        begin
          SQLStr.Add('ALTER TABLE '+theTable.ObjName+' drop index '+TEERIndex(IndexRecreate[j]).IndexName);

          //Get all columnnames
          s:='(';
          for k:=0 to TEERIndex(IndexRecreate[j]).Columns.Count-1 do
          begin
            s:=s+TEERColumn(theTable.GetColumnByID(StrToInt(TEERIndex(IndexRecreate[j]).Columns[k]))).ColName;

            if(TEERIndex(IndexRecreate[j]).ColumnParams.Values[TEERIndex(IndexRecreate[j]).Columns[k]]<>'')then
              s:=s+'('+TEERIndex(IndexRecreate[j]).ColumnParams.Values[TEERIndex(IndexRecreate[j]).Columns[k]]+')';

            if(k<TEERIndex(IndexRecreate[j]).Columns.Count-1)then
              s:=s+', ';
          end;
          s:=s+')';

          case TEERIndex(IndexRecreate[j]).IndexKind of
            ik_INDEX:
              SQLStr.Add('ALTER TABLE '+theTable.ObjName+' ADD INDEX '+TEERIndex(IndexRecreate[j]).IndexName+' '+s);
            ik_UNIQUE_INDEX:
              SQLStr.Add('ALTER TABLE '+theTable.ObjName+' ADD UNIQUE '+TEERIndex(IndexRecreate[j]).IndexName+' '+s);
            ik_FULLTEXT_INDEX:
              SQLStr.Add('ALTER TABLE '+theTable.ObjName+' ADD FULLTEXT '+TEERIndex(IndexRecreate[j]).IndexName+' '+s);
          end;
        end;

        //Create new indices
        for j:=0 to theTable.Indices.Count-1 do
        begin
          //If the Model index is not in DB, create it
          if(DBIndices.IndexOf(theTable.Indices[j])=-1)and
            (TEERIndex(theTable.Indices[j]).IndexName<>'PRIMARY')then
          begin
            Log.Add(DMMain.GetTranslatedMessage('Create index %s on table %s', 163,
              TEERIndex(theTable.Indices[j]).IndexName, theTable.ObjName));
            inc(IndexCreateCounter);

            //Get all columnnames
            s:='(';
            for k:=0 to TEERIndex(theTable.Indices[j]).Columns.Count-1 do
            begin
              s:=s+TEERColumn(theTable.GetColumnByID(StrToInt(TEERIndex(theTable.Indices[j]).Columns[k]))).ColName;

              if(TEERIndex(theTable.Indices[j]).ColumnParams.Values[TEERIndex(theTable.Indices[j]).Columns[k]]<>'')then
                s:=s+'('+TEERIndex(theTable.Indices[j]).ColumnParams.Values[TEERIndex(theTable.Indices[j]).Columns[k]]+')';

              if(k<TEERIndex(theTable.Indices[j]).Columns.Count-1)then
                s:=s+', ';
            end;
            s:=s+')';

            case TEERIndex(theTable.Indices[j]).IndexKind of
              ik_INDEX:
                SQLStr.Add('ALTER TABLE '+theTable.ObjName+' ADD INDEX '+TEERIndex(theTable.Indices[j]).IndexName+' '+s);
              ik_UNIQUE_INDEX:
                SQLStr.Add('ALTER TABLE '+theTable.ObjName+' ADD UNIQUE '+TEERIndex(theTable.Indices[j]).IndexName+' '+s);
              ik_FULLTEXT_INDEX:
                SQLStr.Add('ALTER TABLE '+theTable.ObjName+' ADD FULLTEXT '+TEERIndex(theTable.Indices[j]).IndexName+' '+s);
            end;
          end;
        end;


        //Execute the SQL Commandos
        for j:=0 to SQLStr.Count-1 do
        begin
          DMDB.SchemaSQLQuery.SQL.Text:=SQLStr[j];
          DMDB.SchemaSQLQuery.ExecSQL(True);
        end;


        // -----------------------------------------------

        //Clear previous colname for DB-Sync
        for j:=0 to theTable.Columns.Count-1 do
          TEERColumn(theTable.Columns[j]).PrevColName:='';
      end;

      //---------------------
      //Compare Std. Inserts
      if(StdInsertsSync)then
      begin
        for i:=0 to ModelTables.Count-1 do
        begin
          theTable:=TEERTable(ModelTables[i]);

          EERMySQLSyncStdInserts(theTable, Log);
        end;
      end;

    finally
      //Disable Foreign Key checks
      DMDB.ExecSQL('SET FOREIGN_KEY_CHECKS=1');
    end;


    Log.Add(DMMain.GetTranslatedMessage('Syncronisation finished.', 164)+#13#10+
      '-------------------------------------');

    if(ModelTables.Count>1)then
      Log.Add(DMMain.GetTranslatedMessage('%s Tables compared.', 165,
        IntToStr(ModelTables.Count)))
    else
      Log.Add(DMMain.GetTranslatedMessage('1 Table compared.', 166));

    if(ColumnCompCounter>0)then
      if(ColumnCompCounter>1)then
        Log.Add(DMMain.GetTranslatedMessage('%s Columns compared.', 167,
          IntToStr(ColumnCompCounter)))
      else
        Log.Add(DMMain.GetTranslatedMessage('1 Column compared.', 168));

    if(TableCreateCounter>0)then
      if(TableCreateCounter>1)then
        Log.Add(DMMain.GetTranslatedMessage('%s Tables created.', 169,
          IntToStr(TableCreateCounter)))
      else
        Log.Add(DMMain.GetTranslatedMessage('1 Table created.', 170));

    if(TableRenameCounter>0)then
      if(TableRenameCounter>1)then
        Log.Add(DMMain.GetTranslatedMessage('%s Tables renamed.', 171,
          IntToStr(TableRenameCounter)))
      else
        Log.Add(DMMain.GetTranslatedMessage('1 Table renamed.', 172));

    if(TableDropCounter>0)then
      if(TableDropCounter>1)then
        Log.Add(DMMain.GetTranslatedMessage('%s Tables dropped.', 173,
          IntToStr(TableDropCounter)))
      else
        Log.Add(DMMain.GetTranslatedMessage('1 Table dropped.', 174));

    if(ColumnAddCounter>0)then
      if(ColumnAddCounter>1)then
        Log.Add(DMMain.GetTranslatedMessage('%s Columns added.', 175,
          IntToStr(ColumnAddCounter)))
      else
        Log.Add(DMMain.GetTranslatedMessage('1 Column added.', 176));

    if(ColumnDelCounter>0)then
      if(ColumnDelCounter>1)then
        Log.Add(DMMain.GetTranslatedMessage('%s Columns deleted.', 177,
          IntToStr(ColumnDelCounter)))
      else
        Log.Add(DMMain.GetTranslatedMessage('1 Column deleted.', 178));

    if(ColumnModCounter>0)then
      if(ColumnModCounter>1)then
        Log.Add(DMMain.GetTranslatedMessage('%s Columns modified.', 179,
          IntToStr(ColumnModCounter)))
      else
        Log.Add(DMMain.GetTranslatedMessage('1 Column modified.', 180));

    if(IndexDropCounter>0)then
      if(IndexDropCounter>1)then
        Log.Add(DMMain.GetTranslatedMessage('%s Indices dropped.', 181,
          IntToStr(IndexDropCounter)))
      else
        Log.Add(DMMain.GetTranslatedMessage('1 Index dropped.', 182));

    if(IndexCreateCounter>0)then
      if(IndexCreateCounter>1)then
        Log.Add(DMMain.GetTranslatedMessage('%s Indices created.', 183,
          IntToStr(IndexCreateCounter)))
      else
        Log.Add(DMMain.GetTranslatedMessage('1 Index created.', 184));

    if(IndexUpdateCounter>0)then
      if(IndexUpdateCounter>1)then
        Log.Add(DMMain.GetTranslatedMessage('%s Indices updated.', 185,
          IntToStr(IndexUpdateCounter)))
      else
        Log.Add(DMMain.GetTranslatedMessage('1 Index updated.', 186));

    Log.Add('');

    EERModel.ModelHasChanged;
  finally
    DbTables.Free;
    ModelTables.Free;
    DbColumns.Free;
    SQLStr.Free;
    SQL2Str.Free;
    IndexColumnParams.Free;
    IndexColumns.Free;
    IndexRecreate.Free;
    DBIndices.Free;
  end;
end;

procedure TDMDBEER.EERMySQLSyncStdInserts(EERTable: Pointer; var Log: TStrings);
var j, k, PKcount: integer;
  s, s2, wherestr, reasonstr: string;
  doInsert, doDelete: Boolean;
  theInserts: TStringList;
  theTable: TEERTable;
  oldDezSep: Char;
begin
  theTable:=TEERTable(EERTable);

  if(Trim(theTable.StandardInserts.Text)='')then
    Exit;

  PKcount:=0;
  for k:=0 to theTable.Columns.Count-1 do
    if(TEERColumn(theTable.Columns[k]).PrimaryKey)then
      inc(PKcount);

  if(PKcount>0)then
  begin
    Log.Add(DMMain.GetTranslatedMessage('Compare Standard Inserts from table %s', 187,
      theTable.ObjName));

    theInserts:=TStringList.Create;
    try
      //put all lines from insert to one line for theInserts
      s:='';
      for j:=0 to theTable.StandardInserts.Count-1 do
      begin
        s:=s+theTable.StandardInserts[j];

        if(Copy(Trim(s), Length(Trim(s)), 1)=';')then
        begin
          theInserts.Add(Copy(Trim(s), 1, Length(Trim(s))-1));
          s:='';
        end;
      end;


      for j:=0 to theInserts.Count-1 do
      begin
        if(DMMain.GetSubStringCountInString(theInserts[j], '(')<2)then
        begin
          Log.Add(DMMain.GetTranslatedMessage('Insert skipped because of invalid syntax. INSERT INTO table(fields, ...) VALUES(values, ...)', 188));
          Continue;
        end;

        //Build select-where string
        wherestr:='';

        PKcount:=0;
        for k:=0 to theTable.Columns.Count-1 do
        begin
          if(TEERColumn(theTable.Columns[k]).PrimaryKey)then
          begin
            inc(PKcount);

            if(PKcount=1)then
              wherestr:=wherestr+'WHERE '
            else
              wherestr:=wherestr+' AND ';

            wherestr:=wherestr+
              TEERColumn(theTable.Columns[k]).ColName+'='''+
              DMMain.GetValueFromSQLInsert(TEERColumn(theTable.Columns[k]).ColName, theInserts[j])+'''';
          end;
        end;


        doDelete:=False;
        doInsert:=False;

        //Compare values
        DMDB.SchemaSQLQuery.SQL.Text:='SELECT * FROM '+theTable.ObjName+' '+
          wherestr;
        DMDB.SchemaSQLQuery.Open;
        if(DMDB.SchemaSQLQuery.EOF)then
          doInsert:=True;

        while(Not(DMDB.SchemaSQLQuery.EOF))and(Not(doInsert))do
        begin
          try
            for k:=0 to theTable.Columns.Count-1 do
            begin
              s:=UpperCase(DMMain.GetValueFromSQLInsert(TEERColumn(theTable.Columns[k]).ColName, theInserts[j]));

              s2:=UpperCase(DMDB.SchemaSQLQuery.FieldByName(TEERColumn(theTable.Columns[k]).ColName).AsString);

              //Reformat Date values
              if(DMDB.SchemaSQLQuery.FieldByName(TEERColumn(theTable.Columns[k]).ColName).DataType=ftDate)then
              begin
                //Ignore missing 0s
                if(Length(s)<Length('yyyy-mm-dd'))then
                  s2:=FormatDateTime('yyyy-m-d', DMDB.SchemaSQLQuery.FieldByName(TEERColumn(theTable.Columns[k]).ColName).AsDateTime)
                else
                  s2:=FormatDateTime('yyyy-mm-dd', DMDB.SchemaSQLQuery.FieldByName(TEERColumn(theTable.Columns[k]).ColName).AsDateTime);
              end;
              if(DMDB.SchemaSQLQuery.FieldByName(TEERColumn(theTable.Columns[k]).ColName).DataType=ftDateTime)or
                (DMDB.SchemaSQLQuery.FieldByName(TEERColumn(theTable.Columns[k]).ColName).DataType=ftTimeStamp)then
              begin
                if(Length(s)<Length('yyyy-mm-dd'))then
                  s2:=FormatDateTime('yyyy-m-d hh:nn:ss', DMDB.SchemaSQLQuery.FieldByName(TEERColumn(theTable.Columns[k]).ColName).AsDateTime)
                else
                  s2:=FormatDateTime('yyyy-mm-dd hh:nn:ss', DMDB.SchemaSQLQuery.FieldByName(TEERColumn(theTable.Columns[k]).ColName).AsDateTime);

                if(s<>s2)then
                  s:=s+' 00:00:00';
              end;
              //Reformat FloatValues
              if(DMDB.SchemaSQLQuery.FieldByName(TEERColumn(theTable.Columns[k]).ColName).DataType=ftFloat)and
                (s<>s2)then
              begin
                try
                  if(DMDB.SchemaSQLQuery.FieldByName(TEERColumn(theTable.Columns[k]).ColName).AsFloat=
                    StrToFloat(s))then
                    s2:=s;
                except
                  oldDezSep:=DefaultFormatSettings.DecimalSeparator;
                  try
                    DefaultFormatSettings.DecimalSeparator:=',';
                    try
                      if(DMDB.SchemaSQLQuery.FieldByName(TEERColumn(theTable.Columns[k]).ColName).AsFloat=
                        StrToFloat(s))then
                        s2:=s;
                    except
                      DefaultFormatSettings.DecimalSeparator:='.';
                      try
                        if(FloatToStr(DMDB.SchemaSQLQuery.FieldByName(TEERColumn(theTable.Columns[k]).ColName).AsFloat)=
                          FloatToStr(StrToFloat(s)))then
                          s2:=s;
                      except
                      end;
                    end;
                  finally
                    DefaultFormatSettings.DecimalSeparator:=oldDezSep;
                  end;
                end;
              end;

              if(s<>s2)and(s<>''''+s2+'''')and
                (Not((s='NULL')and(s2='')))and(s<>'NOTININSERT')then
              begin
                doDelete:=True;
                doInsert:=True;

                reasonstr:=s+'<>'+s2;
              end;
            end;
          except
            doDelete:=True;
            doInsert:=True;
          end;

          DMDB.SchemaSQLQuery.Next;
        end;
        DMDB.SchemaSQLQuery.Close;

        //if the values don't match, delete row if exists
        if(doDelete)then
        begin
          Log.Add(DMMain.GetTranslatedMessage('Change values %s because %s', 189,
            wherestr, reasonstr));

          DMDB.ExecSQL('DELETE FROM '+theTable.ObjName+' '+
            wherestr);
        end;

        //execute insert
        if(doInsert)then
        begin
          if(not(doDelete))then
            Log.Add('Insert values '+wherestr);

          DMDB.ExecSQL(theInserts[j]);
        end;
      end;
    finally
      theInserts.Free;
    end;
  end
  else
  begin
    Log.Add(DMMain.GetTranslatedMessage('Compare Standard Inserts from table %s skipped, no PK.', 190,
      theTable.ObjName));
  end;

end;

function TDMDBEER.RemoveCommentsFromSQLCmd(cmd: string): string;
var s: string;
begin
  s:=cmd;

  while(Pos('/*', s)>0)do
    //if there is a closing */
    if(Pos('*/', s)>0)then
    begin
      s:=Copy(s, 1, Pos('/*', s))+Copy(s, Pos('*/', s)+2, Length(s));
    end
    else
      s:=Copy(s, 1, Pos('/*', s));

  while(Pos('--', s)>0)do
    if(Pos(#13, s)>0)then
      s:=Copy(s, 1, Pos('--', s))+Copy(s, Pos(#13, s), Length(s))
    else
      s:=Copy(s, 1, Pos('--', s));

  while(Pos('//', s)>0)do
    if(Pos(#13, s)>0)then
      s:=Copy(s, 1, Pos('//', s))+Copy(s, Pos(#13, s), Length(s))
    else
      s:=Copy(s, 1, Pos('//', s));

  result := s;
end;


procedure TDMDBEER.EERMySQLReverseEngineer2(theModel: Pointer; DBConn: Pointer; theTables: TStringList; XCount: integer; BuildRelations: Boolean; BuildRelUsingPrimKey: Boolean; DatatypeSubst: TStringList; StatusLbl: TLabel = nil; CreateStdInserts: Boolean = False; limitStdIns: integer = 0);
var EERModel: TEERModel;
  i{, j, xpos, ypos, xanz, defwidth, defheight}: integer;
  DbTables: TList;
  theTable: TEERTable;
  {theColumn: TEERColumn;
  theIndex: TEERIndex;
  theDatatype: TEERDatatype;
  DatatypeName, DatatypeParams, prevIndex: string;
  tblAtPos: Boolean;}
  tableCreateStatement: string;
  DBQuoteCharacter: string;
begin
  EERModel:=theModel;

  DBQuoteCharacter:='`';

  {defwidth:=250;
  defheight:=160;
  xanz:=XCount;
  xpos:=0;
  ypos:=0;}

  if(StatusLbl<>nil)then
  begin
    StatusLbl.Caption:=DMMain.GetTranslatedMessage('Fetching Tables', 147);
    StatusLbl.Refresh;
    Application.ProcessMessages;
  end;

  //Get Tables
  DbTables:=TList.Create;
  try
    DMDB.SchemaSQLQuery.SetSchemaInfo(stNoSchema, '', '');
    DMDB.SchemaSQLQuery.SQL.Text:='show tables';
    DMDB.SchemaSQLQuery.Open;

    while(Not(DMDB.SchemaSQLQuery.EOF))do
    begin
      //get only selected tables
      if(theTables.IndexOf(DMDB.SchemaSQLQuery.Fields[0].AsString)<>-1)then
      begin
        theTable:=EERModel.NewTable(EERModel.EERModel_Width-250, 0, False);
        theTable.ObjName:=DMDB.SchemaSQLQuery.Fields[0].AsString;

        DbTables.Add(theTable);
      end;

      DMDB.SchemaSQLQuery.Next;
    end;
    DMDB.SchemaSQLQuery.Close;

    //Get every DbTable
    for i:=0 to DbTables.Count-1 do
    begin
      DMDB.SchemaSQLQuery.SQL.Text:='show create table '+
        DBQuoteCharacter+TEERTable(DbTables[i]).ObjName+DBQuoteCharacter;
      DMDB.SchemaSQLQuery.Open;
      tableCreateStatement:=DMDB.SchemaSQLQuery.Fields[1].AsString;
      DMDB.SchemaSQLQuery.Close;

      BuildTableFromCreateStatement(DbTables[i], tableCreateStatement, MySQL);
    end;

  finally
    DbTables.Free;
  end;
end;

procedure TDMDBEER.BuildTableFromCreateStatement(theTable: TEERTable; theCreateStatement: string; Syntax: CreateTableSyntax);
var r : TRegExpr;
  //colstr: string;
begin
  theCreateStatement:=Trim(theCreateStatement);

  r:=TRegExpr.Create;
  try
    //MySQL CreateTableSyntax
    if(Syntax=MySQL)then
    begin
      {if(UpperCase(Copy(theCreateStatement, 1, 7))<>'CREATE ')then
        raise EDatabaseError.Create('A Create Table Command has to start with CREATE.');}

      //Find Column-Definition
      r.Expression:='(?img)(\w+\b)\s{0,}(TINYINT|SMALLINT|FLOAT){1}\s{0,}(\((\d+)(\s*,\s*(\d+))?\)){0,}\s*(UNSIGNED)?\s*(ZEROFILL)?\s*(BINARY)?\s*,?';
      if(r.Exec('idinserat smallint(15) UNSIGNED,'+#13#10+
        'gewicht TINYINT bZEROFILL'))then
      begin
        repeat
          ShowMessage(r.Match[0]);
        until not r.ExecNext;
      end;

    end
    else
      Exit;
  finally
    r.Free;
  end;
end;

end.
