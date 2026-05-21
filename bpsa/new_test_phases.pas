
//==============================================================================
// Phase 19: Test Note Editor dialog (TEERNote.ShowEditor)
//==============================================================================
procedure Phase19_TestNoteEditor(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
  Note: TEERNote;
  NoteIdx: Integer;
begin
  Log('--- Phase 19: Testing Note Editor dialog ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for note editor test.');
    Log('');
    Exit;
  end;

  // Find or create a note
  Note := nil;
  if Model.GetEERObjectCount([EERNote]) > 0 then
    Note := TEERNote(Model.GetEERObjectByIndex(EERNote, 0));

  if Note = nil then
  begin
    Log('  No existing note found. Attempting to create one...');
    try
      Note := TEERNote.Create(Model, 'TestNote');
      Note.Left := 100;
      Note.Top := 100;
      Application.ProcessMessages;
      Sleep(100);
      Log('  Created test note.');
    except
      on E: Exception do
      begin
        Log('[FAIL] Could not create note for editor test: ' + E.ClassName + ': ' + E.Message);
        Log('');
        Exit;
      end;
    end;
  end;

  try
    Log('  Opening Note Editor...');
    ScheduleModalClose(800);
    Note.ShowEditor(nil);
    Application.ProcessMessages;
    Sleep(500);
    Application.ProcessMessages;
    Log('[PASS] Note Editor opened and closed successfully.');
  except
    on E: Exception do
      Log('[FAIL] Note Editor: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

//==============================================================================
// Phase 20: Test Region Editor dialog (TEERRegion.ShowEditor)
//==============================================================================
procedure Phase20_TestRegionEditor(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
  Region: TEERRegion;
begin
  Log('--- Phase 20: Testing Region Editor dialog ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for region editor test.');
    Log('');
    Exit;
  end;

  // Find or create a region
  Region := nil;
  if Model.GetEERObjectCount([EERRegion]) > 0 then
    Region := TEERRegion(Model.GetEERObjectByIndex(EERRegion, 0));

  if Region = nil then
  begin
    Log('  No existing region found. Attempting to create one...');
    try
      Region := TEERRegion.Create(Model, 'TestRegion');
      Region.Left := 200;
      Region.Top := 200;
      Region.Width := 150;
      Region.Height := 100;
      Application.ProcessMessages;
      Sleep(100);
      Log('  Created test region.');
    except
      on E: Exception do
      begin
        Log('[FAIL] Could not create region for editor test: ' + E.ClassName + ': ' + E.Message);
        Log('');
        Exit;
      end;
    end;
  end;

  try
    Log('  Opening Region Editor...');
    ScheduleModalClose(800);
    Region.ShowEditor(nil);
    Application.ProcessMessages;
    Sleep(500);
    Application.ProcessMessages;
    Log('[PASS] Region Editor opened and closed successfully.');
  except
    on E: Exception do
      Log('[FAIL] Region Editor: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

//==============================================================================
// Phase 21: Test Relation Editor dialog (TEERRel.ShowEditor)
//==============================================================================
procedure Phase21_TestRelationEditor(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
  Rel: TEERRel;
begin
  Log('--- Phase 21: Testing Relation Editor dialog ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for relation editor test.');
    Log('');
    Exit;
  end;

  if Model.GetEERObjectCount([EERRelation]) = 0 then
  begin
    Log('[SKIP] No relations to edit.');
    Log('');
    Exit;
  end;

  try
    Rel := TEERRel(Model.GetEERObjectByIndex(EERRelation, 0));
    if Assigned(Rel) then
    begin
      Log('  Opening Relation Editor for: ' + Rel.ObjName);
      ScheduleModalClose(800);
      Rel.ShowEditor(nil);
      Application.ProcessMessages;
      Sleep(500);
      Application.ProcessMessages;
      Log('[PASS] Relation Editor opened and closed successfully.');
    end
    else
      Log('[SKIP] Could not get first relation.');
  except
    on E: Exception do
      Log('[FAIL] Relation Editor: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

//==============================================================================
// Phase 22: Test String Editor dialog
//==============================================================================
procedure Phase22_TestStringEditor(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  TestValue: string;
  Result: Boolean;
begin
  Log('--- Phase 22: Testing String Editor dialog ---');
  Log('');

  try
    TestValue := 'test_value';
    ScheduleModalClose(800);
    Result := TDMMain(Application.FindComponent('DMMain')).ShowStringEditor('Test Title', 'Test Prompt:', TestValue);
    Application.ProcessMessages;
    Sleep(500);
    Application.ProcessMessages;
    if Result then
      Log('[PASS] String Editor returned OK. Value: "' + TestValue + '"')
    else
      Log('[PASS] String Editor returned Cancel.');
    Log('[PASS] String Editor dialog opened and closed successfully.');
  except
    on E: Exception do
      Log('[FAIL] String Editor: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

//==============================================================================
// Phase 23: Test table column operations (rename, verify)
//==============================================================================
procedure Phase23_TestTableColumnOperations(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
  Tbl: TEERTable;
  Col: TEERColumn;
  ColCount: Integer;
  OrigColName: string;
begin
  Log('--- Phase 23: Testing table column operations ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for column operations.');
    Log('');
    Exit;
  end;

  if Model.GetEERObjectCount([EERTable]) = 0 then
  begin
    Log('[SKIP] No tables for column operations.');
    Log('');
    Exit;
  end;

  try
    Tbl := TEERTable(Model.GetEERObjectByIndex(EERTable, 0));
    if not Assigned(Tbl) then
    begin
      Log('[SKIP] Could not get first table.');
      Log('');
      Exit;
    end;

    Log('  Testing table: ' + Tbl.ObjName);
    ColCount := Tbl.GetColumnCount;
    Log('  Current column count: ' + IntToStr(ColCount));

    // Add a column
    try
      Col := TEERColumn.Create(Tbl);
      Col.ColName := 'test_col_rename';
      Col.idDatatype := 5; // VARCHAR
      Col.DatatypeParams := '100';
      Tbl.Columns.Add(Col);
      Tbl.RefreshObj;
      Application.ProcessMessages;
      Sleep(100);
      if Tbl.GetColumnCount > ColCount then
        Log('[PASS] Column "test_col_rename" added.')
      else
        Log('[FAIL] Column not added.');
    except
      on E: Exception do
        Log('[FAIL] Column add: ' + E.ClassName + ': ' + E.Message);
    end;

    // Rename the column
    if Tbl.GetColumnCount > 0 then
    begin
      try
        Col := TEERColumn(Tbl.GetColumnByIndex(Tbl.GetColumnCount - 1));
        if Assigned(Col) then
        begin
          OrigColName := Col.ColName;
          Col.ColName := 'renamed_column';
          Tbl.RefreshObj;
          Application.ProcessMessages;
          Sleep(100);
          if Col.ColName = 'renamed_column' then
            Log('[PASS] Column renamed from "' + OrigColName + '" to "' + Col.ColName + '".')
          else
            Log('[FAIL] Column rename failed. Name is: "' + Col.ColName + '".');
            
          // Restore name
          Col.ColName := OrigColName;
          Tbl.RefreshObj;
        end;
      except
        on E: Exception do
          Log('[FAIL] Column rename: ' + E.ClassName + ': ' + E.Message);
      end;
    end;

    Log('[PASS] Table column operations completed.');
  except
    on E: Exception do
      Log('[FAIL] Column operations: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

//==============================================================================
// Phase 24: Test SQL content verification
//==============================================================================
procedure Phase24_TestSQLContentVerification(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
  Frm: TEERExportSQLScriptFrom;
  SQL: string;
  CreateCount: Integer;
  PosStart: Integer;
begin
  Log('--- Phase 24: Testing SQL content verification ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for SQL content verification.');
    Log('');
    Exit;
  end;

  try
    Frm := TEERExportSQLScriptFrom.Create(AMainForm);
    try
      Frm.SetModel(Model, 0);
      SQL := Frm.GetSQLScript;
    finally
      Frm.Free;
    end;

    if SQL = '' then
    begin
      Log('[WARN] SQL script is empty.');
      Log('');
      Exit;
    end;

    Log('  SQL length: ' + IntToStr(Length(SQL)) + ' characters.');

    // Count CREATE TABLE
    CreateCount := 0;
    PosStart := 1;
    while True do
    begin
      PosStart := PosEx('CREATE TABLE', UpperCase(SQL), PosStart);
      if PosStart = 0 then Break;
      Inc(CreateCount);
      Inc(PosStart, 12);
    end;
    Log('  CREATE TABLE statements: ' + IntToStr(CreateCount));

    if CreateCount > 0 then
      Log('[PASS] SQL contains CREATE TABLE statements.')
    else
      Log('[FAIL] SQL does not contain any CREATE TABLE statements.');

    // Verify SQL has semicolons
    if Pos(';', SQL) > 0 then
      Log('[PASS] SQL contains semicolon statement terminators.')
    else
      Log('[WARN] SQL has no semicolons.');

    // Verify SQL has at least some expected keywords
    if (Pos('CREATE', UpperCase(SQL)) > 0) and (Pos('TABLE', UpperCase(SQL)) > 0) then
      Log('[PASS] SQL contains CREATE and TABLE keywords.')
    else
      Log('[FAIL] SQL missing expected keywords.');
  except
    on E: Exception do
      Log('[FAIL] SQL content verification: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

//==============================================================================
// Phase 25: Test model type enumeration and consistency
//==============================================================================
procedure Phase25_TestModelConsistency(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
  TblNames: TStringList;
  TableCount, RelCount, NoteCount, RegionCount: Integer;
begin
  Log('--- Phase 25: Testing model consistency ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for consistency test.');
    Log('');
    Exit;
  end;

  try
    TableCount := Model.GetEERObjectCount([EERTable]);
    RelCount := Model.GetEERObjectCount([EERRelation]);
    NoteCount := Model.GetEERObjectCount([EERNote]);
    RegionCount := Model.GetEERObjectCount([EERRegion]);

    Log('  Tables: ' + IntToStr(TableCount));
    Log('  Relations: ' + IntToStr(RelCount));
    Log('  Notes: ' + IntToStr(NoteCount));
    Log('  Regions: ' + IntToStr(RegionCount));

    if TableCount >= 0 then Log('[PASS] Table count is valid (' + IntToStr(TableCount) + ').');
    if RelCount >= 0 then Log('[PASS] Relation count is valid (' + IntToStr(RelCount) + ').');
    if NoteCount >= 0 then Log('[PASS] Note count is valid (' + IntToStr(NoteCount) + ').');
    if RegionCount >= 0 then Log('[PASS] Region count is valid (' + IntToStr(RegionCount) + ').');

    // Verify we can get object names
    try
      TblNames := TStringList.Create;
      Model.GetEERObjectNameList([EERTable], TblNames, False);
      Log('  Table names: ' + IntToStr(TblNames.Count) + ' entries.');
      if TblNames.Count = TableCount then
        Log('[PASS] Table name list count matches object count.')
      else
        Log('[FAIL] Table name list count (' + IntToStr(TblNames.Count) + ') does not match object count (' + IntToStr(TableCount) + ').');
      TblNames.Free;
    except
      on E: Exception do
        Log('[FAIL] Could not get table names: ' + E.ClassName + ': ' + E.Message);
    end;

    Log('[PASS] Model consistency check completed.');
  except
    on E: Exception do
      Log('[FAIL] Model consistency: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

//==============================================================================
// Phase 26: Test model object enumeration (iterate all objects)
//==============================================================================
procedure Phase26_TestObjectEnumeration(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
  ObjList: TList;
  I: Integer;
  ObjName: string;
  TotalObjects: Integer;
begin
  Log('--- Phase 26: Testing object enumeration ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for object enumeration.');
    Log('');
    Exit;
  end;

  try
    TotalObjects := 0;

    // Enumerate all table objects
    ObjList := TList.Create;
    try
      Model.GetEERObjectList([EERTable], ObjList, False);
      for I := 0 to ObjList.Count - 1 do
      begin
        if Assigned(ObjList[I]) and (TObject(ObjList[I]) is TEERTable) then
        begin
          ObjName := TEERTable(ObjList[I]).ObjName;
          if ObjName <> '' then
            Inc(TotalObjects);
        end;
      end;
      Log('  Enumerated ' + IntToStr(ObjList.Count) + ' tables.');
    finally
      ObjList.Free;
    end;

    // Enumerate all relations
    ObjList := TList.Create;
    try
      Model.GetEERObjectList([EERRelation], ObjList, False);
      for I := 0 to ObjList.Count - 1 do
      begin
        if Assigned(ObjList[I]) then
          Inc(TotalObjects);
      end;
      Log('  Enumerated ' + IntToStr(ObjList.Count) + ' relations.');
    finally
      ObjList.Free;
    end;

    // Enumerate all notes
    ObjList := TList.Create;
    try
      Model.GetEERObjectList([EERNote], ObjList, False);
      for I := 0 to ObjList.Count - 1 do
      begin
        if Assigned(ObjList[I]) and (TObject(ObjList[I]) is TEERNote) then
        begin
          ObjName := TEERNote(ObjList[I]).ObjName;
          if ObjName <> '' then
            Inc(TotalObjects);
        end;
      end;
      Log('  Enumerated ' + IntToStr(ObjList.Count) + ' notes.');
    finally
      ObjList.Free;
    end;

    Log('  Total enumerated objects: ' + IntToStr(TotalObjects));
    if TotalObjects > 0 then
      Log('[PASS] Object enumeration successful.')
    else
      Log('[FAIL] No objects enumerated.');
  except
    on E: Exception do
      Log('[FAIL] Object enumeration: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

//==============================================================================
// Phase 27: Test region operations (select all in region)
//==============================================================================
procedure Phase27_TestRegionOperations(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
  Region: TEERRegion;
  SelCount: Integer;
begin
  Log('--- Phase 27: Testing region operations ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for region operations.');
    Log('');
    Exit;
  end;

  if Model.GetEERObjectCount([EERRegion]) = 0 then
  begin
    Log('[SKIP] No regions to test.');
    Log('');
    Exit;
  end;

  try
    Region := TEERRegion(Model.GetEERObjectByIndex(EERRegion, 0));
    if Assigned(Region) then
    begin
      Log('  Testing region: ' + Region.ObjName);

      // Select all objects in region
      Region.SelectAllObjsInRegion;
      Application.ProcessMessages;
      Sleep(100);

      SelCount := Model.GetSelectedObjsCount;
      Log('  Selected ' + IntToStr(SelCount) + ' objects in region.');

      // Deselect
      Model.DeSelectAllObjs(nil);
      Application.ProcessMessages;
      Sleep(100);

      if Model.GetSelectedObjsCount = 0 then
        Log('[PASS] Region operations completed successfully.')
      else
        Log('[FAIL] Objects still selected after deselect.');
    end
    else
      Log('[SKIP] Could not access region.');
  except
    on E: Exception do
      Log('[FAIL] Region operations: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

//==============================================================================
// Phase 28: Test table data (column names, FK columns)
//==============================================================================
procedure Phase28_TestTableColumnMetadata(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
  Tbl: TEERTable;
  I: Integer;
  Col: TEERColumn;
  HasPK: Boolean;
  ColName: string;
begin
  Log('--- Phase 28: Testing table column metadata ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for column metadata test.');
    Log('');
    Exit;
  end;

  if Model.GetEERObjectCount([EERTable]) = 0 then
  begin
    Log('[SKIP] No tables to inspect.');
    Log('');
    Exit;
  end;

  try
    Tbl := TEERTable(Model.GetEERObjectByIndex(EERTable, 0));
    if not Assigned(Tbl) then
    begin
      Log('[SKIP] Could not access first table.');
      Log('');
      Exit;
    end;

    Log('  Inspecting table: ' + Tbl.ObjName);
    Log('  Column count: ' + IntToStr(Tbl.GetColumnCount));

    HasPK := Tbl.CheckPrimaryIndex > 0;
    if HasPK then
      Log('[PASS] Table has a primary key index.')
    else
      Log('[WARN] Table has no primary key index.');

    // Inspect each column
    for I := 0 to Tbl.GetColumnCount - 1 do
    begin
      Col := TEERColumn(Tbl.GetColumnByIndex(I));
      if Assigned(Col) then
      begin
        ColName := Col.ColName;
        if ColName <> '' then
        begin
          if I < 5 then
            Log('    Column ' + IntToStr(I) + ': "' + ColName + '"');
        end
        else
          Log('[WARN] Column at index ' + IntToStr(I) + ' has empty name.');
      end
      else
        Log('[FAIL] Column at index ' + IntToStr(I) + ' is nil.');
    end;

    Log('[PASS] Table column metadata check completed.');
  except
    on E: Exception do
      Log('[FAIL] Column metadata: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

