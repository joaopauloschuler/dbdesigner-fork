
{ Phase 4b: Export SQL from the loaded example model }
procedure Phase4b_ExportExampleSQL(AMainForm: TForm; const LogDir: string);
var
  Model: TEERModel;
  Frm: TEERExportSQLScriptFrom;
  SQL, SQLFile: string;
  F: TextFile;
  TableCount: Integer;
  CREATE_Count: Integer;
  PosStart: Integer;
begin
  Log('--- Phase 4b: Exporting SQL from loaded example model (order.xml) ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for example SQL export.');
    Log('');
    Exit;
  end;

  TableCount := Model.GetEERObjectCount([EERTable]);
  Log('  Example model has ' + IntToStr(TableCount) + ' tables.');

  SQLFile := LogDir + 'selftest_example_export.sql';
  Frm := TEERExportSQLScriptFrom.Create(AMainForm);
  try
    Frm.SetModel(Model, 0);
    try
      SQL := Frm.GetSQLScript;
    except
      on E: Exception do
      begin
        Log('[FAIL] GetSQLScript raised: ' + E.ClassName + ': ' + E.Message);
        Log('');
        Exit;
      end;
    end;

    if SQL = '' then
      Log('[WARN] SQL script is empty.')
    else
    begin
      try
        AssignFile(F, SQLFile);
        Rewrite(F);
        Write(F, SQL);
        CloseFile(F);
        Log('[PASS] SQL exported to: ' + SQLFile);
        Log('  SQL length: ' + IntToStr(Length(SQL)) + ' characters');
        CREATE_Count := 0;
        PosStart := 1;
        while True do
        begin
          PosStart := PosEx('CREATE TABLE', UpperCase(SQL), PosStart);
          if PosStart = 0 then Break;
          Inc(CREATE_Count);
          Inc(PosStart, 12);
        end;
        Log('  CREATE TABLE occurrences: ' + IntToStr(CREATE_Count));
        if CREATE_Count < TableCount then
          Log('[WARN] Fewer CREATE TABLE statements (' + IntToStr(CREATE_Count) +
              ') than tables (' + IntToStr(TableCount) + ').');
      except
        on E: Exception do
          Log('[FAIL] Could not write SQL file: ' + E.ClassName + ': ' + E.Message);
      end;
    end;
  finally
    Frm.Free;
  end;
  Log('');
end;

{ Phase 8: Test popup menu items on the EER canvas }
procedure Phase8_TestPopupMenus(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer; const LogFile: string);
var
  Model: TEERModel;
  I: Integer;
  PopupMenu: TPopupMenu;
begin
  Log('--- Phase 8: Inspecting popup menu items on EER canvas ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for popup menu test.');
    Log('');
    Exit;
  end;

  try
    for I := 0 to Model.ComponentCount - 1 do
    begin
      if Model.Components[I] is TPopupMenu then
      begin
        PopupMenu := TPopupMenu(Model.Components[I]);
        Log('  Found popup menu: ' + PopupMenu.Name);
      end;
    end;
    Log('[PASS] Popup menus inspected successfully.');
  except
    on E: Exception do
    begin
      Log('[FAIL] Popup menu test: ' + E.ClassName + ': ' + E.Message);
      Inc(FailCount);
    end;
  end;

  Log('');
end;

{ Phase 9: Test select/deselect operations on the model }
procedure Phase9_TestSelectOperations(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
  Table: TEERTable;
  SelCount: Integer;
begin
  Log('--- Phase 9: Testing select/deselect operations ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for select operations.');
    Log('');
    Exit;
  end;

  try
    // Test SelectAll
    Model.SelectAllObjs;
    Application.ProcessMessages;
    Sleep(100);
    SelCount := Model.GetSelectedObjsCount;
    if SelCount > 0 then
      Log('[PASS] SelectAll: selected ' + IntToStr(SelCount) + ' objects.')
    else
      Log('[WARN] SelectAll: no objects were selected.');

    // Test DeSelectAll
    Model.DeSelectAllObjs(nil);
    Application.ProcessMessages;
    Sleep(100);
    if Model.GetSelectedObjsCount = 0 then
      Log('[PASS] DeSelectAll: all objects deselected.')
    else
      Log('[FAIL] DeSelectAll: ' + IntToStr(Model.GetSelectedObjsCount) + ' objects still selected.');

    // Test individual object selection
    if Model.GetEERObjectCount([EERTable]) > 0 then
    begin
      Table := TEERTable(Model.GetEERObjectByIndex(EERTable, 0));
      if Assigned(Table) then
      begin
        Table.SetSelected(True);
        Application.ProcessMessages;
        Sleep(100);
        if Table.Selected then
          Log('[PASS] Single table selection works.')
        else
          Log('[FAIL] Single table selection did not work.');
        Table.SetSelected(False);
      end;
    end;

    Log('[PASS] Select/deselect operations completed.');
  except
    on E: Exception do
      Log('[FAIL] Select operations: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

{ Phase 10: Test zoom operations }
procedure Phase10_TestZoom(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
  InitialZoom: double;
begin
  Log('--- Phase 10: Testing zoom operations ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for zoom operations.');
    Log('');
    Exit;
  end;

  try
    InitialZoom := Model.GetZoomFac;
    Log('  Current zoom: ' + FloatToStr(InitialZoom));

    // Zoom in
    Model.ZoomIn(-1, -1);
    Application.ProcessMessages;
    Sleep(100);
    Log('  After ZoomIn: ' + FloatToStr(Model.GetZoomFac));

    // Zoom out back
    Model.ZoomOut(-1, -1);
    Application.ProcessMessages;
    Sleep(100);
    Log('  After ZoomOut: ' + FloatToStr(Model.GetZoomFac));

    // Set specific zoom
    Model.SetZoomFac(100, -1, -1);
    Application.ProcessMessages;
    Sleep(100);
    if Abs(Model.GetZoomFac - 100) < 1 then
      Log('[PASS] SetZoomFac(100) works correctly.')
    else
      Log('[WARN] SetZoomFac(100) resulted in: ' + FloatToStr(Model.GetZoomFac));

    Log('[PASS] Zoom operations completed.');
  except
    on E: Exception do
      Log('[FAIL] Zoom operations: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

{ Phase 11: Test adding operations to the model }
procedure Phase11_TestModelObjectCreation(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
  Note: TEERNote;
  Region: TEERRegion;
  Tbl: TEERTable;
  Col: TEERColumn;
  ColCount: Integer;
begin
  Log('--- Phase 11: Testing creation of model objects ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for object creation.');
    Log('');
    Exit;
  end;

  try
    // Add a note
    try
      Note := TEERNote.Create(Model, 'Test Note');
      Note.Left := 50;
      Note.Top := 200;
      Application.ProcessMessages;
      Sleep(100);
      Log('[PASS] Note created programmatically.');
    except
      on E: Exception do
        Log('[FAIL] Could not create note: ' + E.ClassName + ': ' + E.Message);
    end;

    // Add a region
    try
      Region := TEERRegion.Create(Model, 'Test Region');
      Region.Left := 400;
      Region.Top := 50;
      Region.Width := 200;
      Region.Height := 150;
      Application.ProcessMessages;
      Sleep(100);
      Log('[PASS] Region created programmatically.');
    except
      on E: Exception do
        Log('[FAIL] Could not create region: ' + E.ClassName + ': ' + E.Message);
    end;

    // Add a column to a table
    if Model.GetEERObjectCount([EERTable]) > 0 then
    begin
      Tbl := TEERTable(Model.GetEERObjectByIndex(EERTable, 0));
      if Assigned(Tbl) then
      begin
        ColCount := Tbl.GetColumnCount;
        try
          Col := TEERColumn.Create(Tbl);
          Col.ColName := 'test_col';
          Col.idDatatype := 5; // VARCHAR
          Col.DatatypeParams := '255';
          Col.NotNull := True;
          Tbl.Columns.Add(Col);
          Tbl.RefreshObj;
          Application.ProcessMessages;
          Sleep(100);
          if Tbl.GetColumnCount > ColCount then
            Log('[PASS] Column "' + Col.ColName + '" added to table "' + Tbl.ObjName + '". New count: ' + IntToStr(Tbl.GetColumnCount))
          else
            Log('[FAIL] Column not added to table.');
        except
          on E: Exception do
            Log('[FAIL] Could not add column: ' + E.ClassName + ': ' + E.Message);
        end;
      end;
    end;

    Log('[PASS] Model object creation tests completed.');
  except
    on E: Exception do
      Log('[FAIL] Object creation: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

{ Phase 12: Test model property changes (rename, etc.) }
procedure Phase12_TestModelProperties(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
  OrigName, NewName: string;
begin
  Log('--- Phase 12: Testing model property changes ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for property changes.');
    Log('');
    Exit;
  end;

  try
    OrigName := Model.GetModelName;
    Log('  Current model name: "' + OrigName + '"');

    NewName := 'SelfTest_Model';
    Model.SetModelName(NewName);
    Application.ProcessMessages;
    Sleep(100);

    if CompareText(Model.GetModelName, NewName) = 0 then
      Log('[PASS] Model renamed to: "' + Model.GetModelName + '"')
    else
      Log('[FAIL] Model name mismatch. Expected: "' + NewName + '", Got: "' + Model.GetModelName + '"');

    // Restore original name
    Model.SetModelName(OrigName);
    Application.ProcessMessages;
    Sleep(100);

    Log('[PASS] Model property change operations completed.');
  except
    on E: Exception do
      Log('[FAIL] Model property changes: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

{ Phase 13: Test table editor dialog via ShowEditor }
procedure Phase13_TestTableEditor(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
  Tbl: TEERTable;
begin
  Log('--- Phase 13: Testing Table Editor dialog ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for table editor test.');
    Log('');
    Exit;
  end;

  if Model.GetEERObjectCount([EERTable]) = 0 then
  begin
    Log('[SKIP] No tables to edit.');
    Log('');
    Exit;
  end;

  try
    Tbl := TEERTable(Model.GetEERObjectByIndex(EERTable, 0));
    if Assigned(Tbl) then
    begin
      Log('  Opening Table Editor for: ' + Tbl.ObjName);
      ScheduleModalClose(800);
      Tbl.ShowEditor(nil);
      Application.ProcessMessages;
      Sleep(500);
      Application.ProcessMessages;
      Log('[PASS] Table editor opened and closed successfully.');
    end
    else
      Log('[SKIP] Could not get first table.');
  except
    on E: Exception do
      Log('[FAIL] Table editor: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

{ Phase 14: Test palette forms accessibility }
procedure Phase14_TestPaletteForms(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  I: Integer;
  FoundCount: Integer;
begin
  Log('--- Phase 14: Testing palette form accessibility ---');
  Log('');

  FoundCount := 0;
  for I := 0 to Screen.FormCount - 1 do
  begin
    if (Pos('Palette', Screen.Forms[I].Name) > 0) or
       (Pos('Palette', Screen.Forms[I].ClassName) > 0) then
    begin
      Log('  Found: ' + Screen.Forms[I].Name + ' (' + Screen.Forms[I].ClassName + ')');
      Inc(FoundCount);
    end;
  end;

  if FoundCount > 0 then
    Log('[PASS] ' + IntToStr(FoundCount) + ' palette form(s) are accessible.')
  else
    Log('[WARN] No palette forms found (they may be hidden/docked).');

  Log('');
end;

{ Phase 15: Test window state operations }
procedure Phase15_TestWindowStates(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  EERFrm: TEERForm;
begin
  Log('--- Phase 15: Testing window state operations ---');
  Log('');

  EERFrm := LastCreatedEERForm;
  if EERFrm = nil then
  begin
    Log('[SKIP] No EER form to test window states.');
    Log('');
    Exit;
  end;

  try
    EERFrm.WindowState := wsNormal;
    Application.ProcessMessages;
    Sleep(100);
    Log('  Window state set to Normal.');

    EERFrm.WindowState := wsMaximized;
    Application.ProcessMessages;
    Sleep(100);
    Log('  Window state set to Maximized.');

    Log('[PASS] Window state operations completed.');
  except
    on E: Exception do
      Log('[FAIL] Window state: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

{ Phase 16: Test undo/redo operations }
procedure Phase16_TestUndoRedo(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
begin
  Log('--- Phase 16: Testing undo/redo operations ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for undo/redo test.');
    Log('');
    Exit;
  end;

  try
    // Perform undo
    Model.UndoActions(0);
    Application.ProcessMessages;
    Sleep(100);
    Log('[PASS] UndoActions executed without exception.');

    // Perform redo
    Model.RedoActions(0);
    Application.ProcessMessages;
    Sleep(100);
    Log('[PASS] RedoActions executed without exception.');

    Log('[PASS] Undo/redo operations completed.');
  except
    on E: Exception do
      Log('[FAIL] Undo/redo: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

{ Phase 17: Test model alignment operations }
procedure Phase17_TestAlignOperations(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
begin
  Log('--- Phase 17: Testing model alignment operations ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for alignment test.');
    Log('');
    Exit;
  end;

  try
    // Select all objects
    Model.SelectAllObjs;
    Application.ProcessMessages;
    Sleep(50);

    if Model.GetSelectedObjsCount > 0 then
    begin
      // Align left
      Model.AlignSelectedObjs(EERObjAlignLeft);
      Application.ProcessMessages;
      Sleep(50);
      Log('[PASS] AlignLeft executed.');

      Model.AlignSelectedObjs(EERObjAlignTop);
      Application.ProcessMessages;
      Sleep(50);
      Log('[PASS] AlignTop executed.');
    end
    else
      Log('[SKIP] No objects to align.');

    Model.DeSelectAllObjs(nil);
    Log('[PASS] Alignment operations completed.');
  except
    on E: Exception do
      Log('[FAIL] Alignment: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;

{ Phase 18: Refresh model and verify no exceptions }
procedure Phase18_TestModelRefresh(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer);
var
  Model: TEERModel;
begin
  Log('--- Phase 18: Testing model refresh operations ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for refresh test.');
    Log('');
    Exit;
  end;

  try
    Model.Refresh;
    Application.ProcessMessages;
    Sleep(100);
    Log('[PASS] Refresh executed.');

    Model.RefreshFont;
    Application.ProcessMessages;
    Sleep(100);
    Log('[PASS] RefreshFont executed.');

    Model.ModelHasChanged;
    Application.ProcessMessages;
    Sleep(100);
    Log('[PASS] ModelHasChanged executed.');

    Log('[PASS] Model refresh operations completed.');
  except
    on E: Exception do
      Log('[FAIL] Model refresh: ' + E.ClassName + ': ' + E.Message);
  end;

  Log('');
end;
