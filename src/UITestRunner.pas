
unit UITestRunner;

//----------------------------------------------------------------------------------------------------------------------
//
// UITestRunner - Automated UI Self-Test Runner
//
// Comprehensive automated testing:
//   Phase 0: Close startup dialogs (Tips etc.)
//   Phase 1: Open a test model file (Examples/order.xml)
//   Phase 2: Create tables and relationships programmatically
//   Phase 3: Open and close modal dialogs (Options, Model Options, SQL Script)
//   Phase 4: Export SQL create script to file
//   Phase 5: Save model to a new file
//   Phase 6: Click all menu items and check for exceptions
//   Phase 7: Click all buttons and check for exceptions
//
//----------------------------------------------------------------------------------------------------------------------

{$I DBDesigner4.inc}

interface

uses
  SysUtils, Classes, Forms, Controls, Menus, Buttons, StdCtrls, ExtCtrls;

function RunUITests(AMainForm: TForm; const LogFileName: string = ''): Integer;
function HasSelfTestParam: Boolean;

implementation

uses EER, EERModel, EERExportSQLScript, OptionsModel, Options, Main, MainDM, Dialogs, StrUtils;

type
  TTestResult = (trPass, trFail, trSkip);

  TTestEntry = record
    ComponentName: string;
    ComponentClass: string;
    Result: TTestResult;
    ErrorMessage: string;
    StackTrace: string;
  end;

  { Helper class to provide TNotifyEvent for the modal-close timer }
  TModalCloser = class
    procedure OnTimer(Sender: TObject);
  end;

var
  TestLog: TStringList;
  ModalCloseTimer: TTimer;
  ModalCloser: TModalCloser;
  LastCreatedEERForm: TEERForm;
  ExampleModel: TEERModel; // Saved from Phase1 for Phase4b testing

// ---------------------------------------------------------------------------
// Logging
// ---------------------------------------------------------------------------
procedure Log(const Msg: string);
begin
  if Assigned(TestLog) then
    TestLog.Add(Msg);
  WriteLn(Msg);
end;

procedure LogSeparator;
begin
  Log(StringOfChar('=', 78));
end;

procedure FlushLog(const FileName: string);
begin
  if Assigned(TestLog) then
  try
    TestLog.SaveToFile(FileName);
  except
  end;
end;

function GetExceptionStackTrace: string;
var
  I: Integer;
  Frames: PPointer;
begin
  Result := BackTraceStrFunc(ExceptAddr);
  if ExceptFrameCount > 0 then
  begin
    Frames := ExceptFrames;
    for I := 0 to ExceptFrameCount - 1 do
      Result := Result + LineEnding + '  ' + BackTraceStrFunc(Frames[I]);
  end;
end;

function HasSelfTestParam: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if (CompareText(ParamStr(I), '--selftest') = 0) or
       (CompareText(ParamStr(I), '-selftest') = 0) then
    begin
      Result := True;
      Exit;
    end;
end;

// ---------------------------------------------------------------------------
// Modal-close timer
// ---------------------------------------------------------------------------
procedure TModalCloser.OnTimer(Sender: TObject);
var
  I: Integer;
  F: TForm;
begin
  TTimer(Sender).Enabled := False;
  for I := Screen.FormCount - 1 downto 0 do
  begin
    F := Screen.Forms[I];
    if (F.Visible) and (fsModal in F.FormState) then
    begin
      Log('  [AUTO-CLOSE] Closing modal: ' + F.Name + ' (' + F.ClassName + ')');
      F.ModalResult := mrCancel;
      Exit;
    end;
  end;
end;

procedure ScheduleModalClose(DelayMs: Integer);
begin
  if not Assigned(ModalCloser) then
    ModalCloser := TModalCloser.Create;
  if not Assigned(ModalCloseTimer) then
  begin
    ModalCloseTimer := TTimer.Create(nil);
    ModalCloseTimer.OnTimer := ModalCloser.OnTimer;
  end;
  ModalCloseTimer.Interval := DelayMs;
  ModalCloseTimer.Enabled := True;
end;

// ---------------------------------------------------------------------------
// Unsafe list
// ---------------------------------------------------------------------------
function IsUnsafe(const AName: string): Boolean;
const
  UnsafeNames: array[0..27] of string = (
    'ExitMI', 'CloseMI', 'CloseAllMI',
    'SaveMI', 'SaveAsMI', 'SaveinDatabaseMI',
    'OpenMI', 'OpenfromDatabaseMI',
    'Save2DiskImg', 'Save2DBImg',
    'PrintMI',
    'ConnecttoDatabaseMI', 'DisconnectfromDatabaseMI',
    'ConnectionSBtn', 'ReverseEngineeringMI',
    'DatabasesyncronisationMI', 'SyncImg',
    'DeleteMI', 'CutMI',
    'ImportERwin41XMLModelMI',
    'AddLinkModelFromFileMI', 'AddLinkModelfromDBMI',
    'AddLinkModelfromOnlineLibraryMI',
    'ExportMDBXMLFileMI',
    'SaveModelasImageMI', 'ExportSelectedObjectsAsImgMi',
    'Test1',
    'RefreshLinkedObjectsMI'
  );
var
  I: Integer;
  Mid: string;
  AllDigits: Boolean;
begin
  Result := False;
  for I := Low(UnsafeNames) to High(UnsafeNames) do
    if CompareText(AName, UnsafeNames[I]) = 0 then
    begin
      Result := True;
      Exit;
    end;

  // Skip dynamically created plugin menu items (Plugin<digits>MI).
  // Their OnClick launches an external plugin executable and waits for it,
  // which deadlocks the self-test.
  if (Length(AName) > 8) and
     (CompareText(Copy(AName, 1, 6), 'Plugin') = 0) and
     (CompareText(Copy(AName, Length(AName) - 1, 2), 'MI') = 0) then
  begin
    Mid := Copy(AName, 7, Length(AName) - 8);
    AllDigits := Length(Mid) > 0;
    for I := 1 to Length(Mid) do
      if not (Mid[I] in ['0'..'9']) then
      begin
        AllDigits := False;
        Break;
      end;
    if AllDigits then
      Result := True;
  end;
end;

// ---------------------------------------------------------------------------
// Test helpers
// ---------------------------------------------------------------------------
function TestMenuItem(Item: TMenuItem; const FormName: string): TTestEntry;
begin
  Result.ComponentName := FormName + '.' + Item.Name;
  Result.ComponentClass := Item.ClassName;
  Result.ErrorMessage := '';
  Result.StackTrace := '';

  if IsUnsafe(Item.Name) then
  begin
    Result.Result := trSkip;
    Result.ErrorMessage := 'In unsafe/skip list';
    Exit;
  end;
  if not Item.Enabled then
  begin
    Result.Result := trSkip;
    Result.ErrorMessage := 'Disabled';
    Exit;
  end;
  if (Item.Caption = '-') or (Item.Caption = '') then
  begin
    Result.Result := trSkip;
    Result.ErrorMessage := 'Separator';
    Exit;
  end;
  if Item.Count > 0 then
  begin
    Result.Result := trSkip;
    Result.ErrorMessage := 'Submenu parent';
    Exit;
  end;

  try
    ScheduleModalClose(800);
    Item.Click;
    Application.ProcessMessages;
    Sleep(500);
    Application.ProcessMessages;
    Result.Result := trPass;
  except
    on E: Exception do
    begin
      Result.Result := trFail;
      Result.ErrorMessage := E.ClassName + ': ' + E.Message;
      Result.StackTrace := GetExceptionStackTrace;
    end;
  end;
end;

function TestButton(Btn: TControl; const FormName: string): TTestEntry;
var
  BtnEnabled: Boolean;
begin
  Result.ComponentName := FormName + '.' + Btn.Name;
  Result.ComponentClass := Btn.ClassName;
  Result.ErrorMessage := '';
  Result.StackTrace := '';

  if IsUnsafe(Btn.Name) then
  begin
    Result.Result := trSkip;
    Result.ErrorMessage := 'In unsafe/skip list';
    Exit;
  end;

  BtnEnabled := True;
  if Btn is TSpeedButton then
    BtnEnabled := TSpeedButton(Btn).Enabled
  else if Btn is TButton then
    BtnEnabled := TButton(Btn).Enabled
  else if Btn is TBitBtn then
    BtnEnabled := TBitBtn(Btn).Enabled;

  if not BtnEnabled then
  begin
    Result.Result := trSkip;
    Result.ErrorMessage := 'Disabled';
    Exit;
  end;

  try
    ScheduleModalClose(800);
    if Btn is TSpeedButton then
      TSpeedButton(Btn).Click
    else if Btn is TButton then
      TButton(Btn).Click
    else if Btn is TBitBtn then
      TBitBtn(Btn).Click;
    Application.ProcessMessages;
    Sleep(500);
    Application.ProcessMessages;
    Result.Result := trPass;
  except
    on E: Exception do
    begin
      Result.Result := trFail;
      Result.ErrorMessage := E.ClassName + ': ' + E.Message;
      Result.StackTrace := GetExceptionStackTrace;
    end;
  end;
end;

procedure LogTestEntry(const Entry: TTestEntry);
var
  Lines: TStringList;
  I: Integer;
begin
  case Entry.Result of
    trPass:
      Log('[PASS] ' + Entry.ComponentName + ' (' + Entry.ComponentClass + ')');
    trFail:
    begin
      Log('[FAIL] ' + Entry.ComponentName + ' (' + Entry.ComponentClass + ')');
      Log('       Error: ' + Entry.ErrorMessage);
      if Entry.StackTrace <> '' then
      begin
        Log('       Stack trace:');
        Lines := TStringList.Create;
        try
          Lines.Text := Entry.StackTrace;
          for I := 0 to Lines.Count - 1 do
            Log('         ' + Lines[I]);
        finally
          Lines.Free;
        end;
      end;
    end;
    trSkip:
      Log('[SKIP] ' + Entry.ComponentName + ' (' + Entry.ComponentClass +
          ') - ' + Entry.ErrorMessage);
  end;
end;

procedure CollectMenuItems(AItem: TMenuItem; var List: TList);
var
  I: Integer;
begin
  List.Add(AItem);
  for I := 0 to AItem.Count - 1 do
    CollectMenuItems(AItem.Items[I], List);
end;

// ---------------------------------------------------------------------------
// Helper: Get the current model - prefer tracked form, fall back to FActiveEERForm
// ---------------------------------------------------------------------------
function GetCurrentModel(AMainForm: TForm): TEERModel;
begin
  Result := nil;
  // First try our tracked form
  if Assigned(LastCreatedEERForm) and Assigned(LastCreatedEERForm.EERModel) then
  begin
    Result := LastCreatedEERForm.EERModel;
    Exit;
  end;
  // Fall back to MainForm.FActiveEERForm
  if Assigned(MainForm.FActiveEERForm) and (MainForm.FActiveEERForm.ClassName = 'TEERForm') then
    Result := TEERForm(MainForm.FActiveEERForm).EERModel;
end;

// ---------------------------------------------------------------------------
// Find test file
// ---------------------------------------------------------------------------
function FindTestFile: string;
var
  AppDir, Candidate: string;
begin
  Result := '';
  AppDir := ExtractFilePath(Application.ExeName);

  Candidate := AppDir + 'Examples' + PathDelim + 'order.xml';
  if FileExists(Candidate) then begin Result := Candidate; Exit; end;

  Candidate := 'bin' + PathDelim + 'Examples' + PathDelim + 'order.xml';
  if FileExists(Candidate) then begin Result := Candidate; Exit; end;

  Candidate := AppDir + '..' + PathDelim + 'test-base' + PathDelim + 'db-sql-create-test.xml';
  if FileExists(Candidate) then begin Result := Candidate; Exit; end;
end;

// ===========================================================================
// Phases
// ===========================================================================

{ Phase 0: Close startup dialogs }
procedure Phase0_CloseStartupDialogs(AMainForm: TForm);
var
  I: Integer;
begin
  Log('--- Phase 0: Closing startup dialogs ---');
  Log('');
  for I := Screen.FormCount - 1 downto 0 do
  begin
    if (Screen.Forms[I] <> AMainForm) and Screen.Forms[I].Visible then
    begin
      if (Pos('Tips', Screen.Forms[I].ClassName) > 0) or
         (Pos('Tips', Screen.Forms[I].Name) > 0) then
      begin
        Log('Closing: ' + Screen.Forms[I].Name + ' (' + Screen.Forms[I].ClassName + ')');
        try
          Screen.Forms[I].Close;
          Application.ProcessMessages;
          Sleep(300);
          Application.ProcessMessages;
        except
          on E: Exception do
            Log('WARNING: Could not close ' + Screen.Forms[I].Name + ': ' + E.Message);
        end;
      end;
    end;
  end;
  Log('');
end;

{ Phase 1: Open test file }
procedure Phase1_OpenTestFile(AMainForm: TForm);
var
  TestFile: string;
  NewForm: TEERForm;
  I: Integer;
begin
  Log('--- Phase 1: Opening test file ---');
  Log('');

  TestFile := FindTestFile;
  if TestFile = '' then
  begin
    Log('[SKIP] No test file found. Creating blank model instead.');
    try
      for I := 0 to AMainForm.ComponentCount - 1 do
        if (CompareText(AMainForm.Components[I].Name, 'NewMI') = 0) and
           (AMainForm.Components[I] is TMenuItem) then
        begin
          TMenuItem(AMainForm.Components[I]).Click;
          Application.ProcessMessages;
          Sleep(300);
          Application.ProcessMessages;
          Log('[PASS] Created new blank model via NewMI.');
          Break;
        end;
    except
      on E: Exception do
        Log('[FAIL] Could not create new model: ' + E.Message);
    end;
  end
  else
  begin
    Log('Test file found: ' + TestFile);
    try
      NewForm := TEERForm.Create(AMainForm);
      NewForm.EERModel.LoadFromFile(TestFile, True, False, True, False);
      NewForm.WindowState := wsMaximized;
      LastCreatedEERForm := NewForm;
      ExampleModel := NewForm.EERModel;
      Application.ProcessMessages;
      Sleep(500);
      Application.ProcessMessages;
      Log('[PASS] Opened test file: ' + TestFile);
    except
      on E: Exception do
        Log('[FAIL] Could not open test file: ' + E.ClassName + ': ' + E.Message);
    end;
  end;
  Log('');
end;

{ Phase 2: Create tables and relationships }
procedure Phase2_CreateTablesAndRelations(AMainForm: TForm);
var
  Model: TEERModel;
  NewForm: TEERForm;
  Tbl1, Tbl2, Tbl3: Pointer;
  Rel: Pointer;
begin
  Log('--- Phase 2: Creating tables and relationships ---');
  Log('');

  try
    NewForm := TEERForm.Create(AMainForm);
    NewForm.WindowState := wsMaximized;
    LastCreatedEERForm := NewForm;
    Application.ProcessMessages;
    Sleep(200);
    Application.ProcessMessages;
    Model := NewForm.EERModel;
  except
    on E: Exception do
    begin
      Log('[FAIL] Could not create new model: ' + E.Message);
      Log('');
      Exit;
    end;
  end;

  Tbl1 := nil;
  Tbl2 := nil;
  Tbl3 := nil;

  // Table 1: customers
  try
    Tbl1 := Model.NewTable(80, 80, True);
    TEERTable(Tbl1).ObjName := 'customers';
    TEERTable(Tbl1).RefreshObj;
    Application.ProcessMessages;
    Log('[PASS] Created table: customers');
  except
    on E: Exception do
    begin
      Log('[FAIL] Could not create table customers: ' + E.Message);
      Log('');
      Exit;
    end;
  end;

  // Table 2: orders
  try
    Tbl2 := Model.NewTable(300, 80, True);
    TEERTable(Tbl2).ObjName := 'orders';
    TEERTable(Tbl2).RefreshObj;
    Application.ProcessMessages;
    Log('[PASS] Created table: orders');
  except
    on E: Exception do
    begin
      Log('[FAIL] Could not create table orders: ' + E.Message);
      Log('');
      Exit;
    end;
  end;

  // Table 3: order_items
  try
    Tbl3 := Model.NewTable(520, 80, True);
    TEERTable(Tbl3).ObjName := 'order_items';
    TEERTable(Tbl3).RefreshObj;
    Application.ProcessMessages;
    Log('[PASS] Created table: order_items');
  except
    on E: Exception do
    begin
      Log('[FAIL] Could not create table order_items: ' + E.Message);
      Log('');
      Exit;
    end;
  end;

  // Relation: customers -> orders (1:n non-identifying, rk_1nNonId=2)
  try
    Rel := Model.NewRelation(2, Tbl1, Tbl2, True);
    Application.ProcessMessages;
    Log('[PASS] Created relation: customers -> orders (1:n non-identifying)');
  except
    on E: Exception do
      Log('[FAIL] Could not create relation customers->orders: ' + E.Message);
  end;

  // Relation: orders -> order_items (1:n identifying, rk_1n=1)
  try
    Rel := Model.NewRelation(1, Tbl2, Tbl3, True);
    Application.ProcessMessages;
    Log('[PASS] Created relation: orders -> order_items (1:n identifying)');
  except
    on E: Exception do
      Log('[FAIL] Could not create relation orders->order_items: ' + E.Message);
  end;

  Application.ProcessMessages;
  Sleep(300);
  Application.ProcessMessages;
  Log('');
end;

{ Phase 3: Open and close modal dialogs }
procedure Phase3_OpenModalDialogs(AMainForm: TForm);
var
  Model: TEERModel;
  OptFrm: TOptionsForm;
  OptModelFrm: TOptionsModelForm;
  SQLFrm: TEERExportSQLScriptFrom;
begin
  Log('--- Phase 3: Opening and closing modal dialogs ---');
  Log('');

  // DBDesigner Options
  try
    Log('  Opening DBDesigner Options...');
    OptFrm := TOptionsForm.Create(AMainForm);
    try
      ScheduleModalClose(800);
      OptFrm.ShowModal;
    finally
      OptFrm.Free;
    end;
    Application.ProcessMessages;
    Log('[PASS] DBDesigner Options dialog opened and closed.');
  except
    on E: Exception do
      Log('[FAIL] DBDesigner Options: ' + E.ClassName + ': ' + E.Message);
  end;

  // Model Options
  Model := GetCurrentModel(AMainForm);
  if Model <> nil then
  begin
    try
      Log('  Opening Model Options...');
      OptModelFrm := TOptionsModelForm.Create(AMainForm);
      try
        OptModelFrm.SetModel(Model);
        ScheduleModalClose(800);
        OptModelFrm.ShowModal;
      finally
        OptModelFrm.Free;
      end;
      Application.ProcessMessages;
      Log('[PASS] Model Options dialog opened and closed.');
    except
      on E: Exception do
        Log('[FAIL] Model Options: ' + E.ClassName + ': ' + E.Message);
    end;
  end
  else
    Log('[SKIP] Model Options - no active model');

  // SQL Create Script
  Model := GetCurrentModel(AMainForm);
  if Model <> nil then
  begin
    try
      Log('  Opening SQL Create Script...');
      SQLFrm := TEERExportSQLScriptFrom.Create(AMainForm);
      try
        SQLFrm.SetModel(Model);
        ScheduleModalClose(800);
        SQLFrm.ShowModal;
      finally
        SQLFrm.Free;
      end;
      Application.ProcessMessages;
      Log('[PASS] SQL Create Script dialog opened and closed.');
    except
      on E: Exception do
        Log('[FAIL] SQL Create Script: ' + E.ClassName + ': ' + E.Message);
    end;
  end
  else
    Log('[SKIP] SQL Create Script - no active model');

  Application.ProcessMessages;
  Log('');
end;

{ Phase 4: Export SQL create script to file }
procedure Phase4_ExportSQL(AMainForm: TForm; const LogDir: string);
var
  Model: TEERModel;
  Frm: TEERExportSQLScriptFrom;
  SQL, SQLFile: string;
  F: TextFile;
begin
  Log('--- Phase 4: Exporting SQL create script to file ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model for SQL export.');
    Log('');
    Exit;
  end;

  SQLFile := LogDir + 'selftest_export.sql';
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
      Log('[WARN] SQL script is empty (model may have no tables).')
    else
    begin
      try
        AssignFile(F, SQLFile);
        Rewrite(F);
        Write(F, SQL);
        CloseFile(F);
        Log('[PASS] SQL script exported to: ' + SQLFile);
        Log('  SQL length: ' + IntToStr(Length(SQL)) + ' characters');
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

{ Phase 5: Save model to a new file }
procedure Phase5_SaveModel(AMainForm: TForm; const LogDir: string);
var
  Model: TEERModel;
  SaveFile: string;
  SR: TSearchRec;
  FSize: Int64;
begin
  Log('--- Phase 5: Saving model to a new file ---');
  Log('');

  Model := GetCurrentModel(AMainForm);
  if Model = nil then
  begin
    Log('[SKIP] No active model to save.');
    Log('');
    Exit;
  end;

  SaveFile := LogDir + 'selftest_saved_model.xml';
  try
    Model.SaveToFile(SaveFile);
    Log('[PASS] Model saved to: ' + SaveFile);
  except
    on E: Exception do
      Log('[FAIL] Could not save model: ' + E.ClassName + ': ' + E.Message);
  end;

  if FileExists(SaveFile) then
  begin
    if FindFirst(SaveFile, faAnyFile, SR) = 0 then
    begin
      FSize := SR.Size;
      FindClose(SR);
      Log('  Saved file size: ' + IntToStr(FSize) + ' bytes');
      if FSize > 100 then
        Log('[PASS] Saved file is non-trivial.')
      else
        Log('[WARN] Saved file seems too small.');
    end;
  end
  else
    Log('[FAIL] Saved file does not exist after SaveToFile call.');

  Log('');
end;

{ Phase 6: Test all menu items }
procedure Phase6_TestMenuItems(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer; const LogFile: string);
var
  I, J: Integer;
  Entry: TTestEntry;
  MenuItems: TList;
  Menu: TMainMenu;
  ItemName: string;
begin
  Log('--- Phase 6: Testing Menu Items ---');
  Log('');

  MenuItems := TList.Create;
  try
    for I := 0 to AMainForm.ComponentCount - 1 do
      if AMainForm.Components[I] is TMainMenu then
      begin
        Menu := TMainMenu(AMainForm.Components[I]);
        for J := 0 to Menu.Items.Count - 1 do
          CollectMenuItems(Menu.Items[J], MenuItems);
      end;

    Log('Found ' + IntToStr(MenuItems.Count) + ' menu items to test.');
    Log('');

    for I := 0 to MenuItems.Count - 1 do
    begin
      ItemName := TMenuItem(MenuItems[I]).Name;

      Log('  [TRYING] menu item: ' + ItemName);
      FlushLog(LogFile);

      // Schedule auto-close for menu items that open modal dialogs
      if (CompareText(ItemName, 'AboutMI') = 0) or
         (CompareText(ItemName, 'EERModelOptionsMI') = 0) or
         (CompareText(ItemName, 'DBDesignerOptionsMI') = 0) or
         (CompareText(ItemName, 'PageSetupMI') = 0) or
         (CompareText(ItemName, 'SQLCreateScriptMI') = 0) or
         (CompareText(ItemName, 'SQLDropScriptMI') = 0) or
         (CompareText(ItemName, 'SQLOptimizeTableScriptMI') = 0) or
         (CompareText(ItemName, 'SQLRepairTableScriptMI') = 0) then
      begin
        ScheduleModalClose(800);
      end;

      Entry := TestMenuItem(TMenuItem(MenuItems[I]), AMainForm.Name);
      LogTestEntry(Entry);
      case Entry.Result of
        trPass: Inc(PassCount);
        trFail: Inc(FailCount);
        trSkip: Inc(SkipCount);
      end;
      Application.ProcessMessages;
      FlushLog(LogFile);
    end;
  finally
    MenuItems.Free;
  end;
  Log('');
end;

{ Phase 7: Test buttons on MainForm and visible forms }
procedure Phase7_TestButtons(AMainForm: TForm; var PassCount, FailCount, SkipCount: Integer; const LogFile: string);
var
  I, J: Integer;
  Entry: TTestEntry;
  Component: TComponent;
  VisibleForms: TList;
  ButtonList: TList;
  AForm: TForm;
begin
  Log('--- Phase 7: Testing Buttons on MainForm ---');
  Log('');

  for I := 0 to AMainForm.ComponentCount - 1 do
  begin
    Component := AMainForm.Components[I];
    if (Component is TSpeedButton) or (Component is TButton) or (Component is TBitBtn) then
    begin
      Log('  [TRYING] button: ' + AMainForm.Name + '.' + Component.Name);
      FlushLog(LogFile);
      Entry := TestButton(TControl(Component), AMainForm.Name);
      LogTestEntry(Entry);
      case Entry.Result of
        trPass: Inc(PassCount);
        trFail: Inc(FailCount);
        trSkip: Inc(SkipCount);
      end;
      Application.ProcessMessages;
      FlushLog(LogFile);
    end;
  end;

  Log('');
  Log('--- Phase 7b: Testing Buttons on Other Visible Forms ---');
  Log('');

  VisibleForms := TList.Create;
  try
    for I := 0 to Screen.FormCount - 1 do
      if (Screen.Forms[I] <> AMainForm) and Screen.Forms[I].Visible then
        VisibleForms.Add(Screen.Forms[I]);

    for I := 0 to VisibleForms.Count - 1 do
    begin
      AForm := TForm(VisibleForms[I]);
      Log('  Form: ' + AForm.Name + ' (' + AForm.ClassName + ')');

      ButtonList := TList.Create;
      try
        for J := 0 to AForm.ComponentCount - 1 do
        begin
          Component := AForm.Components[J];
          if (Component is TSpeedButton) or (Component is TButton) or (Component is TBitBtn) then
            ButtonList.Add(Component);
        end;

        for J := 0 to ButtonList.Count - 1 do
        begin
          Log('  [TRYING] button: ' + AForm.Name + '.' + TComponent(ButtonList[J]).Name);
          FlushLog(LogFile);
          Entry := TestButton(TControl(ButtonList[J]), AForm.Name);
          LogTestEntry(Entry);
          case Entry.Result of
            trPass: Inc(PassCount);
            trFail: Inc(FailCount);
            trSkip: Inc(SkipCount);
          end;
          Application.ProcessMessages;
          FlushLog(LogFile);
        end;
      finally
        ButtonList.Free;
      end;
    end;
  finally
    VisibleForms.Free;
  end;
  Log('');
end;


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

  Model := ExampleModel;
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

{$I ../bpsa/new_test_phases.pas}
// ===========================================================================
// Main entry point
// ===========================================================================
function RunUITests(AMainForm: TForm; const LogFileName: string): Integer;
var
  ActualLogFile, LogDir: string;
  PassCount, FailCount, SkipCount: Integer;
  StartTime: TDateTime;
begin
  if LogFileName = '' then
    ActualLogFile := '/tmp/UITestResults.log'
  else
    ActualLogFile := LogFileName;

  LogDir := ExtractFilePath(ActualLogFile);
  if LogDir = '' then
    LogDir := '/tmp/';

  LastCreatedEERForm := nil;
  ExampleModel := nil;
  TestLog := TStringList.Create;
  try
    PassCount := 0;
    FailCount := 0;
    SkipCount := 0;
    StartTime := Now;

    LogSeparator;
    Log('UI TEST RUNNER - Comprehensive Self-Test');
    Log('Started: ' + DateTimeToStr(StartTime));
    Log('Form: ' + AMainForm.Name + ' (' + AMainForm.ClassName + ')');
    LogSeparator;
    Log('');

    Phase0_CloseStartupDialogs(AMainForm);
    FlushLog(ActualLogFile);

    Phase1_OpenTestFile(AMainForm);
    FlushLog(ActualLogFile);

    Phase2_CreateTablesAndRelations(AMainForm);
    FlushLog(ActualLogFile);

    Phase3_OpenModalDialogs(AMainForm);
    FlushLog(ActualLogFile);

    Phase4_ExportSQL(AMainForm, LogDir);
    FlushLog(ActualLogFile);

    Phase5_SaveModel(AMainForm, LogDir);
    FlushLog(ActualLogFile);

    Phase6_TestMenuItems(AMainForm, PassCount, FailCount, SkipCount, ActualLogFile);
    FlushLog(ActualLogFile);

    Phase7_TestButtons(AMainForm, PassCount, FailCount, SkipCount, ActualLogFile);
    FlushLog(ActualLogFile);


    Phase4b_ExportExampleSQL(AMainForm, LogDir);
    FlushLog(ActualLogFile);

    Phase8_TestPopupMenus(AMainForm, PassCount, FailCount, SkipCount, ActualLogFile);
    FlushLog(ActualLogFile);

    Phase9_TestSelectOperations(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase10_TestZoom(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase11_TestModelObjectCreation(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase12_TestModelProperties(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase13_TestTableEditor(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase14_TestPaletteForms(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase15_TestWindowStates(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase16_TestUndoRedo(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase17_TestAlignOperations(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase18_TestModelRefresh(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);


    Phase19_TestNoteEditor(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase20_TestRegionEditor(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase21_TestRelationEditor(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase22_TestStringEditor(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase23_TestTableColumnOperations(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase24_TestSQLContentVerification(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase25_TestModelConsistency(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase26_TestObjectEnumeration(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase27_TestRegionOperations(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);

    Phase28_TestTableColumnMetadata(AMainForm, PassCount, FailCount, SkipCount);
    FlushLog(ActualLogFile);


    // Summary
    LogSeparator;
    Log('TEST SUMMARY');
    LogSeparator;
    Log('Total tests: ' + IntToStr(PassCount + FailCount + SkipCount));
    Log('  PASS: ' + IntToStr(PassCount));
    Log('  FAIL: ' + IntToStr(FailCount));
    Log('  SKIP: ' + IntToStr(SkipCount));
    Log('');
    Log('Finished: ' + DateTimeToStr(Now));
    Log('Duration: ' + FormatDateTime('nn:ss.zzz', Now - StartTime));
    Log('Log file: ' + ActualLogFile);
    LogSeparator;

    Result := FailCount;

    try
      TestLog.SaveToFile(ActualLogFile);
    except
      on E: Exception do
      begin
        ActualLogFile := 'UITestResults.log';
        TestLog.SaveToFile(ActualLogFile);
      end;
    end;

  finally
    TestLog.Free;
    TestLog := nil;
    if Assigned(ModalCloseTimer) then
    begin
      ModalCloseTimer.Free;
      ModalCloseTimer := nil;
    end;
    if Assigned(ModalCloser) then
    begin
      ModalCloser.Free;
      ModalCloser := nil;
    end;
  end;
end;

end.
