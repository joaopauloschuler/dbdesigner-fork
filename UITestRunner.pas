
unit UITestRunner;

//----------------------------------------------------------------------------------------------------------------------
//
// UITestRunner - Automated UI Button/Menu Click Tester
//
// This unit provides a safe automated testing tool that simulates clicking
// on buttons and menu items to detect unhandled exceptions. It logs results
// with full stack traces to a file for later analysis.
//
// Usage:
//   - Call RunUITests from any event handler (e.g., Test1Click in Main.pas)
//   - Or run the app with --selftest to auto-run tests and exit
//
//----------------------------------------------------------------------------------------------------------------------

{$I DBDesigner4.inc}

interface

uses
  SysUtils, Classes, Forms, Controls, Menus, Buttons, StdCtrls;

{ RunUITests: Runs all safe UI click tests on AMainForm.
  Results are written to LogFileName (default: UITestResults.log next to the exe).
  Returns the number of FAIL results (0 = all passed). }
function RunUITests(AMainForm: TForm; const LogFileName: string = ''): Integer;

{ HasSelfTestParam: Returns True if --selftest was passed on the command line. }
function HasSelfTestParam: Boolean;

implementation

type
  TTestResult = (trPass, trFail, trSkip);

  TTestEntry = record
    ComponentName: string;
    ComponentClass: string;
    Result: TTestResult;
    ErrorMessage: string;
    StackTrace: string;
  end;

var
  TestLog: TStringList;

procedure Log(const Msg: string);
begin
  if Assigned(TestLog) then
    TestLog.Add(Msg);
  // Also write to stdout immediately for --selftest mode
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
    // ignore flush errors
  end;
end;

{ Collect stack trace into a string }
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

{ Returns true if the component name is in the unsafe list }
function IsUnsafe(const AName: string): Boolean;
const
  UnsafeNames: array[0..39] of string = (
    // Application exit/close
    'ExitMI', 'CloseMI', 'CloseAllMI',
    // File open/save dialogs
    'SaveMI', 'SaveAsMI', 'SaveinDatabaseMI',
    'OpenMI', 'OpenfromDatabaseMI',
    'Save2DiskImg', 'Save2DBImg',
    // Print
    'PrintMI', 'PageSetupMI',
    // Database operations
    'ConnecttoDatabaseMI', 'DisconnectfromDatabaseMI',
    'ConnectionSBtn', 'ReverseEngineeringMI',
    'DatabasesyncronisationMI', 'SyncImg',
    // Destructive
    'DeleteMI', 'CutMI',
    // Import/Export dialogs
    'ImportERwin41XMLModelMI',
    'AddLinkModelFromFileMI', 'AddLinkModelfromDBMI',
    'AddLinkModelfromOnlineLibraryMI',
    'ExportMDBXMLFileMI',
    'SaveModelasImageMI', 'ExportSelectedObjectsAsImgMi',
    'CopyselectedObjectsasImageMI',
    // SQL scripts (may need DB connection or open dialogs)
    'SQLCreateScriptMI', 'SQLDropScriptMI',
    'SQLOptimizeTableScriptMI', 'SQLRepairTableScriptMI',
    // Browser/external
    'OnlinedocumentationMI', 'VisitHomepageMI',
    'CheckfornewversionsMI', 'SubmitabugfeaturerequestMI',
    // Modal dialogs
    'AboutMI', 'EERModelOptionsMI', 'DBDesignerOptionsMI',
    // Our own test hook - avoid recursion!
    'Test1'
  );
var
  I: Integer;
begin
  Result := False;
  for I := Low(UnsafeNames) to High(UnsafeNames) do
    if CompareText(AName, UnsafeNames[I]) = 0 then
    begin
      Result := True;
      Exit;
    end;
end;

{ Attempts to click a TMenuItem, returns the test entry }
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
    Item.Click;
    Application.ProcessMessages;
    Sleep(2000);
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

{ Attempts to click a button (TSpeedButton, TButton, TBitBtn), returns the test entry }
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
    if Btn is TSpeedButton then
      TSpeedButton(Btn).Click
    else if Btn is TButton then
      TButton(Btn).Click
    else if Btn is TBitBtn then
      TBitBtn(Btn).Click;
    Application.ProcessMessages;
    Sleep(2000);
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

{ Recursively collect all menu items }
procedure CollectMenuItems(AItem: TMenuItem; var List: TList);
var
  I: Integer;
begin
  List.Add(AItem);
  for I := 0 to AItem.Count - 1 do
    CollectMenuItems(AItem.Items[I], List);
end;

function RunUITests(AMainForm: TForm; const LogFileName: string): Integer;
var
  ActualLogFile: string;
  I, J: Integer;
  Entry: TTestEntry;
  PassCount, FailCount, SkipCount: Integer;
  MenuItems: TList;
  VisibleForms: TList;
  ButtonList: TList;
  Component: TComponent;
  AForm: TForm;
  StartTime: TDateTime;
  Menu: TMainMenu;
begin
  if LogFileName = '' then
    ActualLogFile := '/tmp/UITestResults.log'
  else
    ActualLogFile := LogFileName;

  TestLog := TStringList.Create;
  MenuItems := TList.Create;
  try
    PassCount := 0;
    FailCount := 0;
    SkipCount := 0;
    StartTime := Now;

    LogSeparator;
    Log('UI TEST RUNNER - Automated Button/Menu Click Test');
    Log('Started: ' + DateTimeToStr(StartTime));
    Log('Form: ' + AMainForm.Name + ' (' + AMainForm.ClassName + ')');
    LogSeparator;
    Log('');

    // =====================================================
    // PHASE 0: Close any Tips/startup dialogs
    // =====================================================
    Log('--- Phase 0: Closing startup dialogs ---');
    Log('');
    WriteLn('[DEBUG] Phase 0: Closing startup dialogs...');
    for I := Screen.FormCount - 1 downto 0 do
    begin
      if (Screen.Forms[I] <> AMainForm) and Screen.Forms[I].Visible then
      begin
        if (Pos('Tips', Screen.Forms[I].ClassName) > 0) or
           (Pos('Tips', Screen.Forms[I].Name) > 0) then
        begin
          Log('Closing: ' + Screen.Forms[I].Name + ' (' + Screen.Forms[I].ClassName + ')');
          WriteLn('[DEBUG] Closing Tips form: ' + Screen.Forms[I].Name);
          try
            Screen.Forms[I].Close;
            Application.ProcessMessages;
            Sleep(500);
            Application.ProcessMessages;
          except
            on E: Exception do
              Log('WARNING: Could not close ' + Screen.Forms[I].Name + ': ' + E.Message);
          end;
        end;
      end;
    end;
    Log('');

    // =====================================================
    // PHASE 1: Create a new model so buttons have context
    // =====================================================
    Log('--- Phase 1: Creating new model for testing context ---');
    Log('');
    WriteLn('[DEBUG] Phase 1: Creating new model...');
    try
      for I := 0 to AMainForm.ComponentCount - 1 do
      begin
        if CompareText(AMainForm.Components[I].Name, 'NewMI') = 0 then
        begin
          if AMainForm.Components[I] is TMenuItem then
          begin
            Log('Clicking NewMI to create a blank model...');
            WriteLn('[DEBUG] About to click NewMI...');
            TMenuItem(AMainForm.Components[I]).Click;
            WriteLn('[DEBUG] NewMI clicked, processing messages...');
            Application.ProcessMessages;
            Sleep(200);
            Application.ProcessMessages;
            Log('New model created successfully.');
            WriteLn('[DEBUG] New model created.');
          end;
          Break;
        end;
      end;
    except
      on E: Exception do
      begin
        Log('WARNING: Failed to create new model: ' + E.ClassName + ': ' + E.Message);
        WriteLn('[DEBUG] NewMI EXCEPTION: ' + E.ClassName + ': ' + E.Message);
      end;
    end;
    Log('');
    FlushLog(ActualLogFile);

    // =====================================================
    // PHASE 2: Test all menu items
    // =====================================================
    Log('--- Phase 2: Testing Menu Items ---');
    Log('');

    for I := 0 to AMainForm.ComponentCount - 1 do
    begin
      if AMainForm.Components[I] is TMainMenu then
      begin
        Menu := TMainMenu(AMainForm.Components[I]);
        for J := 0 to Menu.Items.Count - 1 do
          CollectMenuItems(Menu.Items[J], MenuItems);
      end;
    end;

    WriteLn('[DEBUG] Phase 2: Testing ' + IntToStr(MenuItems.Count) + ' menu items...');
    for I := 0 to MenuItems.Count - 1 do
    begin
      WriteLn('[DEBUG] Testing menu item: ' + TMenuItem(MenuItems[I]).Name);
      Entry := TestMenuItem(TMenuItem(MenuItems[I]), AMainForm.Name);
      LogTestEntry(Entry);
      case Entry.Result of
        trPass: Inc(PassCount);
        trFail: Inc(FailCount);
        trSkip: Inc(SkipCount);
      end;
      Application.ProcessMessages;
    end;

    Log('');
    FlushLog(ActualLogFile);

    // =====================================================
    // PHASE 3: Test all buttons on MainForm
    // =====================================================
    Log('--- Phase 3: Testing Buttons on MainForm ---');
    Log('');
    WriteLn('[DEBUG] Phase 3: Testing buttons on MainForm...');

    for I := 0 to AMainForm.ComponentCount - 1 do
    begin
      Component := AMainForm.Components[I];
      if (Component is TSpeedButton) or
         (Component is TButton) or
         (Component is TBitBtn) then
      begin
        WriteLn('[DEBUG] Testing button: ' + Component.Name);
        Entry := TestButton(TControl(Component), AMainForm.Name);
        LogTestEntry(Entry);
        case Entry.Result of
          trPass: Inc(PassCount);
          trFail: Inc(FailCount);
          trSkip: Inc(SkipCount);
        end;
        Application.ProcessMessages;
      end;
    end;

    Log('');
    FlushLog(ActualLogFile);

    // =====================================================
    // PHASE 4: Test buttons on other visible forms
    // =====================================================
    Log('--- Phase 4: Testing Buttons on Other Visible Forms ---');
    Log('');
    WriteLn('[DEBUG] Phase 4: Testing buttons on other visible forms...');
    WriteLn('[DEBUG] Screen.FormCount = ' + IntToStr(Screen.FormCount));

    // IMPORTANT: Snapshot visible forms first since button clicks may close forms,
    // changing Screen.FormCount during iteration.
    VisibleForms := TList.Create;
    try
      for I := 0 to Screen.FormCount - 1 do
        if (Screen.Forms[I] <> AMainForm) and Screen.Forms[I].Visible then
          VisibleForms.Add(Screen.Forms[I]);

      WriteLn('[DEBUG] Found ' + IntToStr(VisibleForms.Count) + ' visible non-main forms');

      for I := 0 to VisibleForms.Count - 1 do
      begin
        AForm := TForm(VisibleForms[I]);
        WriteLn('[DEBUG] Form: ' + AForm.Name + ' (' + AForm.ClassName + ')');
        Log('  Form: ' + AForm.Name + ' (' + AForm.ClassName + ')');

        // Snapshot buttons on this form too
        ButtonList := TList.Create;
        try
          for J := 0 to AForm.ComponentCount - 1 do
          begin
            Component := AForm.Components[J];
            if (Component is TSpeedButton) or
               (Component is TButton) or
               (Component is TBitBtn) then
              ButtonList.Add(Component);
          end;

          for J := 0 to ButtonList.Count - 1 do
          begin
            WriteLn('[DEBUG]   Testing button: ' + AForm.Name + '.' + TControl(ButtonList[J]).Name);
            Entry := TestButton(TControl(ButtonList[J]), AForm.Name);
            LogTestEntry(Entry);
            case Entry.Result of
              trPass: Inc(PassCount);
              trFail: Inc(FailCount);
              trSkip: Inc(SkipCount);
            end;
            Application.ProcessMessages;
          end;
        finally
          ButtonList.Free;
        end;
      end;
    finally
      VisibleForms.Free;
    end;

    Log('');

    // =====================================================
    // SUMMARY
    // =====================================================
    WriteLn('[DEBUG] Phase 4 complete. Writing summary...');
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

    // Write log to file
    try
      TestLog.SaveToFile(ActualLogFile);
    except
      on E: Exception do
      begin
        ActualLogFile := 'UITestResults.log';
        TestLog.SaveToFile(ActualLogFile);
      end;
    end;

    // Also write to stdout for --selftest mode
    if HasSelfTestParam then
      for I := 0 to TestLog.Count - 1 do
        WriteLn(TestLog[I]);

  finally
    MenuItems.Free;
    TestLog.Free;
    TestLog := nil;
  end;
end;

end.
