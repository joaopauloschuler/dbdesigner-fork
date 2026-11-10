program TestSQLExport;
{$I DBDesigner4.inc}

// SQL export test: loads bin/Examples/order.xml into a TEERModel and prints
// the CREATE TABLE statement of every table.
//
// Build:  lazbuild tests/TestSQLExport.lpi
// Run:    bin/TestSQLExport        (from the repository root)
//
// The exit code is 0 only when the model loaded and every table produced a
// CREATE TABLE statement. Failures are written to stdout and end with exit
// code 1; the program never shows a dialog.

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Interfaces, // LCL
  // InterfaceBase added: hook for the message dialogs
  //Classes, SysUtils, Forms, Controls, Graphics,
  Classes, SysUtils, Forms, Controls, Graphics, InterfaceBase,
  // MainDM added: the model needs the DMMain and DMEER data modules
  //EERModel, EERDM, LibXmlParser;
  MainDM, EERModel, EERDM, LibXmlParser;

type
  // Exceptions raised inside LCL message handlers would otherwise end in the
  // modal LCL exception dialog.
  TTestExceptionHandler = class
    procedure HandleException(Sender: TObject; E: Exception);
  end;

var
  Model: TEERModel;
  i: Integer;
  Table: TEERTable;
  SQL: string;
  ModelFile: string;
  ParentForm: TForm;
  ExceptionHandler: TTestExceptionHandler;
  TableCount, FailCount: Integer;

procedure TTestExceptionHandler.HandleException(Sender: TObject; E: Exception);
begin
  WriteLn('FAILED: ', E.ClassName, ': ', E.Message);
  Halt(1);
end;

// Replaces the LCL message dialog (MessageDlg, ShowMessage): the application
// code reports many errors with a modal dialog, a test must not wait for a click.
function TestPromptDialog(const DialogCaption, DialogMessage: String;
  DialogType: longint; Buttons: PLongint;
  ButtonCount, DefaultIndex, EscapeResult: Longint;
  UseDefaultPos: boolean;
  X, Y: Longint): Longint;
begin
  WriteLn('FAILED: the program asked for a message dialog: ', DialogMessage);
  Halt(1);
  Result := EscapeResult;
end;

begin
  Application.Initialize;

  // No modal exception dialog: report on stdout and exit with code 1
  ExceptionHandler := TTestExceptionHandler.Create;
  Application.OnException := ExceptionHandler.HandleException;
  PromptDialogFunction := @TestPromptDialog;

  WriteLn('=== SQL Export Test ===');
  WriteLn;

  ModelFile := 'bin/Examples/order.xml';
  if not FileExists(ModelFile) then
  begin
    WriteLn('ERROR: Model file not found: ', ModelFile);
    // hint added
    WriteLn('Run from the project root directory');
    Halt(1);
  end;

  // Everything below is guarded: an exception outside a message handler is
  // reported here instead of by the default handler (slow backtrace + dialog).
  try
    // The model reads its texts, datatypes and display settings from the
    // DMMain and DMEER data modules, like the main form creates them.
    // The test uses the default settings shipped next to the executable
    // (bin/Data) and never writes settings.
    SettingsReadOnly := True;
    DMMain := TDMMain.Create(Application);
    DMMain.SettingsPath := ExtractFilePath(Application.ExeName) + 'Data' + PathDelim;
    DMMain.ProgName := 'DBDesignerFork';
    if not FileExists(DMMain.SettingsPath + 'DBDesignerFork_DatabaseInfo.ini') then
    begin
      WriteLn('ERROR: Default settings not found in: ', DMMain.SettingsPath);
      Halt(1);
    end;
    DMMain.LoadLanguageFromIniFile;
    DMMain.LoadTranslatedMessages;
    DMEER := TDMEER.Create(Application);

    // Create a parent form (TEERModel inherits from TPanel, needs a parent)
    ParentForm := TForm.Create(nil);
    try
      Model := TEERModel.Create(ParentForm);
      Model.Parent := ParentForm;

      WriteLn('Loading model: ', ModelFile);
      try
        Model.LoadFromFile(ModelFile);
      except
        on E: Exception do
        begin
          WriteLn('Load error: ', E.ClassName, ': ', E.Message);
          Halt(1);
        end;
      end;
      WriteLn('Components: ', Model.ComponentCount);
      WriteLn;

      TableCount := 0;
      FailCount := 0;

      // Iterate through components to find tables
      for i := 0 to Model.ComponentCount - 1 do
      begin
        if Model.Components[i] is TEERTable then
        begin
          Table := TEERTable(Model.Components[i]);
          Inc(TableCount);
          WriteLn('--- ', Table.ObjName, ' ---');
          try
            // wrong parameter list in the old comment
            //// params: DefinePK, DefineIndices, HideNullField, DefaultBeforeNotNull, DatabaseType, OutputComments
            // params: DefinePK, CreateIndices, DefineFK, TblOptions, StdInserts, OutputComments
            SQL := Table.GetSQLCreateCode(True, True, False, True, False, True);
            WriteLn(SQL);
            if Pos('CREATE TABLE', SQL) = 0 then
            begin
              WriteLn('  SQL ERROR: no CREATE TABLE statement');
              Inc(FailCount);
            end;
          except
            on E: Exception do
            begin
              WriteLn('  SQL ERROR: ', E.ClassName, ': ', E.Message);
              Inc(FailCount);
            end;
          end;
        end;
      end;

      WriteLn;
      if TableCount = 0 then
      begin
        WriteLn('FAILED: the model contains no tables');
        ExitCode := 1;
      end
      else if FailCount > 0 then
      begin
        WriteLn('FAILED: ', FailCount, ' of ', TableCount, ' tables without CREATE TABLE statement');
        ExitCode := 1;
      end
      else
        //WriteLn('SUCCESS: SQL export test complete');
        WriteLn('SUCCESS: SQL export test complete, ', TableCount, ' tables');
    finally
      ParentForm.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('FAILED: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
