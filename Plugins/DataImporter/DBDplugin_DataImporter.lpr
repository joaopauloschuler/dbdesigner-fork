program DBDplugin_DataImporter;

{$IFDEF MSWINDOWS}
{$I ..\..\src\DBDesigner4.inc}
{$ELSE}
{$I ../../src/DBDesigner4.inc}
{$ENDIF}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Interfaces,
  Forms,
  Main in 'Main.pas' {MainForm},
  DBImportData in 'DBImportData.pas' {DBImportDataForm},
  Progress in 'Progress.pas' {ProgressForm},
  DBDM in '../../src/DBDM.pas' {DMDB: TDataModule},
  MainDM in '../../src/MainDM.pas' {DMMain: TDataModule},
  DBConnSelect in '../../src/DBConnSelect.pas' {DBConnSelectForm},
  DBConnLogin in '../../src/DBConnLogin.pas' {DBConnLoginForm},
  DBConnEditor in '../../src/DBConnEditor.pas' {DBConnEditorForm},
  EditorString in '../../src/EditorString.pas' {EditorStringForm},
  GlobalSysFunctions in '../../src/GlobalSysFunctions.pas';

{$R *.res}

begin
  Application.Scaled := True;
  Application.Initialize;
  Application.Title := 'DataImporter';
  Application.ShowMainForm := False;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
