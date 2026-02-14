program DBDplugin_DataImporter;

{$IFDEF MSWINDOWS}
{$I ..\..\DBDesigner4.inc}
{$ELSE}
{$I ../../DBDesigner4.inc}
{$ENDIF}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Interfaces,
  Forms,
  Main in 'Main.pas' {MainForm},
  DBImportData in 'DBImportData.pas' {DBImportDataForm},
  Progress in 'Progress.pas' {ProgressForm},
  DBDM in '../../DBDM.pas' {DMDB: TDataModule},
  MainDM in '../../MainDM.pas' {DMMain: TDataModule},
  DBConnSelect in '../../DBConnSelect.pas' {DBConnSelectForm},
  DBConnLogin in '../../DBConnLogin.pas' {DBConnLoginForm},
  DBConnEditor in '../../DBConnEditor.pas' {DBConnEditorForm},
  EditorString in '../../EditorString.pas' {EditorStringForm},
  GlobalSysFunctions in '../../GlobalSysFunctions.pas';

{$R *.res}

begin
  Application.Scaled := True;
  Application.Initialize;
  Application.Title := 'DataImporter';
  Application.ShowMainForm := False;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
