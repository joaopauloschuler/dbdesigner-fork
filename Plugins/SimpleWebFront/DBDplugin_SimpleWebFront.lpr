program DBDplugin_SimpleWebFront;

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
  EERModel in '../../src/EERModel.pas',
  EERDM in '../../src/EERDM.pas' {DMEER: TDataModule},
  MainDM in '../../src/MainDM.pas' {DMMain: TDataModule},
  EditorString in '../../src/EditorString.pas' {EditorStringForm},
{$IFDEF USE_IXMLDBMODELType}
  EERModel_XML in '../../src/EERModel_XML.pas',
{$ENDIF}
  Weboutput in 'Weboutput.pas',
  SplitFns in 'SplitFns.pas',
  Layer in 'Layer.pas',
  DialogImageSelection in 'DialogImageSelection.pas' {ImageSelectionForm},
  StringConstants in 'StringConstants.pas',
  DialogDirectorySelect in 'DialogDirectorySelect.pas' {DialogDirectorySelectForm},
  EditorView in 'EditorView.pas' {EditorViewForm},
  EditorGroup in 'EditorGroup.pas' {EditorGroupForm},
  Splash in 'Splash.pas' {SplashForm},
  DialogPopupSettings in 'DialogPopupSettings.pas' {DialogPopupSettingsForm},
  SWF_XML_Binding in 'SWF_XML_Binding.pas',
  DialogBugs in 'DialogBugs.pas' {DialogBugsForm},
  LibXmlParser in '../../src/LibXmlParser.pas',
  GlobalSysFunctions in '../../src/GlobalSysFunctions.pas';

{$R *.res}

begin
  Application.Scaled := True;
  Application.Initialize;
  Application.Title := 'Simple Web Front';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
