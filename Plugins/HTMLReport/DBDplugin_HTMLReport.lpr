program DBDplugin_HTMLReport;

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
  EERModel in '../../EERModel.pas',
  EERDM in '../../EERDM.pas' {DMEER: TDataModule},
  MainDM in '../../MainDM.pas' {DMMain: TDataModule},
  EditorString in '../../EditorString.pas' {EditorStringForm},
  GlobalSysFunctions in '../../GlobalSysFunctions.pas',
{$IFDEF USE_IXMLDBMODELType}
  EERModel_XML in '../../EERModel_XML.pas',
{$ENDIF}
  LibXmlParser in '../../LibXmlParser.pas';

{$R *.res}

begin
  Application.Scaled := True;
  Application.Initialize;
  Application.Title := 'HTML Reporter';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
