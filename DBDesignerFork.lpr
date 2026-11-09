
program DBDesignerFork;

//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of DBDesigner Fork which is forked from DBDesigner 4.
// Lazarus/FPC port.
//
//----------------------------------------------------------------------------------------------------------------------

{$I src/DBDesigner4.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // LCL widgetset
  Forms,

//-------------------------------------------------------------
// Include all units

  Main in 'src/Main.pas' {MainForm},
  MainDM in 'src/MainDM.pas' {DMMain: TDataModule},
  Splash in 'src/Splash.pas' {SplashForm},
  EER in 'src/EER.pas' {EERForm},
  PaletteTools in 'src/PaletteTools.pas' {PaletteToolsForm},
  EERModel in 'src/EERModel.pas',
  ZoomSel in 'src/ZoomSel.pas' {ZoomSelForm},
  PaletteModel in 'src/PaletteModel.pas' {PaletteModelFrom},
  PaletteDatatypes in 'src/PaletteDatatypes.pas' {PaletteDataTypesForm},
  EditorTable in 'src/EditorTable.pas' {EditorTableForm},
  EditorDatatype in 'src/EditorDatatype.pas' {EditorDatatypeForm},
  EditorString in 'src/EditorString.pas' {EditorStringForm},
  EditorRegion in 'src/EditorRegion.pas' {EditorRegionForm},
  EditorNote in 'src/EditorNote.pas' {EditorNoteForm},
  OptionsModel in 'src/OptionsModel.pas' {OptionsModelForm},
  EditorRelation in 'src/EditorRelation.pas' {EditorRelationForm},
  Options in 'src/Options.pas' {OptionsForm},
  EERPageSetup in 'src/EERPageSetup.pas' {EERPageSetupForm},
  PaletteNav in 'src/PaletteNav.pas' {PaletteNavForm},
  EditorImage in 'src/EditorImage.pas' {EditorImageForm},
  EERExportSQLScript in 'src/EERExportSQLScript.pas' {EERExportSQLScriptFrom},
  DBConnSelect in 'src/DBConnSelect.pas' {DBConnSelectForm},
  PaletteDataTypesReplace in 'src/PaletteDataTypesReplace.pas' {PaletteDataTypesReplaceForm},
  EERReverseEngineering in 'src/EERReverseEngineering.pas' {EERReverseEngineeringForm},
  EERSynchronisation in 'src/EERSynchronisation.pas' {EERSynchronisationForm},
  DBConnEditor in 'src/DBConnEditor.pas' {DBConnEditorForm},
  DBConnLogin in 'src/DBConnLogin.pas' {DBConnLoginForm},
  EERStoreInDatabase in 'src/EERStoreInDatabase.pas' {EERStoreInDatabaseForm},
  EERDM in 'src/EERDM.pas' {DMEER: TDataModule},
  EditorTableField in 'src/EditorTableField.pas',
  EditorTableFieldDatatypeInplace in 'src/EditorTableFieldDatatypeInplace.pas',
  GUIDM in 'src/GUIDM.pas' {DMGUI: TDataModule},
  DBDM in 'src/DBDM.pas' {DMDB: TDataModule},
  DBEERDM in 'src/DBEERDM.pas' {DMDBEER: TDataModule},
  EditorTableFieldParam in 'src/EditorTableFieldParam.pas' {EditorTableFieldParamForm},
  Tips in 'src/Tips.pas' {TipsForm},
  EditorQuery in 'src/EditorQuery.pas' {EditorQueryForm},
  EditorQueryDragTarget in 'src/EditorQueryDragTarget.pas' {EditorQueryDragTargetForm},
  EERExportImportDM in 'src/EERExportImportDM.pas',

//-------------------------------------------------------------
// Include SynEdit if USE_SYNEDIT is defined
// Lazarus SynEdit package is used instead of bundled QSynEdit

{$IFDEF USE_SYNEDIT}
  // SynEdit units are provided by the Lazarus SynEdit package
{$ENDIF}

//-------------------------------------------------------------
// Include EmbeddedPDF

  EmbeddedPdfDoc in 'src/EmbeddedPDF/EmbeddedPdfDoc.pas',
  EmbeddedPdfFonts in 'src/EmbeddedPDF/EmbeddedPdfFonts.pas',
  EmbeddedPdfTypes in 'src/EmbeddedPDF/EmbeddedPdfTypes.pas',
  EmbeddedPdfImages in 'src/EmbeddedPDF/EmbeddedPdfImages.pas',
  EmbeddedPdfDB in 'src/EmbeddedPDF/EmbeddedPdfDB.pas',

//-------------------------------------------------------------
// Include XML model support if USE_IXMLDBMODELType is defined

{$IFDEF USE_IXMLDBMODELType}
  EERModel_XML_ERwin41_Import in 'src/EERModel_XML_ERwin41_Import.pas',
  EERModel_XML in 'src/EERModel_XML.pas',
{$ENDIF}

//-------------------------------------------------------------
// Include the new LibXmlParser

  LibXmlParser in 'src/LibXmlParser.pas',

  EERPlaceModel in 'src/EERPlaceModel.pas',
  MSSQLRevEngUtils in 'src/MSSQLRevEngUtils.pas',
  // RegExpr, // using system regexpr
  GlobalSysFunctions in 'src/GlobalSysFunctions.pas',
  UITestRunner in 'src/UITestRunner.pas';

{$R src/DBDesignerFork.res}

begin
  Application.Initialize;
  Application.Title := 'DBDesigner Fork';

  //Initialize Application Font
  LoadApplicationFont;

  //Show Splash Form
  //SplashForm:=TSplashForm.Create(Application);
  //SplashForm.VersionLbl.Caption:='1.5';
  //SplashForm.Show;
  //SplashForm.Update;

  Application.ShowMainForm := False;
  Application.CreateForm(TMainForm, MainForm);
  //Bring Splash Screen back to top again...
  //SplashForm.BringToFront;

  Application.Run;
end.
