
# DBDesigner Fork — Lazarus Port Task List

> **Instructions:** As you complete each task, mark it by changing `[ ]` to `[X]`.
> Example: `[X] Task completed` ✅
>
> Work through the phases in order. Within each phase, tasks are listed in recommended order.
> Commit after completing each logical group of tasks.
>
> See [port-to-lazarus.md](port-to-lazarus.md) for detailed instructions on each task.

---

## Phase 0 — Project Setup & Scaffolding

### 0.1 Lazarus Project File
- [ ] Copy `DBDesignerFork.dpr` to `DBDesignerFork.lpr`
- [ ] Replace `QForms` with `Forms` and add `Interfaces`, `cthreads` in `.lpr` uses clause
- [ ] Remove SynEdit unit paths and `{$IFDEF MSWINDOWS}` blocks from `.lpr`
- [ ] Create `.lpi` project file by opening `.lpr` in Lazarus IDE
- [ ] Configure output directory (`bin/`), unit output directory (`dcu/` or `lib/`)
- [ ] Add `LCL` as a required package in project inspector

### 0.2 CLX → LCL Shim Units
- [ ] Create `clx_shims/` folder
- [ ] Add `clx_shims/` to project unit search path
- [ ] Create `clx_shims/QForms.pas` (re-exports `Forms`)
- [ ] Create `clx_shims/QControls.pas` (re-exports `Controls`)
- [ ] Create `clx_shims/QGraphics.pas` (re-exports `Graphics`)
- [ ] Create `clx_shims/QDialogs.pas` (re-exports `Dialogs`)
- [ ] Create `clx_shims/QStdCtrls.pas` (re-exports `StdCtrls`)
- [ ] Create `clx_shims/QExtCtrls.pas` (re-exports `ExtCtrls`)
- [ ] Create `clx_shims/QMenus.pas` (re-exports `Menus`)
- [ ] Create `clx_shims/QImgList.pas` (re-exports `ImgList`)
- [ ] Create `clx_shims/QComCtrls.pas` (re-exports `ComCtrls`)
- [ ] Create `clx_shims/QPrinters.pas` (re-exports `Printers`)
- [ ] Create `clx_shims/QClipbrd.pas` (re-exports `Clipbrd`)
- [ ] Create `clx_shims/QTypes.pas` (stub with `LCLType` + type aliases as needed)

### 0.3 Compiler Mode Directive
- [ ] Add `{$mode delphi}` and `{$H+}` to `DBDesigner4.inc`
- [ ] Verify all `.pas` files include `{$I DBDesigner4.inc}` (add where missing)
- [ ] For files that don't include the `.inc`, add `{$mode delphi}` directly

### 0.4 Form File Conversion
- [ ] Batch-copy all 39 `.xfm` files to `.lfm` (root directory)
- [ ] Batch-copy plugin `.xfm` files to `.lfm` (`Plugins/*/`)
- [ ] Open each `.lfm` in Lazarus and note unknown properties
- [ ] Remove CLX-specific properties (`WidgetFlags`, etc.) from `.lfm` files

### 0.5 First Compilation Attempt
- [ ] Run `lazbuild DBDesignerFork.lpi` and save output to `build-errors-phase0.log`
- [ ] Review errors to establish baseline scope
- [ ] Commit Phase 0 work

---

## Phase 1 — Non-Visual Core Units

### 1.1 LibXmlParser.pas
- [ ] Verify `{$mode delphi}` is included
- [ ] Compile `LibXmlParser.pas` standalone
- [ ] Fix any FPC-specific issues (PChar, string types)
- [ ] Verify unit compiles cleanly

### 1.2 GlobalSysFunctions.pas
- [ ] Review `uses` clause for CLX dependencies
- [ ] Compile and fix any issues
- [ ] Verify unit compiles cleanly

### 1.3 RegExpr.pas
- [ ] Decide: keep bundled version or use FPC's built-in RegExpr
- [ ] If keeping: add `{$mode delphi}`, compile and fix
- [ ] If replacing: update all call sites to match FPC RegExpr API
- [ ] Verify unit compiles cleanly

### 1.4 EERModel.pas — ⚠️ Critical (14,343 lines)
- [ ] Inventory all `Qt` unit references (search for `Qt.`, `QCustomEvent`, `QEventType`, `QApplication`, `QWidget`, `QPainter`, etc.)
- [ ] Consider creating `QtCompat.pas` wrapper unit for common Qt patterns
- [ ] Replace `QApplication_postEvent` → `PostMessage` / `Application.QueueAsyncCall`
- [ ] Replace `QCustomEvent` / `QEventType` → LCL message constants (`WM_USER + N`)
- [ ] Replace `QWidget_*` calls → LCL equivalents (`SetBounds`, `Constraints`, etc.)
- [ ] Replace `QPainter_*` calls → `Canvas` methods
- [ ] Handle `{$IFDEF USE_IXMLDBMODELType}` — decide approach (see 1.5)
- [ ] Compile and fix remaining errors iteratively
- [ ] Verify unit compiles cleanly

### 1.5 XML Handling
- [ ] Replace `xmldom` → `laz2_DOM` in all affected files
- [ ] Replace `XMLDoc` → `laz2_XMLRead`, `laz2_XMLWrite` in all affected files
- [ ] Replace `XMLIntf` → `laz2_DOM` in all affected files
- [ ] Replace `IXMLDocument` → `TXMLDocument` usage patterns
- [ ] Replace `IXMLNode` → `TDOMNode` usage patterns
- [ ] Update `EERModel.pas` XML sections
- [ ] Update `EERModel_XML.pas` (4,830 lines)
- [ ] Update `EERModel_XML_ERwin41_Import.pas` (6,332 lines)
- [ ] Update `MainDM.pas` XML sections
- [ ] Verify all XML-related units compile cleanly

### 1.6 EERExportImportDM.pas
- [ ] Compile and fix (depends on EERModel + XML units)
- [ ] Verify unit compiles cleanly

### 1.7 Phase 1 Wrap-up
- [ ] All non-visual core units compile
- [ ] Commit Phase 1 work

---

## Phase 2 — Database Layer (DBXpress → SQLDB)

### 2.1 DBDM.pas — Core Database Module (1,050 lines)
- [ ] Replace `uses` clause: `DBXpress, FMTBcd, DBClient, Provider, SqlExpr` → `SQLDB, BufDataset`
- [ ] Add SQLDB connector units (mysql, postgres, sqlite, etc.)
- [ ] Replace `TDataSetProvider` / `TClientDataSet` → direct `TSQLQuery` or `TBufDataset`
- [ ] Add `TSQLTransaction` between connection and queries
- [ ] Rewrite `ConnectToDB` to create connector by DriverName (factory pattern)
- [ ] Update connection parameter mapping (HostName, DatabaseName, UserName, Password)
- [ ] Update query execution patterns (remove provider/clientdataset indirection)
- [ ] Update `GetTableNames`, `GetFieldNames` for SQLDB metadata API
- [ ] Compile and fix remaining errors
- [ ] Verify unit compiles cleanly

### 2.2 DBEERDM.pas — EER Database Operations (3,074 lines)
- [ ] Replace `uses` clause DB units
- [ ] Update reverse engineering queries for SQLDB
- [ ] Update synchronisation queries for SQLDB
- [ ] Update metadata retrieval calls
- [ ] Compile and fix remaining errors
- [ ] Verify unit compiles cleanly

### 2.3 MainDM.pas — Main Data Module (1,881 lines)
- [ ] Replace `uses` clause DB units
- [ ] Update any DB component references
- [ ] Compile and fix remaining errors
- [ ] Verify unit compiles cleanly

### 2.4 Database UI Forms
- [ ] Update `DBConnSelect.pas` (1,434 lines)
- [ ] Update `DBConnEditor.pas` (562 lines)
- [ ] Update `DBConnLogin.pas` (127 lines)
- [ ] Update `EditorQuery.pas` (3,085 lines) — DB portions
- [ ] Update `EditorTableData.pas` (805 lines)
- [ ] Update `EERStoreInDatabase.pas` (618 lines)
- [ ] Update `EERReverseEngineering.pas` (592 lines) — DB portions
- [ ] Update `EERSynchronisation.pas` (226 lines) — DB portions

### 2.5 Configuration Compatibility
- [ ] Review `bin/Data/DBConn_DefaultSettings.ini` — adapt parameter names if needed
- [ ] Review `bin/Data/DBDesignerFork_DatabaseInfo.ini` — verify compatibility
- [ ] Test connection with at least one database engine

### 2.6 Phase 2 Wrap-up
- [ ] All database-related units compile
- [ ] Commit Phase 2 work

---

## Phase 3 — UI Forms (CLX → LCL)

### 3.1 Qt Unit Replacement Preparation
- [ ] Create list of all `Qt` unit patterns used across the 32 affected files
- [ ] Create `QtCompat.pas` helper unit if beneficial (wrapper functions)
- [ ] Define LCL message constants to replace `QEventType_*` constants

### 3.2 Tier 1 — Simple Dialogs
- [ ] `Splash.pas` / `.lfm` — Splash screen
- [ ] `Tips.pas` / `.lfm` — Tips dialog
- [ ] `ZoomSel.pas` / `.lfm` — Zoom selector
- [ ] `EditorString.pas` / `.lfm` — String editor
- [ ] `EditorNote.pas` / `.lfm` — Note editor
- [ ] `EditorImage.pas` / `.lfm` — Image editor
- [ ] `EditorRegion.pas` / `.lfm` — Region editor
- [ ] `PrinterSettings.pas` / `.lfm` — Printer settings
- [ ] `DBConnLogin.pas` / `.lfm` — Login dialog
- [ ] `EditorDatatype.pas` / `.lfm` — Datatype editor
- [ ] Compile all Tier 1 forms
- [ ] Commit Tier 1

### 3.3 Tier 2 — Medium Complexity
- [ ] `EditorRelation.pas` / `.lfm` — Relation editor
- [ ] `EditorTableFieldParam.pas` / `.lfm` — Field parameter editor
- [ ] `EditorTable.pas` / `.lfm` — Table editor (2,041 lines)
- [ ] `PaletteTools.pas` / `.lfm` — Tools palette
- [ ] `PaletteDataTypesReplace.pas` / `.lfm` — Datatype replace palette
- [ ] `PaletteDatatypes.pas` / `.lfm` — Datatypes palette
- [ ] `Options.pas` / `.lfm` — Options dialog
- [ ] `OptionsModel.pas` / `.lfm` — Model options
- [ ] `DBConnEditor.pas` / `.lfm` — Connection editor
- [ ] `DBConnSelect.pas` / `.lfm` — Connection selector
- [ ] Compile all Tier 2 forms
- [ ] Commit Tier 2

### 3.4 Tier 3 — Complex Forms
- [ ] `PaletteModel.pas` / `.lfm` — Model palette
- [ ] `PaletteNav.pas` / `.lfm` — Navigation palette
- [ ] `EERPageSetup.pas` / `.lfm` — Page setup
- [ ] `EERExportSQLScript.pas` / `.lfm` — SQL export
- [ ] `EERPlaceModel.pas` / `.lfm` — Model placement
- [ ] `EERReverseEngineering.pas` / `.lfm` — Reverse engineering
- [ ] `EERStoreInDatabase.pas` / `.lfm` — Store in DB
- [ ] `EERSynchronisation.pas` / `.lfm` — Synchronisation
- [ ] `EditorTableData.pas` / `.lfm` — Table data editor
- [ ] `EditorQuery.pas` / `.lfm` — Query editor (3,085 lines)
- [ ] `EditorQueryDragTarget.pas` / `.lfm` — Query drag target
- [ ] Compile all Tier 3 forms
- [ ] Commit Tier 3

### 3.5 Tier 4 — Core Forms & Data Modules
- [ ] `GUIDM.pas` / `.lfm` — GUI data module
- [ ] `EERDM.pas` / `.lfm` — EER data module
- [ ] `EER.pas` / `.lfm` — EER form (hosts the model canvas)
- [ ] `EERExportImportDM.pas` — Export/import data module
- [ ] `MainDM.pas` / `.lfm` — Main data module
- [ ] `Main.pas` / `.lfm` — Main application form (3,514 lines)
- [ ] Compile all Tier 4 forms
- [ ] Commit Tier 4

### 3.6 Non-Form Units with Qt Dependencies
- [ ] `EditorTableField.pas` — replace Qt usage
- [ ] `EditorTableFieldDatatypeInplace.pas` — review for Qt usage
- [ ] Compile and verify

### 3.7 Phase 3 Wrap-up
- [ ] All UI forms compile
- [ ] Full project compiles (possibly with SynEdit disabled)
- [ ] Commit Phase 3 work

---

## Phase 4 — SynEdit Integration

### 4.1 Remove Bundled SynEdit
- [ ] Remove `QSynEdit*` and `QSynHighlighter*` references from `.lpr`
- [ ] Add `SynEdit` package as project dependency in `.lpi`

### 4.2 Update SynEdit References
- [ ] Replace `QSynEdit` → `SynEdit` in all source files
- [ ] Replace `QSynHighlighterSQL` → `SynHighlighterSQL` in all source files
- [ ] Replace `QSynEditHighlighter` → `SynEditHighlighter` in all source files
- [ ] Replace `QSynEditKeyCmds` → `SynEditKeyCmds` in all source files
- [ ] Replace `QSynEditTextBuffer` → `SynEditTextBuffer` in all source files
- [ ] Replace `QSynEditMiscClasses` → `SynEditMiscClasses` in all source files
- [ ] Replace `QSynEditMiscProcs` → `SynEditMiscProcs` in all source files
- [ ] Replace `QSynEditStrConst` → `SynEditStrConst` in all source files
- [ ] Replace `QSynEditKbdHandler` → `SynEditKbdHandler` in all source files
- [ ] Replace `QSynEditKeyConst` → `SynEditKeyConst` in all source files
- [ ] Replace `QSynEditTypes` → `SynEditTypes` in all source files
- [ ] Replace `QSynHighlighterHashEntries` → `SynHighlighterHashEntries` in all source files
- [ ] Handle `kTextDrawer.pas` — may not be needed with Lazarus SynEdit

### 4.3 Fix API Differences
- [ ] Replace `TSynMemo` → `TSynEdit` if used
- [ ] Update `Gutter.ShowLineNumbers` → `Gutter.LineNumberPart.Visible` if used
- [ ] Fix any other Lazarus SynEdit API differences
- [ ] Re-enable `{$DEFINE USE_SYNEDIT}` in `DBDesigner4.inc`

### 4.4 Phase 4 Wrap-up
- [ ] SynEdit compiles and integrates correctly
- [ ] SQL syntax highlighting works in editor
- [ ] Commit Phase 4 work

---

## Phase 5 — Plugins & Extras

### 5.1 EmbeddedPDF Library
- [ ] `EmbeddedPdfConf.inc` — add `{$mode delphi}` / FPC compatibility
- [ ] `EmbeddedPdfTypes.pas` — compile and fix (string/char types)
- [ ] `EmbeddedPdfFonts.pas` — compile and fix
- [ ] `EmbeddedPdfDoc.pas` — compile and fix (2,841 lines)
- [ ] `EmbeddedPdfImages.pas` — replace `Qt` image calls with LCL equivalents
- [ ] `EmbeddedPdfDB.pas` — compile and fix
- [ ] Test PDF export with a sample model

### 5.2 Plugin Loading Infrastructure
- [ ] Update plugin loading code to use FPC `dynlibs` unit
- [ ] Add platform-aware library extension (`.dll` / `.so` / `.dylib`)
- [ ] Test plugin discovery and loading

### 5.3 Demo Plugin
- [ ] Convert `Plugins/Demo/DBDplugin_Demo.dpr` → `.lpr`
- [ ] Port `Plugins/Demo/Main.pas` and `.xfm` → `.lfm`
- [ ] Compile and test plugin loading

### 5.4 HTMLReport Plugin
- [ ] Convert `Plugins/HTMLReport/DBDplugin_HTMLReport.dpr` → `.lpr`
- [ ] Port `Plugins/HTMLReport/Main.pas` and `.xfm` → `.lfm`
- [ ] Compile and test HTML report generation

### 5.5 DataImporter Plugin
- [ ] Convert `Plugins/DataImporter/DBDplugin_DataImporter.dpr` → `.lpr`
- [ ] Port all DataImporter `.pas` files (replace DBXpress with SQLDB)
- [ ] Convert DataImporter `.xfm` → `.lfm`
- [ ] Compile and test data import

### 5.6 SimpleWebFront Plugin
- [ ] Convert `Plugins/SimpleWebFront/DBDplugin_SimpleWebFront.dpr` → `.lpr`
- [ ] Port all SimpleWebFront `.pas` files (replace XMLDoc, Qt references)
- [ ] Convert SimpleWebFront `.xfm` → `.lfm`
- [ ] Compile and test web front-end generation

### 5.7 Phase 5 Wrap-up
- [ ] All plugins compile as shared libraries
- [ ] EmbeddedPDF compiles and generates valid PDFs
- [ ] Commit Phase 5 work

---

## Final — Integration Testing & Cleanup

### Functional Testing
- [ ] Application launches without errors
- [ ] Load example model (`bin/Examples/order.xml`) — verify display
- [ ] Create a new model with tables, fields, and relations
- [ ] Save model to XML and reload — verify round-trip
- [ ] Export SQL script (MySQL) — verify output
- [ ] Export SQL script (PostgreSQL) — verify output
- [ ] Export SQL script (Oracle) — verify output
- [ ] Export SQL script (SQLite) — verify output
- [ ] Connect to a live MySQL database
- [ ] Reverse-engineer a database schema
- [ ] Synchronise model with database
- [ ] Test print / page setup
- [ ] Test PDF export
- [ ] Test zoom, navigation palette, model palette
- [ ] Test copy/paste of tables and relations
- [ ] Test undo functionality
- [ ] Load a plugin (Demo) — verify it runs
- [ ] Generate HTML report via plugin
- [ ] Test on Linux
- [ ] Test on Windows
- [ ] Test on macOS (if applicable)

### Code Cleanup
- [ ] Remove `clx_shims/` folder — replace all `Q*` references with direct LCL unit names
- [ ] Remove or archive the bundled `SynEdit/` folder
- [ ] Remove unused Delphi-specific files (`.dpr`, `.dof`, `.dsk`, `.dsm`, `.cfg`)
- [ ] Update `DBDesigner4.inc` — remove obsolete defines
- [ ] Review and clean up any remaining `{$IFDEF}` blocks for Delphi/Kylix
- [ ] Update `README.md` with new build instructions for Lazarus
- [ ] Update `port-to-lazarus.md` with any lessons learned
- [ ] Final commit and tag release

---

## Progress Summary

| Phase | Status | Tasks | Done |
|---|---|---|---|
| Phase 0 — Setup & Scaffolding | ⬜ Not started | 30 | 0 |
| Phase 1 — Non-Visual Core | ⬜ Not started | 34 | 0 |
| Phase 2 — Database Layer | ⬜ Not started | 33 | 0 |
| Phase 3 — UI Forms | ⬜ Not started | 54 | 0 |
| Phase 4 — SynEdit | ⬜ Not started | 22 | 0 |
| Phase 5 — Plugins & Extras | ⬜ Not started | 27 | 0 |
| Final — Testing & Cleanup | ⬜ Not started | 29 | 0 |
| **Total** | | **229** | **0** |

> Update the "Done" column and status as you progress:
> - ⬜ Not started
> - 🟡 In progress
> - ✅ Complete
