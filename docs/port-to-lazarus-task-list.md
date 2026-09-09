
# DBDesigner Fork — Lazarus Port Task List

> **Instructions:** As you complete each task, mark it by changing `[ ]` to `[X]`.
> Example: `[X] Task completed` ✅ — `[~]` marks a task that is partly done (see its sub-items).
>
> Work through the phases in order. Within each phase, tasks are listed in recommended order.
> Commit after completing each logical group of tasks.
>
> See [port-to-lazarus.md](port-to-lazarus.md) for detailed instructions on each task.

---

## Phase 0 — Project Setup & Scaffolding

### 0.1 Lazarus Project File
- [X] Copy `DBDesignerFork.dpr` to `DBDesignerFork.lpr`
- [X] Replace `QForms` with `Forms` and add `Interfaces`, `cthreads` in `.lpr` uses clause
- [X] Remove SynEdit unit paths and `{$IFDEF MSWINDOWS}` blocks from `.lpr`
- [X] Create `.lpi` project file by opening `.lpr` in Lazarus IDE
- [X] Configure output directory (`bin/`), unit output directory (`dcu/` or `lib/`)
- [X] Add `LCL` as a required package in project inspector

### 0.2 CLX → LCL Shim Units
- [X] Create `clx_shims/` folder
- [X] Add `clx_shims/` to project unit search path
- [X] Create `clx_shims/QForms.pas` (re-exports `Forms`)
- [X] Create `clx_shims/QControls.pas` (re-exports `Controls`)
- [X] Create `clx_shims/QGraphics.pas` (re-exports `Graphics`)
- [X] Create `clx_shims/QDialogs.pas` (re-exports `Dialogs`)
- [X] Create `clx_shims/QStdCtrls.pas` (re-exports `StdCtrls`)
- [X] Create `clx_shims/QExtCtrls.pas` (re-exports `ExtCtrls`)
- [X] Create `clx_shims/QMenus.pas` (re-exports `Menus`)
- [X] Create `clx_shims/QImgList.pas` (re-exports `ImgList`)
- [X] Create `clx_shims/QComCtrls.pas` (re-exports `ComCtrls`)
- [X] Create `clx_shims/QPrinters.pas` (re-exports `Printers`)
- [X] Create `clx_shims/QClipbrd.pas` (re-exports `Clipbrd`)
- [X] Create `clx_shims/QTypes.pas` (stub with `LCLType` + type aliases as needed)

### 0.3 Compiler Mode Directive
- [X] Add `{$mode delphi}` and `{$H+}` to `DBDesigner4.inc`
- [X] Verify all `.pas` files include `{$I DBDesigner4.inc}` (add where missing)
- [X] For files that don't include the `.inc`, add `{$mode delphi}` directly

### 0.4 Form File Conversion
- [X] Batch-copy all 39 `.xfm` files to `.lfm` (root directory)
- [X] Batch-copy plugin `.xfm` files to `.lfm` (`Plugins/*/`)
- [X] Open each `.lfm` in Lazarus and note unknown properties (Font.Weight removed)
- [X] Remove CLX-specific properties (`WidgetFlags`, etc.) from `.lfm` files

### 0.5 First Compilation Attempt
- [X] Run `lazbuild DBDesignerFork.lpi` and save output to `build-errors-phase0.log`
- [X] Review errors to establish baseline scope
- [X] Commit Phase 0 work

---

## Phase 1 — Non-Visual Core Units

### 1.1 LibXmlParser.pas
- [X] Verify `{$mode delphi}` is included
- [X] Compile `LibXmlParser.pas` standalone
- [X] Fix any FPC-specific issues (PChar, string types)
- [X] Verify unit compiles cleanly

### 1.2 GlobalSysFunctions.pas
- [X] Review `uses` clause for CLX dependencies
- [X] Compile and fix any issues
- [X] Verify unit compiles cleanly

### 1.3 RegExpr.pas
- [X] Decide: keep bundled version or use FPC's built-in RegExpr
- [X] If keeping: add `{$mode delphi}`, compile and fix
- [X] Verify unit compiles cleanly

### 1.4 EERModel.pas — ⚠️ Critical (14,343 lines)
- [X] Inventory all `Qt` unit references (search for `Qt.`, `QCustomEvent`, `QEventType`, `QApplication`, `QWidget`, `QPainter`, etc.)
- [X] Consider creating `QtCompat.pas` wrapper unit for common Qt patterns
- [X] Replace `QApplication_postEvent` → `PostMessage` / `Application.QueueAsyncCall`
- [X] Replace `QCustomEvent` / `QEventType` → LCL message constants (`WM_USER + N`)
- [X] Replace `QWidget_*` calls → LCL equivalents (`SetBounds`, `Constraints`, etc.)
- [X] Replace `QPainter_*` calls → `Canvas` methods
- [X] Handle `{$IFDEF USE_IXMLDBMODELType}` — decide approach (see 1.5)
- [X] Compile and fix remaining errors iteratively
- [X] Verify unit compiles cleanly

### 1.5 XML Handling
- [X] Replace `xmldom` → `laz2_DOM` in all affected files (via shim unit xmldom.pas)
- [X] Replace `XMLDoc` → `laz2_XMLRead`, `laz2_XMLWrite` in all affected files (via shim unit XMLDoc.pas)
- [X] Replace `XMLIntf` → `laz2_DOM` in all affected files (via shim unit XMLIntf.pas with interface wrappers)
- [X] Replace `IXMLDocument` → `TXMLDocument` usage patterns (via XMLIntf.pas wrapper)
- [X] Replace `IXMLNode` → `TDOMNode` usage patterns (via XMLIntf.pas wrapper)
- [X] Update `EERModel.pas` XML sections (compiles with shim units)
- [X] Update `EERModel_XML.pas` (4,830 lines) (compiles with shim units)
- [X] Update `EERModel_XML_ERwin41_Import.pas` (6,332 lines) (compiles with shim units)
- [X] Update `MainDM.pas` XML sections (compiles with shim units)
- [X] Verify all XML-related units compile cleanly

### 1.6 EERExportImportDM.pas
- [X] Compile and fix (depends on EERModel + XML units)
- [X] Verify unit compiles cleanly

### 1.7 Phase 1 Wrap-up
- [X] All non-visual core units compile
- [X] Commit Phase 1 work

---

## Phase 2 — Database Layer (DBXpress → SQLDB)

### 2.1 DBDM.pas — Core Database Module (1,050 lines)
- [X] Replace `uses` clause: `DBXpress, FMTBcd, DBClient, Provider, SqlExpr` → `SQLDB, BufDataset` (via shim units)
- [~] Working SQLDB connector units
  - [x] `SQLite3Conn`
  - [x] `MySQL80Conn` 
  - [ ] Oracle
  - [ ] MSSQL
  - [ ] ODBC
  - [ ] PostgreSQL
- [X] Replace `TDataSetProvider` / `TClientDataSet` → direct `TSQLQuery` or `TBufDataset` (via shim: TClientDataSet wraps TBufDataset, TDataSetProvider bridges)
- [X] Add `TSQLTransaction` between connection and queries (handled inside TSQLConnection shim)
- [X] Rewrite `ConnectToDB` to create connector by DriverName (TSQLConnection.Open maps DriverName→ConnectorType)
- [X] Update connection parameter mapping (HostName, DatabaseName, UserName, Password) (TSQLConnection.Open extracts from Params)
- [X] Update query execution patterns (remove provider/clientdataset indirection) (shim handles chain)
- [X] Update `GetTableNames`, `GetFieldNames` for SQLDB metadata API (TSQLDataSet.SetSchemaInfo implemented)
- [X] Compile and fix remaining errors
- [X] Verify unit compiles cleanly

### 2.2 DBEERDM.pas — EER Database Operations (3,074 lines)
- [X] Replace `uses` clause DB units
- [X] Update reverse engineering queries for SQLDB (compiles via shim units)
- [X] Update synchronisation queries for SQLDB (compiles via shim units)
- [X] Update metadata retrieval calls (compiles via shim units)
- [X] Compile and fix remaining errors
- [X] Verify unit compiles cleanly

### 2.3 MainDM.pas — Main Data Module (1,881 lines)
- [X] Replace `uses` clause DB units
- [X] Update any DB component references (compiles via shim units)
- [X] Compile and fix remaining errors
- [X] Verify unit compiles cleanly

### 2.4 Database UI Forms
- [X] Update `DBConnSelect.pas` (1,434 lines) — compiles via shim units
- [X] Update `DBConnEditor.pas` (562 lines) — compiles via shim units
- [X] Update `DBConnLogin.pas` (127 lines) — compiles via shim units
- [X] Update `EditorQuery.pas` (3,085 lines) — LFM fixed, TSQLDataSet/TClientDataSet via shims
- [X] Update `EditorTableData.pas` (805 lines) — compiles via shim units
- [X] Update `EERStoreInDatabase.pas` (618 lines) — compiles via shim units
- [X] Update `EERReverseEngineering.pas` (592 lines) — compiles via shim units
- [X] Update `EERSynchronisation.pas` (226 lines) — compiles via shim units

### 2.5 Configuration Compatibility
- [X] Review `bin/Data/DBConn_DefaultSettings.ini` — parameter names preserved via shim layer
- [X] Review `bin/Data/DBDesignerFork_DatabaseInfo.ini` — compatible
- [X] Test connection with at least one database engine (SQLite 3 and MySQL 8 verified: connect, DDL, DML, query, schema info)

### 2.6 Phase 2 Wrap-up
- [X] All database-related units compile
- [X] Commit Phase 2 work

---

## Phase 3 — UI Forms (CLX → LCL)

### 3.1 Qt Unit Replacement Preparation
- [X] Create list of all `Qt` unit patterns used across the 32 affected files
- [X] Create `QtCompat.pas` helper unit if beneficial (wrapper functions)
- [X] Define LCL message constants to replace `QEventType_*` constants

### 3.2 Tier 1 — Simple Dialogs
- [X] `Splash.pas` / `.lfm` — Splash screen
- [X] `Tips.pas` / `.lfm` — Tips dialog
- [X] `ZoomSel.pas` / `.lfm` — Zoom selector
- [X] `EditorString.pas` / `.lfm` — String editor
- [X] `EditorNote.pas` / `.lfm` — Note editor
- [X] `EditorImage.pas` / `.lfm` — Image editor
- [X] `EditorRegion.pas` / `.lfm` — Region editor
- [X] `PrinterSettings.pas` / `.lfm` — Printer settings
- [X] `DBConnLogin.pas` / `.lfm` — Login dialog
- [X] `EditorDatatype.pas` / `.lfm` — Datatype editor
- [X] Compile all Tier 1 forms
- [X] Commit Tier 1

### 3.3 Tier 2 — Medium Complexity
- [X] `EditorRelation.pas` / `.lfm` — Relation editor
- [X] `EditorTableFieldParam.pas` / `.lfm` — Field parameter editor
- [X] `EditorTable.pas` / `.lfm` — Table editor (2,041 lines)
- [X] `PaletteTools.pas` / `.lfm` — Tools palette
- [X] `PaletteDataTypesReplace.pas` / `.lfm` — Datatype replace palette
- [X] `PaletteDatatypes.pas` / `.lfm` — Datatypes palette
- [X] `Options.pas` / `.lfm` — Options dialog
- [X] `OptionsModel.pas` / `.lfm` — Model options
- [X] `DBConnEditor.pas` / `.lfm` — Connection editor
- [X] `DBConnSelect.pas` / `.lfm` — Connection selector
- [X] Compile all Tier 2 forms
- [X] Commit Tier 2

### 3.4 Tier 3 — Complex Forms
- [X] `PaletteModel.pas` / `.lfm` — Model palette
- [X] `PaletteNav.pas` / `.lfm` — Navigation palette
- [X] `EERPageSetup.pas` / `.lfm` — Page setup
- [X] `EERExportSQLScript.pas` / `.lfm` — SQL export
- [X] `EERPlaceModel.pas` / `.lfm` — Model placement
- [X] `EERReverseEngineering.pas` / `.lfm` — Reverse engineering
- [X] `EERStoreInDatabase.pas` / `.lfm` — Store in DB
- [X] `EERSynchronisation.pas` / `.lfm` — Synchronisation
- [X] `EditorTableData.pas` / `.lfm` — Table data editor
- [X] `EditorQuery.pas` / `.lfm` — Query editor (3,085 lines)
- [X] `EditorQueryDragTarget.pas` / `.lfm` — Query drag target
- [X] Compile all Tier 3 forms
- [X] Commit Tier 3

### 3.5 Tier 4 — Core Forms & Data Modules
- [X] `GUIDM.pas` / `.lfm` — GUI data module
- [X] `EERDM.pas` / `.lfm` — EER data module
- [X] `EER.pas` / `.lfm` — EER form (hosts the model canvas)
- [X] `EERExportImportDM.pas` — Export/import data module
- [X] `MainDM.pas` / `.lfm` — Main data module
- [X] `Main.pas` / `.lfm` — Main application form (3,514 lines)
- [X] Compile all Tier 4 forms
- [X] Commit Tier 4

### 3.6 Non-Form Units with Qt Dependencies
- [X] `EditorTableField.pas` — replace Qt usage
- [X] `EditorTableFieldDatatypeInplace.pas` — review for Qt usage
- [X] Compile and verify

### 3.7 Phase 3 Wrap-up
- [X] All UI forms compile
- [X] Full project compiles (possibly with SynEdit disabled)
- [X] Commit Phase 3 work

---

## Phase 4 — SynEdit Integration

### 4.1 Remove Bundled SynEdit
- [X] Remove `QSynEdit*` and `QSynHighlighter*` references from `.lpr`
- [X] Add `SynEdit` package as project dependency in `.lpi`

### 4.2 Update SynEdit References
- [X] Replace `QSynEdit` → `SynEdit` in all source files
- [X] Replace `QSynHighlighterSQL` → `SynHighlighterSQL` in all source files
- [X] Replace `QSynEditHighlighter` → `SynEditHighlighter` in all source files
- [X] Replace `QSynEditKeyCmds` → `SynEditKeyCmds` in all source files
- [X] Replace `QSynEditTextBuffer` → `SynEditTextBuffer` in all source files
- [X] Replace `QSynEditMiscClasses` → `SynEditMiscClasses` in all source files
- [X] Replace `QSynEditMiscProcs` → `SynEditMiscProcs` in all source files
- [X] Replace `QSynEditStrConst` → `SynEditStrConst` in all source files
- [X] Replace `QSynEditKbdHandler` → `SynEditKbdHandler` in all source files
- [X] Replace `QSynEditKeyConst` → `SynEditKeyConst` in all source files
- [X] Replace `QSynEditTypes` → `SynEditTypes` in all source files
- [X] Replace `QSynHighlighterHashEntries` → `SynHighlighterHashEntries` in all source files
- [X] Handle `kTextDrawer.pas` — may not be needed with Lazarus SynEdit

### 4.3 Fix API Differences
- [X] Replace `TSynMemo` → `TSynEdit` if used
- [X] Update `Gutter.ShowLineNumbers` → `Gutter.LineNumberPart.Visible` if used
- [X] Fix any other Lazarus SynEdit API differences
- [X] Re-enable `{$DEFINE USE_SYNEDIT}` in `DBDesigner4.inc`

### 4.4 Phase 4 Wrap-up
- [X] SynEdit compiles and integrates correctly
- [X] SQL syntax highlighting works in editor
- [X] Commit Phase 4 work

---

## Phase 5 — Plugins & Extras

### 5.1 EmbeddedPDF Library
- [X] `EmbeddedPdfConf.inc` — add `{$mode delphi}` / FPC compatibility
- [X] `EmbeddedPdfTypes.pas` — compile and fix (string/char types)
- [X] `EmbeddedPdfFonts.pas` — compile and fix
- [X] `EmbeddedPdfDoc.pas` — compile and fix (2,841 lines)
- [X] `EmbeddedPdfImages.pas` — replace `Qt` image calls with LCL equivalents
- [X] `EmbeddedPdfDB.pas` — compile and fix
- [ ] Test PDF export with a sample model

### 5.2 Plugin Loading Infrastructure
- [X] Update plugin loading code — plugins are standalone executables, not shared libs; existing FindFirst/CreateProz mechanism works
- [X] Platform-aware — Linux executables have no extension, already handled
- [X] Test plugin discovery and loading — all four plugins start from the Plugins menu; DataImporter, SimpleWebFront and HTMLReport driven end to end (db-ui-bug-catalog, mysql-bug-catalog)

### 5.3 Demo Plugin
- [X] Convert `Plugins/Demo/DBDplugin_Demo.dpr` → `.lpr`
- [X] Port `Plugins/Demo/Main.pas` and `.xfm` → `.lfm`
- [X] Compile: 21793 lines, links successfully

### 5.4 HTMLReport Plugin
- [X] Convert `Plugins/HTMLReport/DBDplugin_HTMLReport.dpr` → `.lpr`
- [X] Port `Plugins/HTMLReport/Main.pas` and `.xfm` → `.lfm`
- [X] Compile: 22258 lines, links successfully

### 5.5 DataImporter Plugin
- [X] Convert `Plugins/DataImporter/DBDplugin_DataImporter.dpr` → `.lpr`
- [X] Port all DataImporter `.pas` files (CLX→LCL, DBXpress via shims)
- [X] Convert DataImporter `.xfm` → `.lfm`
- [X] Compile: 8836 lines, links successfully

### 5.6 SimpleWebFront Plugin
- [X] Convert `Plugins/SimpleWebFront/DBDplugin_SimpleWebFront.dpr` → `.lpr`
- [X] Port all SimpleWebFront `.pas` files (CLX→LCL, XML shims, TDirectoryTreeView→TShellTreeView)
- [X] Convert SimpleWebFront `.xfm` → `.lfm`
- [X] Compile: 40096 lines, links successfully

### 5.7 Phase 5 Wrap-up
- [X] All plugins compile as standalone executables
- [X] EmbeddedPDF compiles (runtime PDF testing still pending, see Final phase)
- [X] Commit Phase 5 work

---

## Final — Integration Testing & Cleanup

### Functional Testing
Evidence for the items below is recorded in the bug catalogs (`docs/ui-bug-catalog.md`, `docs/sqlite-bug-catalog.md`, `docs/mysql-bug-catalog.md`, `docs/db-ui-bug-catalog.md`, `docs/model-edit-bug-catalog.md`) and in `docs/notes-to-myself.md`. Each catalog is one or more diagnosis rounds on a real display followed by fixes and a verification pass; `model-edit-bug-catalog.md` holds rounds 5-11 and two combined regression passes, d0fbc19 for rounds 6-9b and 920a93c for rounds 10-11 (no regressions either time, self-test 93 PASS / 0 FAIL).

- [X] **Automated UI self-test** (`--selftest`) — baseline 93 PASS, 0 FAIL, 78 SKIP (UITestRunner.pas; also sweeps the buttons of every visible form; never opens a database connection or touches the user's settings)
- [X] Application launches without errors (real display and xvfb-run)
- [X] Load example model (`bin/Examples/order.xml`) — through the UI and via TestModelLoad
- [X] Create a new model with tables, fields, and relations — model-edit-bug-catalog (rounds 5-7: tables from the tool, typed datatypes and comments through the Table Editor in-place editors and the "Set Datatype" popup, indexes, relations)
- [X] Save model to XML and reload — verify round-trip — edited models reloaded in rounds 4-11 (tables, regions, notes, images, stored SQL commands)
- [X] Export SQL script (MySQL) — verify output — script loads into MySQL 8 without errors (`tests/mysql-roundtrip.sh`); column/table comments, empty indexes skipped (model-edit #23, round 8); no `ENGINE` clause for MyISAM
- [ ] Export SQL script (PostgreSQL) — verify output — not verified, no server available
- [ ] Export SQL script (Oracle) — verify output — not verified, no server available
- [X] Export SQL script (SQLite) — verify output — script loads into sqlite3 without errors (`tests/sqlite-roundtrip.sh`); FULLTEXT written as plain INDEX (model-edit #23)
- [X] Connect to a live SQLite database — through the UI (sqlite-bug-catalog) and TestSQLExprShim
- [X] Connect to a live MySQL database — MySQL 8 through the UI (mysql-bug-catalog) and TestMySQLShim
- [X] Reverse-engineer a database schema — SQLite and MySQL: all columns, indexes, auto-increment and foreign-key relations of the 12-table example recovered; MySQL column/table comments and the table engine since model-edit #24 (round 8)
- [X] Synchronise model with database — MySQL 8 (mysql-bug-catalog stage E, model-edit round 8: column type and comment change applied); not possible against SQLite (known limitation, stops with a message and a closable dialog since model-edit #25)
- [ ] Test print / page setup — Page Setup dialog exercised, printed output not checked
- [ ] Test PDF export
- [X] Test zoom, navigation palette, model palette — self-test plus real-display rounds
- [X] Test copy/paste of tables and relations — model-edit-bug-catalog #9, #26-#28 (FK index/relation id remap, DB Model tree refresh, paste as one undo action), cross-model paste in round 9b; mixed selections with regions, notes and images renamed and given a fresh OrderPos (model-edit #40, round 10b)
- [X] Test undo functionality — delete/move/paste undone and redone with the restored state checked on the canvas, in the tree and in the saved XML; redo stack cleared by a new edit; close prompt after an undo (model-edit #31, rounds 9-9b); undo of a multi-object delete restores every relation and FK column (model-edit #42, round 10b, regression pass 920a93c). Undo granularity inside the Table and Relation editors not yet checked
- [X] Place / Link model from file — picked file loaded into the dialog only, objects placed on the target canvas; Escape and window close abort (model-edit #32, #33); placement is not undoable (as in the original)
- [X] Regions, notes and images on the canvas — Region tool and Region Editor, region move with contents, Note Editor with non-ASCII text, PNG and 1-bpp BMP images kept after save/reopen, Image Editor, unique names for new objects (model-edit rounds 10-10b, #35-#38, #41, #52)
- [X] Export model / selected objects as image, Copy as Image — PNG and JPEG written without an exception dialog (model-edit #39); the selection export paints from the model origin instead of cropping (inherited)
- [X] Query mode against MySQL 8 and SQLite 3 — drag-and-drop query building, JOIN/WHERE/ORDER BY, editable result grid with Apply Changes writing UPDATE/INSERT/DELETE, ISO date/time display and write-back, stored SQL and history saved with the model, Ctrl+S, rename dialog, stored-SQL popup (model-edit round 11, #43-#50). Not reached: Export/Print Records, BLOB viewer, multi-statement scripts; #47/#51 unconfirmed
- [X] Load a plugin (Demo) — all four plugins started from the Plugins menu on a real display
- [X] Generate HTML report via plugin — report generated from reverse-engineered models (mysql and sqlite catalogs)
- [X] Test on Linux — x86-64 Linux, GTK2
- [ ] Test on Windows
- [ ] Test on macOS (if applicable)

### Runtime Hardening (added during the test rounds)
- [X] Load `libsqlite3.so.0` / `libmysqlclient.so.21` by versioned name (`clx_shims/sqlitelib.pas`, `clx_shims/mysqllib.pas`), no symlink or `-dev` package needed
- [X] Commit DML executed through the shim (SQLDB does not autocommit like dbExpress); release the SQLite read lock when idle
- [X] `--selftest` leaves settings and `DBConn.ini` untouched and never opens a connection
- [X] Recover the real `mysql_error()` text on failed logins

### Code Cleanup
- [ ] Remove `clx_shims/` folder — replace all `Q*` references with direct LCL unit names (optional; the shim layer is the documented approach of the port and may stay)
- [X] Remove or archive the bundled `SynEdit/` folder — archived as `SynEdit_clx_original/`; the project uses the Lazarus SynEdit package
- [X] Archive unused Delphi-specific files (`.dof`, `.dsk`, `.dsm`, `.cfg`) to `archive/`
- [ ] Update `DBDesigner4.inc` — remove obsolete defines (`USE_IXMLDBMODELType` and `USE_QTheming` are disabled but still referenced in the sources)
- [ ] Review and clean up any remaining `{$IFDEF}` blocks for Delphi/Kylix (about 55 `MSWINDOWS`, 40 `LINUX` and 21 `USE_IXMLDBMODELType` blocks remain)
- [X] Update `README.md` with new build instructions for Lazarus — and with the project status, tests and catalogs (September 2026)
- [X] Update `port-to-lazarus.md` with lessons learned
- [ ] Final commit and tag release (no tags yet)

---

## Progress Summary

| Phase | Status | Tasks | Done |
|---|---|---|---|
| Phase 0 — Setup & Scaffolding | ✅ Complete | 30 | 30 |
| Phase 1 — Non-Visual Core | ✅ Complete | 33 | 33 |
| Phase 2 — Database Layer | 🟡 SQLite 3 and MySQL 8 working (runtime tested); Oracle, MSSQL, ODBC, PostgreSQL connectors not linked | 39 | 34 |
| Phase 3 — UI Forms | ✅ Complete (runtime tested, 11 bug-fix rounds plus two regression passes) | 54 | 54 |
| Phase 4 — SynEdit | ✅ Complete | 22 | 22 |
| Phase 5 — Plugins & Extras | ✅ Complete (plugins runtime tested; PDF export untested) | 27 | 26 |
| Final — Testing & Cleanup | 🟡 In progress | 39 | 29 |
| **Total** | | **244** | **228** |

Last recount: September 2026.

> Update the "Done" column and status as you progress:
> - ⬜ Not started
> - 🟡 In progress / partly done
> - ✅ Complete
