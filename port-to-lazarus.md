
# Porting DBDesigner Fork to Free Pascal / Lazarus

## Developer Guide — Step-by-Step Instructions

This document provides detailed instructions for porting DBDesigner Fork from Delphi 7 / Kylix 3 (CLX) to Free Pascal (FPC) and the Lazarus IDE (LCL).

**Strategy:** Shim Layer Approach — create thin compatibility units that map CLX names to LCL equivalents, allowing incremental compilation with minimal source changes. Direct replacements are used where shims are impractical.

**Prerequisites:**
- Lazarus IDE 3.x+ with Free Pascal Compiler 3.2.2+
- Familiarity with Delphi/Object Pascal and the Lazarus Component Library (LCL)
- Git for version control

**Task List:** Track your progress with the companion checklist: **[port-to-lazarus-task-list.md](port-to-lazarus-task-list.md)** (229 tasks across all phases).

**Golden Rule:** Make one small change at a time, compile, commit. Never combine multiple unrelated changes in a single commit.

---

## Table of Contents

1. [Phase 0 — Project Setup & Scaffolding](#phase-0--project-setup--scaffolding)
2. [Phase 1 — Non-Visual Core Units](#phase-1--non-visual-core-units)
3. [Phase 2 — Database Layer (DBXpress → SQLDB)](#phase-2--database-layer-dbxpress--sqldb)
4. [Phase 3 — UI Forms (CLX → LCL)](#phase-3--ui-forms-clx--lcl)
5. [Phase 4 — SynEdit Integration](#phase-4--synedit-integration)
6. [Phase 5 — Plugins & Extras](#phase-5--plugins--extras)
7. [Reference Tables](#reference-tables)
8. [File Inventory](#file-inventory)

---

## Phase 0 — Project Setup & Scaffolding

### 0.1 Create the Lazarus Project File

Create `DBDesignerFork.lpr` based on the existing `DBDesignerFork.dpr`. The `.lpr` file is nearly identical to a `.dpr` but uses LCL instead of CLX.

1. Copy `DBDesignerFork.dpr` to `DBDesignerFork.lpr`.
2. Replace the `uses` clause opening:
   ```pascal
   // BEFORE (CLX):
   uses
     QForms,
   
   // AFTER (LCL):
   uses
     {$IFDEF UNIX}
     cthreads,
     {$ENDIF}
     Interfaces, // LCL widgetset
     Forms,
   ```
3. Remove the SynEdit unit paths from the `.lpr` — these will come from the Lazarus SynEdit package instead (see [Phase 4](#phase-4--synedit-integration)).
4. Remove the `{$IFDEF MSWINDOWS}` / `{$ELSE}` path-separator blocks for SynEdit (lines ~92–122 in the original `.dpr`).
5. Create a minimal `.lpi` project file by opening the `.lpr` in Lazarus IDE and saving the project. Configure:
   - **Unit output directory:** `dcu/` (or `lib/$(TargetCPU)-$(TargetOS)`)
   - **Target output directory:** `bin/`
   - **Required packages:** `LCL`, `SynEdit` (add later in Phase 4)
   - **Other unit files path:** add `.` (project root) plus any needed subfolders

### 0.2 Create CLX → LCL Shim Units

Create a folder called `clx_shims/` and add it to the project's unit search path. Each shim unit re-exports the equivalent LCL unit under the CLX name, so existing `uses` clauses compile without changes.

Create the following shim files:

#### `clx_shims/QForms.pas`
```pascal
unit QForms;
{$mode delphi}
interface
uses Forms;
implementation
end.
```

#### `clx_shims/QControls.pas`
```pascal
unit QControls;
{$mode delphi}
interface
uses Controls;
implementation
end.
```

#### `clx_shims/QGraphics.pas`
```pascal
unit QGraphics;
{$mode delphi}
interface
uses Graphics;
implementation
end.
```

#### `clx_shims/QDialogs.pas`
```pascal
unit QDialogs;
{$mode delphi}
interface
uses Dialogs;
implementation
end.
```

#### `clx_shims/QStdCtrls.pas`
```pascal
unit QStdCtrls;
{$mode delphi}
interface
uses StdCtrls;
implementation
end.
```

#### `clx_shims/QExtCtrls.pas`
```pascal
unit QExtCtrls;
{$mode delphi}
interface
uses ExtCtrls;
implementation
end.
```

#### `clx_shims/QMenus.pas`
```pascal
unit QMenus;
{$mode delphi}
interface
uses Menus;
implementation
end.
```

#### `clx_shims/QImgList.pas`
```pascal
unit QImgList;
{$mode delphi}
interface
uses ImgList;
implementation
end.
```

#### `clx_shims/QComCtrls.pas`
```pascal
unit QComCtrls;
{$mode delphi}
interface
uses ComCtrls;
implementation
end.
```

#### `clx_shims/QPrinters.pas`
```pascal
unit QPrinters;
{$mode delphi}
interface
uses Printers;
implementation
end.
```

#### `clx_shims/QClipbrd.pas`
```pascal
unit QClipbrd;
{$mode delphi}
interface
uses Clipbrd;
implementation
end.
```

#### `clx_shims/QTypes.pas`
This one is more complex. `QTypes` contains CLX-specific type definitions. Start with a minimal stub and add types as compilation errors reveal them:
```pascal
unit QTypes;
{$mode delphi}
interface
uses LCLType, Classes;

// Add CLX type aliases here as needed during porting.
// Example:
// type
//   TWidgetPosition = record ... end;

implementation
end.
```

> **Note:** The `Qt` unit has **no shim equivalent** and must be handled by direct source code modification (see Phase 3). It provides low-level Qt widget bindings that have no LCL counterpart.

### 0.3 Add `{$mode delphi}` Directive

FPC needs a language mode directive. Add `{$mode delphi}` near the top of every `.pas` file (after the `unit` line, before `interface`). This enables Delphi-compatible syntax.

You can do this with a script:
```bash
# For each .pas file in the project root, insert {$mode delphi} after the unit line
for f in *.pas EmbeddedPDF/*.pas; do
  if ! grep -q '{\$mode' "$f" 2>/dev/null; then
    sed -i '/^interface/i {$mode delphi}' "$f"
  fi
done
```

Alternatively, use a global project include. In `DBDesigner4.inc`, add:
```pascal
{$mode delphi}
{$H+}  // Use AnsiString by default (Delphi-compatible)
```
Then ensure every unit includes `{$I DBDesigner4.inc}` (most already do — verify and add where missing).

### 0.4 Convert Form Files (.xfm → .lfm)

The `.xfm` (CLX form) format is text-based and very similar to `.lfm`. Batch-convert them:

```bash
# Simple rename — the text format is compatible for most properties
for f in *.xfm; do
  cp "$f" "${f%.xfm}.lfm"
done
```

After renaming, open each `.lfm` in Lazarus. The IDE will report unknown properties — fix them one by one. Common issues:

| CLX Property | LCL Equivalent | Action |
|---|---|---|
| `WidgetFlags` | — | Remove |
| `Color` (CLX-specific values) | `Color` (LCL values) | Adjust if needed |
| `Anchors` | `Anchors` | Usually compatible |
| `Flat = True` on buttons | May not exist | Remove or adjust |
| `LookAndFeel` | — | Remove |

> **Tip:** You can also use Lazarus's built-in **Delphi → Lazarus converter** (`Tools → Convert Delphi Project`) which handles some of this automatically.

### 0.5 First Compilation Attempt

At this point, try to compile. You will get many errors — that's expected. The goal is to establish a baseline and see the full scope of remaining issues. Save the compiler output to a file for reference:

```bash
lazbuild DBDesignerFork.lpi 2>&1 | tee build-errors-phase0.log
```

Commit everything done so far:
```
git add -A
git commit -m "Phase 0: Lazarus project setup, CLX shim units, form conversion"
```

---

## Phase 1 — Non-Visual Core Units

Start with units that have minimal or no UI dependencies. The goal is to get the data model engine compiling first.

### 1.1 `LibXmlParser.pas` (2,728 lines)

This is a standalone XML parser with likely zero CLX dependencies.

- Add `{$mode delphi}` if not present.
- Try to compile. Fix any FPC-specific issues (typically minor: `PChar` vs `PAnsiChar`, string type differences).
- This unit has no `uses` clause dependencies on CLX — it should compile quickly.

### 1.2 `GlobalSysFunctions.pas` (178 lines)

Small utility unit. Review the `uses` clause, replace any CLX units with LCL equivalents (shims should handle this). Compile and fix.

### 1.3 `RegExpr.pas` (4,260 lines)

FPC ships with its own `RegExpr` unit. You have two options:
- **Option A (recommended):** Keep this bundled version — it's self-contained and avoids API differences.
- **Option B:** Remove it and use FPC's built-in `RegExpr`. This may require changing call sites if the API differs.

Add `{$mode delphi}`, compile, and fix.

### 1.4 `EERModel.pas` (14,343 lines) — ⚠️ THE CRITICAL FILE

This is the heart of the application — the EER diagram model engine. It has significant dependencies:

```pascal
uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QImgList, QMenus, QTypes, IniFiles, Math, StrUtils,
  QPrinters, QClipbrd, QComCtrls, Qt,
  {$IFDEF USE_IXMLDBMODELType}
  XMLDoc,
  {$ENDIF}
  LibXmlParser;
```

**Steps:**
1. The `Q*` units will be handled by shims — no changes needed there.
2. **`Qt` unit:** This is the hard part. Search for all `Qt.` references and CLX-specific types/functions. Common patterns to replace:

   | CLX / Qt Pattern | LCL Replacement |
   |---|---|
   | `QWidget_*` function calls | LCL `Handle`-based APIs or remove |
   | `QApplication_postEvent` | `Application.QueueAsyncCall` or `PostMessage` |
   | `QCustomEvent`, `QCustomEvent_create` | `TLMessage` / custom LCL messages |
   | `QEventType_*` constants | Custom message constants (`WM_USER + N`) |
   | `QPainter_*` calls | `Canvas` methods (already available via LCL) |
   | `QPixmap_*`, `QBitmap_*` | `TBitmap`, `TPixmap` via LCL |
   | `QCursor_setPos` | `Mouse.CursorPos` |
   | `QWidget_setGeometry` | `SetBounds` / `BoundsRect` |
   | `QScrollView_*` | `TScrollBox` or `TScrollingWinControl` |

3. **`XMLDoc`:** Replace with FPC XML units (see section 1.5).
4. Compile iteratively, fixing errors one by one. This file will take the most time.

> **Strategy for `Qt` unit calls:** Rather than replacing them inline immediately, consider creating a `QtCompat.pas` unit that provides wrapper functions with the same signatures but implemented using LCL. This minimizes changes to `EERModel.pas` itself.

### 1.5 XML Handling — `XMLDoc` / `XMLIntf` / `xmldom`

**Files affected:**
- `EERModel.pas`
- `EERModel_XML.pas` (4,830 lines)
- `EERModel_XML_ERwin41_Import.pas` (6,332 lines)
- `MainDM.pas`

**Replacement mapping:**

| Delphi Unit/Type | FPC/Lazarus Unit/Type |
|---|---|
| `xmldom` | `laz2_DOM` (or `DOM`) |
| `XMLDoc` | `laz2_XMLRead`, `laz2_XMLWrite` |
| `XMLIntf` | `laz2_DOM` |
| `IXMLDocument` | `TXMLDocument` |
| `IXMLNode` | `TDOMNode` |
| `IXMLNodeList` | `TDOMNodeList` |
| `TXMLDocument.Create` | `ReadXMLFile(doc, filename)` |
| `Node.Attributes['name']` | `Node.Attributes.GetNamedItem('name').NodeValue` |
| `Node.ChildNodes` | `Node.ChildNodes` (compatible) |
| `Doc.SaveToFile` | `WriteXMLFile(doc, filename)` |

The `{$IFDEF USE_IXMLDBMODELType}` conditional in `EERModel.pas` suggests there's already an abstraction layer — investigate whether disabling this define simplifies the port (falling back to `LibXmlParser` instead of DOM).

### 1.6 `EERModel_XML.pas` and `EERModel_XML_ERwin41_Import.pas`

These depend on `xmldom`, `XMLDoc`, `XMLIntf`. Apply the XML replacements from section 1.5. The `EERModel_XML.pas` file uses Delphi's XML Data Binding (auto-generated interfaces like `IXMLDBMODELType`). Options:
- **Option A:** Rewrite to use `laz2_DOM` directly (cleaner, more work).
- **Option B:** Create a compatibility layer that mimics the Delphi XML interfaces using `laz2_DOM` underneath.

### 1.7 `EERExportImportDM.pas` (472 lines)

Export/import logic. Should compile once EERModel and XML handling are working.

**Commit after Phase 1:**
```
git commit -m "Phase 1: Port non-visual core units to FPC"
```

---

## Phase 2 — Database Layer (DBXpress → SQLDB)

### 2.1 Overview

The database connectivity is isolated in a few key files:

| File | Role | Lines |
|---|---|---|
| `DBDM.pas` | Core DB module — connections, queries | 1,050 |
| `DBEERDM.pas` | DB operations for EER models (reverse engineering, sync) | 3,074 |
| `MainDM.pas` | Main data module | 1,881 |
| `DBConnSelect.pas` | Connection selection dialog | 1,434 |
| `DBConnEditor.pas` | Connection editor dialog | 562 |
| `DBConnLogin.pas` | Login dialog | 127 |
| `EditorQuery.pas` | SQL query editor | 3,085 |
| `EditorTableData.pas` | Table data viewer/editor | 805 |
| `EERStoreInDatabase.pas` | Store model in DB | 618 |
| `EERReverseEngineering.pas` | Reverse engineering dialog | 592 |
| `EERSynchronisation.pas` | DB sync dialog | 226 |

### 2.2 Unit Replacements in `uses` Clauses

In each file above, replace:
```pascal
// BEFORE:
uses ... DBXpress, FMTBcd, DBClient, Provider, SqlExpr, DB ...

// AFTER:
uses ... SQLDB, DB, BufDataset ...
```

Also add the specific SQLDB connector units as needed:
```pascal
uses
  mysql80conn,    // MySQL 8.0
  // mysql57conn,  // MySQL 5.7
  // pqconnection, // PostgreSQL
  // sqlite3conn,  // SQLite
  // mssqlconn,    // MS SQL Server
  // oracleconnection, // Oracle
  // odbcconn,     // ODBC
```

> **Tip:** You could conditionally include connectors via defines in `DBDesigner4.inc`.

### 2.3 Component Replacements

In `DBDM.pas`, the data module declares:
```pascal
SQLConn: TSQLConnection;
OutputQry: TSQLQuery;
OutputDataSetProvider: TDataSetProvider;
OutputClientDataSet: TClientDataSet;
SchemaSQLQuery: TSQLQuery;
```

Replace with:
```pascal
SQLConn: TSQLConnection;       // Same name in SQLDB! But different unit.
OutputQry: TSQLQuery;           // Same name in SQLDB!
SchemaSQLQuery: TSQLQuery;      // Same name in SQLDB!
SQLTransaction: TSQLTransaction; // NEW — SQLDB requires explicit transactions
// Remove: OutputDataSetProvider, OutputClientDataSet
// Or replace OutputClientDataSet with: TBufDataset
```

**Key difference:** SQLDB requires a `TSQLTransaction` between the connection and queries:
```pascal
SQLConn := TSQLConnection.Create(nil);  // or specific: TMySQL80Connection.Create
SQLTransaction := TSQLTransaction.Create(nil);
SQLTransaction.Database := SQLConn;
OutputQry := TSQLQuery.Create(nil);
OutputQry.Database := SQLConn;
OutputQry.Transaction := SQLTransaction;
```

### 2.4 Connection Handling in `DBDM.pas`

The `ConnectToDB` method reads driver info from `TDBConn` records (DriverName, LibraryName, VendorLib, etc.). This DBXpress-specific connection model must be adapted:

```pascal
// BEFORE (DBXpress):
SQLConn.DriverName := DBConn.DriverName;
SQLConn.GetDriverFunc := DBConn.GetDriverFunc;
SQLConn.LibraryName := DBConn.LibraryName;
SQLConn.VendorLib := DBConn.VendorLib;
SQLConn.Params.Assign(DBConn.Params);
SQLConn.Open;

// AFTER (SQLDB):
// Create the appropriate connection type based on DriverName
case LowerCase(DBConn.DriverName) of
  'mysql': SQLConn := TMySQL80Connection.Create(nil);
  'postgresql': SQLConn := TPQConnection.Create(nil);
  'sqlite': SQLConn := TSQLite3Connection.Create(nil);
  'mssql': SQLConn := TMSSQLConnection.Create(nil);
  'oracle': SQLConn := TOracleConnection.Create(nil);
  'odbc': SQLConn := TODBCConnection.Create(nil);
end;
SQLConn.HostName := DBConn.Params.Values['HostName'];
SQLConn.DatabaseName := DBConn.Params.Values['Database'];
SQLConn.UserName := DBConn.Params.Values['User_Name'];
SQLConn.Password := DBConn.Params.Values['Password'];
SQLConn.Open;
SQLTransaction.Active := True;
```

### 2.5 Query Execution Pattern Changes

```pascal
// BEFORE (DBXpress + ClientDataSet):
OutputQry.SQL.Text := 'SELECT * FROM table';
OutputDataSetProvider.DataSet := OutputQry;
OutputClientDataSet.Open;

// AFTER (SQLDB):
OutputQry.SQL.Text := 'SELECT * FROM table';
OutputQry.Open;
// Access data directly from OutputQry — no provider/clientdataset needed
```

### 2.6 INI File Compatibility

The file `bin/Data/DBConn_DefaultSettings.ini` stores connection presets. Review and adapt the parameter names to match SQLDB expectations. The structure can largely remain the same — just the interpretation in code changes.

### 2.7 `TTableScopes` and Metadata

`DBEERDM.pas` uses `TSQLConnection.GetTableNames`, `GetFieldNames`, etc. for reverse engineering. SQLDB provides similar methods but with slightly different signatures. Check each call and adapt.

**Commit after Phase 2:**
```
git commit -m "Phase 2: Replace DBXpress with SQLDB for database connectivity"
```

---

## Phase 3 — UI Forms (CLX → LCL)

### 3.1 The `Qt` Unit — Direct Replacement Guide

32 files use the `Qt` unit directly. For each file, search for `Qt`-dependent code and replace. Here are the most common patterns found in this codebase:

#### Custom Events (used extensively for inter-component communication)
```pascal
// BEFORE (CLX):
const
  QEventType_MyCustomEvent = QEventType(Integer(QEventType_ClxUser) + 100);

procedure TMyForm.HandleEvent;
var
  Event: QCustomEventH;
begin
  Event := QCustomEvent_create(QEventType_MyCustomEvent, 0);
  QApplication_postEvent(Handle, Event);
end;

// AFTER (LCL):
const
  LM_MyCustomEvent = WM_USER + 100;

procedure TMyForm.HandleEvent;
begin
  PostMessage(Handle, LM_MyCustomEvent, 0, 0);
end;

// Add message handler:
procedure TMyForm.WMMyCustomEvent(var Msg: TLMessage); message LM_MyCustomEvent;
begin
  // Handle the event
end;
```

#### Widget Handle Operations
```pascal
// BEFORE (CLX):
QWidget_setMinimumSize(SomeControl.Handle, 100, 50);

// AFTER (LCL):
SomeControl.Constraints.MinWidth := 100;
SomeControl.Constraints.MinHeight := 50;
```

#### Application Event Processing
```pascal
// BEFORE (CLX):
QApplication_processEvents(Application.Handle);

// AFTER (LCL):
Application.ProcessMessages;
```

### 3.2 Form-by-Form Porting Order

Port forms in dependency order — simpler/leaf forms first, then forms that depend on them:

**Tier 1 — Simple dialogs (few dependencies):**
1. `Splash.pas` / `.lfm` — Splash screen
2. `Tips.pas` / `.lfm` — Tips dialog
3. `ZoomSel.pas` / `.lfm` — Zoom selector
4. `EditorString.pas` / `.lfm` — String editor
5. `EditorNote.pas` / `.lfm` — Note editor
6. `EditorImage.pas` / `.lfm` — Image editor
7. `EditorRegion.pas` / `.lfm` — Region editor
8. `PrinterSettings.pas` / `.lfm` — Printer settings
9. `DBConnLogin.pas` / `.lfm` — Login dialog
10. `EditorDatatype.pas` / `.lfm` — Datatype editor

**Tier 2 — Medium complexity:**
11. `EditorRelation.pas` / `.lfm` — Relation editor
12. `EditorTableFieldParam.pas` / `.lfm` — Field parameter editor
13. `EditorTable.pas` / `.lfm` — Table editor (2,041 lines)
14. `PaletteTools.pas` / `.lfm` — Tools palette
15. `PaletteDataTypesReplace.pas` / `.lfm` — Datatype replace palette
16. `PaletteDatatypes.pas` / `.lfm` — Datatypes palette
17. `Options.pas` / `.lfm` — Options dialog
18. `OptionsModel.pas` / `.lfm` — Model options
19. `DBConnEditor.pas` / `.lfm` — Connection editor
20. `DBConnSelect.pas` / `.lfm` — Connection selector

**Tier 3 — Complex forms (heavy Qt usage expected):**
21. `PaletteModel.pas` / `.lfm` — Model palette
22. `PaletteNav.pas` / `.lfm` — Navigation palette
23. `EERPageSetup.pas` / `.lfm` — Page setup
24. `EERExportSQLScript.pas` / `.lfm` — SQL export
25. `EERPlaceModel.pas` / `.lfm` — Model placement
26. `EERReverseEngineering.pas` / `.lfm` — Reverse engineering
27. `EERStoreInDatabase.pas` / `.lfm` — Store in DB
28. `EERSynchronisation.pas` / `.lfm` — Synchronisation
29. `EditorTableData.pas` / `.lfm` — Table data editor
30. `EditorQuery.pas` / `.lfm` — Query editor (3,085 lines)
31. `EditorQueryDragTarget.pas` / `.lfm` — Query drag target

**Tier 4 — Core forms:**
32. `GUIDM.pas` / `.lfm` — GUI data module
33. `EERDM.pas` / `.lfm` — EER data module
34. `EER.pas` / `.lfm` — EER form (hosts the model canvas)
35. `MainDM.pas` / `.lfm` — Main data module
36. `Main.pas` / `.lfm` — Main application form (3,514 lines)

### 3.3 Common .lfm Fixups

When Lazarus opens a converted `.lfm`, it may complain about unknown properties. Common fixes:

```
// Remove these CLX-specific properties if found:
DesktopFont = ...       → remove
ParentBiDiMode = ...    → remove  
OnEndDock = ...          → remove (or adapt)
object XXX: TWidgetControl → object XXX: TWinControl
TextHeight = ...        → usually fine, Lazarus recalculates
PixelsPerInch = ...     → usually fine
```

### 3.4 Data Module Forms

`DBDM.pas`, `GUIDM.pas`, `EERDM.pas`, `MainDM.pas`, `EERExportImportDM.pas` are `TDataModule` descendants. Their `.xfm` → `.lfm` conversion is simpler since they have no visual controls — only non-visual components. The main work is replacing the DB components (already covered in Phase 2).

**Commit after each tier:**
```
git commit -m "Phase 3 Tier N: Port [form names] to LCL"
```

---

## Phase 4 — SynEdit Integration

### 4.1 Remove Bundled SynEdit

The `SynEdit/` folder contains a CLX-era SynEdit (with `QSynEdit` wrapper units). Lazarus ships with a modern, maintained SynEdit package.

1. Remove all `QSynEdit*` and `QSynHighlighter*` unit references from the `.lpr` file.
2. Add the `SynEdit` package as a project dependency in the `.lpi` (Lazarus: `Project → Project Inspector → Add → New Requirement → SynEdit`).
3. In all source files that reference SynEdit:
   ```pascal
   // BEFORE:
   uses ... QSynEdit, QSynHighlighterSQL ...
   
   // AFTER:
   uses ... SynEdit, SynHighlighterSQL ...
   ```

### 4.2 Files That Reference SynEdit

Based on `{$IFDEF USE_SYNEDIT}` usage, check these files:
- `EditorQuery.pas` — SQL editor with syntax highlighting
- `EERExportSQLScript.pas` — SQL export with preview
- `Main.pas` — May have SynEdit toolbar integration
- Any file using `TSynMemo` or `TSynEdit` components

### 4.3 API Differences

The Lazarus SynEdit API is largely compatible with the Delphi version, but some differences exist:

| Delphi SynEdit | Lazarus SynEdit |
|---|---|
| `TSynMemo` | `TSynEdit` (TSynMemo may not exist) |
| `TSynSQLSyn` | `TSynSQLSyn` (same) |
| `Highlighter` property | `Highlighter` property (same) |
| `Gutter.ShowLineNumbers` | `Gutter.LineNumberPart.Visible` |

### 4.4 Conditional Compilation

The existing `{$DEFINE USE_SYNEDIT}` in `DBDesigner4.inc` is useful — you can temporarily disable it to compile without SynEdit, then re-enable once integration is ready.

**Commit:**
```
git commit -m "Phase 4: Replace bundled SynEdit with Lazarus SynEdit package"
```

---

## Phase 5 — Plugins & Extras

### 5.1 EmbeddedPDF Library (5 files)

The `EmbeddedPDF/` folder is a pure Pascal PDF generation library. It should be mostly FPC-compatible.

**Watch for:**
- `String` vs `AnsiString` — PDF requires byte-accurate strings. Use `{$H+}` and possibly `RawByteString` where needed.
- `Qt` unit usage in `EmbeddedPdfImages.pas` — this file uses Qt for image conversion. Replace with LCL equivalents (`TBitmap`, `TLazIntfImage`, etc.).
- `Char` type — In FPC with `{$mode delphi}`, `Char` = `AnsiChar`, which is correct for PDF output.

### 5.2 Plugin Architecture

Plugins are Delphi DLL projects (`.dpr` files in `Plugins/`). Each exports functions that DBDesigner calls.

1. Convert each plugin `.dpr` to a `.lpr` (Lazarus library project).
2. On Linux, plugins become `.so` shared libraries instead of `.dll`.
3. Update the plugin loading code (likely in `MainDM.pas` or `Main.pas`) to use platform-appropriate library extensions:
   ```pascal
   {$IFDEF MSWINDOWS}
   PluginExt := '.dll';
   {$ELSE}
   PluginExt := '.so';
   {$ENDIF}
   ```
4. Ensure `LoadLibrary` / `GetProcAddress` calls use FPC's `dynlibs` unit.

### 5.3 Plugin Files

| Plugin | Main File | Lines | Notes |
|---|---|---|---|
| Demo | `Plugins/Demo/DBDplugin_Demo.dpr` | 83 | Simple example |
| HTMLReport | `Plugins/HTMLReport/DBDplugin_HTMLReport.dpr` | 85 | HTML generation |
| DataImporter | `Plugins/DataImporter/DBDplugin_DataImporter.dpr` | 36 | Uses DBXpress |
| SimpleWebFront | `Plugins/SimpleWebFront/DBDplugin_SimpleWebFront.dpr` | 172 | Most complex |

Port in order: Demo → HTMLReport → DataImporter → SimpleWebFront.

The DataImporter and SimpleWebFront plugins have their own DB and XML dependencies — apply the same Phase 2 and Phase 1.5 techniques.

**Commit:**
```
git commit -m "Phase 5: Port plugins and EmbeddedPDF to FPC"
```

---

## Reference Tables

### CLX → LCL Unit Mapping (Complete)

| CLX Unit | LCL Unit | Notes |
|---|---|---|
| `QForms` | `Forms` | Shim |
| `QControls` | `Controls` | Shim |
| `QGraphics` | `Graphics` | Shim |
| `QDialogs` | `Dialogs` | Shim |
| `QStdCtrls` | `StdCtrls` | Shim |
| `QExtCtrls` | `ExtCtrls` | Shim |
| `QMenus` | `Menus` | Shim |
| `QTypes` | `LCLType` | Shim + type stubs |
| `QImgList` | `ImgList` | Shim |
| `QComCtrls` | `ComCtrls` | Shim |
| `QPrinters` | `Printers` | Shim |
| `QClipbrd` | `Clipbrd` | Shim |
| `Qt` | `LCLType`, `LCLIntf`, `LMessages` | **Manual replacement** |
| `DBXpress` | `SQLDB` | Direct replacement |
| `SqlExpr` | `SQLDB` | Direct replacement |
| `DBClient` | `BufDataset` | Direct replacement |
| `Provider` | — | Remove |
| `FMTBcd` | — | Remove (or use FPC equivalent) |
| `XMLDoc` | `laz2_XMLRead`, `laz2_XMLWrite` | Direct replacement |
| `XMLIntf` | `laz2_DOM` | Direct replacement |
| `xmldom` | `laz2_DOM` | Direct replacement |

### Delphi → FPC Language Differences to Watch

| Delphi | FPC `{$mode delphi}` | Action Needed |
|---|---|---|
| `string` | `AnsiString` | Usually compatible |
| `Char` | `AnsiChar` | Usually compatible |
| `PChar` | `PAnsiChar` | Usually compatible |
| `WideString` | `WideString` | Compatible |
| `TStringList` | `TStringList` | Compatible |
| `Pointer` arithmetic | Needs `{$T-}` or explicit casts | May need fixes |
| `as` / `is` operators | Compatible | OK |
| Generics | Available in FPC 3.2+ | OK if used |
| `resourcestring` | Compatible | OK |
| Inline variables | Not in `{$mode delphi}` | Avoid |

---

## File Inventory

### Core Application Files (root directory)

| File | Lines | CLX Deps | Qt | DBXpress | XML | Priority |
|---|---|---|---|---|---|---|
| `EERModel.pas` | 14,343 | ✅ | ✅ | — | ✅ | 🔴 Critical |
| `EERModel_XML.pas` | 4,830 | — | — | — | ✅ | 🔴 Critical |
| `Main.pas` | 3,514 | ✅ | — | ✅ | — | 🔴 Critical |
| `EditorQuery.pas` | 3,085 | ✅ | ✅ | ✅ | — | 🟡 High |
| `DBEERDM.pas` | 3,074 | ✅ | — | ✅ | — | 🟡 High |
| `LibXmlParser.pas` | 2,728 | — | — | — | — | 🟢 Easy |
| `EditorTable.pas` | 2,041 | ✅ | ✅ | — | — | 🟡 High |
| `MainDM.pas` | 1,881 | ✅ | — | ✅ | ✅ | 🟡 High |
| `DBConnSelect.pas` | 1,434 | ✅ | ✅ | — | — | 🟡 Medium |
| `EERPlaceModel.pas` | 1,113 | ✅ | — | — | — | 🟡 Medium |
| `DBDM.pas` | 1,050 | ✅ | ✅ | ✅ | — | 🔴 Critical |
| `EERDM.pas` | 866 | ✅ | ✅ | — | — | 🟡 High |
| `EERPageSetup.pas` | 849 | ✅ | — | — | — | 🟢 Medium |
| `EditorTableData.pas` | 805 | ✅ | — | ✅ | — | 🟡 Medium |
| `EERExportSQLScript.pas` | 777 | ✅ | — | — | — | 🟢 Medium |
| `PaletteModel.pas` | 734 | ✅ | — | — | — | 🟢 Medium |
| `PaletteDatatypes.pas` | 703 | ✅ | ✅ | — | — | 🟡 Medium |
| `PaletteNav.pas` | 695 | ✅ | ✅ | — | — | 🟡 Medium |
| `EERStoreInDatabase.pas` | 618 | ✅ | — | ✅ | — | 🟡 Medium |
| `Options.pas` | 608 | ✅ | — | — | — | 🟢 Medium |
| `EERReverseEngineering.pas` | 592 | ✅ | ✅ | — | — | 🟡 Medium |
| `DBConnEditor.pas` | 562 | ✅ | ✅ | — | — | 🟡 Medium |
| `OptionsModel.pas` | 524 | ✅ | ✅ | — | — | 🟡 Medium |
| `GUIDM.pas` | 524 | ✅ | ✅ | — | — | 🟡 Medium |
| `EERExportImportDM.pas` | 472 | — | — | — | — | 🟢 Easy |
| `RegExpr.pas` | 4,260 | — | — | — | — | 🟢 Easy |
| `EER.pas` | 388 | ✅ | ✅ | — | — | 🟡 Medium |
| `EditorQueryDragTarget.pas` | 376 | ✅ | — | — | — | 🟢 Medium |
| `EditorTableField.pas` | 353 | ✅ | ✅ | — | — | 🟡 Medium |
| `EditorRelation.pas` | 345 | ✅ | ✅ | — | — | 🟡 Medium |
| `PaletteTools.pas` | 278 | ✅ | — | — | — | 🟢 Easy |
| `EditorTableFieldDatatypeInplace.pas` | 269 | ✅ | — | — | — | 🟢 Easy |
| `EditorRegion.pas` | 250 | ✅ | ✅ | — | — | 🟡 Medium |
| `EditorImage.pas` | 234 | ✅ | ✅ | — | — | 🟡 Medium |
| `EERSynchronisation.pas` | 226 | ✅ | ✅ | — | — | 🟡 Medium |
| `PaletteDataTypesReplace.pas` | 210 | ✅ | — | — | — | 🟢 Easy |
| `EditorNote.pas` | 198 | ✅ | ✅ | — | — | 🟡 Medium |
| `EditorTableFieldParam.pas` | 195 | ✅ | ✅ | — | — | 🟡 Medium |
| `GlobalSysFunctions.pas` | 178 | ✅ | — | — | — | 🟢 Easy |
| `Splash.pas` | 176 | ✅ | — | — | — | 🟢 Easy |
| `EditorString.pas` | 147 | ✅ | — | — | — | 🟢 Easy |
| `DBConnLogin.pas` | 127 | ✅ | — | — | — | 🟢 Easy |
| `EditorDatatype.pas` | 268 | ✅ | ✅ | — | — | 🟡 Medium |
| `ZoomSel.pas` | 107 | ✅ | — | — | — | 🟢 Easy |
| `Tips.pas` | 57 | ✅ | — | — | — | 🟢 Easy |
| `PrinterSettings.pas` | 47 | ✅ | — | — | — | 🟢 Easy |
| `EERModel_XML_ERwin41_Import.pas` | 6,332 | — | — | — | ✅ | 🟡 High |

### Legend
- 🔴 **Critical** — Must be ported for core functionality; complex dependencies
- 🟡 **High/Medium** — Important but can be ported incrementally
- 🟢 **Easy** — Minimal dependencies, straightforward port

---

## Tips & Best Practices

1. **Compile early, compile often.** After every change, try `lazbuild`. Don't batch too many changes.
2. **Use `{$IFDEF FPC}` guards** where needed to keep Delphi compatibility (if desired):
   ```pascal
   {$IFDEF FPC}
   uses laz2_DOM, laz2_XMLRead;
   {$ELSE}
   uses xmldom, XMLDoc, XMLIntf;
   {$ENDIF}
   ```
3. **Test with the example file** `bin/Examples/order.xml` — load it, verify the model displays correctly.
4. **Keep the shims temporary.** Once all files compile, consider a cleanup pass to replace shim references with direct LCL unit names. This makes the code clearer for future maintainers.
5. **Use Lazarus's Delphi Converter** (`Tools → Convert Delphi Project/Package/Unit`) as a helper — it can automate some mechanical replacements, but always review its output.
6. **Watch for `WideString` vs `UTF8String`.** FPC/Lazarus uses UTF-8 internally. Most string operations are transparent, but if the code does byte-level string manipulation (common in XML and PDF code), you may need to be explicit about encoding.
7. **The `bin/` folder structure should remain unchanged** — it contains runtime data (config, graphics, docs) that the application expects at specific relative paths.


## Lessons Learned from the Port

### What Worked Well
1. **Shim/compatibility unit approach** — Creating 31 thin shim units in `clx_shims/` that map CLX names to LCL equivalents was extremely effective. It minimized changes to original source files and allowed incremental compilation.
2. **FPC's `{$mode delphi}`** — This mode handled most Delphi syntax differences (string types, implicit Result, etc.) automatically.
3. **`LibXmlParser` as XML backbone** — The core XML handling uses `LibXmlParser` (not DOM interfaces), which is pure Pascal and compiled without any changes. This avoided a massive rewrite of `EERModel.pas`.
4. **Plugin architecture as executables** — Since plugins are standalone executables (not DLLs), they compiled and linked independently with minimal coupling.
5. **Lazarus's built-in SynEdit package** — Dropped the bundled SynEdit source and used the system package directly.

### Key Challenges and Solutions
1. **FPC doesn't transitively export types** — Each unit must directly reference the defining unit for types it uses. The initial shim approach of `unit QForms; uses Forms; end.` doesn't re-export Forms types in FPC mode. Solution: Add direct LCL unit references to source files where needed.
2. **TSQLConnector driver mapping** — Delphi's DBExpress uses `DriverName` strings and separate driver libraries. Our `TSQLConnection` shim maps `DriverName` → `ConnectorType` (e.g., `'MySQL'` → `'MySQL 5.7'`, `'SQLite'` → `'SQLite3'`).
3. **Transaction management** — SQLDB requires explicit `TSQLTransaction` between connection and queries. Delphi's DBExpress auto-commits. Our shim creates an auto-transaction and commits after `ExecuteDirect`.
4. **MDI forms** — LCL has limited MDI support. Changed `fsMDIForm`/`fsMDIChild` to `fsNormal`.
5. **`{$ELSEIF}` syntax** — FPC doesn't support `{$ELSEIF}` in `{$IFDEF}` blocks. Changed to `{$ELSE}` + nested `{$IFDEF}`. Similarly `{$IFEND}` → `{$ENDIF}`.
6. **`TDirectoryTreeView`** — CLX component not available in LCL. Replaced with `TShellTreeView` from `ShellCtrls` package.
7. **Qt event handling** — Delphi/CLX's direct Qt API calls (`QApplication_postEvent`, `QKeyEvent_key`) were replaced with LCL equivalents using `Application.AddOnKeyDownHandler` and a custom shim layer.

### Schema/Metadata Compatibility
The `SetSchemaInfo` implementation provides Delphi-compatible field layouts:
- **stTables**: RECNO, CATALOG_NAME, SCHEMA_NAME, TABLE_NAME, TABLE_TYPE (TABLE_NAME at field[3])
- **stColumns**: 14 fields matching Delphi's layout (TABLE_NAME, COLUMN_NAME, COLUMN_POSITION, COLUMN_TYPE, etc.)
- **stIndexes**: 11 fields matching Delphi's layout (TABLE_NAME, INDEX_NAME, COLUMN_NAME, etc.)

All three verified working with SQLite. MySQL and PostgreSQL queries use INFORMATION_SCHEMA equivalents.

### Build Environment
- **Lazarus 3.0** with **FPC 3.2.2** on Linux x86-64
- Compile time: ~14 seconds for all 5 projects (149,591 lines total)
- Only 2 compiler warnings (ScanLine portability in EmbeddedPdfImages.pas)
