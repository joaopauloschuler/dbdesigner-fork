# Notes to Myself — DBDesigner Fork Lazarus Port

## Current Status: ✅ All 5 Projects Compile and Launch Successfully

### Build Results (Clean Build)
| Project | Lines | Time | Binary |
|---------|-------|------|--------|
| Main App | 56,608 | 4.4s | bin/DBDesignerFork |
| Demo Plugin | 21,793 | 2.3s | bin/DBDplugin_Demo |
| HTMLReport Plugin | 22,258 | 2.3s | bin/DBDplugin_HTMLReport |
| DataImporter Plugin | 8,836 | 2.1s | bin/DBDplugin_DataImporter |
| SimpleWebFront Plugin | 40,096 | 3.0s | bin/DBDplugin_SimpleWebFront |
| **Total** | **149,591** | **~14s** | |

### Compiler Warnings: Only 2
- `EmbeddedPDF/EmbeddedPdfImages.pas` lines 202, 211: ScanLine portability (expected, harmless)

### Runtime Verification
- All 5 binaries launch under `xvfb-run` without crashing (exit 124 = killed by timeout = stayed alive)
- XML model loading verified (14 tables from order.xml via TestModelLoad)
- **Automated UI self-test passes: 63 PASS, 0 FAIL, 79 SKIP** (run via `--selftest`)

### Automated UI Self-Test (`UITestRunner.pas`)
- Runs via `./bin/DBDesignerFork --selftest` — exits with 0 on success, N on N failures
- Also accessible via **Database → Run UI Tests** menu item during normal usage
- **Phase 0**: Closes Tips/startup dialogs programmatically
- **Phase 1**: Creates a new blank model (clicks NewMI) for testing context
- **Phase 2**: Clicks all 113 menu items (skips unsafe: exit, save, open, print, DB ops, browser links)
- **Phase 3**: Clicks all 29 speed buttons on MainForm
- **Phase 4**: Clicks buttons on other visible forms (snapshots form list to avoid iteration bugs)
- 2-second delay between each click for UI stability
- Full stack traces captured on any failure via `BackTraceStrFunc` + `ExceptFrames`
- Results logged to `/tmp/UITestResults.log` with incremental flushing
- **4 bugs found and fixed**: EAccessViolation in DatatypesMI, DBModelMI, ResetPalettePositionsMI, DockPalettesMI (all due to nil palette form references — added `Assigned()` guards)

### Database Shim Layer — Verified with SQLite
- `DriverName="SQLite"` → `ConnectorType="SQLite3"` mapping ✅
- `TSQLConnection.Open` with Params extraction ✅
- `ExecuteDirect` with auto-commit (CREATE TABLE, INSERT) ✅
- `TSQLDataSet.SQLConnection` property bridging ✅
- `TSQLDataSet` query execution (SELECT with field access) ✅
- `SetSchemaInfo(stTables)` returns correct table names ✅
- `SetSchemaInfo(stColumns)` returns column name, position, type, typename, nullability ✅
- `SetSchemaInfo(stIndexes)` returns index name, column name, uniqueness ✅
- Requires `libsqlite3.so` symlink in `LD_LIBRARY_PATH`

### Architecture: Shim Layer (`clx_shims/`)
31 compatibility units mapping CLX/Delphi APIs to LCL/SQLDB:
- **CLX→LCL**: QForms→Forms, QControls→Controls, QGraphics→Graphics, etc.
- **Qt shim**: Maps Qt types/constants to LCL equivalents
- **DB shims**: sqlexpr.pas (TSQLConnection, TSQLDataSet), dbclient.pas (TClientDataSet→TBufDataset), provider.pas (TDataSetProvider)
- **XML shims**: xmlintf.pas, xmldoc.pas, xmldom.pas (wrap laz2_DOM)

### Key Build Commands
```bash
# Main app
cd /workspaces/dbdesigner-fork && lazbuild DBDesignerFork.lpi

# All plugins
for p in Demo HTMLReport DataImporter SimpleWebFront; do
  lazbuild Plugins/$p/DBDplugin_$p.lpi
done

# Run (needs display)
xvfb-run -a ./bin/DBDesignerFork

# Run with model
xvfb-run -a ./bin/DBDesignerFork bin/Examples/order.xml
```

### Test Programs
- `TestModelLoad.pas` — Standalone XML parser test (no LCL needed, compiles with fpc directly)
- `TestSQLite.pas` — Direct SQLDB SQLite3 connectivity test
- `TestSQLExprShim.pas` — Tests our sqlexpr.pas shim with SQLite
- `TestSQLExport.pas` — SQL export test (needs full app infrastructure, hangs standalone)

### Known Runtime Risks
1. Some stubs are no-ops: SaveBitmap (QPixMap_save), custom cursor loading
2. TPanel.Bitmap usage commented out in EditorQueryDragTarget.pas
3. TTreeNode.SubItems via class helper (global dictionary) — untested at runtime
4. Qt event dispatch (QApplication_sendEventAndDelete) is a no-op
5. MDI changed to fsNormal — window management differs from original

### Task Progress: ~203/230 done

### Remaining Work
- **Functional UI testing** (requires real display or VNC)
- **Database connectivity** with MySQL/PostgreSQL (requires DB server)
- **PDF export** testing
- **SQL export** verification (works through UI menu)
- **Code cleanup** (optional): replace Q* shims with direct LCL unit names
- **Cross-platform** testing (Windows, macOS)

### Fix: Font.Weight Runtime Error (Latest)
- **Problem**: `TControl.ReadState` raised errors when loading .lfm forms containing `Font.Weight = 40` — a CLX/Qt property that doesn't exist in LCL.
- **Fix**: Removed all 54 `Font.Weight` / `TitleFont.Weight` lines from 17 .lfm files.
- **Also fixed**: Plugin .lpi files had hardcoded absolute paths (`/workspaces/dbdesigner-fork`); changed to relative paths (`../../`).
- **Result**: All 5 binaries compile and run without the Weight error.

### Fix: "Masked" Property Runtime Error
- **Problem**: `TControl.ReadState` raised errors for unknown property "Masked" in .lfm files.
- **Fix**: Removed all 27 `Masked = True` lines from 9 .lfm files.
- **Reason**: `Masked` is a CLX/Qt-specific bitmap transparency property; LCL handles transparency differently.
- **Result**: All 5 binaries compile and run clean (no stderr output).

## CLX Color Constants Replaced (commit 2921ded)
Replaced deprecated CLX/Qt color constants with LCL equivalents in 10 .lfm files (35 replacements):
- clDark → clBtnShadow, clButton → clBtnFace, clNormalBackground → clWindow
- clText → clWindowText, clMidlight → clBtnHighlight

## Gtk-Message: Failed to load module "canberra-gtk-module"
This is a **harmless** GTK warning. The app uses GTK2 (LCL default), but only the GTK3 
version of the canberra sound module is installed. To silence it:
  sudo apt install libcanberra-gtk-module
This has zero impact on application functionality - it's only about UI sound effects.

## Embedded Image Data Fixed for LCL (commit 0c23bb7)
Two issues fixed:

1. **Icon.Data BMP→ICO conversion**: Main.lfm and Plugins/Demo/Main.lfm had BMP data
   in Icon.Data, but LCL's TCustomIcon.ReadData expects ICO format. Converted to valid
   ICO files (ICONDIR header + ICONDIRENTRY + BMP info header + AND mask).

2. **Glyph.Data/Picture.Data size prefix fix**: 217 fixes across 29 .lfm files.
   Delphi stores size = BMP_size + 4 (includes size field), LCL expects size = BMP_size.
   Subtracted 4 from each size prefix.

App runs clean with no errors.

## Fix: "Unknown property: images" Runtime Error
- **Problem**: `TControl.ReadState` raised "Unknown property: images" when loading forms containing `TListView` with `Images = <ImageListName>`.
- **Root cause**: In Delphi/CLX, `TListView` has an `Images` property. In LCL, the equivalent property is `SmallImages`.
- **Note**: `TTreeView.Images` IS valid in LCL and needed no change — only `TListView.Images` was the problem.
- **Fix**: Changed `Images` → `SmallImages` in 2 .lfm files for TListView components:
  - `PaletteDatatypes.lfm:56` (CommonDataTypesListView: TListView)
  - `DBConnSelect.lfm:264` (ConnectionsListView: TListView)
- **Result**: App starts clean with no property errors (only harmless GTK canberra-gtk-module warning).

## Fix: "Unknown property: columns" Runtime Error
- **Problem**: `TReader` raised "Unknown property: columns" when loading forms with `TTreeView` having CLX-specific `Columns` property.
- **Root cause**: In CLX, `TTreeView` supported multi-column mode with a `Columns` collection. LCL's `TTreeView` does not have `Columns` (only `TListView` has it in LCL).
- **Fix**: Removed `Columns = < ... >` blocks from 10 TTreeView instances in 8 .lfm files. Also removed `ColumnClick = False` from 2 TTreeView instances (CLX-only property).
- **Files modified**: DBConnSelect.lfm, EERPlaceModel.lfm, EERStoreInDatabase.lfm, EditorQuery.lfm, EditorTable.lfm, Options.lfm, OptionsModel.lfm, PaletteDatatypes.lfm, PaletteModel.lfm
- **TListView Columns**: Left intact (valid in LCL).

## Why Selftest Doesn't Catch Property Errors
- "Unknown property" exceptions are raised by `TReader.PropertyError` during form resource loading.
- LCL's `TReader` catches these exceptions internally (try/except in `ReadProperty`/`ReadData`).
- They never propagate to user code or `Application.OnException`.
- The selftest only tests button/menu clicks on already-loaded forms.
- Solution: Could add try/except wrappers around form creation in `ShowPalettesTmrTimer` to detect `EReadError`, or add static .lfm analysis to the selftest.


## BMP/Image Fix Analysis — Comprehensive Study (Pre-Implementation)

### Context
The CLX→LCL port involved converting .xfm form files to .lfm. Embedded image data has several format incompatibilities. Some were already fixed, but 23 image data blocks remain broken or missing.

### What Was Already Fixed
1. **Glyph.Data/Picture.Data size prefix** (217 fixes, 29 files): Delphi stores `size = BMP_size + 4`, LCL expects `size = BMP_size`. Fixed in commit `0c23bb7`.
2. **Icon.Data BMP→ICO conversion** (2 files): Main.lfm and Demo plugin had BMP data in Icon.Data but LCL expects ICO format. Fixed in commit `0c23bb7`.
3. **IMGL TImageList removal** (2 files): DatatypesImgList in PaletteDatatypes.lfm and ModelImgList in PaletteModel.lfm were removed because IMGL format caused "wrong image format" errors. Fixed in commit `bb914b6`. **Note**: these ImageLists are now empty — icons are missing at runtime!

### Image Block Counts: .xfm (original) vs .lfm (current)
| File | .xfm | .lfm | Diff | What's Missing |
|------|-------|------|------|----------------|
| Main | 55 | 49 | -6 | TPanel.Bitmap.Data (6 panel backgrounds) |
| PaletteModel | 11 | 5 | -6 | TMenuItem.Bitmap.Data (6 menu icons) |
| PaletteDatatypes | 7 | 3 | -4 | TImageList IMGL data (DatatypesImgList) + TMenuItem.Bitmap.Data |
| Options | 4 | 2 | -2 | TPanel.Bitmap.Data (2 panel backgrounds) |
| PaletteNav | 6 | 5 | -1 | TPanel.Bitmap.Data (1 panel background) |
| OptionsModel | 5 | 4 | -1 | TPanel.Bitmap.Data (1 panel background) |
| EditorTable | 6 | 5 | -1 | TPanel.Bitmap.Data (1 panel background) |
| EditorQueryDragTarget | 1 | 0 | -1 | TPanel.Bitmap.Data (1 panel background) |
| EERPlaceModel | 1 | 0* | -1 | * Has IMGL data in ImageList but ImageList still present |
| SimpleWebFront/Main | 19 | 18 | -1 | TPanel.Bitmap.Data (1 panel background) |
| **Total** | **206** | **183** | **-23** | |

### What Still Needs Fixing — 3 Categories

#### 🔴 Category 1: TImageList IMGL Data — HIGH PRIORITY (7 ImageLists, functional impact)
These TImageList components have IMGL-format Bitmap data that **silently fails to load** at runtime. Users see blank spaces where icons should appear.

**Still present in .lfm but silently broken (5 files):**
| File | Component | Used For |
|------|-----------|----------|
| `EditorTable.lfm` | DatatypesImgList | Datatype icons drawn in table editor column grid |
| `DBConnSelect.lfm` | DBConnImgList | Connection type icons in connection selector list |
| `EditorQuery.lfm` | StoredSQLImageList | Query/SQL tree node icons |
| `EERPlaceModel.lfm` | LMImgList | Place model dialog icons |
| `EERStoreInDatabase.lfm` | (ImageList) | Store-in-DB dialog icons |

**Already removed from .lfm (2 files — ImageLists now empty!):**
| File | Component | Used For |
|------|-----------|----------|
| `PaletteDatatypes.lfm` | DatatypesImgList | Datatype icons in datatypes palette |
| `PaletteModel.lfm` | ModelImgList | Model tree node icons |

**Root cause**: LCL's `TCustomImageListResolution.ReadData` (in `imglist.inc` line 701+) reads a 2-byte signature:
- `IL` = Delphi 3+ format (SIG_D3)
- `li`/`Li`/`Lz`/`#1#0` = Lazarus formats
- CLX/Delphi data starts with `IMGL` (hex `49 4D 47 4C`), so 2-byte signature = `IM` → **matches nothing** → falls through to D2 handler → fails silently.

**IMGL Binary Format** (parsed from .xfm hex data):
```
'IMGL' (4 bytes magic)
Count  (4 bytes, little-endian) — number of images
For each image:
  NameLength (4 bytes, little-endian)
  Name       (NameLength bytes, UTF-16LE string)
  DataSize   (4 bytes, little-endian)
  BMP data   (DataSize bytes, standard Windows BMP)
```

**LCL's `Li` (SIG_LAZ3) format** (what we need to convert TO):
- See `imglist.inc` lines 860-950 for read, lines 1037-1055 for write.
- Signature `Li` + individual images stored with width/height + raw pixel data.

#### 🟡 Category 2: TPanel.Bitmap.Data — MEDIUM PRIORITY (cosmetic, ~12 blocks)
LCL's TPanel has no published `Bitmap` property. A class helper (`clx_shims/panelbitmap.pas`) provides runtime property access via a global dictionary, but LCL's TReader/streaming system cannot use class helper properties.

**Affected panels** (all stripped from .lfm, data only in .xfm):
- Main.lfm: 6 panel backgrounds (toolbar/status panels)
- Options.lfm: 2 panels
- PaletteNav.lfm: 1 panel
- OptionsModel.lfm: 1 panel
- EditorTable.lfm: 1 panel
- EditorQueryDragTarget.lfm: 1 panel
- SimpleWebFront/Main.lfm: 1 panel

#### 🟢 Category 3: TMenuItem.Bitmap.Data — LOW PRIORITY (6 blocks in PaletteModel)
Removed in commit `bb914b6` along with the IMGL ImageList fix, but LCL's TMenuItem **does** have a valid published `Bitmap` property. These are small menu item icons that could be restored.

### Fix Strategy

#### Phase A: TImageList IMGL→LCL Conversion (Category 1)
**Approach**: Write a Python script that:
1. Parses IMGL binary data from .xfm hex blocks
2. Extracts individual BMP images
3. Re-encodes in LCL-compatible format (either `Li` Lazarus format, or Delphi `IL` format which LCL supports)
4. Replaces hex data in .lfm files
5. For the 2 already-removed ImageLists (PaletteDatatypes, PaletteModel), restores them from .xfm

**Alternative**: Extract images as individual .png files into `bin/Gfx/` and populate ImageLists programmatically in FormCreate. More maintainable but requires Pascal code changes in 7+ files.

#### Phase B: TMenuItem.Bitmap Restoration (Category 3)
Restore 6 TMenuItem.Bitmap.Data blocks from PaletteModel.xfm to PaletteModel.lfm with size prefix fix (subtract 4).

#### Phase C: TPanel.Bitmap (Category 2)
Options:
- Accept cosmetic loss (simplest)
- Load backgrounds in FormCreate via `Panel.Bitmap.LoadFromFile(...)` using existing class helper
- Extract BMP data from .xfm, save as files in `bin/Gfx/Panels/`, load at runtime

### Execution Order
1. **First**: Category 1 — IMGL conversion (biggest user-visible impact, icons missing everywhere)
2. **Second**: Category 3 — TMenuItem.Bitmap restoration (quick win, 6 menu icons)
3. **Third**: Category 2 — TPanel.Bitmap (optional cosmetic polish)

### Key Files to Modify
- 7 .lfm files (ImageList data replacement/restoration)
- 1 .lfm file (PaletteModel TMenuItem.Bitmap restoration)
- Potentially 7+ .pas files (if using runtime loading approach)
- New Python conversion script (tooling)

### Verification Plan
After fixes, verify by:
1. `lazbuild DBDesignerFork.lpi` — compiles clean
2. `xvfb-run -a ./bin/DBDesignerFork --selftest` — no new failures
3. Visual inspection: open EditorTable, DBConnSelect, EditorQuery dialogs and verify icons appear
4. Check stderr for any "wrong image format" or "Unknown property" errors

### Results — Phase A: IMGL Conversion ✅ COMPLETED (commit d7ece4f)
- All 7 TImageList IMGL→LCL conversions successful
- Created `imgl_to_lcl.py` converter tool
- **Fixed the only startup error**: `DockedEditorQueryForm: Error reading StoredSQLImageList.Bitmap: Wrong image format`
- Selftest now shows: **167 tests, 82 PASS, 0 FAIL, 85 SKIP — "Self-test PASSED: no failures detected"**
- Additional forms now load correctly (PaletteNavForm, PaletteDataTypesForm, PaletteModelFrom discovered by test runner)

### Remaining Work
- **Phase B**: TMenuItem.Bitmap restoration in PaletteModel.lfm (6 menu icons) — LOW PRIORITY
- **Phase C**: TPanel.Bitmap backgrounds — COSMETIC, optional

## Important: Selftest Timeout
When running the selftest (`bin/DBDesignerFork --selftest`), use **at least 300 seconds** for timeout:
```bash
run_os_command('bin/DBDesignerFork --selftest 2>&1', timeout=300, max_memory=1024*1024*1024)
```
The selftest clicks ~113 menu items and ~29 buttons with 2-second delays between each, plus opens/closes 
dialogs, so it takes several minutes to complete. Previous runs have taken 2-4 minutes.


## Important: Always use `--build-all` after editing `.lfm` files

The Free Pascal Compiler (FPC) does **not** track `.lfm` file changes during incremental builds. If you only edit a `.lfm` form file without touching its corresponding `.pas` unit, the compiler will consider the precompiled `.ppu` unit up-to-date and skip recompilation — meaning your `.lfm` changes will **not** appear in the binary.

### What to do

After editing any `.lfm` file, always rebuild with:

```bash
lazbuild --build-all DBDesignerFork.lpi
```

Or in the Lazarus IDE use **Run → Build All** (`Ctrl+Shift+F9`) instead of **Run → Build** (`Shift+F9`).

### Why this happens

The `{$R *.lfm}` directive embeds form data into the `.ppu`/`.o` file at unit compilation time. FPC only checks `.pas` file timestamps to decide whether to recompile a unit — it does not check the timestamp of the corresponding `.lfm` file. So an incremental build will silently use stale form data from the previous compilation.

### Verified experimentally
1. Changed `Caption` in `src/Splash.lfm` → ran `lazbuild` (incremental) → only 164 lines compiled (just `.lpr`), binary had OLD caption
2. Ran `lazbuild --build-all` → 58,102 lines compiled (full rebuild), binary had NEW caption


## How to add more editor tests
- Pattern: `Frm := TXxxForm.Create(AMainForm); Frm.SetXxx(...); ScheduleModalClose(800); Frm.ShowModal; Frm.Free;`
- Wrap in try/except, increment PassCount/FailCount counters, log [PASS]/[FAIL]/[SKIP].
- Editors NOT yet tested directly: TEditorTableForm, TEditorQueryForm, TEditorString/Datatype/Note/Region/Relation, TEERReverseEngineering/StoreInDatabase/Synchronisation, TPlaceModelForm, TPrinterSettingsForm.
- See `src/UITestRunner.pas:386` Phase0 / `src/UITestRunner.pas:573` Phase3 for reference templates.


## Fix: black table title bars after load (ui-bug-catalog #7)

Cause: a Delphi/CLX-to-LCL scoping trap, not a bitmap problem. `TEERTable.PaintCachedImg`
and `PaintObj2Canvas` declared locals `width, height` and used them inside
`with theCanvas do`. Delphi's `TCanvas` has no `Width`/`Height`, so the locals were meant;
LCL's `TCanvas` does have them, and the `with` scope wins. `TCanvas.Width` is 0 until the
canvas has a handle, and the freshly resized `StrechedImg` cache bitmap gets its handle
created *by the first drawing call* - so the header `StretchDraw` ran with
`Rect(xo+1, 0, xo+0-3, 18)` (empty) and drew nothing; the uninitialised pixmap stayed black
and the (black) table name was invisible on it. Every later call saw the real width, which
is why the rest of the table was fine and why any recache (mode switch, zoom) "fixed" it.

Fix: renamed the locals to `tblWidth`/`tblHeight` (src/EERModel.pas). Verified with
`xdotool`/`import` screenshots on DISPLAY=:0: names visible right after load.

How it was found: the GTK2 widgetset call for the very first op arrived with
`rect=1,0,-3,14` (gdb breakpoint on `TGtk2WidgetSet.FillRect`); everything above and below
it (bitmap handles, GC, drawable XIDs, masks) was fine.

Same latent pattern (locals `width, height` + `with theCanvas do`) still exists in
`src/EERModel.pas` around lines 12351, 13008 and 13432 (note/image/region-type paints);
likely the cause of catalog #9 (notes drawn 1 px high). Left untouched here on purpose.
Catalog #19 (Visual Options header preview) has a different cause: the `panelbitmap.pas`
shim stores `TPanel.Bitmap` but never paints it.

## Fix: oversized text over the Navigator / Page Setup thumbnails (ui-bug-catalog #8)

Cause: a missing `begin/end`, not a font-size or zoom-factor problem. `PaletteNav.pas` and
`EERPageSetup.pas` call `EERModel.PaintModel(..., doDrawText=False)`, which sets
`DMEER.DisableTextOutput`. In `TEERTable.PaintCachedImg` (src/EERModel.pas) the column
name, index name and index-column name are written as

    if(Not(DMEER.DisableTextOutput))then
      Brush.Style := bsClear;
      TextOut(...);

so only the `Brush.Style` assignment was guarded and `TextOut` ran unconditionally. With
text output disabled `Font.Height := ParentEERModel.GetFontHeight` is skipped too, so the
strings came out at the canvas' default (100 %) size on a ~5 % thumbnail - hence the few
huge "productgroup (FK)" strings covering the miniature. The table-name/header paths were
properly bracketed, which is why only column names showed.

Fix: wrapped the three pairs in `begin/end`. Verified on DISPLAY=:0 (shots
`fix08-before-nav.png` -> `fix08-after-nav-zoom.png`, `fix08-after-pagesetup.png`): the
Navigator shows the region/tables/relations miniature, Page Setup preview likewise.

Driving the app with xdotool: the client window is at +42+69 (frame 42,32 + 37 px title),
so menubar y is ~83, not 120. GTK menu popups are override-redirect windows; find them with
`xwininfo -root -tree` (e.g. 212x399 for File), not `xdotool search`. The Tips dialog's
"Close Tip Window" button is at client-relative (340,217).

Catalog #13 (Navigator "Info" tab) not attempted: a misclick had switched the main window
into query mode (palettes hidden) before I got to it; needs a fresh session.

## Fix: notes collapsed to a 1-px line and clipped relation labels (ui-bug-catalog #9, #14)

Cause: `TEERModel.GetTextExtent` (src/EERModel.pas) measured text on `SelectionRect.Canvas`.
`SelectionRect` is an invisible `TPaintBox`; under LCL its canvas never gets a handle, so
`TextExtent` returned nothing useful. Three more things hid behind that: (1) the measurement
canvas kept the constructor font ("Nimbus Sans L") while loading a model set
`DefModelFont` to "Tahoma" (substituted by fontconfig with the wider Noto Sans) on every
object - so even measuring on a real canvas gave a narrower width than the paint; (2) LCL's
`TextExtent` does not understand line breaks (it returns the summed width and one line
height), and `TextRect` draws a single line, so the CLX-era `Obj_H := ... +4-14` gave a
negative height; (3) `EvalZoomFac(ReEvalZoomFac(px)+6)` loses 1-2 px at 75 % zoom (the
default for order.xml) and `TextRect` clips at `Width-2` with the text starting at 1+3 px,
so the last glyph of "CreditCardRel" was cut.

Fix: `TEERModel` now owns `TextMeasureBmp: TBitmap`; `GetTextExtent` re-applies
`DefModelFont` on each call and measures line by line (widest line x lines*TextHeight,
trailing line break ignored). `TEERNote.RefreshObj` uses `+4` instead of `+4-14` and adds a
few pixels before the model-unit conversion; `TEERNote.PaintObj2Canvas` paints
`NoteText[i]` per line. `RelCaption`/`RelStartInterval`/`RelEndInterval` are sized in
pixels (`theSize.cx+EvalZoomFac(6)+2`). Also renamed the `width`/`height` locals in the
note/region/image painters and introduced `capW`/`capH` in the three relation-label painters
(they were resolving to `TCanvas.Width/Height` inside `with theCanvas do`, harmless on the
control's own canvas but wrong when painting to an export canvas).

Verified on DISPLAY=:0 (shots `fix09-before-half.png` -> `fix09-after-crop.png`,
`fix09-zooms.png`): both notes show their full two-line text, "CreditCardRel",
"ProductgroupRel", "OnlineorderRel" are complete.

Gotcha for xdotool sessions: `pkill -f DBDesignerFork` kills the calling bash too when the
command line mentions the binary; use `pkill -x DBDesignerFork`. Launch with `setsid`.

## Fix: DB Connection editor "Index Out of range Cell[Col=0 Row=5]" and Delete shortcut (ui-bug-catalog #1, #6)

#1: `TDBConnEditorForm.FormCreate` (src/DBConnEditor.pas) filled `ParamStrGrid.Cells[0,5]`
and `[0,6]` while the .lfm left `RowCount` at the LCL default of 5. CLX grew the sparse cell
storage silently; LCL raises. `FormCreate` aborted before `DatabaseTypesCBox` was filled,
which produced the follow-up "List index (-1) out of bounds" in `SetData`. Fix: set
`ParamStrGrid.RowCount:=7` before filling. (`SetData` still does `RowCount:=6+Params.Count`
and `Cols[1].Clear`, which also clears the "Value" header - same as the original, left alone.)

#6: `DeleteMI.ShortCut = 20487` ($5007) in src/Main.lfm was CLX: Qt `Key_Delete` ($1007)
or-ed with `scCtrl` ($4000). LCL only knows VK codes, so it rendered as Ctrl+Meta+Word('7').
The original binding was Ctrl+Del (the `FormKeyDown` handler in Main.pas at "Keys in Design
Mode" also checks `VK_DELETE` with `[ssCtrl]`), so it is now `ShortCut = 16430`
(ShortCut(VK_DELETE,[ssCtrl])), shown as "Ctrl+Del". Plain Del was deliberately not used:
an LCL menu shortcut is evaluated before edit controls see the key. Audit of the remaining
`ShortCut =` values in src/*.lfm and Plugins/*/*.lfm: 16461/16467/16468/16471 are
Ctrl+M/S/T/W (VK codes < 256, valid in both CLX and LCL) - nothing else to translate.

Gotcha: lazbuild does not notice a changed .lfm alone; `touch src/Main.pas` (or `-B`) to get
the resource rebuilt. Verified on DISPLAY=:0: `fix01-edit-menu.png`,
`fix01-db-conn-editor.png`, `fix01-db-conn-editor-advanced.png`.

## Fix: plugins crash at start-up (ui-bug-catalog #2, #3, #4)

First of all the plugin projects could not be built at all: commit 8b2b15f moved the sources
to `src/` but `Plugins/*/DBDplugin_*.lpi` (IncludeFiles/OtherUnitFiles) and the `.lpr` files
(`{$I ../../DBDesigner4.inc}`, `MainDM in '../../MainDM.pas'`, ...) still pointed at the old
layout. Now `../../src/...`. Build from the plugin directory:
`cd Plugins/Demo && lazbuild DBDplugin_Demo.lpi` (the include path in the .lpr is relative).
Note DataImporter has a stale local `MainDM.pas`; the `.lpr` deliberately names the main
app's `src/MainDM.pas` (the plugin uses `ProgName`/`SettingsPath`/`LoadValueFromSettingsIniFile`).

#2: `TEERModel.LoadFromFile`/`LoadFromFile2` (src/EERModel.pas) and `TDMEER.SetWorkTool`
(src/EERDM.pas) call `Application.MainForm.SetFocus`, guarded only by `Enabled` (or `Visible`).
`TCustomForm.SetFocus` in LCL raises `EInvalidOperation` "Can not focus" unless
`IsControlVisible and Enabled`; the plugins load the model in `FormCreate`, before the form is
shown, so the exception escaped `FormCreate` and `InitControls` (fills the table list) never
ran. Guard replaced by `Application.MainForm.CanFocus`, which for a parentless form is exactly
`IsControlVisible and Enabled`. No try/except.

#3: `Rows = 6` on `SpecialFieldsLBox: TListBox` in Plugins/DataImporter/DBImportData.lfm is a
CLX-only property (same class as `Font.Weight`/`Masked`); removed. Audit: no other `Rows =`
(FixedRows is fine) in Plugins/*/*.lfm.

#4: every `Glyph.Data` block in Plugins/SimpleWebFront/*.lfm (28 blocks, 6 files) still had
the Delphi `size = BMP_size + 4` prefix; the 0c23bb7 conversion had skipped this plugin.
Fixed with a script that subtracts 4 whenever `prefix == BMP header size + 4`. After that
the form still failed on `PageControlTreeView.Columns` (CLX-only TTreeView property, removed;
the Delphi-format `Items.Data` stream is read fine by LCL's `TTreeNodes.ReadData`) and
`FormCreate` then raised "Invalid type cast": order.xml carries SimpleWebFront plugin data,
`LoadSWF_DataFromString` does `LoadXMLData(..).GetDocBinding('SWF_Data', TXMLSWF_DataType, ..)
as IXMLSWF_DataType`, and the `XMLIntf` shim's `GetDocBinding` ignored the class and returned
a plain `TXMLNodeWrapper`. `RegisterChildNode` also only stored the class *name*, so every
`ChildNodes['X'] as IXML...` / `List[i] as IXML...` in the generated binding would have failed
the same way. `src/clx_shims/xmlintf.pas` now keeps the class pointer, instantiates it in
`GetDocBinding`, `TXMLNodeIndexed.GetNodeByName`, `TXMLNodeCollection.GetList/AddItem`
(`CreateChildNodeObject`), and binding nodes hold an `IXMLDocument` reference (`FDocRef`) so
the DOM survives the temporary `LoadXMLData(...)` interface. The main app links this shim too
(EERModel_XML, MainDM) and was rebuilt and re-checked.

Verified on DISPLAY=:0 with `./DBDplugin_<Name> Examples/order.xml`, shots `fix02-*`:
Demo lists the 14 tables (`fix02-Demo-*.png`), HTMLReport lists the "Forum" region's tables
(`fix02-HTMLReport-*.png`), DataImporter opens (`fix02-DataImporter-*.png`; it has no model
table list), SimpleWebFront opens with the stored Web Title "Order" and its View Editor's
Table combo lists all 14 tables (`fix02-SWF3-*.png`).

## Fix: empty font combos / literal "FontCBox" (ui-bug-catalog #10)

Cause: NOT an empty `Screen.Fonts`. A stand-alone LCL/GTK2 test program returns 236
families on this machine (`fc-list | wc -l` = 535), and the dropdowns in the app were in
fact populated. The combo *text* was blank because the saved font name is never an
installed family here: loaded models carry `DefModelFont="Tahoma"`, the Linux default is
the obsolete `Nimbus Sans L` (fontconfig now calls it "Nimbus Sans"), and the SQL font
default is `Helvetica`. `Items.IndexOf` returned -1, `ItemIndex:=-1` left the edit empty
(Model Options) or kept the design-time `Text = 'FontCBox'` from `Options.lfm`. Because
OK only stored a font when `ItemIndex>=0`, the choice was also silently dropped.

Fix: `TDMMain.FillFontCBox` / `GetFontCBoxSelection` (src/MainDM.pas). The fill inserts
the current font at the top of the list when it is not installed (so it stays visible and
selectable; the canvas still gets fontconfig's substitute), then sets `ItemIndex` and
`Text`. Save reads `Text` (typed names allowed) with the old value as fallback. Both
callers (src/OptionsModel.pas, src/Options.pas) use it; the `Text = 'FontCBox'` line was
removed from src/Options.lfm. Verified on DISPLAY=:0: Model Options shows "Tahoma" and the
dropdown lists all families (`fix10-model-after.png`); typing "Z003" + OK re-renders every
table/relation label in that font (`fix10-canvas-after.png`), i.e. `EERModel.RefreshFont`
does take effect; DBDesigner Options > Database Options shows "Helvetica" with a populated
list (`fix10-dbd-options-database.png`).

## Fix: Table Editor column grid (ui-bug-catalog #11) and Advanced page (#18, Table Editor part)

Cause (icons): `src/imgl_to_lcl.py` (commit d7ece4f) converted the CLX `IMGL` image-list
streams assuming the Windows mask convention "1 = transparent". Six of the seven streams
are Kylix/Qt `QBitmap` masks where color0 = transparent and color1 = opaque, so every
coloured pixel in `EditorTable.lfm`'s `DatatypesImgList` got alpha 0 and only the white
background was opaque -> `DatatypesImgList.Draw` painted nothing visible. (PaletteDatatypes
is the one list with the VCL convention, which is why the Datatypes palette icons worked.)
The converter now auto-detects the polarity per stream: the transparent side of a mask
covers exactly one key colour (white), so the mask value with a single distinct colour
under it is the transparent one. It also accepts .lfm names on the command line
(`python3 imgl_to_lcl.py EditorTable.lfm`) to regenerate one list. Only EditorTable.lfm
was regenerated here; running it without arguments would fix the DBConnSelect,
EERPlaceModel, EERStoreInDatabase, EditorQuery and PaletteModel lists too (catalog #15).

Cause (headers): `ColumnGridDrawCell` drew "Column Name"/"DataType" at `Rect.Left+1-18`
so the caption would span the 20 px icon column on the left (CLX did not clip the fixed
row to the cell). LCL clips OnDrawCell output to the cell, so the first 18 px of the
caption vanished ("umn Name", "aType"). Drawn at `Rect.Left+1` now; the icon columns keep
their empty header.

Cause (Advanced page): the RAID group box of `TabSheet1` was laid out for Tahoma 8. Under
LCL the auto-sized "Use Table RAID" check box ran into "RAID Type:" (labels had a `Width`
and `Alignment = taRightJustify` but no `AutoSize = False`, so the alignment was ignored),
the 73 px combo showed "STRI" and "kB" fell outside the 283 px box. Layout only
(`src/EditorTable.lfm`): box moved to Left 236/Top 0/Width 298, labels right-justified
with `AutoSize = False` + explicit `Height = 15` (an LCL label without a Height is
0 px high when AutoSize is off), edits/combo 90 px wide, "kB" at 279.
Verified on DISPLAY=:0: `fix11-grid3.png`, `fix11-advanced3-c.png`.

Gotcha while testing: `~/.DBDesigner4/DBDesignerFork_Settings.ini` `WorkMode=2` (Query
mode, written by whichever instance exits last, including --selftest runs) makes a table
double-click open "Select Database Connection" instead of the Table Editor. Set
`WorkMode=1` or press Ctrl+Tab first.

## Fix: catalog #12 (not a bug), #15 (image lists), #18 Datatype Editor part

#12 `ssss...INTEGER`: not reproducible - a fresh double-click on INTEGER/VARCHAR shows the
name correctly (`fix12-dt-editor-before.png`). The diagnosis session had a stuck
auto-repeating key: `31-db-sync.png` (8 min after `26-datatype-editor.png`) shows the
Connect dialog's Password field full of characters that was empty in `09-db-connect.png`,
and nothing in the sources synthesises key/char events (`QKeyEvent_create` is only used
to forward LCL key-downs to `DoApplicationEvent`). `xdotool keydown s` reproduces the
look (`fix12-stuckkey-test-top.png`). Lesson for xdotool sessions: pair every `keydown`
with `keyup`, and if edits start filling with one character run `xdotool keyup <key>`.

#15: `cd src && python3 imgl_to_lcl.py` (must run inside `src/`, the CONVERSIONS table
uses bare file names) regenerated the remaining five image lists with the corrected mask
polarity. Verified DB-connection tree (`fix12-dbconn-combo.png`) and DB Model tree
(`fix12-after1.png`); EERPlaceModel/EERStoreInDatabase/EditorQuery need a live DB
connection and were not looked at, but they are the same stream format.

#18 Datatype Editor: `src/EditorDatatype.lfm` only. Group boxes "Parameter"/"Options"
126 -> 140 px (LCL check boxes need ~19 px), list boxes/check boxes moved accordingly.
`EnablePhysicalMappingCBox` was placed on top of `GroupBox3`'s caption line (Top 278 vs
box Top 280); under GTK2 the later-created group box covers it completely, so the
"Enable Physical Datatype Mapping" option was invisible. It now sits above the box
(Top 284, box Top 304), ClientHeight 399 -> 411. "Synonymgrp." was fine.

Testing gotcha: after the Tips dialog is closed its X window stays in `xwininfo -tree`
as IsViewable and `xdotool getactivewindow` may still report it; do not rely on those to
decide whether it is open. Also the dialog can be placed over the main menu bar
(+28+20), so close it before clicking menus.

## Fix: Options dialogs clipped/overlapping (ui-bug-catalog #16, #17)

Cause: both forms were laid out for the Qt 8 pt font; LCL/GTK2 renders `Sans -11`
wider, so fixed-width check boxes truncated and the `Various` group boxes (already
overflowing the page by 20 px at design size) were cut at the page edge. Two more
LCL-specific problems on the same pages: a check box placed over a group box caption
(`UsePosGridCBox`, Top -1/1) is completely covered by the native group box under GTK2
(seen before in EditorDatatype, #18), and a label whose converted .lfm carries
`AutoSize = True` immediately followed by `AutoSize = False` without a Width ends up
0 px wide and invisible (the grey hint labels under the Table Prefixes list).

Fix (.lfm only): `src/Options.lfm` form 800x372 with every control that was anchored
to the right/bottom edge widened explicitly (Anchors only act on later resizes), wider
group boxes/check boxes, taller Reset button; `src/OptionsModel.lfm` "Table Prefixes:"
label `AutoSize = False` at Left 270/Width 94, hint labels autosized, grid/canvas group
boxes moved down so the snap check box is visible. Before/after: `fix16-before-*`,
`fix16-after-*` in the shots dir.

Testing gotchas: the DBDesigner Options dialog cannot be closed with xdotool: its
OK/Cancel speed buttons are `Enabled = False` in the .lfm and are only enabled in
`SubmitBtnMouseEnter`, which GTK2 never fires for a disabled control; Escape works only
when the form itself has focus (no KeyPreview). Likely a real bug for users too (not
in the catalog yet). Workaround: one app launch per dialog (`fix16-cycle.sh` in the
scratchpad: launch, close Tips at (782,617), Options menu at (364,82), item at
(400,140)/(400,107), click tree rows at +75/+22+14*i, `import -window`, kill by PID).
Model Options takes 3-5 s to appear (font enumeration); poll `xwininfo -root -tree`.
