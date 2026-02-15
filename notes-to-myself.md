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

### Latest Commits
```
ab48590 Add SQL export test program
313c1cf Merge branch 'main'
40e0793 Add Lessons Learned section to port-to-lazarus.md
22782f0 Archive unused Delphi project files
74fdb3c Verify full schema info works with SQLite
f52b469 Fix SetSchemaInfo test
1408ab9 Add SQLite and SQLExpr shim integration tests
```


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

## Splash Screen Commented Out (commit 17d9573)
- `Splash.pas:92` was loading `splashscreen.png` via `TBitmap.LoadFromFile`
- LCL's `TBitmap` only supports BMP format, not PNG → "Wrong image format" exception
- Commented out splash screen creation in `DBDesignerFork.lpr`
- Set `Version := '1.5'` directly in `Main.pas` instead of reading from `SplashForm.VersionLbl.Caption`
- App now starts clean with no errors (exit 124 = killed by timeout = stayed alive)
- Future fix: could convert splashscreen.png to BMP, or use `TPicture.LoadFromFile` which auto-detects format

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
