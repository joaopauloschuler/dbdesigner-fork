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
