# Notes to Myself — DBDesigner Fork Lazarus Port

## Current Status (Latest)

**ALL projects compile successfully under Lazarus/FPC!**

| Project | Lines | Time | Binary |
|---------|-------|------|--------|
| Main App (DBDesignerFork) | 56,608 | 4.4s | bin/DBDesignerFork |
| Demo Plugin | 21,793 | 2.3s | bin/DBDplugin_Demo |
| HTMLReport Plugin | 22,258 | 3.5s | bin/DBDplugin_HTMLReport |
| DataImporter Plugin | 8,836 | 3.0s | bin/DBDplugin_DataImporter |
| SimpleWebFront Plugin | 40,096 | 3.7s | bin/DBDplugin_SimpleWebFront |
| **Total** | **149,591** | | |

## Architecture: Shim Layer Approach

Instead of rewriting all source files, we created **compatibility shim units** in `clx_shims/`:

### CLX → LCL Shims
Simple re-exports: `QForms→Forms`, `QControls→Controls`, `QGraphics→Graphics`, `QDialogs→Dialogs`, `QStdCtrls→StdCtrls`, `QExtCtrls→ExtCtrls`, `QMenus→Menus`, `QComCtrls→ComCtrls`, `QButtons→Buttons`, `QCheckLst→CheckLst`, `QImgList→ImgList`, `QClipbrd→Clipbrd`, `QPrinters→Printers`, `QMask→MaskEdit`, `QGrids→Grids`, `QDBGrids→DBGrids`, `QFileCtrls→FileUtil`

### Qt Shim (`qt.pas`)
Maps Qt widget types (QObjectH, QEventH, etc.) to LCL equivalents. Provides `ButtonStateToShiftState`, `Key_*` constants, `QEvent_type`, `QKeyEvent_key` functions. Application event handling maps to LCL `OnKeyDown`.

### Database Shims
- **`sqlexpr.pas`**: `TSQLConnection` wraps `TSQLConnector`, maps DriverName→ConnectorType. `TSQLDataSet` wraps `SQLDB.TSQLQuery` with `SetSchemaInfo` for stTables/stColumns/stIndexes.
- **`dbclient.pas`**: `TClientDataSet` wraps `TBufDataset` with ProviderName chain support.
- **`provider.pas`**: `TDataSetProvider` bridges datasets.
- **`dbxpress.pas`**: Schema type constants (stNoSchema, stTables, etc.)
- **`FMTBcd.pas`**: Stub types.

### XML Shims
- **`xmlintf.pas`** (~600 lines): Full interface wrappers (`IXMLDocument`, `IXMLNode`, `IXMLNodeList`, `TXMLNode`, `TXMLNodeCollection`, `TXMLNodeIndexed`, `TXMLDocumentWrapper`) over `laz2_DOM`.
- **`xmldoc.pas`**: `NewXMLDocument`, `LoadXMLDocument`, `LoadXMLData` functions.
- **`xmldom.pas`**: `IDOMDocument`, `IDOMNode` etc. mapped to laz2_DOM types.

## Key Technical Decisions

1. **Plugins are standalone executables** — not shared libraries. The existing `FindFirst('DBDplugin_*')` + `CreateProcess` mechanism works as-is.

2. **`USE_IXMLDBMODELType` is NOT defined** — Core XML uses `LibXmlParser` (TXmlParser), not DOM interfaces. The DOM interfaces are only used by `EERModel_XML.pas`, `EERModel_XML_ERwin41_Import.pas`, and `SWF_XML_Binding.pas`.

3. **TDirectoryTreeView → TShellTreeView** — CLX's `TDirectoryTreeView` replaced with LCL's `TShellTreeView` from `ShellCtrls`. Property mapping: `RootDirectory→Root`, `Directory→Path`.

4. **Conditional compilation** — `{$ELSEIF LINUX}` / `{$IFEND}` are Delphi-only; FPC uses `{$ELSE}` / `{$ENDIF}`.

## Known Runtime Risks

1. Some stubs are no-ops (SaveBitmap, custom cursor loading)
2. TPanel.Bitmap usage commented out
3. TTreeNode.SubItems via class helper (global dictionary) — untested
4. TSQLConnection shim wraps TSQLConnector — DB connectivity untested
5. LoadApplicationFont writes to Screen.SystemFont — may need adjustment
6. ScanLine portability warnings in EmbeddedPdfImages.pas (expected)

## Remaining Work

- **Runtime testing** (requires X11/display)
- Open each .lfm in Lazarus IDE to check for unknown properties
- Test DB connections (MySQL, PostgreSQL, SQLite)
- Test PDF export
- Code cleanup (optional: remove clx_shims, replace Q* with direct LCL names)
- Update README with Lazarus build instructions

## Build Commands
```bash
# Main app
cd /workspaces/dbdesigner-fork && rm -rf lib/ && lazbuild DBDesignerFork.lpi

# All plugins
for p in Demo HTMLReport DataImporter SimpleWebFront; do
  rm -rf Plugins/$p/lib/ && lazbuild Plugins/$p/DBDplugin_$p.lpi
done
```
