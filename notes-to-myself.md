# Notes to Myself — DBDesigner Fork Lazarus Port

## Current Status (Latest Update)
- **Project compiles successfully** with only 2 unavoidable warnings (ScanLine portability)
- Binary: `bin/DBDesignerFork` (42MB ELF x86-64)
- **Cannot test runtime** in headless container (no X11 display, Xvfb causes hangs)

## Build Command
```bash
cd /workspaces/dbdesigner-fork && rm -rf lib/ && lazbuild DBDesignerFork.lpi
```

## Git History (latest first)
- `f8d371a` - Fix remaining compiler warnings: WideString→string, TTreeView custom draw (2 warnings left)
- `6b3ae0a` - Fix compiler warnings: destructor/constructor visibility, uninitialized vars, reintroduce (49→21)
- `b67a57a` - Remove CLX OnCanResize from EditorTable.lfm
- `95930f1` - Replace Windows-specific fonts with cross-platform alternatives in .lfm files
- `910ec86` - Remove CLX Bitmap.Data from .lfm files and TextHeight/TextWidth
- `26892db` - Fix deprecation warnings (DecimalSeparator, Thread.Resume)
- `1814934` - Fix .lfm files: remove CLX-specific properties
- Earlier commits: Phase 0 setup, bulk CLX→LCL replacements, compilation fixes, SynEdit

## Architecture
- **Shim approach**: `clx_shims/` has 30+ compatibility units mapping CLX Q* → LCL
- Key shims: `Qt.pas`, `SqlExpr.pas` (wraps SQLDB), `DBClient.pas` (wraps BufDataset), `Provider.pas` (stub)
- `PanelBitmap.pas` and `TreeNodeSubItems.pas` class helpers
- `USE_SYNEDIT` enabled (uses Lazarus SynEdit package)
- `USE_IXMLDBMODELType` disabled (XML model loading uses TXmlParser instead, which works)

## What's Working
1. Full compilation with 2 warnings
2. All .lfm files cleaned for LCL compatibility
3. SynEdit integration enabled
4. Font names use cross-platform alternatives

## Known Runtime Risks (untestable without X11)
1. Some stubs are no-ops (SaveBitmap, Application.OnEvent, custom cursor loading)
2. TPanel.Bitmap usage commented out in EditorQueryDragTarget.pas
3. TTreeNode.SubItems via class helper (uses global dictionary) - untested
4. TSQLConnection shim wraps TSQLConnector - DB connectivity untested
5. LoadApplicationFont writes to Screen.SystemFont - may need adjustment

## What Remains (Priority Order)
1. **Phase 3 forms** - All forms compile but need runtime verification
2. **Phase 2 DB layer** - Shims compile but actual DB ops need real testing/fixing
3. **Phase 1.5 XML** - USE_IXMLDBMODELType disabled, XML loading uses TXmlParser (works)
4. **Phase 4 SynEdit** - Enabled and compiling, may have API diffs at runtime
5. **Phase 5 Plugins** - Compile but untested
6. **Phase 0.4** - Should verify all .lfm files load correctly at runtime

## Remaining 2 Warnings
- `EmbeddedPDF/EmbeddedPdfImages.pas(202)` - ScanLine not portable (expected)
- `EmbeddedPDF/EmbeddedPdfImages.pas(211)` - ScanLine not portable (expected)
