# UI Bug Catalog — DBDesigner Fork (Lazarus port), branch a1

Date: 2026-09-07. Environment: GNOME/Wayland, app running via XWayland (GTK2 LCL widgetset).
All screenshots live in
`/tmp/claude-1001/-home-bpsa-app-dbdesigner-fork/e506f9cb-2d39-419c-bc88-17e433cbec7a/scratchpad/shots/`
(paths below are relative to that directory). Model used: `bin/Examples/order.xml`.

Severity legend: **crash** = exception dialog / hang; **unreadable** = feature unusable or content not visible; **cosmetic** = ugly/clipped but usable.

Summary: 21 entries — 6 crash, 7 unreadable, 8 cosmetic.

---

## Crash / exception

### 1. "New Database Connection" editor raises `Index Out of range Cell[Col=0 Row=5]` and never opens
- **Severity:** crash (exception dialog, then a second `List index (-1) out of bounds`, editor never shows)
- **Repro:** Database > Connect to Database > click "New Database Connection" (bottom-left button). Same path from Database Synchronisation / Reverse Engineering.
- **Screenshots:** `10-db-conn-editor.png`, `10c-err2-top.png`
- **Symptom:** Error box "Index Out of range Cell[Col=0 Row=5]"; after OK a second box "List index (-1) out of bounds"; the connection editor form is never displayed.
- **Suspected files:** `src/DBConnEditor.pas` lines 127-135 (`ParamStrGrid.Cells[0,5]`, `Cells[0,6]` in `FormCreate`), `src/DBConnEditor.lfm` (`ParamStrGrid` has no `RowCount`). LCL `TStringGrid` defaults to `RowCount=5` and raises on out-of-range cells, whereas Delphi/CLX silently grew the sparse cell storage. Because `FormCreate` aborts, `DatabaseTypesCBox` is never filled, hence the follow-up `ItemIndex=-1` error in `SetData`.
- **Complexity:** small (set `RowCount := 7` before filling; also check other `TStringGrid.Cells[]` writers, e.g. `SetData` line 430 already sets RowCount).

### 2. All plugins throw `[TCustomForm.SetFocus] MainForm:TMainForm Can not focus` at startup; table lists stay empty
- **Severity:** crash (exception dialog at plugin start, plugin then shows with no model data)
- **Repro:** Plugins > Demo (or HTMLReport). Error box appears; after OK the plugin window shows an empty "Tables" list.
- **Screenshots:** `13-plugin-demo-14680672.png`, `13b-plugin-demo-after-ok-14680622.png`, `13-plugin-htmlreport-16777824.png`, `13b-plugin-htmlreport-after-ok-16777774.png`
- **Symptom:** Exception raised inside `EERModel.LoadFromFile`, which is called from the plugin's `FormCreate` while the main form is not yet visible; `InitControls` (fills the table list) is skipped.
- **Suspected files:** `src/EERModel.pas` lines 4366 and 5043 (`Application.MainForm.SetFocus` guarded only by `Enabled`), also `src/EERDM.pas:317`. Plugin callers: `Plugins/Demo/Main.pas:139`, `Plugins/HTMLReport/Main.pas` (same pattern). LCL raises when `SetFocus` is called on a form that is not visible/has no handle; CLX ignored it.
- **Complexity:** small (guard with `Visible and CanFocus`, or wrap in try/except).

### 3. DataImporter plugin fails to load its form: `Error reading SpecialFieldsLBox.Rows: Unknown property "Rows"`
- **Severity:** crash
- **Repro:** Plugins > DataImporter (or run `bin/DBDplugin_DataImporter <model.xml>` directly).
- **Screenshots:** `14-plugin-DataImporter-14680147.png`
- **Suspected files:** `Plugins/DataImporter/DBImportData.lfm` line ~749 (`object SpecialFieldsLBox: TListBox` contains a CLX-only `Rows = ...` property). Same class of problem as the already-fixed `Font.Weight` / `Masked` properties.
- **Complexity:** small (delete the property line; grep all plugin .lfm files for other CLX-only properties).

### 4. SimpleWebFront plugin fails to load its form: `Error reading CreateBtn.Glyph.Data: Stream read error`
- **Severity:** crash
- **Repro:** Plugins > SimpleWebFront (or run `bin/DBDplugin_SimpleWebFront <model.xml>` directly).
- **Screenshots:** `13-plugin-simplewebfront-14680106.png`
- **Suspected files:** `Plugins/SimpleWebFront/Main.lfm` line 33 (`CreateBtn: TSpeedButton`, `Glyph.Data` block). The hex blob is a CLX/Qt-format bitmap stream (length header `FA160000`, 74x26 BMP) that LCL's `TBitmap.ReadData` cannot parse. Main.lfm has 18 `Glyph.Data` blocks; probably several are broken.
- **Complexity:** small-medium (regenerate the glyphs from the original `.xfm`/bitmaps or strip them).

### 5. `--selftest` under Xvfb hangs before Phase 0 at 70 % CPU, never writes the log
- **Severity:** crash (hang)
- **Repro:** `xvfb-run -a ./bin/DBDesignerFork --selftest`. After >60 minutes the process is still alive, `/tmp/UITestResults.log` does not exist, stdout stops after the "Form: MainForm (TMainForm)" line, and the Tips dialog is still viewable on the Xvfb display.
- **Screenshots:** `19-selftest-root-crop.png` (Xvfb root capture; note the same black table headers as entry 7)
- **Suspected files:** `src/UITestRunner.pas` lines 386-415 (`Phase0_CloseStartupDialogs` closes the modal Tips form with `.Close` then `ProcessMessages`/`Sleep`), `src/Main.pas:562` (self-test is scheduled "after full initialization" — possibly from inside the Tips form's modal loop). Hypothesis: closing the modal Tips form from inside its own `ShowModal` loop, or the settings-driven "Reopen Last File" that loaded order.xml, re-enters a message loop and spins. Note that this environment has persisted settings (`~/.DBDesigner4/DBDesignerFork_Settings.ini`: `WorkMode=2`, reopen last file on), which the notes' "63 PASS" run did not.
- **Complexity:** medium (needs a debugger/backtrace; `gdb` is not available here).

### 6. Delete shortcut in Edit menu displays as `Ctrl+Meta+Word('7')`
- **Severity:** crash-adjacent: the Delete key does not map to "Delete selected Object(s)" (listed here because the shortcut is functionally wrong, not just cosmetic)
- **Repro:** Open Edit menu.
- **Screenshots:** `03-menus-1.png`
- **Suspected files:** `src/Main.lfm` line 3235 `ShortCut = 20487`. 20487 = $5007 = Qt `Key_Delete` ($1007) | Ctrl modifier from the CLX form; LCL interprets it as Ctrl+Meta+char 7. Should be `ShortCut = 46` (VK_DELETE). Other CLX shortcut constants in the .lfm files (`grep -n "ShortCut = " src/*.lfm`) should be audited; `16461` (Ctrl+M) is fine.
- **Complexity:** small

---

## Unreadable

### 7. Table title bars are solid black with no table name after a model is loaded
- **Status:** FIXED. Cause: in `TEERTable.PaintCachedImg`/`PaintObj2Canvas` the locals `width`/`height` were used inside `with theCanvas do`, where LCL resolves them to `TCanvas.Width`/`Height`; these are 0 while the fresh cached bitmap has no handle, so the header rectangle was empty and never painted (renamed to `tblWidth`/`tblHeight`). Not a bitmap-loading problem.
- **Severity:** unreadable
- **Repro:** Launch `./DBDesignerFork Examples/order.xml`, close Tips. Every table header is a black box. Reproduces with fresh settings (design mode) and with the persisted query mode. Switching Display > Query Mode then Display > Design Mode (any `SetWorkMode` call) repaints the headers correctly; after that they stay correct in both modes.
- **Screenshots:** `02c-tables.png` (broken), `33a-fresh-tables.png` (broken, fresh settings), `24-header-compare.png` (correct after mode switch)
- **Suspected files:** `src/EERModel.pas` `TEERTable.PaintCachedImg` (lines 8138-8200: `StretchDraw(..., ParentEERModel.TblHeaderBmp)`), header bitmap loading in the `TEERModel` constructor (lines 1957-1985) versus `RefreshTblImgs` (line 1828). Bitmaps exist in `bin/Gfx/Table/Header_*.bmp`. Hypothesis: the header `TBitmap` loaded in the constructor is drawn before it has a valid GTK2 pixmap/handle (or is treated as masked with a black transparent colour) so the first cached image is black; `SetWorkMode` triggers a reload/recache that works. Check whether `SetWorkMode` calls `RefreshTblImgs`/`ClearCachedImg` and reproduce by calling that after `LoadFromFile`.
- **Complexity:** medium

### 8. Navigator thumbnail and Page Setup preview draw huge, overlapping text instead of a scaled model
- **Severity:** unreadable
- **Repro:** Look at the Navigator palette (top right) with a model loaded; File > Page & Printer Setup shows the same in its page preview.
- **Screenshots:** `02b-navigator.png`, `04-page-setup.png`
- **Symptom:** A few column names ("productgroup (FK)", "creditcard...") painted at ~100 % font size over a thumbnail that is ~5 % scale; the rest of the model is invisible.
- **Suspected files:** `src/EERModel.pas` `PaintModel` (line 5111, sets `theCanvas.Font.Name` but each object's `PaintObj2Canvas` does `Font.Height := ParentEERModel.GetFontHeight` — lines 8222, 11254, 11344, 11398, 12371, 13038), `GetFontHeight` (line 1891: `Round(EvalZoomFac(12)*(72/DPI))`), `PaintModel` temporarily sets `DPI := theDPI` (line 5125). Hypothesis: for the preview canvas the font height is computed from the model's own zoom factor rather than the preview zoom (or `EvalZoomFac` uses the live `ZoomFac` while text positions use `theZoomfac`), and LCL treats a positive `Font.Height` differently from CLX. Callers: `src/PaletteNav.pas:252` and `:555`, `src/EERPageSetup.pas` preview.
- **Complexity:** medium
- **Status:** FIXED. Cause: in `TEERTable.PaintCachedImg` three `if(Not(DMEER.DisableTextOutput))then` guards were followed by two statements (`Brush.Style := bsClear; TextOut(...)`) without `begin/end`, so the column/index-name `TextOut` calls always ran - with the canvas default font, since `Font.Height` is only set when text output is enabled - even when `PaintModel(..., doDrawText=False)` was used by the Navigator and Page Setup previews. Wrapped them in `begin/end`.

### 9. Notes are rendered as a 1-pixel line (height collapsed)
- **Severity:** unreadable
- **Repro:** Load order.xml; the two notes ("This region contains all system tables", "The onlineorder n:m table") are not visible; at their positions only a thin horizontal line is drawn. Double-clicking there does not open the Note editor.
- **Screenshots:** `25-note-collapsed.png`, `23-main-after-edits-half.png` (thin lines at ~(400,130) and ~(175,240) in half-size coords)
- **Suspected files:** `src/EERModel.pas` `TEERNote.RefreshObj` (line 12404: `Obj_H := ReEvalZoomFac(theSize.cy)+4-14`) and `TEERModel.GetTextExtent` (line 1902) which measures on `SelectionRect.Canvas` — `SelectionRect` is an invisible `TPaintBox` (line 1274) whose canvas has no handle under LCL, so `TextExtent` returns 0 and the note height becomes negative. Also `Font.Height` assignments there.
- **Complexity:** small-medium (measure on a bitmap canvas or the model's own canvas).

### 10. Font combo boxes are empty / show the literal "FontCBox"
- **Severity:** unreadable (cannot pick the model font or the SQL font)
- **Repro:** Options > Model Options > General Options ("Default Font" empty); Options > DBDesigner Options > Database Options ("Font for SQL Text" shows "FontCBox").
- **Screenshots:** `11-model-options.png`, `12-dbd-options-pages.png`
- **Suspected files:** `src/OptionsModel.pas:185` and `src/Options.pas:245` (`FontCBox.Items.Assign(Screen.Fonts)`). `Screen.Fonts` is apparently empty on this LCL/GTK2 build, so `IndexOf` returns -1 and the design-time `Text` remains. The Visual Options page shows the current font ("Nimbus Sans L, 8") via a different path, which works.
- **Complexity:** small (verify `Screen.Fonts` under GTK2; fall back to fontconfig list or `FontDialog`).

### 11. Table Editor column grid: header captions clipped, no key/NN/AI/flag icons
- **Severity:** unreadable (column properties NN/AI/flags cannot be seen; "Column Name" shows as "umn Name", "DataType" as "aType")
- **Repro:** Double-click any table (or a DB Model tree entry) to open the Table Editor.
- **Screenshots:** `16-table-editor.png`, `16a-table-grid-header.png`
- **Suspected files:** `src/EditorTable.pas` `ColumnGridDrawCell` (lines 698-850): header text is drawn at `Rect.Left+1-18` (lines 845/847, relying on CLX clipping/offset behaviour), and every icon/checkbox comes from `DatatypesImgList.Draw(...)` (lines 735-807) which draws nothing. Same image-list problem is visible in the DB-connection tree (entry 15) and the Indices "column" list is empty. Check `src/EditorTable.lfm:968` (`DatatypesImgList` bitmap stream is a CLX-format `Bitmap = {` blob that LCL may load as empty).
- **Complexity:** medium (regenerate image lists in LCL format; fix header x offsets).

### 12. Datatype Editor "Datatype Name" field shows `ssssssssssssssssINTEGER`
- **Severity:** unreadable
- **Repro:** Double-click "INTEGER" (or "VARCHAR") in the Datatypes palette.
- **Screenshots:** `26-datatype-editor.png`, `29-datatype-editor-varchar.png`
- **Symptom:** The edit is filled with many `s` characters followed by the real name (scrolled to the end). Group combo and description are correct, so `Datatype.TypeName` itself is fine.
- **Suspected files:** `src/EditorDatatype.pas:128` (`DatatypeNameEd.Text := Datatype.TypeName`), `src/EditorDatatype.lfm` (`DatatypeNameEd: TEdit`, no `Text` property), `src/MainDM.pas` `InitForm`/`TranslateForm` (line 1168+) which runs first. No `'s'` literal exists in the sources; hypothesis is a string/PChar length mix-up in the translation pass or a shim (`src/clx_shims`) that pads/masks `TEdit` text. Needs a debug print in `SetDataType`.
- **Complexity:** small once located (unknown cause).

### 13. Navigator palette "Info" tab cannot be activated
- **Severity:** unreadable (Info page unreachable)
- **Repro:** Click the "Info" tab in the Navigator & Info palette.
- **Screenshots:** `27c-navigator-info-tab.png`
- **Suspected files:** `src/PaletteNav.pas:689` (`InfoPBoxClick` switches `PageControl.ActivePage:=InfoSheet` and z-orders `TabsImg`/`Tabs2Img`), `src/PaletteNav.lfm` (`InfoPBox: TPaintBox` at 76,4 51x13 overlapping the tab `TImage`s). Hypothesis: under LCL the `TImage` receives the click instead of the transparent `TPaintBox`, or `BringToFront` ordering differs, so the handler never fires. Low confidence — could also be my click position, but three attempts inside the InfoPBox rect failed.
- **Complexity:** small

---

## Cosmetic

### 14. Relation-name labels on the canvas are clipped ("CreditCardRe", "CustOrderRe", "OnlineorderRe")
- **Severity:** cosmetic
- **Repro:** Load order.xml, Display Relation Names on (default).
- **Screenshots:** `02c-tables.png`, `24-header-compare.png`
- **Suspected files:** `src/EERModel.pas` `TEERRel` caption sizing (uses `GetTextExtent`, see entry 9) — the label box is sized with one font metric and drawn with another (`Font.Height` positive value interpreted differently by LCL). Likely the same root cause as entries 8/9 (font height / text measurement).
- **Complexity:** small once entry 9 is fixed.

### 15. Tree views and lists show no icons (DB-connection tree, DB Model palette, Datatypes "All types")
- **Severity:** cosmetic
- **Repro:** Database > Connect to Database (tree left has blank icon slots); DB Model palette tree.
- **Screenshots:** `09-db-connect.png`, `02d-dbmodel.png`
- **Suspected files:** `TImageList` `Bitmap = {` streams in `src/DBConnSelect.lfm`, `src/Main.lfm`, `src/PaletteModel.lfm`, `src/EditorTable.lfm` (CLX-format image-list data). Shared cause with entry 11. The Datatypes palette *does* show icons (`33-fresh-main-half.png`), so compare how that list is built.
- **Complexity:** medium (regenerate image lists).

### 16. DBDesigner Options dialog: right-hand "Various" group is cut off, "Reset Personal Settings" button text clipped, check-box captions truncated
- **Severity:** cosmetic (some options partly unreadable: "Limit the number of Undo Actions to", "...when Application loses the Foc", "Enclose names by quote charact")
- **Repro:** Options > DBDesigner Options, pages General and Database.
- **Screenshots:** `12-dbd-options.png`, `12-dbd-options-pages.png`
- **Suspected files:** `src/Options.lfm` — `Various` group at Left 242 Width 313 inside a `PageControl` of Width 535 (overflows by 20 px even at design size); check-box widths (e.g. `LimitUndoCBox` Width 199) are sized for the CLX 11-px font while LCL renders the form font `Sans -11` larger. Form-level `Font.Height = -11`/`Font.Name = 'Sans'` in most .lfm files.
- **Complexity:** small-medium (widen controls / use `AutoSize`, or enlarge forms).

### 17. Model Options > Database Options: "Table Prefixes" label overlapped by the datatype combo
- **Severity:** cosmetic
- **Repro:** Options > Model Options > Database Options.
- **Screenshots:** `11-model-options-pages.png` (middle panel, "ble Prefixes:")
- **Suspected files:** `src/OptionsModel.lfm` (label/combo positions on the Database page; same font-metric cause as 16).
- **Complexity:** small

### 18. Table Editor "Advanced" page: "Use Table RAID" check box overlaps "RAID Type:" label; combo shows "STRI"; "kB" clipped. Datatype Editor: "Edit values as strings" check box cut off, "Synonymgrp." label clipped
- **Severity:** cosmetic
- **Screenshots:** `16-table-editor-pages.png` (Advanced), `26-datatype-editor.png`
- **Suspected files:** `src/EditorTable.lfm` (RAID group box), `src/EditorDatatype.lfm` (Parameter group height, label widths). Same font-metric cause as 16.
- **Complexity:** small

### 19. Visual Options "Header Preview" is a blank grey box
- **Severity:** cosmetic
- **Repro:** Options > DBDesigner Options > Visual Options.
- **Screenshots:** `12-dbd-options-pages.png` (top panel)
- **Suspected files:** `src/Options.pas` header-preview paint (draws `Header_<style>.bmp` like `TEERTable.PaintCachedImg`). Shares the header-bitmap drawing problem of entry 7.
- **Note (after fixing 7):** does NOT share the cause of 7. `TblHeaderBGPnl.Bitmap` is served by the `src/clx_shims/panelbitmap.pas` class helper, which only stores the bitmap in a hash list; nothing paints it onto the panel (CLX's `TPanel.Bitmap` was drawn as the panel background). Needs an `OnPaint`/custom-draw in the shim or in `Options.pas`.
- **Complexity:** small once 7 is fixed.

### 20. Windows menu lists the loaded model as "Noname1"; main window title lacks the file name
- **Severity:** cosmetic
- **Repro:** Load order.xml, open the Windows menu.
- **Screenshots:** `03-menus-2.png`
- **Suspected files:** `src/Main.pas:775` (menu item caption set at form creation from `GetModelName`), `src/EER.pas:386-387` (caption update after load/save is commented out: `{Caption:='DB Model | '+...; theFormMenuItem.Caption:=EERModel.GetModelName;}`), MDI replaced by `fsNormal` so the caption update path was dropped.
- **Complexity:** small

### 21. Minor: duplicate "order.xml" entry in File > Open Recent; GLib-CRITICAL `spacing -1` and Pango "Invalid UTF-8 string" warnings on stderr
- **Severity:** cosmetic
- **Repro:** File > Open Recent after opening the same file via relative and absolute path (duplicate); open Options > DBDesigner Options (Pango warning); startup (GLib spacing warning).
- **Screenshots:** `05-file-submenus.png`
- **Suspected files:** `src/GUIDM.pas:380` (`RecentFiles.IndexOf(fname)` — compare `ExpandFileName`d paths); Pango warning comes from a non-UTF-8 caption/string set while building the Options dialog (`src/Options.pas`, translation strings from `bin/Data/DBDesignerFork_Translations.ini` — Latin-1 text assigned to a GTK2 label); GLib `spacing=-1` originates from an LCL toolbar/panel property in `src/Main.lfm`.
- **Complexity:** small

---

## Things that looked fine
Tips dialog, all top-level menus (apart from the shortcut text), Export SQL Script dialog (disabled check boxes in the Advanced group draw without a box, harmless), Select Database Connection dialog layout, Relation Editor, Region Editor, Standard Inserts/Comments pages, Datatypes palette (with icons), notation switching (EER/Traditional), all 17 design-tool speed buttons and the 11 query-tool buttons (no exceptions on stderr when clicked), Windows > Cascade/Tile, mode toggle.

## Not yet tested (and why)
- File > Open / Add-Link / Export as Image / Export MDB XML / Import ERwin (native GTK file dialogs; ERwin import is disabled in the menu).
- Real database connect, Reverse Engineering and Database Synchronisation beyond the connection-select dialog (no DB server; entry 1 blocks creating a connection anyway).
- Database > Run UI Tests from the menu (would hit the hang in entry 5).
- Note editor and Image editor (notes are collapsed, entry 9; double-clicking the two images at the top-left did not open an editor within the time budget — untested, not confirmed as a bug).
- Query mode SQL panes with actual content, drag/drop of columns into the SQL builder (`src/EditorQuery.pas`, `EditorQueryDragTarget.pas` has a commented-out `TPanel.Bitmap`).
- Table Editor "Table Options" page (tree click collapsed the node instead of selecting it), inplace datatype editor, column drag/drop.
- Windows > Style submenu items, Help links (open a browser), Print, Save, Exit.
- Palette undocking (Windows > Dock Palettes / the palette items are disabled while docked), Reset Palette Positions.
- DPI/HiDPI and dark-theme rendering.
- The Xvfb self-test log could not be inspected because it never got written (entry 5); `gdb` is not installed, so no backtrace.
