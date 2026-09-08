# Model editing bug catalog (tables, columns, relations)

Date: 2026-09-08. Branch: a3. Status: final (30-minute exploratory session on the real display, DISPLAY=:0 / XWayland; see Coverage). Fixed so far: #5 (8208145), #1, #6, #8 (670864e).

Scope: creating/editing/deleting tables, columns and relations in the model editor, the Table Editor, Relation Editor, persistence (save/reload) and the generated SQL (MySQL / SQLite). Earlier catalogs (ui-bug-catalog.md, db-ui-bug-catalog.md, sqlite-/mysql-bug-catalog.md) are not repeated; regressions are marked as such. Screenshots: `<scratchpad>/shots/model-edit/`.

## Entries

### 1. Table Editor, Indices page: the column list of the selected index is empty (PRIMARY shows no `idproductgroup`)

Severity: functional (minor - display only, the index itself is intact in the model).
Steps: open order.xml; double-click the `productgroup` table header; the Indices page is shown with `PRIMARY` selected in the index list.
Observed: the right-hand "index columns" list box is empty although the PRIMARY index contains `idproductgroup` (`08-tableeditor.png`). Same after adding/renaming (`10-newcol-dt.png`). No stderr output.
Expected: the columns of the selected index listed (as in the original DBDesigner 4), so that columns can be removed with the eraser button.
Suspected cause: the index-columns list is filled in the index-list `OnClick`/`RefreshIndexColumns` path of `src/EditorTable.pas`; the LCL `TListBox` under GTK2 is probably filled before the page/handle exists or the selection-change event that triggers the fill does not fire on programmatic `ItemIndex` (compare db-ui #19). Fix scope: small.
Status: fixed in 670864e - the list was filled (probe: 1 item) but painted white on white: `ShowIndex` set `IndexColListBox.Color:=clWindow`, which the GTK2 LCL applies to the NORMAL/ACTIVE/PRELIGHT base of the tree view, and GTK2 draws the pre-selected row of an *unfocused* list in the ACTIVE state (white text) - a click into the list (SELECTED, orange) made it appear. Now `clDefault`/`clBtnFace` (theme colours). Adding a column via the grid popup "Add Column(s) to Selected Index", changing the kind to UNIQUE INDEX (mouse pick in the dropdown), removing it with the eraser and OK + reopen all verified (`fix01-06-08/20-30`). See notes-to-myself.md "Fix: model-edit #1, #6, #8".

### 2. Canvas keeps a stale horizontal line after a table is renamed/resized through the Table Editor

Severity: cosmetic.
Steps: open order.xml; double-click `productgroup`; change the Table Name to `productgroup_renamed` (table becomes wider); OK.
Observed: the table, the DB Model tree and the `ProductgroupRel` line update correctly, but a light grey 1-px horizontal line remains across the whole `OnlineStore` region at canvas y~503 (from the region's left edge to its right edge, `12-main-after.png`). It is not part of any object and disappears on a full repaint.
Expected: no leftover pixels after the editor closes.
Suspected cause: the invalidate rectangle after `EditorTable` closes (`TEERTable.RefreshObj`/`DoPaint` in `src/EERModel.pas`) covers the old table bounds only, while the region background is redrawn with an off-by-one row (or the selection frame of the table is cleared with the region's colour minus one line). Fix scope: small.

Status: fixed in 3d2c6f3 - not an invalidation problem: the white 190-px line at y=502 is the `RelEnd` paintbox of the *splitted* `forumpost` relation (onlinecustomer -> forumpost). `TEERRel.PaintObj2Canvas_RelStart/_RelMiddle/_RelEnd` read `width`/`height` inside `with RelEnd do with theCanvas do`, which the LCL resolves to `TCanvas.Width/Height` (GDK drawable size 3072, or 0 before the handle exists). `TEERRel.DoPaint` paints RelEnd through `RelEnd.Canvas`: the first `width` read gives 0 (`w:=-1`), `MoveTo` allocates the handle, `LineTo(xo+width-1-w)` then draws the white "clear" pass from -1 to 3072 clipped to the control, and the black dash-dot-dot pass lands off-clip. It shows after the Table Editor OK because that path repaints directly outside an expose. The painters now capture the control size first; the splitted stubs and end icons appear too. Verified with two renames and a table drag (`fix02-04-07/30-fixed1/2`, `31-moved.png`). See notes-to-myself.md "Fix: model-edit #2, #4, #7".

### 3. Return in the Table Editor's "Table Name" edit closes the dialog (acts as OK)

Severity: cosmetic / usability (minor).
Steps: Table Editor open; click into Table Name, type a name, press Return.
Observed: the dialog closes and the change is applied (`11-rename.png` was taken before Return, `12-main-after.png` after). Tab from Table Name goes to the Table Prefix combo, where typed text is ignored silently.
Expected: original behaviour is the same (OK is the default button), so this is only listed for completeness - not a defect unless the caller wants Return to move to the grid. Fix scope: n/a.

### 4. Relation Editor: Foreign Keys grid clips the source column name and draws the "Comment" header cell black

Severity: cosmetic (minor).
Steps: order.xml; double-click the `CartRel` relation label.
Observed: the `Source Column` cell shows `idonlinecustome` (last letter clipped, column too narrow for the text), and the `Comment` header cell has a black background band on its right/top edge (`13-releditor.png`). Name, kind (1:n), visibility, source/destination tables, FK mapping, Reference Definition (RESTRICT/RESTRICT) are all shown correctly.
Expected: columns wide enough for the longest name (or auto-sized), uniform header background.
Suspected cause: fixed `ColWidths` in `src/EditorRelation.lfm` sized for the Windows font; header drawn in `OnDrawCell` of `src/EditorRelation.pas` with a partly uninitialised brush. Fix scope: small.

Status: fixed in 3d2c6f3 - there is no `OnDrawCell`; the black band is the LCL `tsLazarus` title bevel (`cl3DDKShadow` right/bottom edge of every fixed cell, most visible on the last column). `FKGrid` is now `Flat` with silver fixed/border lines and `SetRelation` runs `AutoAdjustColumns` (old widths as minimums, Comment column fills the rest): `idonlinecustomer` is complete (`fix02-04-07/43-releditor-final.png`).

### 5. Objects created in the running session (tables from the Table tool, relations from the relation tools) cannot be selected or edited with the mouse; objects loaded from the file can

Severity: functional (major for model editing - a table created with the tool cannot be opened in the Table Editor, a new relation cannot be opened in the Relation Editor or deleted).
Steps: order.xml; click the Table tool (palette, 10th button); click on empty canvas at ~(958,180) and again at ~(958,430) - `Table_15` and `Table_16` appear and are added to the DB Model tree (`16-crop.png`); select the Pointer tool; double-click the `Table_15` header (3 attempts, mouse moved onto the header first, `xdotool click --repeat 2`). Then: 1:n relation tool (12th button); click `News`, click `Employee` - `Rel_12` is drawn and `FKidNewsCol (FK)` is added to `Employee` (`27-crop.png`; the FK name is correct, order.xml has `FKPrefix="FK" FKPostfix="Col"`); Pointer tool; click on the `Rel_12` line and on its label; Edit > Delete selected Object(s) (Ctrl+Del).
Observed: `Table_15`: no selection frame, no Table Editor (`21-crop.png`, `22-crop.png`); the same double-click on `productgroup` immediately afterwards opens its Table Editor (`24-dt.png`). `Rel_12`: the double-click on its label opens the *Region Editor* of the `Forum` region underneath (`31-regioneditor.png`), the click on the line selects nothing and the delete confirmation lists no object (`39-confirms.png`, top; "Yes" changes nothing - 6 px difference between `33-selrel.png` and `35-afterdel.png`). The same probe on the loaded `CartRel` lists `CartRel`, and on `productgroup` lists `productgroup, ProductgroupRel` (`39-confirms.png`). No stderr output.
Expected: new objects behave like loaded ones - selectable, editable by double-click, deletable.
Suspected cause: every EER object is a control with its own `OnMouseDown` (`TEERObj.Create`, `src/EERModel.pas:~7273`; relation parts at ~10349-10428). `TEERModel.NewTable` (`src/EERModel.pas:2626`) and `NewRelation` (`:2670`) call `SendRegionsToBack; theTbl.Show; theTbl.BringToFront` after the model form is already shown; under LCL GTK2 the z-order change is not applied to the GTK layout children (or the new control's widget is realised without the button-press event mask), so the region / the model's `GridPaintBox` (`:1312`) stays above the new control and receives the clicks. Objects created during XML load (before the form is shown) get the right order. Check whether the objects become selectable after Save + reopen (not tested). Fix scope: medium (z-order handling in `TEERObj`/`SendRegionsToBack`, or an explicit hit-test in `TEERModel.DoMouseDown`).
Status: fixed in 8208145 - not reproducible with clicks placed on the objects: with `writeln` probes in the mouse handlers every session-created table/relation control sat at the top of `Parent.Controls` and received its clicks (inside and outside a region, on order.xml and on a File > New model; single click selects, double-click opens the Table/Relation Editor, Ctrl+Del lists and removes the relation plus its FK column). The reported symptoms match clicks that landed beside the very small targets (the News->Employee line is ~13 px long and 1 px wide inside a 14-px control, the label 16 px high): a click on the region background deselects everything (empty Ctrl+Del list, #7) and a double-click there opens the Region Editor. The real defect found on the way: `TEERModel.SendRegionsToBack` set `ComponentIndex`, which is a no-op for LCL z-order, so the guarantee `NewTable`/`NewRelation`/`NewNote` relied on did not exist; it now really sends the regions and the `GridPaintBox` to the back and `LoadFromFile2` applies it after every XML load (paste, undo, plugin import, appended model). See notes-to-myself.md "Fix: model-edit #5".

### 6. Table Editor column grid: Tab inside the in-place Column Name editor does not move to DataType - the next keystrokes are appended to the name

Severity: functional (minor). Regression of the navigation part of db-ui #16 (826a071 says Tab goes Column Name -> DataType).
Steps: Table Editor of `productgroup`; click the Column Name cell of the empty last row; type `abc`; press Tab; type `DECIMAL(10,2)`.
Observed: the cell contains `abcDECIMAL(10,2)` (`24-dt.png`); Tab did nothing visible. Return then commits the name and opens the datatype in-place combo with `INTEGER`; Ctrl+A + `DECIMAL(10,2)` + Return is accepted, the flags column switches to ZEROFILL and the next row's editor opens (`25-dt2.png`); the canvas shows the new column after OK (`26-crop.png`). The first-character doubling itself is fixed (`abc` -> `abc`).
Expected: Tab commits the name and moves to the DataType cell.
Suspected cause: the fix in `ColumnGridKeyDown` (`src/EditorTable.pas`) handles Tab only while the grid itself has the focus; while `EditorTableFieldEdit` (`src/EditorTableField.pas`) is active the Tab is swallowed by that control (GTK2 `TEdit` with no `OnKeyDown` forwarding) - Return is forwarded, Tab is not. Fix scope: small.
Status: fixed in 670864e - VK_TAB did reach `TEditorTableFieldEdit.DoKeyDown` (probe) and was ignored there; the LCL's tab navigation did not move the focus either. Tab/Shift+Tab now commit the cell (`ApplyChanges(goRight/goLeft)`, the constants existed but were unimplemented), consume the key and feed VK_TAB/VK_LEFT into `ColumnGridKeyDown`, so the cursor moves like the grid's own Tab/Left; a new row continues with the datatype editor, and Tab in the datatype editor of the last row opens the next row's name editor (Shift+Tab goes back to Column Name). Verified `abc` + Tab -> datatype editor with `abc` (no doubling), `VARCHAR(10)` + Tab -> next row, `g` + Tab -> DataType cell, Return + Shift+Tab -> Column Name (`fix01-06-08/31-35`, `60-62`).

### 7. Ctrl+Del with nothing selected shows the delete confirmation with an empty object list

Severity: cosmetic (minor).
Steps: click on empty canvas (or on an object that cannot be selected, see #5); press Ctrl+Del.
Observed: "Are you sure you want to delete the selected Objects? The following Objects will be deleted:" with an empty list (`30-confirm.png`, `34-confirm.png`); Yes does nothing.
Expected: nothing happens (the Edit menu item is disabled by `DeleteMIShow` when nothing is selected, but the shortcut bypasses the `OnShow`-time check).
Suspected cause: `TMainForm.DeleteMIClick` (`src/Main.pas:~950-990`) does not test `ObjectList.Count=0` before `MessageDlg`. Fix scope: small.

Status: fixed in 3d2c6f3 - `DeleteMIClick` shows the confirmation only for a non-empty list; click on empty canvas + Ctrl+Del opens nothing.

### 8. Table Editor: after renaming a column in the grid the OK button disappears - the edit cannot be confirmed

Severity: functional.
Steps: double-click `News`; click the `idNews` Column Name cell (twice - the first click after window activation is lost); type `x` (the in-place editor opens and replaces the name); End, `2`, Return.
Observed: the grid shows `x2` (`43-comb.png`, middle), but the green OK check mark at the bottom right is gone - only the red Cancel X is painted (`44-editor-still.png`); two clicks on the OK position do nothing; Cancel closes the dialog. The canvas keeps `idNews` and `Employee.FKidNewsCol` (`46-crop.png`), so PK-rename propagation to FK columns could not be evaluated. Compare `25-dt2.png`: after adding a *new* column (name + datatype) both buttons are present and OK works. Re-check on `productgroup`: renaming the non-key `groupname` (`g` + Return, `50-comb.png`) and the PK `idproductgroup` (`p` + Return, `53-comb.png`, top) both keep the OK button; End inside the grid moves to the Comments cell (`53-comb.png`, bottom). So the trigger is specific to the News sequence - either the End key pressed while the in-place name editor is open, or the collapsed (►) display state of `News`. Reproduced once; needs the exact sequence above.
Expected: OK stays visible; the rename is applied and the FK column of `Employee` is renamed to `FKx2Col` (order.xml has FK prefix/postfix).
Suspected cause: the only code that hides the button is `SubmitBtn.Visible:=False` for read-only / linked tables (`src/EditorTable.pas:463-466`), which does not apply here; the button is most likely covered or destroyed by the in-place `EditorTableFieldEdit` / datatype combo teardown - the sequence "Return in the name editor of an *existing* row" differs from the new-row path (`ColumnGridKeyDown`, `src/EditorTable.pas:~1013`, and the `EditCellStr` callback that ends editing). A `TSpeedButton` under GTK2 that loses its parent panel's repaint after a child control is freed would explain a painted-away *and* unclickable button only if the control is really hidden, so check `SubmitBtn.Visible/Enabled` in the rename path first. Fix scope: small-medium.
Status: fixed in 670864e - the rename was not the trigger: the OK button is already absent when the editor of `News` opens (`fix01-06-08/05-te-news.png`). `News` and `Employee` are *linked objects* in order.xml (`IsLinkedObject="1"`, placed from the `bookshop` model listed in `<LINKEDMODELS>`; blue border, no header bitmap on the canvas) and `src/EditorTable.pas:463-466` hides `SubmitBtn` for linked objects and read-only models while `FormClose` discards the edits - original semantics. `productgroup` kept the button because it is not linked. The editor now shows `Linked object from model "bookshop" - read only, changes cannot be applied.` (new `ReadOnlyLbl`), sets the button state explicitly and refuses the in-place edits, toggles, insert/delete and index changes for such tables (`40-41`). PK-rename propagation, verified on `productgroup` -> `product` instead (News/Employee cannot be edited): `ApplyChanges` ran `CheckAllRelations` before the relations rebuilt their PK -> FK mapping, so `product` lost `idproductgroup (FK)` and got no replacement until the next check (`50-52`); `SourceEERTable.RefreshRelations` now runs first and `pg` + Return + OK yields `product.FKpgCol (FK)` immediately with the canvas repainted (`53-canvas-after-fix.png`).

## Not bugs / could not reproduce

- Double-click on a table header opens the Table Editor and double-click on a relation label opens the Relation Editor; both close with the OK/Cancel speed buttons (ui-bug #22 fix holds).
- Renaming a table through the Table Editor: canvas text, table width, the attached relation line and the DB Model palette tree all update (`12-main-after.png`) - apart from the stray line in #2.
- The Table Editor grid shows key/NN/AI icons and the datatype icons (ui-bug #11 fix holds); the first-character doubling (db-ui #16) could not be re-tested because the click into the new-column row was lost (see below).
- File menu keyboard navigation (`alt+f`, Down x7, Return) landed on "Close" instead of "Save As" and left dead override-redirect popup windows that swallowed all further input (`06.png`, `07-newtables.png` show the resulting empty main window). This is an xdotool/GTK2 menu driving problem (submenus open on Down), not an application bug; the app had to be restarted.
- FK columns created by a new relation are named `FK<pk>Col` - correct, order.xml carries `FKPrefix="FK" FKPostfix="Col"`.
- Delete confirmation for a table lists the table and its relations (`productgroup, ProductgroupRel`) - correct cascade.
- No exceptions, GLib criticals or Pango warnings on stderr during the session (only the canberra-gtk-module message).

## Coverage

Exercised (real display, `<scratchpad>/shots/model-edit/`): opening order.xml; Table Editor of `productgroup` (Columns grid, Indices page, Table Name edit, rename + OK, canvas repaint, DB Model tree refresh, new column via typing, in-place datatype editor with DECIMAL(10,2), canvas after OK); creating two tables with the Table tool (canvas + tree refresh); Relation Editor of `CartRel` (all fields inspected, Cancel); creating a 1:n relation News -> Employee with the tool (line, label, FK column); selection/delete probing of `Rel_12`, `CartRel`, `productgroup` via the delete confirmation (Escape); Table Editor of `News`: rename of the PK column in the grid (blocked by #8).

Not reached in the 30-minute limit (part of the budget was lost to a mis-routed click that switched the main window into Query mode and to stuck File-menu popups): in-place rename, move/resize, copy/paste/delete/undo, table options pages (Table Options, Advanced, Standard Inserts, Comments), prefixes/colours, collapse/expand; datatype palette drag, ENUM parameters, NN/AI/PK toggles, default values, reordering, deleting columns that take part in relations/indices, Column Parameters dialog, creating/deleting indices; non-identifying / n:m / 1:1 / self relations, changing relation kind, FK propagation on PK changes (blocked by #8), deleting relations and tables with relations, line dragging; Save/Save As/reload persistence; SQL Create Script export (MySQL/SQLite); Navigator/zoom/context menus.

Recommended next round: start the app, immediately do File > Save As with the mouse (File menu popup is at +42+95, 212x399; "Save As ..." is the 9th row including separators) and never use keyboard menu navigation; the main-window client origin is +42+69 when the window is moved to 0,0 (the mode-toggle button sits at client (18,49), any click there flips to Query mode).

## Pass 2 entries

Date: 2026-09-08, second 30-minute pass (real display). Screenshots: `<scratchpad>/shots/model-edit/pass2/`. Model: order.xml saved as `<scratchpad>/p2.xml` right after opening.

### 9. Edit menu: Copy / Cut / Paste / Select All Objects / Center Model are permanently disabled; Ctrl+C / Ctrl+V do nothing

Severity: functional (copy/paste of tables is impossible; "Select All" and "Center Model" unreachable).
Steps: open order.xml; click the `productgroup` header (selected, dashed frame); Edit menu.
Observed: `Copy selected Object(s)`, `Copy ... as Image`, `Cut`, `Paste`, `Select All Objects`, `Center Model` are greyed with a table selected (`16-editmenu-sel.png`) and without (`13-editmenu.png`); only Undo/Redo/Delete are enabled. Ctrl+C then Ctrl+V with `productgroup` selected changes nothing on the canvas (`15-paste.png`). Undo via Ctrl+Z and Redo via the menu work for a table move (`10-12-comb.png`, `14-redo-menu.png`).
Expected: the items enabled while a model is active (Copy/Cut with a selection, Paste with clipboard content), as in DBDesigner 4 where the Edit menu is refreshed on open.
Suspected cause: `src/Main.lfm:3203-3253` declare the items with `Enabled = False` and nothing in `src/` ever sets `CopyMI/CutMI/PasteMI/SelectAllMI/CenterModelMI.Enabled` (grep `\.(Enabled|Visible)` over `src/` has no hit for them); the Delphi original enabled them in an `OnClick` of the top-level `EditMI` (the LCL `EditMI` has no handler, `src/Main.lfm:3183`). The handlers `CopyMIClick`/`PasteMIClick` exist (`src/Main.pas:1298`, `:1340`-ish). No shortcuts are declared for Copy/Paste/Undo/Redo either (`ShortCut` only on DeleteMI 16430 = Ctrl+Del and one more), so Ctrl+C/V go nowhere; Ctrl+Z is handled elsewhere (works).
Suggested fix scope: small - add an `EditMI.OnClick` (or `OnMenuShow`-like refresh in `DeleteMIShow`'s place) that enables the items from `GetSelectedObjsCount>0` / clipboard state / `FActiveEERForm<>nil`, and give Copy/Cut/Paste/Undo/Redo their standard shortcuts.

### 10. Table Editor OK after adding a second PK column: the table's bottom row is clipped on the canvas (height not recomputed)

Severity: cosmetic.
Steps: double-click `product`; click the key cell of `name` (becomes PK, NN, AI toggled via the flag cells); Comments page; OK.
Observed: `name` moves under `idproduct` and the children get `FKnameCol (FK)` (correct, `29-after-ok.png`), but the `product` table is now cut off at the bottom: `pic` is half visible and the bottom border is gone (compare `01-main.png`). The PK separator line is drawn below the two key columns.
Expected: the table control grows to hold all rows.
Suspected cause: `TEERTable.RefreshObj`/`PaintObj` height calculation in `src/EERModel.pas` does not add the extra pixels for the PK/non-PK separator when the PK count changes, or the control's `Height` is set before the columns are re-sorted (`ApplyChanges` in `src/EditorTable.pas` sorts PK columns first after the size was computed). Fix scope: small.

### 11. n:m relation tool: the generated join table is dropped on top of existing tables

Severity: cosmetic (minor).
Steps: n:m tool (palette centre y=360); click `productgroup`, click `creditcard`.
Observed: `productgroup_has_creditcard` with `FKidproductgroupCol (FK)` / `FKidcreditcardCol (FK)` and `Rel_13`/`Rel_14` are created correctly, but the table is placed at the midpoint of the two parents, over `carthasproduct` and the "Stores all products..." note (`54-nm.png`). Its columns are not marked as PK (no key icon) although an n:m join table normally gets a composite PK of the two FKs.
Expected: a free spot near the midpoint, FK columns as PK (check `TEERModel.NewRelation`/`nmTable` creation in `src/EERModel.pas`, search `_has_`).
Suggested fix scope: small (position search); the PK question needs comparison with DBDesigner 4 semantics.

### 12. Table Editor, Table Options page: the "Row format" combo is too narrow ("defau" clipped)

Severity: cosmetic (minor).
Steps: Table Editor of `product`; tree node Table Options.
Observed: the combo at the bottom right of "How Settings" shows `defau` (`25-tableopts.png`); the Password field shows the password in clear text (`theproducts...`). All other fields readable.
Expected: combo wide enough for "default"/"dynamic"/"fixed"/"compressed".
Suspected cause: fixed `Width` in `src/EditorTable.lfm` (`RowFormatCBox` or similar) sized for the Windows font. Fix scope: trivial.

## Pass 2 not bugs / verified OK

- Table move by dragging the header, Ctrl+Z undo and Redo (menu) restore the position (`10-12-comb.png`, `14-redo-menu.png`).
- Deleting a table with a relation (`productgroup`): confirmation lists `productgroup, ProductgroupRel`, the relation and the child FK column `product.idproductgroup (FK)` are removed and the DB Model tree is refreshed (`20-21.png`); Ctrl+Z restores table, relation and FK column (`22-undo-del.png`).
- Table Editor flag cells: clicking NN / AI / the key cell of `name` toggles them (`24-27.png`); OK propagates the new second PK column to both children (`carthasproduct.FKnameCol`, `onlineorderhasproduct.FKnameCol`, `29-after-ok.png`) and the columns persist in the saved XML (`PrimaryKey="1" NotNull="1" AutoInc="1"`, two `FKnameCol`).
- Table Options / Advanced / Standard Inserts / Comments pages display the stored values (Next Auto-Increment 100, Checksum, Chunks 2/64 kB). Typing into the Comments memo works when the memo is really clicked (client y~380 of the 491-px dialog) and the comment is saved (`Comments="cmt p2"` in p2.xml after File > Save).
- Double-click on an already selected table opens its Table Editor.
- Ctrl+Del on a selected table opens the confirmation (needs the window to be activated first; the first click after activation is lost - driver issue).
- No exceptions or GLib criticals on stderr in the whole session (`stderr.log`: only the canberra message).
- Driving note (not an app bug): a Table Editor whose Cancel click was lost stays open as a modal and silently swallows all clicks on the palette and canvas for the rest of the session - always `waitgone` the dialog and re-check `xdotool search --name "Table Editor"` before reporting "tool does not respond". Palette centres: Table y=293, 1:n identifying 315, 1:1 non-id 338, n:m 360, 1:1 390, 1:n 413... (lower group 390/413/435), Pointer 80; File > Save is the 7th popup row (screen 92,243 with the window at 0,0).

## Pass 2 coverage

Exercised (`shots/model-edit/pass2/`): Save As into the scratchpad; table drag/undo/redo; Edit menu state with and without selection (#9); Ctrl+C/V; delete table with relation + undo; Table Editor of `product`/`productgroup`: NN/AI/PK toggles, second PK column propagation (#10), Table Options/Advanced/Comments pages (#12), comment persistence via File > Save; n:m relation creation (#11).

Not reached in 30 minutes (about 8 minutes were lost to a Table Editor left open by a lost Cancel click and to wrong palette coordinates): in-place rename, resize, table prefix/colour options, collapse/expand, table context menu; datatype palette drag, ENUM/SET parameters, VARCHAR length change, default values, column reorder, deleting a column that is in an index / relation, duplicate names, Column Parameters dialog, creating/deleting a two-column index; 1:1 and self relations (one attempt of a self 1:n relation on `productgroup` created nothing, `55-self.png` - unconfirmed, the second click may have been lost), Relation Editor kind change and ON DELETE/UPDATE options, deleting a relation, dragging line segments/labels, splitted display; persistence of the n:m join table (the last File > Save click did not register twice, p2.xml timestamp 14:05 - unverified), reload of p2.xml, SQL Create Script export (MySQL/SQLite); Navigator repaint/zoom, DB Model tree after n:m, context menus, Datatypes palette. Only opened at the end: File > Export > SQL Create Script dialog (`62-sqlexport-*.png`, target SQLite preselected, the SQLite-specific option labels are greyed with grey check marks - not evaluated) on a fresh restart that reopened bin/Examples/order.xml (not p2.xml), so the DDL of the edited model was not generated. WorkMode=1 and git status (only this file) confirmed; no stderr output in the second session either.
