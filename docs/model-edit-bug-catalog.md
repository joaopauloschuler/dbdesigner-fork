# Model editing bug catalog (tables, columns, relations)

Date: 2026-09-08. Branch: a3. Status: final (30-minute exploratory session on the real display, DISPLAY=:0 / XWayland; see Coverage).

Scope: creating/editing/deleting tables, columns and relations in the model editor, the Table Editor, Relation Editor, persistence (save/reload) and the generated SQL (MySQL / SQLite). Earlier catalogs (ui-bug-catalog.md, db-ui-bug-catalog.md, sqlite-/mysql-bug-catalog.md) are not repeated; regressions are marked as such. Screenshots: `<scratchpad>/shots/model-edit/`.

## Entries

### 1. Table Editor, Indices page: the column list of the selected index is empty (PRIMARY shows no `idproductgroup`)

Severity: functional (minor - display only, the index itself is intact in the model).
Steps: open order.xml; double-click the `productgroup` table header; the Indices page is shown with `PRIMARY` selected in the index list.
Observed: the right-hand "index columns" list box is empty although the PRIMARY index contains `idproductgroup` (`08-tableeditor.png`). Same after adding/renaming (`10-newcol-dt.png`). No stderr output.
Expected: the columns of the selected index listed (as in the original DBDesigner 4), so that columns can be removed with the eraser button.
Suspected cause: the index-columns list is filled in the index-list `OnClick`/`RefreshIndexColumns` path of `src/EditorTable.pas`; the LCL `TListBox` under GTK2 is probably filled before the page/handle exists or the selection-change event that triggers the fill does not fire on programmatic `ItemIndex` (compare db-ui #19). Fix scope: small.

### 2. Canvas keeps a stale horizontal line after a table is renamed/resized through the Table Editor

Severity: cosmetic.
Steps: open order.xml; double-click `productgroup`; change the Table Name to `productgroup_renamed` (table becomes wider); OK.
Observed: the table, the DB Model tree and the `ProductgroupRel` line update correctly, but a light grey 1-px horizontal line remains across the whole `OnlineStore` region at canvas y~503 (from the region's left edge to its right edge, `12-main-after.png`). It is not part of any object and disappears on a full repaint.
Expected: no leftover pixels after the editor closes.
Suspected cause: the invalidate rectangle after `EditorTable` closes (`TEERTable.RefreshObj`/`DoPaint` in `src/EERModel.pas`) covers the old table bounds only, while the region background is redrawn with an off-by-one row (or the selection frame of the table is cleared with the region's colour minus one line). Fix scope: small.

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

### 7. Ctrl+Del with nothing selected shows the delete confirmation with an empty object list

Severity: cosmetic (minor).
Steps: click on empty canvas (or on an object that cannot be selected, see #5); press Ctrl+Del.
Observed: "Are you sure you want to delete the selected Objects? The following Objects will be deleted:" with an empty list (`30-confirm.png`, `34-confirm.png`); Yes does nothing.
Expected: nothing happens (the Edit menu item is disabled by `DeleteMIShow` when nothing is selected, but the shortcut bypasses the `OnShow`-time check).
Suspected cause: `TMainForm.DeleteMIClick` (`src/Main.pas:~950-990`) does not test `ObjectList.Count=0` before `MessageDlg`. Fix scope: small.

### 8. Table Editor: after renaming a column in the grid the OK button disappears - the edit cannot be confirmed

Severity: functional.
Steps: double-click `News`; click the `idNews` Column Name cell (twice - the first click after window activation is lost); type `x` (the in-place editor opens and replaces the name); End, `2`, Return.
Observed: the grid shows `x2` (`43-comb.png`, middle), but the green OK check mark at the bottom right is gone - only the red Cancel X is painted (`44-editor-still.png`); two clicks on the OK position do nothing; Cancel closes the dialog. The canvas keeps `idNews` and `Employee.FKidNewsCol` (`46-crop.png`), so PK-rename propagation to FK columns could not be evaluated. Compare `25-dt2.png`: after adding a *new* column (name + datatype) both buttons are present and OK works. Re-check on `productgroup`: renaming the non-key `groupname` (`g` + Return, `50-comb.png`) and the PK `idproductgroup` (`p` + Return, `53-comb.png`, top) both keep the OK button; End inside the grid moves to the Comments cell (`53-comb.png`, bottom). So the trigger is specific to the News sequence - either the End key pressed while the in-place name editor is open, or the collapsed (►) display state of `News`. Reproduced once; needs the exact sequence above.
Expected: OK stays visible; the rename is applied and the FK column of `Employee` is renamed to `FKx2Col` (order.xml has FK prefix/postfix).
Suspected cause: the only code that hides the button is `SubmitBtn.Visible:=False` for read-only / linked tables (`src/EditorTable.pas:463-466`), which does not apply here; the button is most likely covered or destroyed by the in-place `EditorTableFieldEdit` / datatype combo teardown - the sequence "Return in the name editor of an *existing* row" differs from the new-row path (`ColumnGridKeyDown`, `src/EditorTable.pas:~1013`, and the `EditCellStr` callback that ends editing). A `TSpeedButton` under GTK2 that loses its parent panel's repaint after a child control is freed would explain a painted-away *and* unclickable button only if the control is really hidden, so check `SubmitBtn.Visible/Enabled` in the rename path first. Fix scope: small-medium.

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
