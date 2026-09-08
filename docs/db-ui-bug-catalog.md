# DB-related UI bug catalog (MySQL / SQLite3 dialogs)

Date: 2026-09-07. Branch: a3 (HEAD f967d35). Status: **final** (30-minute real-display run,
GNOME/XWayland DISPLAY=:0, MySQL 8 at 127.0.0.1/dbdtest, SQLite order.sqlite).
Earlier catalogs (ui-bug-catalog.md, sqlite-bug-catalog.md, mysql-bug-catalog.md) are not repeated
here; regressions of fixed items are flagged as such.

Screenshots: `<scratchpad>/shots/db-ui/`.

## Entries

### 1. Connection selector: the "..." button at the end of each connection row does nothing (no way to edit an existing connection)

Severity: functional.
Steps: Database > Connect to Database; click the OrderMySQL row; click the small "..." button at the right end of the row.
Observed: nothing happens (no editor window, no stderr output); `connsel_afterdots.png`. The only route to the editor is "New Database Connection"; existing connections cannot be edited from the UI (DBConn.ini must be edited by hand).
Expected: the Database Connection Editor opens pre-filled with the row's settings (that is what the button did in the CLX build via `ConnectionsListView` row buttons / DblClick).
Suspected cause: `src/DBConnSelect.pas` - `ConnectionsListViewDblClick` (line ~103) / the per-row button image is drawn by the list view but its click is not routed to an edit handler under LCL. Also double-clicking the row connects rather than edits (`ConnectionsListViewDblClick` -> connect). Fix scope: small/medium.
Status: fixed in c9c00bc - the `...` button was wired to `ConnectionsListViewClick` but its hit-test required `mx < x2` where col5 is only 22 px wide, so clicks on the visibly-wider button missed. Dropped the upper bound (col5 is last), extracted `EditSelectedDBConn`, and added an "Edit Connection" popup-menu item as a guaranteed route. Editor OK persists via `StoreDBConns`. Verified: `...` and right-click Edit open the editor pre-filled; edited OrderMySQL description round-trips through `DBConn.ini`.

### 2. Database Connection Editor: "Port" field is permanently disabled and new connections are saved without a `Port=` line

Severity: functional (minor).
Steps: Connect to Database > New Database Connection; Driver = MySQL; click into the Port field and type.
Observed: the field is greyed and does not take focus - the keystrokes go to whatever control was focused before (in this run the "7" landed in Username, which became `bpsa7`; `conneditor_filled2.png`). After OK the new `[TestConnUI]` section in `~/.DBDesigner4/DBConn.ini` has `HostName=` but no `Port=` key (the hand-written `[OrderMySQL]` section has `Port=3306`).
Expected: editable port, persisted as `Port=`.
Suspected cause: `src/DBConnEditor.pas:515` `PortEd.Enabled:=False` in `CheckHostEdits` (label enabled, edit disabled - looks like a typo for `True`), and the save routine never writes `Port`. Fix scope: small.
Status: fixed in c9c00bc - `CheckHostEdits` set `PortEd.Enabled:=False` for MySQL (now `True`); `ConnectBtnClick` now stores `PortEd.Text` in `Params['Port']` and `RefreshParams` loads it. `StoreDBConns` writes it as `Port=`. Verified: Port field takes focus, typed `3307` -> `Port=3307` in the ini for a new MySQL connection.

### 3. Database Connection Editor cosmetics: "Port:" label overlaps the Hostname drop-down button; selecting MySQL pre-fills "root" into Username on top of what was typed; Advanced grid has no "Value" column header

Severity: cosmetic.
Steps: as in #2; see `conneditor_filled.png`, `pair2.png`.
Observed: "Port:" label starts at the right edge of the hostname combo button; switching Driver to MySQL after typing a username produced `rootbpsa` (username is overwritten/prepended rather than left alone when non-empty); Advanced page grid has only "Param" in the header row, second column header blank.
Expected: label to the right of the combo; keep user's text; header "Value".
Suspected cause: `src/DBConnEditor.lfm` (PortLbl Left/HostIPEd Width), `CheckHostEdits` in `DBConnEditor.pas` (~line 507-535) setting `UserNameEd.Text`. Fix scope: small.
Status: fixed in c9c00bc - `PortLbl` moved to `Left=276 Width=37` (clear of the Hostname combo at x=269); `DatabaseTypesCBoxCloseUp` restores the user's non-empty edit-box values after loading defaults, so MySQL no longer overwrites a typed Username with `root`; `RefreshParams` restores `Cells[1,0]:='Value'` after `Cols[1].Clear`. Verified on the real display.

### 4. Failed MySQL login shows a stale MySQL-5-era explanation instead of the server's error text

Severity: functional (minor, misleading).
Steps: Connect to Database; select a MySQL connection with a wrong user/password (here `bpsa7`); Connect.
Observed: modal "Error" box (`error1.png`): "Connection to database MySQL has failed. Possible causes are: ... DB Designer Fork does not connect to MySQL 5.* with password ... TMySQL80Connection : Server connect failed." The real reason (`Access denied for user 'bpsa7'@...`) is never shown. The selector dialog is closed afterwards and has to be reopened (both the connection choice and the typed password are lost).
Expected: show `EDatabaseError.Message` from the connector (which includes the MySQL error) and return to the selector with the password field cleared.
Suspected cause: fixed text in `src/DBConnSelect.pas` `ConnectBtnClick` (~line 378) / DMDB connect wrapper; only `E.Message` of the outer exception is appended. Fix scope: small.
Status: fixed in c9c00bc - the FPC connector drops the real `mysql_error()` (its `SErrServerConnectFailed` has no `%s`), so `TSQLConnection.Open` (`src/clx_shims/sqlexpr.pas`) re-probes with `mysql80dyn` on failure to recover it; `GetConnectErrorMessage` lost the stale MySQL-5 blurb and `GetDBConnButtonClick` appends the real message, clears the tried password and reselects the connection. Verified: wrong password shows "Access denied for user 'bpsa'@'localhost'...", selector stays open with the row selected and password cleared.

### 5. Reverse engineering into an open model moves every existing table (the "skipped" ones) into a grid layout, destroying the model's placement

Severity: functional (data loss of layout; partial regression of sqlite-bug-catalog #14 fix).
Steps: open bin/Examples order.xml (or the default model); Database > Reverse Engineering; OrderMySQL; keep all 12 tables ticked, defaults; Execute; OK on "12 table(s) already exist in the model and were skipped."
Observed: the tables are not duplicated, but all of them are re-laid out in rows of 10 in palette order starting at the top-left of the canvas, over the logo (`main_after_reveng.png`, compare `dbmenu.png` before). Regions, notes and relation labels stay where they were, so the picture is scrambled. Also the connected database is not queried for the model-only tables (fine) but "skipped" tables get the reverse-engineered position anyway.
Expected: skipped tables keep their position (and only newly created tables are laid out).
Suspected cause: `src/EERReverseEngineering.pas` / `EERModel.pas` `ReverseEngineer...` - the "table already exists -> skip" branch still runs the placement code (`xpos/ypos` assignment per table index), or the placement loop iterates the full selected table list rather than the created ones. Fix scope: small.
Status: fixed in 481588d - the five per-driver "Order table positions" loops in `src/DBEERDM.pas` walked every `TEERTable` of the model; they are replaced by `EERReverseEngineerPlaceTables`, which places only the run's new `DbTables` (still avoiding cells occupied by any table). Verified on the real display: OrderMySQL into `order.xml` -> "12 skipped", canvas pixel-identical; after deleting `webserver` only it comes back (default cell), nothing moves; OrderSQLite -> `child`/`parent` in the next free cells. Relations/indices of skipped tables were already untouched. `--selftest` 0 FAIL.

### 6. Synchronisation progress memo does not scroll to the newest line

Severity: cosmetic.
Steps: Database > Database Synchronisation > OrderMySQL > Execute.
Observed: the memo stays scrolled to the top-middle ("Compare columns from table product" visible, `sync_exec.png`); "Syncronisation finished. / 12 Tables compared. / 50 Columns compared." is only seen after clicking into the memo and pressing Ctrl+End (`sync_end.png`). The word "Syncronisation" (and the "Syncronise Standard Inserts" check box) is misspelled.
Expected: memo follows the last line (SelStart := Length; or `Lines.Add` + `Perform(EM_SCROLLCARET)`).
Suspected cause: `src/EERSynchronisation.pas` `AddMsg`/`ProgressMemo.Lines.Add` without scrolling. Fix scope: small.
Status: fixed in 70004a5 - `EERMySQLSyncDB` appends to `ProgressMemo.Lines` and pumps messages, but nothing moved the memo; a new `ProgressMemoChange` (`OnChange`, which the LCL fires for programmatic `Lines.Add`) sets `SelStart:=Length(Text)` so the GTK text view follows the caret. The misspellings are translation *values* (`bin/Data/DBDesignerFork_Translations.txt`: messages 152/164, `SyncStdInsertsCBox`, `SyncImg` hint - en/de/xx and the French hint) and were fixed there plus in the source fallbacks; keys/component names unchanged. Verified: after Execute the memo ends on `Synchronisation finished. / 12 Tables compared. / 50 Columns compared.` without scrolling, sync completes with no error box.

### 7. Query mode: INSERT executed with "Execute SQL" reports "1 Rows affected" but is never committed to MySQL

Severity: functional (silent data loss).
Steps: Display > Query Mode (connected to OrderMySQL); type `INSERT INTO product (idproduct,idproductgroup,name,ean,price,info,pic) VALUES (99,1,'uitest','x',1.5,'','')` in the SQL memo; click the top "Execute SQL" toolbar button.
Observed: status bar "Query(s) executed. 1 Rows affected. Time: 00:00:012" (`query_pair.png`, bottom); `mysql -e "select * from product where idproduct=99"` returns nothing and `count(*)` is still 3, also after several seconds. The SELECT path works (3 records shown).
Expected: the statement is committed (autocommit) or an explicit commit follows the script.
Suspected cause: `src/EditorQuery.pas:1073` `DMDB.ExecuteSQLCmdScript` runs the statements inside the SQLdb `TSQLTransaction` that was started for the schema queries (the fix for sqlite-bug-catalog #2 "Transaction not set." starts a transaction) and never calls `Commit`/`CommitRetaining`; the change is rolled back on disconnect. Fix scope: small (commit after the script in `ExecuteSQLCmdScript`, and in the DBGrid post path - not tested).
Status: fixed in 73fa14d - `TSQLDataSet.ExecSQL` (`src/clx_shims/sqlexpr.pas`) now `CommitRetaining`s after every statement (dbExpress auto-commit), `ExecuteDirect` uses `CommitRetaining` instead of `Commit`. Verified on the real display: the INSERT is visible to `mysql` while the app is connected and after Disconnect, the DELETE likewise; same on a copy of `order.sqlite` with `sqlite3`. `tests/TestSQLExprShim.pas` covers it (old shim: "ExecSQL DELETE rolled back by Close"). The DBGrid path is not a write path: the shim `TClientDataSet` is an in-memory `TBufDataset` copy with no write-back.

### 8. DataImporter plugin: "Column Mapping" and "General Options" tabs cannot be selected; several captions clipped

Severity: functional.
Steps: Plugins > DataImporter; click the "Column Mapping" tab, then "General Options".
Observed: the page never changes - "Text Options" stays visible in both screenshots (`swf_di.png`, lower two panels are after each tab click). Also: "Apply Preset" label is truncated to "Apply Pres", the "Store these settings as Preset" button is cut off at the right window edge ("tore these settings as Prese"), "Fieldname" label clipped to "Fieldnam", the source tab control ("Import from Text Files") shows scroll arrows although it has one page, the area under the "Text with seperator" radio buttons is empty (separator/quote controls not shown), "seperator" typo, and the Execute button is disabled with no hint why. Selecting OrderMySQL from the plugin's connection dialog fails with "TMySQL80Connection : Server connect failed." because the password is no longer in DBConn.ini (the main app dropped the `Password=` line when it rewrote the file after adding a connection - see #2) and the plugin's selector shows an empty password box.
Expected: tabs switch pages; labels fit; the password box is at least offered before connecting.
Suspected cause: plugin form (`DBDplugin_DataImporter` sources, `Main.lfm`/`MainForm.pas` of the DataImporter project) - `TPageControl` pages with `TabVisible`/`OnChanging` returning False, or the tab strip is a `TTabControl` whose `OnChange` handler is not wired after the LCL conversion; label `AutoSize`/`Width` values from the CLX form. Fix scope: medium.
Status: fixed in e94f70a - the tabs did switch (lost first click); the clipping was the CLX 8pt layout under the LCL's 10pt font: `DBImportData.lfm` re-laid out (925x600, autosize labels, taller option groups, 300 px source tab control with "From Text Files"/"From Database" captions - the arrows were GTK2 tab overflow, there were two pages), "separator" spelled, German leftovers translated, separator group visible by default, `DirEd.OnKeyDown` wired, status text + hint explain the disabled Execute. The password box is the shared selector's; the plugin now pre-selects the main app's open connection (`DBConn_Current.ini` written by `PluginMIClick`). On the way: `Progress.lfm` CLX `BorderStyle` (#11), swallowed SQL errors (#12), unmapped columns inserted as `''` (#12). Verified: 3-row CSV imported into `product_import` with Auto-Mapping.

### 9. SimpleWebFront plugin: form does not fill its window (blank strip right/bottom), Database Connection fields empty even though DBDesigner is connected

Severity: cosmetic.
Steps: Plugins > SimpleWebFront while connected to OrderMySQL.
Observed (`plugin_swf.png`): window 812x406 with the client content ending at ~800x350 and a horizontal/vertical scrollbar pair on the outside; Hostname/Database/Username/Password empty; Output Directory "/". Buttons and tree are functional as far as clicked.
Expected: form sized to the content (or anchors), connection fields pre-filled from the current connection passed in `plugin_tmp.xml`.
Suspected cause: SimpleWebFront main form `AutoScroll`/`ClientWidth` in the converted .lfm; connection parameters not read from the plugin XML. Fix scope: small.
Status: fixed in e94f70a - `Main.lfm` had `Width/Height` plus explicit `HorzScrollBar.Range`/`VertScrollBar.Range` (now `ClientWidth/ClientHeight = 799/330`, no ranges); `plugin_tmp.xml` carries no connection, so `PluginMIClick` writes the open connection's name to `~/.DBDesigner4/DBConn_Current.ini` and `PrefillConnectionFromDBDesigner` fills the empty Hostname/Database/Username(/Password if stored) from `DBConn.ini`. Grid Options label/combo overlap and the `Line1..Line4` memo leftovers fixed too. Verified: fields show 127.0.0.1/dbdtest/bpsa, Create Webpages produced `index.php`, `db_open.php` (correct host/user/db) etc. View Editor OK crash found on the way (#13).

### 10. Export SQL Script dialog: "Last change date/user column trigger" caption is overdrawn by the "Trigger prefix" edit; disabled advanced options render as bare text

Severity: cosmetic.
Steps: File > Export > SQL Create Script...; look at the "Advanced SQL Settings Options" group (`export_pair.png`, `export_pair2.png`); change Target Data Base (FireBird, My SQL, Oracle, PostgreSQL, SQL Server, SQLite are listed).
Observed: the group-box caption "Last change date/user column trigger" is partially covered by the EXCDT_ edit above it; for My SQL the disabled options (Hide NULL Field Option, Portable Indexes, GO Statement, ...) show no check-box glyph at all, for PostgreSQL a grey tick appears left of them. Copy Script to Clipboard / Save Script to file / close work (clipboard content not verified: no xclip on this machine).
Expected: proper vertical spacing; consistent disabled check-box rendering.
Suspected cause: `src/EERExportSQLScript.lfm` Top/Height values of the trigger group boxes; disabled `TCheckBox` glyph drawing under GTK2 (may be the theme). Fix scope: small.
Status: fixed in 70004a5 - `EdLastDeleteTriggerPrefix` (Top 125-146) overlapped `CBLastChange` (Top 145); the Last-change block moved down (check box 156, edits 184/209/234) and the `&  trigger` captions became `&&`. The glyphs: the boxes are ordinary disabled `TCheckBox`es; the Yaru gtk-2.0 theme maps the insensitive-unchecked CHECK state to an empty menu asset (`menu-checkbox-insensitive.png`) while insensitive-checked gets the grey tick, so My SQL (options forced off) and PostgreSQL (forced on) differ only by state - under Adwaita gtk-2.0 every disabled box has a glyph (see notes). Not worked around. The real per-target inconsistency was `CBTargetDataBaseChange`: SQLite disabled the trigger/sequence options without resetting them, so values remembered from an Oracle/FireBird session leaked `CREATE SEQUENCE`/trigger tables into the SQLite script; it is now table-driven with an explicit Checked value for every disabled option. Verified for My SQL, PostgreSQL and SQLite; Copy Script to Clipboard yields the SQLite script (12 tables, 5 AUTOINCREMENT, no SEQUENCE/TRIGGER).

### 11. DataImporter plugin: Execute raises "Error reading Label1.BorderStyle: Unknown property" and then imports nothing

Severity: functional.
Steps: DataImporter, connect, check a file, Execute.
Observed: LCL "Press OK to ignore and risk data corruption" box while creating the progress form; after OK nothing is imported and no message appears.
Cause: `Plugins/DataImporter/Progress.lfm` had the CLX-only `BorderStyle = bsSingle` on four `TLabel`s (same family as the `Rows =` property of ui-bug-catalog #3).
Status: fixed in e94f70a - property removed; no other label `BorderStyle` in `Plugins/*/*.lfm`.

### 12. DataImporter plugin: failed INSERTs are reported as success ("3 Lines of Data imported"), unmapped columns inserted as ''

Severity: functional (silent data loss).
Steps: DataImporter, pick a destination table whose columns are not mapped (or any INSERT that MySQL rejects), Execute.
Observed: "Data import finished. 3 Lines of Data imported." but the table is unchanged.
Cause: `TDMDB.ExecSQL` (`src/DBDM.pas`) built its `EDatabaseError` without `raise`, so every error was swallowed (original code); and `ImportBtnClick` inserted `''` for every destination column without a mapping (auto-increment keys, NOT NULL ints under strict mode).
Status: fixed in e94f70a - `ExecSQL(s, RaiseOnError=False)`; the importer passes `True`, catches the error and shows "Data import failed after N lines" with the server message and the statement; only mapped columns go into the INSERT (error if none). Sync callers keep the tolerant behaviour.

### 13. SimpleWebFront View Editor: OK does nothing (a "Division by zero" box is hidden behind the modal editor)

Severity: functional (no view can be created, so nothing can be generated).
Steps: SimpleWebFront > Views > Create View..., name + table, OK.
Observed: the editor stays open; a "Division by zero - Press OK to ignore" box exists behind it.
Cause: `GetOrderByClause` (`Plugins/SimpleWebFront/EditorView.pas`) read `OrderColumnsComboBox.Items[ItemIndex]` with `ItemIndex = -1` (the LCL resets it on `Items.Clear` in `ShowColsInListBox`); `TGtkListStoreStringList.Get` reports out-of-bounds through `RaiseGDBException`, i.e. a deliberate integer division by zero; the `assert`s are compiled out.
Status: fixed in e94f70a - -1 means no order/ascending, `ShowColsInListBox` re-selects index 0; editor re-laid out (500x615, no scroll ranges) so the Order By and Where-clause groups are not clipped.

## Not bugs / could not reproduce

- Connection selector tree: nodes named "..." under "Network Hosts"/"MySQL" are lazy-expansion placeholders (`DBConnSelect.pas:222,259`), not a bug. Clicking "SQLite"/"All Connections" filters the list correctly (first click sometimes only focuses the tree; keyboard navigation works).
- Selecting MySQL in the editor defaults Username to "root" - intended, only the append-to-typed-text part is reported (#3).
- Connect to Database, then Reverse Engineering/Synchronisation ask for the connection again via the selector - same as the original DBDesigner 4 flow.
- Reverse Engineering dialog: check boxes formerly clipped (sqlite-bug-catalog #11) now render fully; Select/Unselect All, function combo (ODBC/MySQL/Oracle/SQLite/MSSQL), datatype substitution, Build Relations radios all work. "Schema" combo only offers "All" for MySQL (original behaviour).
- Synchronisation against MySQL: 12 tables / 50 columns compared, no error boxes, tables unchanged (mysql-bug-catalog #10/#11 fixes hold).
- Query mode SELECT: 3 records fetched, grid, navigation buttons, insert-record (+) and cancel (X) work; focused-cell font issue is the already-known one, not worse.
- HTMLReport plugin: Tables list empty for region "Forum" is a consequence of #5 (the tables were moved out of the region), not a plugin regression; Execute opens a Save File As dialog.
- SimpleWebFront: Views/Grid Options pages open; "Line1..Line4" in the Where-Clause memo and the "Columns visible in Grid" label overlapping the View combo are design-time leftovers (minor, folded into #9). "Create Webpages" with no views does nothing silently.
- Demo plugin opens and closes normally. No exceptions on stderr during the whole run (only the canberra-gtk-module message).
- The `Password=` line of `[OrderMySQL]` was removed when the app rewrote DBConn.ini (known behaviour); restored afterwards.

## Coverage

Exercised (all on the real display, screenshots in `<scratchpad>/shots/db-ui/`): Database menu; connection selector (row selection, tree filtering, "..." button, New Database Connection, wrong credentials, Connect); Database Connection Editor (General/Advanced pages, driver list, all fields, OK, persistence in DBConn.ini); Reverse Engineering against MySQL (all controls, Execute into the open model); Synchronisation against MySQL (Execute, log); Query Mode (SELECT, INSERT, record buttons); Plugins Demo, HTMLReport, DataImporter (tabs, connection dialog), SimpleWebFront (pages); File > Export > SQL Create Script dialog (target list, options, buttons).

Not reached in the 30-minute limit: Reverse Engineering against SQLite (OrderSQLite connection was not used this round), synchronisation error handling with a deliberately broken model, DBGrid editing/posting of data, SQL Drop/Optimize/Repair script dialogs, actual content of the exported SQLite script, Options > Model Options database pages, Table Editor datatype lists for SQLite, "Open from Database"/"Save in Database", plugin end-to-end runs (import a text file, generate web pages/HTML report), deleting a connection with the eraser button.
