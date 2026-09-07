# SQLite database round-trip: bug catalog

Date: 2026-09-07, branch `a2`, binary built with `lazbuild DBDesignerFork.lpi`.
Model under test: `bin/Examples/order.xml` (14 tables: 12 own + 2 linked
`Employee`/`News`, 7 auto-increment PKs, 11 relations, 2 explicit indexes).

Scratch files (screenshots, logs, scripts, databases) live in
`/tmp/claude-1001/-home-bpsa-app-dbdesigner-fork/e506f9cb-2d39-419c-bc88-17e433cbec7a/scratchpad/sqlite/`
(referred to as `$S` below). Nothing there is in the repo.

Repeatable part (stages B and D, no display needed):
`tests/sqlite-roundtrip.sh <script.sql> <out.sqlite>` — loads the script with
`sqlite3`, prints load errors, then tables / columns / PK / NOT NULL / foreign
keys / indexes / AUTOINCREMENT / row counts per table.

## Round-trip status

| Stage | What | Status |
|---|---|---|
| A | Export `order.xml` to SQL create script, target "SQLite" (File > Export > SQL Create Script) | **works** after #3/#4/#6 — `$S/fix03-after2.sql`: inserts once, `INTEGER PRIMARY KEY AUTOINCREMENT`, no index prefix lengths, tidy whitespace |
| B | `sqlite3 order.sqlite < script` | **works** — `tests/sqlite-roundtrip.sh` on `$S/fix03-after2.sql`: zero errors, 12 tables, both explicit indexes, 2 FKs, 5 tables with AUTOINCREMENT, rows loaded once (`$S/fix03-roundtrip.txt`) |
| C | New SQLite connection + connect + reverse engineer | **works** after #1/#2 were fixed — tree node clicks, connection editor, connect and the Reverse Engineering dialog (12 tables listed) all work; Execute adds 12 tables to the model |
| D | Compare reverse-engineered model with original | **works** after #10/#7 (re-checked after #3/#4/#6 with `$S/fix03-reveng.xml`: same result, 5 AutoInc PKs recovered) — 12 tables with all columns (datatype, PK, NOT NULL, AutoInc, default), all indexes and 10 of 11 relations (the self-relation `forumpost` -> `forumpost` is not recoverable: no FK in the DB); compared with `$S/compare_models.py` (`$S/fix10-reveng.xml`) |
| E | Database Synchronisation against the DB | **fails (expected, #8)** — re-checked 2026-09-07 after #2: the dialog now opens, connects and reports "15 Table(s) in Database, 28 Table(s) in Model"; Execute stops after "Get Tables from DB" with the exception `TSQLite3Connection : near "show": syntax error` (MySQL `SHOW` statement), shown in the centred exception dialog; the database is left untouched (`$S/verify/verify-sync-0xc06a34.png`, `$S/verify/verify-sync-0xc06911.png`) |
| F | Query mode, simple SELECT | **works** after #5 — `SELECT * FROM product` shows the 3 rows in the grid, status bar "Query opened. 3 Record(s) fetched" (`$S/fix05-final-grid.png`); an invalid statement shows the SQLite error (`$S/fix05-sqlerror.png`) |

## Entries

### 1. Access violation when clicking a node in the "Network Hosts" tree of the connection selector
- Status: **FIXED** — `DBConnTV.OnClick` now points at a new `TNotifyEvent` wrapper `DBConnTVClick` (`src/DBConnSelect.pas`) that resolves the clicked node with `GetNodeAt(ScreenToClient(Mouse.CursorPos))` and calls the old CLX-signature `DBConnTVItemClick`; the LCL was calling the 4-argument handler as a 1-argument one, so `Node` was garbage. Verified on the real display: SQLite / Network Hosts / Localhost / NewSQLiteConn clicks, no message box (`$S/fix01-connsel2..4.png`).
- Severity: **crash**
- Repro: Database > Connect to Database; click the "SQLite" folder (any node) in the left tree. Message box "Access violation. Press OK to ignore and risk data corruption." Every later click in the tree repeats it.
- Evidence: `$S/msgbox.png`; gdb backtrace `$S/gdb_run.txt`:
  `#0 GETLEVEL (this=0x0) treeview.inc:1736`, `#1 DBConnTVItemClick (... Button=195, Node=0x0 ...) src/DBConnSelect.pas:775`, called from `TControl.Click`.
- Cause: `src/DBConnSelect.lfm:408` wires `OnClick = DBConnTVItemClick`, but the handler keeps the CLX `OnItemClick` signature `(Sender; Button: TMouseButton; Node: TTreeNode; const Pt: TPoint)` (`src/DBConnSelect.pas:765`). LCL calls it as a `TNotifyEvent`, so `Node` is garbage/nil and `Node.Level` dereferences nil. Same pattern may exist in other forms that had CLX `OnItemClick` (grep `OnItemClick`/`Node: TTreeNode; const Pt` in `src/`).
- Fix idea: call it from `OnMouseDown`/`OnClick` wrapper using `DBConnTV.Selected` (or `GetNodeAt`) as the node. Complexity: **small**.
- Side effect: because the crash happens on the node click, the "NewSQLiteConn" child node coded at `DBConnSelect.pas:280` is never reachable; the workaround is to leave "All Connections" selected and use "New Database Connection", then pick "SQLite" in the editor's Driver combo (works, `$S/conneditor2.png`).

### 2. "Transaction not set." on the first schema query after connecting (blocks reverse engineering and synchronisation)
- Status: **FIXED** — the shim's `TSQLConnection` (`src/clx_shims/sqlexpr.pas`) now creates its `TSQLTransaction` in the constructor instead of lazily in `Open`, so datasets streamed with `Database = SQLConn` (or linked before `Open`) inherit it via SQLDB's `TCustomSQLQuery.SetDatabase`. `tests/TestSQLExprShim.pas` now links the dataset before `Open` and fails without the fix. Verified: Reverse Engineering dialog opens and lists the 12 tables (`$S/fix01-revdlg.png`). Stage C now proceeds; see the new entry #10 for what comes back.
- Severity: **crash** (unhandled exception dialog "Press OK to ignore and risk data corruption"; the dialog that was being opened is silently abandoned)
- Repro: connect to a SQLite connection (Driver SQLite, Database File = `$S/order.sqlite`); Database > Reverse Engineering (or Database > Database Synchronisation with the order model active) > select the connection > Connect. Status bar says connected, but the message box appears (it opens off to the top-right at +1266+20 and is easy to miss) and no Reverse Engineering / Synchronisation dialog is shown.
- Evidence: `$S/msgbox2.png`, `$S/sync_msgbox.png`, `$S/main_rev2c.png` (connected, no dialog).
- Cause (high confidence): the shim creates the `TSQLTransaction` lazily in `TSQLConnection.Open` (`src/clx_shims/sqlexpr.pas:154-159`). `DMDB.SchemaSQLQuery` is a `TSQLDataSet` streamed from `src/DBDM.lfm:47-49` with `Database = SQLConn`; SQLDB copies the connection's transaction into a query only at the time `Database` is assigned, and at that moment it is nil. So `SchemaSQLQuery.Transaction` stays nil and `TCustomSQLQuery` raises "Transaction not set" in `TDMDB.GetDBTables` (`src/DBDM.pas:586-604`, `SELECT name FROM sqlite_master`). The standalone test `tests/TestSQLExprShim.pas` does not see this because it assigns `DS.SQLConnection := Conn` after `Conn.Open`. Same problem for every other `TSQLDataSet` streamed with `Database = ...` (`src/DBDM.lfm:31 OutputQry`, `src/EditorTableData.lfm:1157`), and for `EditorQuery` whose `OutputQry.SQLConnection := DMDB.SQLConn` (`src/EditorQuery.pas:355`) runs at form creation, before any connection is open.
- Fix idea: create the transaction in the shim's `TSQLConnection` constructor (so it exists when `.lfm` links are resolved), and/or in `TSQLDataSet.SetSQLConnection`/before `Open` set `Transaction := SQLConnection.Transaction` when nil. Complexity: **small**.

### 3. SQLite (and probably every) SQL create script contains each table's standard inserts twice
- Status: **FIXED** — `TEERTable.getSqlTableComment` (`src/EERModel.pas`) never initialised its `result`; FPC passes the caller's temporary string in, which held the tail of the script (the inserts block just appended), so with "Output Comments" checked the tail was appended a second time for every target. Now `result := ''`.
- Severity: **wrong result**
- Repro: File > Export > SQL Create Script, Target Data Base "SQLite", "Output Standard Inserts" checked (default), Save Script to file. In the script every `INSERT` block appears two times in a row (`$S/order_sqlite.sql`, e.g. lines 8-19 for `productgroup`). Loading it: `UNIQUE constraint failed` for every table with inserts (`$S/roundtrip_summary.txt`, 16 errors).
- The model holds each insert once (`StandardInserts` attribute in `bin/Examples/order.xml`), so the duplication happens on load or export. Suspects: `TEERTable.GetSQLCreateCode` (`src/EERModel.pas:9284-9287`, single append) and the XML attribute decoding used by the two loaders (`src/EERModel.pas:9704` via the `xmlintf` shim `AttributeNodes[...].Text`, and `:10033` via the fast parser) — check whether `StandardInserts.Text` already contains the text twice after loading (Table editor, "Standard Inserts" tab, would show it). Not tested with a MySQL target; likely target-independent. Complexity: **medium** (small once located).

### 4. SQLite export emits MySQL index-prefix syntax `column(length)`
- Status: **FIXED** — the `(n)` suffix from `ColumnParams` is only emitted when `DatabaseType = 'My SQL'` (`TEERTable.GetSQLCreateCode`); the script now has `CREATE INDEX product_name ON product (name, info);` and sqlite3 creates it.
- Severity: **wrong result**
- Repro: same export as #3. `CREATE INDEX product_name ON product (name, info(100));` — sqlite3: `Parse error near line 81: no such function: info`; the index is not created (only `product_ean` and the two autoindexes exist afterwards).
- Cause: `src/EERModel.pas:9108-9109` appends `ColumnParams` (the MySQL prefix length stored as `LengthParam` in the model) to every index column regardless of `DatabaseType`; the SQLite branch of the export dialog (`src/EERExportSQLScript.pas:709-721`) only toggles check boxes. Fix: skip the `(n)` suffix unless target is MySQL. Complexity: **small**.

### 5. Query-mode toolbar buttons do nothing (Execute SQL etc.)
- Status: **FIXED** — the button did fire; `src/EditorQuery.lfm` and `src/DBDM.lfm` had lost `ProviderName = 'OutputDataSetProvider'` on `OutputClientDataSet`, and the shim `TClientDataSet` (`src/clx_shims/dbclient.pas`) could not open anyway (fields assigned inside `InternalOpen`, where `TBufDataset` already demands them) and swallowed the source query's exception. The shim now opens the provider dataset and `CopyFromDataset`s it; error dialogs were being shown but were filtered out of my window listing (see notes). Verified: 3 product rows + status text; invalid SQL -> SQLite error message.
- Severity: **wrong result** (feature unusable)
- Repro: connect to the SQLite DB; Display > Query Mode; type `SELECT idproduct, name, price FROM product` in the SQL memo; click the "Execute SQL Query" speed button (green arrow, left of the result grid), also tried press/release and the second button ("save SQL"). No result rows, no error dialog, status bar caption unchanged (it should become "Query opened. N Record(s) fetched…" from `src/EditorQuery.pas:1120`), no window opens.
- Evidence: `$S/query_result2c.png`, `$S/query_result5c.png`, `$S/status3.png`.
- Hypothesis: the `TSpeedButton`s in the `EditorQuery` frame do not receive clicks (compare ui-bug-catalog #19 "disabled OK/Cancel speed buttons"), or `ExecSQLBtnClick` (`src/EditorQuery.pas:963`) exits early at `GetSQLMemoText=''` (`:1004`) because the visible memo is not `SQLMemo`. Could not attach gdb (`ptrace_scope=1`); running the app under gdb with a breakpoint on `ExecSQLBtnClick` would settle it in a minute. Even once the button works, the SELECT will hit #2 through `OutputQry` (`src/EditorQuery.pas:355`, `:1092`). Complexity: **medium**.

### 6. Auto-increment is dropped when exporting for SQLite
- Status: **FIXED** — new `TEERTable.IsSQLiteAutoIncPK`: a single-column PRIMARY index on an integer-typed `AutoInc` column is emitted inline as `INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT` (`GetSQLColumnCreateDefCode`) and the `PRIMARY KEY(...)` table constraint is skipped for it (`GetSQLCreateCode`). 5 of the 12 exported tables carry it; the reverse engineering recovers all 5 `AutoInc` flags.
- Severity: **limitation**
- The model has 7 `AutoInc="1"` PK columns (`idproduct`, `idonlineorder`, `idonlinecustomer`, `idproductgroup`, `idcreditcard`, `idNews`, `idEmployee`). The SQLite target disables the auto-increment options (`src/EERExportSQLScript.pas:718-720`) and emits `idproduct INTEGER NOT NULL … PRIMARY KEY(idproduct)`, so the round trip cannot recover auto-increment (`tests/sqlite-roundtrip.sh` prints `autoincrement: no` for every table).
- SQLite supports `INTEGER PRIMARY KEY AUTOINCREMENT` (single-column integer PK only); the export could emit that inline for single-column integer auto-inc PKs. Complexity: **small**.

### 7. Reverse engineering from SQLite never creates relations
- Status: **FIXED** — `EERSQLiteReverseEngineer` now reads `pragma_foreign_key_list` for every table (parent/child, `FKFields`, ON DELETE/UPDATE into `RefDef`, `CreateRefDef=1`, identifying when the FK columns are the child's PK) and then runs the usual name/PK heuristic (`EERReverseEngineerMakeRelations` with a new `SkipExisting` flag) for the rest. Both FKs of `order_ai.sqlite` and 8 heuristic relations recovered (10 of 11, see stage D). Self-relations are still not guessed.
- Severity: **limitation** (untested at runtime, blocked by #2; established from code)
- `TDMDBEER.EERSQLiteReverseEngineer` (`src/DBEERDM.pas:1210-1368`) lists tables from `sqlite_master`, parses each table's `CREATE TABLE` text for columns (`GetColumnFromSQLCmd`) and optionally creates standard inserts; the `BuildRelations`/`BuildRelUsingPrimKey` parameters are never used (no call to `EERReverseEngineerMakeRelations` at `:1746`, unlike the MySQL/ODBC paths at `:462/:697/:1090`). Foreign keys that the export did emit (`carthasproduct`, `onlineorderhasproduct`, visible via `PRAGMA foreign_key_list` in `$S/roundtrip_summary.txt`) would therefore be lost.
- What SQLite offers: `PRAGMA foreign_key_list('<table>')` (columns `id, seq, table, from, to, on_update, on_delete, match`) gives referenced table, column pairs and the ON UPDATE/ON DELETE actions — enough to build `TEERRel` with `FKFields` `pkcol=fkcol` exactly like the MSSQL `sp_fkeys` loop at `:1666-1700`. `PRAGMA index_list` + `PRAGMA index_info` give indexes and uniqueness (the shim's `SetSchemaInfo(stIndexes)` already works per docs/notes-to-myself.md). Complexity: **medium**.

### 8. Database Synchronisation is MySQL-only
- Severity: **limitation** (untested at runtime, blocked by #2)
- `TEERSynchronisationForm` always calls `DMDBEER.EERMySQLSyncDB` (`src/EERSynchronisation.pas:213`), which starts with `SET FOREIGN_KEY_CHECKS=0` (`src/DBEERDM.pas:1946`), uses MySQL `SHOW`/`ALTER` style statements and MySQL create syntax. Against SQLite it will fail at the first statement. "Store model in database" (File > Save in Database) likewise assumes a `DBDesigner4` table with MySQL DDL (not tried).
- Fix would be a `CreateTableSyntax`-aware branch (the enum at `src/DBEERDM.pas:64` already lists SQLite) using `PRAGMA table_info` for the diff. Complexity: **large**.

### 10. SQLite reverse engineering creates the tables but no columns, indexes or relations (found after fixing #2)
- Status: **FIXED** — columns now come from `pragma_table_info` (name, declared type -> model datatype with params, `pk`, `notnull`, `dflt_value`), AUTOINCREMENT from the `sqlite_master` DDL text, the PRIMARY index from `CheckPrimaryIndex`, other indexes from `pragma_index_list`/`pragma_index_info` (unique flag, autoindexes of UNIQUE constraints renamed); the parser stubs are deleted. Every column and index of the 12 tables matches `order.xml` (`$S/fix10-reveng.xml`, screenshot `$S/fix10-main2.png`). Details in docs/notes-to-myself.md.
- Severity: **wrong result** (silent, no exception)
- Repro: connect to `$S/order.sqlite`, Database > Reverse Engineering, all 12 tables checked, Execute. The tables are added to the active model (`$S/fix01-main3.png`; saved as `$S/fix01-reveng.xml`) but every one of the 12 has zero `COLUMN`s, zero `INDEX`es and no `RELATION` touches them (the 11 relations in the saved file are the original ones). Stderr stays clean.
- Cause: `TDMDBEER.GetColumnCountFromSQLCmd` and `GetColumnFromSQLCmd` (`src/DBEERDM.pas:2965-2973`) are stubs (`Result:=0`, `col.ColName:=''`), so the `CREATE TABLE` text fetched from `sqlite_master` (`:1274-1296`) is never parsed. Indexes/relations are #7. Fix: implement the parser (or use `pragma_table_info`, which `SetSchemaInfo(stColumns)` already wraps) — Complexity: **medium**.
- Also seen: the status bar reads "Not connected to a Database" after the dialog closes, and the tables are added to the current model rather than a new one (matches the original DBDesigner 4 behaviour, not a bug).

### 9. Cosmetic issues seen on the way
- Severity: **cosmetic**
- Main window title stays "DBDesigner Fork - order" after File > New although the new model ("Noname2" in the Windows menu) is active (`$S/main_newc.png`, `$S/winmenu2.png`). **FIXED** — `RegisterEERForm`/`SwitchToEERForm` now update the caption (`TMainForm.UpdateCaptionForEERForm`); `ModelNameChanged` fired before the new form was the active one.
- The exception message boxes (#1, #2) open at the top-right screen corner, not centred on the app. **FIXED** — `Application.OnException` (`TMainForm.AppException`) shows an own dialog centred on the main window; the LCL's GTK message box is parented to an invisible widget, so mutter placed it.
- Exported script formatting: runs of spaces before commas (`groupname Varchar(45)      ,`), `PRIMARY KEY(idproduct)    );`, 5-6 blank lines between tables. **FIXED for the SQLite target only** (MySQL output is kept byte-for-byte): column definitions are collapsed to single blanks and right-trimmed, the two-space indent is only written in front of an inline index (not for portable ones, which went to `CREATE INDEX` anyway), `TidySQLiteScript` drops trailing blanks and repeated empty lines, and the export dialog separates SQLite tables with one empty line.

### 11. Reverse Engineering dialog: "Build Relations", "Use Datatype Substitution", "Create Standard Inserts" check boxes clipped to a sliver
- Status: **FIXED** — the three check boxes were Delphi "group box caption" check boxes drawn over the top edge of their `TGroupBox` (Height 13); GTK2 paints the group box over them. `src/EERReverseEngineering.lfm` now places them entirely above the boxes (Height 21) and shortens the boxes. Other check boxes/buttons in this dialog, the SQL export dialog and the connection editor (both tabs) are not clipped.
- Severity: **cosmetic** (the options could not be read or toggled reliably)
- Evidence: `$S/fix05-revdlg-before.png` (before), `$S/fix05-revdlg-after.png` (after).

## Observations that are not bugs
- Only 2 of 11 relations are exported as `FOREIGN KEY` (`OnlineorderRel`, `CartRel`): those are the only ones with `CreateRefDef="1"` in `order.xml`, and the export option is "Define Foreign Key References when enabled in Relations' Editors". Model setting, not a bug.
- Linked tables `Employee` and `News` (`IsLinkedObject="1"`) are not exported; 12 of 14 tables is the expected count.
- Connection editor, "Database File:" label for SQLite, saving the connection and connecting to a SQLite file all work (`$S/conneditor2.png`, `$S/connselect_new.png`, `$S/statusbar.png`); `PRAGMA`-free schema listing in `TDMDB.GetDBTables` is correct for SQLite once #2 is fixed.

## Not tested
- Stage D comparison (blocked by #2) — the data type mapping of `Varchar(45)`, `FLOAT(10,2)`, `LONGBLOB`, `DATETIME` back into model datatypes, NOT NULL and PK recovery, index recovery (`SetSchemaInfo(stIndexes)`), datatype substitution list, "Build relations using primary keys" option.
- Stage E beyond #2 (blocked by #8); in Query mode: editing rows in the query grid, BLOB viewer, stored SQL commands.
- File > Open from Database / Save in Database, SQL Drop/Optimize/Repair scripts, DataImporter plugin against SQLite.
- MySQL, ODBC, Oracle, MSSQL connections (no servers).
- Export with a non-SQLite target (to confirm #3 is target-independent).

## Verification (2026-09-07, after commits c06d00b..4d859d1)

Clean build `lazbuild -B DBDesignerFork.lpi` + the four plugins: no errors, only the
pre-existing warnings (deprecated `SelStart` in EditorQuery, `ScanLine` in EmbeddedPdfImages,
`NEW` on untyped pointer in DBImportData, AnsiString->WideString in Weboutput).
`tests/TestSQLite.pas` and `tests/TestSQLExprShim.pas` compile (`fpc -Mdelphi -Fusrc/clx_shims -Fusrc`)
and print SUCCESS with `LD_LIBRARY_PATH` empty. `xvfb-run -a ./bin/DBDesignerFork --selftest`:
exit 0, 0 FAIL, `DBDesignerFork_Settings.ini` unchanged (WorkMode=1) — but it deleted
`DBConn.ini`, see #12 (fixed in e83199e; re-run: 0 FAIL, `DBConn.ini` md5 unchanged).
Real-display round trip from scratch in `$S/verify/` (screenshots `$S/verify/verify-*.png`):

| Entry | Status | Note |
|---|---|---|
| #1 tree click crash | verified | SQLite / Network Hosts / Oracle nodes clicked, no message box (`verify-connsel-clicks.png`) |
| #2 Transaction not set | verified | connect, Reverse Engineering (twice) and Synchronisation dialogs all open and query the schema |
| #3 inserts twice | verified | exported script byte-identical to `$S/fix03-after2.sql`; loader: zero errors, rows once (`verify/roundtrip.txt`) |
| #4 index prefix | verified | `CREATE INDEX product_name ON product (name, info);`, both explicit indexes created |
| #5 Query mode | verified | `SELECT * FROM product` -> 3 rows, "Query opened. 3 Record(s) fetched" (`verify-query-result-bottom.png`); the focused first-row cell is still drawn empty with a smaller font (known LCL DBGrid observation, not part of #5) |
| #6 AUTOINCREMENT | verified | 5 tables with AUTOINCREMENT in the DB, 5 `AutoInc` columns recovered |
| #7 relations | verified | 10 of 11 recovered (self-relation `forumpost` not recoverable), both FK-based ones with correct ON DELETE/UPDATE |
| #8 synchronisation | not done (expected) | failure mode recorded in stage E |
| #9 cosmetic | verified | title "DBDesigner Fork - Noname2" after File > New; SQL error dialog centre 981/512 = main window centre (`verify-sqlerror.png`); script whitespace tidy |
| #10 columns/indexes | verified | `$S/compare_models.py Examples/order.xml $S/verify/reveng.xml`: 12 tables, every column and index OK |
| #11 clipped check boxes | verified | all three check boxes fully visible and clickable (`verify-revdlg.png`) |

Regression found: #12 (self-test wipes `DBConn.ini`), fixed. No regression in the entries above.

## Round 2 findings

### 12. `--selftest` deletes the user's `~/.DBDesigner4/DBConn.ini`
- Status: **FIXED** (e83199e) — `TDMDB.StoreDBConns` skips its `DeleteFile` when `SettingsReadOnly`.
- Severity: **data loss** (every headless self-test run erased all saved database connections)
- Repro: with a `DBConn.ini` in place run `xvfb-run -a ./bin/DBDesignerFork --selftest`; afterwards the file is gone (directory mtime bumped, nothing else touched).
- Cause: `StoreDBConns` (`src/DBDM.pas`) does `DeleteFile(SettingsPath+'DBConn.ini')` before rewriting through `UpdateIniFile`; since 2b0bb7b `UpdateIniFile` discards the rewrite in self-test mode, so only the delete survived. Regression of 2b0bb7b (the baseline commit), not of the SQLite fixes. Complexity: **small**.

### 13. A connected SQLite database stays locked for other writers
- Severity: **minor** (limitation)
- Repro: connect (or run one query in Query mode), then `sqlite3 <db> "CREATE TABLE t(x)"` from a shell: `Error: stepping, database is locked (5)`. Database > Disconnect releases it immediately.
- Suspected cause: the shim's `TSQLTransaction` (`src/clx_shims/sqlexpr.pas`) is started for the first query and never committed while the connection is open, so SQLDB keeps a read transaction (SHARED lock) on the file; SQLite refuses writers meanwhile. A `Commit`/`CommitRetaining` after schema reads and SELECTs (or `sqlite3` `busy_timeout`) would release it. Complexity: **small**.

### 14. Reverse engineering into an open model adds duplicate tables silently
- Severity: **minor** (original DBDesigner 4 behaviour, but worth a warning)
- Repro: with `order.xml` active, Database > Reverse Engineering on the same DB, Execute. The 12 tables are added a second time (`$S/verify/order_dup.xml`: 12 table names twice, 24 relations), placed over the existing objects (`verify-main-dup.png`). A later SQL export would emit each `CREATE TABLE` twice.
- Fix idea: warn or skip/merge tables whose name already exists (the option exists in the MySQL sync path as "Apply changes to Model"). Complexity: **medium**.

### 15. File > Close via keyboard did nothing on the reverse-engineered model (unconfirmed)
- Severity: **minor**, not reproduced twice (time box)
- Repro attempt: model "reveng" active (saved, unmodified); File menu, Down x9, Return. Title stayed "DBDesigner Fork - reveng", the model stayed on screen, no dialog (`verify-afterclose.png`). Down x7 (Save As) and Down x1 (New) reach the right items in the same menu, so the item index should be right. Needs a repeat with a mouse click on "Close" before filing as a bug.

Also exercised without findings: reverse engineering with "Create Standard Inserts from table data" (7 tables with rows got their INSERTs, empty tables none); a hand-written schema with a composite primary key `parent(a,b)` and a two-column FK `child(a,b) -> parent(a,b) ON DELETE CASCADE ON UPDATE SET NULL` plus index `child_ab` — recovered exactly (PK on both columns, `FKFields` `a=a b=b`, `OnDelete=1 OnUpdate=2`, index with both columns, `qty` default `1`); HTMLReport plugin on the reverse-engineered model (report written, 36 tables/sections); Table Editor on a reverse-engineered table (`verify-tableeditor.png`); File > Save As of the reverse-engineered model. Not reached: re-export after editing in the Table Editor, File > Open of the saved model, "Store model in database".
