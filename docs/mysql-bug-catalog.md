# MySQL database round-trip: bug catalog

Date: 2026-09-07, branch `a3`, binary built with `lazbuild DBDesignerFork.lpi`.
Server: MySQL 8.0.46 on 127.0.0.1:3306 (user `bpsa`), client library
`/usr/lib/x86_64-linux-gnu/libmysqlclient.so.21`, FPC 3.2.2 (`mysql80conn` available).
Model under test: `bin/Examples/order.xml` (14 tables: 12 own + 2 linked
`Employee`/`News`, 7 auto-increment PKs, 11 relations, 2 explicit indexes,
3 tables with table type InnoDB).

Scratch files (screenshots, logs, scripts) live in
`/tmp/claude-1001/-home-bpsa-app-dbdesigner-fork/e506f9cb-2d39-419c-bc88-17e433cbec7a/scratchpad/mysql/`
(referred to as `$S` below; the SQLite round's `compare_models.py` is in `$S/../sqlite/`).
Nothing there is in the repo.

Repeatable part (stages B and D, no display needed):
`tests/mysql-roundtrip.sh <script.sql> <database>` — drops and recreates the
database, loads the script with the `mysql` CLI (`--force`, credentials from
`MYSQL_USER`/`MYSQL_PASSWORD`/`MYSQL_HOST`/`MYSQL_PORT`, defaults bpsa/bpsa/127.0.0.1/3306),
prints load errors, then per table: columns (type, nullable, default, key, extra =
auto_increment), primary key, foreign keys with ON UPDATE/DELETE rules, indexes
with uniqueness and prefix lengths, engine and row count.

Diagnosis needed two **temporary, uncommitted** changes in
`src/clx_shims/sqlexpr.pas` (reverted with `git checkout --` afterwards; both are
described in #1 and #2 so the fix agent can do them properly). Without #1 nothing
connects; without #2 stages C, D and the second run of E die in the connection
dialog. Everything below was observed with those two edits in place unless stated.

## Round-trip status

| Stage | What | Status |
|---|---|---|
| A | Export `order.xml` to SQL create script, target "My SQL" (File > Export > SQL Create Script) | **works** (after #3) — `$S/fix01-order_mysql.sql`: the 3 InnoDB tables end in `ENGINE=InnoDB`. Before: `$S/order_mysql.sql` had `TYPE=InnoDB` |
| B | `mysql dbdtest < script` | **works** (after #3) — `tests/mysql-roundtrip.sh $S/fix01-order_mysql.sql dbdtest` (`$S/fix01-roundtrip.txt`): zero errors, 12 tables all `ENGINE=InnoDB`, both indexes (`product_ean` unique, `product_name(name, info(100))`), FKs, 5 auto_increment columns, rows loaded once. Before: 3x `ERROR 1064 ... near 'TYPE=InnoDB'`, 9 of 12 tables (`$S/roundtrip_A.txt`) |
| C | New MySQL connection + connect + reverse engineer | **works** (after #1, #2; `$S/fix01-revdlg.png`, `$S/fix01-main_rev.png`). Before: crash without the shim edits (#1, #2); with them it worked — the connection editor (driver MySQL, host, port 3306, database, user, password; `$S/conned2.png`) and connect work (server shows the session), the Reverse Engineering dialog lists the 12 tables (`$S/revdlg.png`) and Execute adds them with relations (`$S/main_rev.png`). The connection was not written to `DBConn.ini` (#6) |
| D | Compare reverse-engineered model with original | **mostly works** (re-run after the fixes: `$S/fix01-reveng.xml`, `$S/fix01-compare.txt`, same result) — `$S/reveng.xml` vs `Examples/order.xml` with `compare_models.py` (`$S/compare.txt`): 12 tables, all columns by name/type/params/PK/NOT NULL/default; **AutoInc lost on all 5 auto-increment PKs** (#4); the UNIQUE index `product_ean` comes back as a plain INDEX (#5); 10 of 11 relations (self-relation `forumpost -> forumpost` not recoverable, as with SQLite), all with `CreateRefDef=1` / NO ACTION instead of the original's RESTRICT/CASCADE (the "Build Relations based on Primary Keys" heuristic never reads the FK metadata; #7) |
| E | Database Synchronisation: model -> empty `dbdtest2`, then modified model -> same db | first run **works** after #1-#3: all 12 tables created, `SHOW CREATE TABLE` shows `ENGINE=InnoDB`, no error box (`$S/fix01-sync2.png`); the second (modified-model) run was not repeated after the fix. Before: first run **partly works** (needs only #1): 9 of 12 tables created with PKs, indexes, FKs and standard inserts (`$S/sync4_c.png`); the 3 InnoDB tables fail with the `TYPE=InnoDB` error box each time (#3, `$S/sync_err1.png`, `$S/sync_err2.png`) and the sync continues. Second run with `$S/order_mod.xml` (`productgroup.groupname` -> `VARCHAR(60) NOT NULL`, new table `synctest`): **crashes on opening the dialog without #2** ("List index (3) out of bounds", `$S/sync2_msg.png`); with #2 it reports "9 Table(s) in Database, 15 Table(s) in Model", creates `synctest`, alters `groupname` to `varchar(60) NOT NULL` (verified with `SHOW COLUMNS`), and again fails the 3 InnoDB tables (`$S/sync2_errs.png`, `$S/sync2_2c.png`) |
| F | Query mode, `SELECT * FROM product` and an invalid statement | **works** (needs only #1; re-checked after the fix, `$S/fix01-query2c.png`) — grid with the 3 rows, status "Query opened. 3 Record(s) fetched" (`$S/query1_c.png`); `SELECT * FROM nosuch` -> "TMySQL80Connection : Error executing query: Table 'dbdtest.nosuch' doesn't exist" (`$S/query_err.png`) |

## Entries

### 1. No MySQL connector is linked: every MySQL connection fails before contacting the server
- **FIXED** (branch a3, see notes-to-myself.md "Fix: MySQL 8 connector, schema query field loss, ENGINE="). Cause: `sqlexpr.pas` linked only `SQLite3Conn`, so SQLDB had no `MySQL 5.7` connector registered for the `mysql` mapping. Now links `MySQL80Conn` (maps to `'MySQL 8.0'`) plus the new `src/clx_shims/mysqllib.pas`, which probes `libmysqlclient.so.21` before the unversioned `.so` at start-up.
- Severity: **crash** (blocks C, D, E, F)
- Repro: Database > Connect to Database, pick/create a MySQL connection, Connect (or any MySQL action).
- Cause (certain): `src/clx_shims/sqlexpr.pas` `uses ... SQLite3Conn, SQLiteLib;` only. `UpdateConnectorType` maps `DriverName` containing `mysql` to `ConnectorType := 'MySQL 5.7'`, but no `mysqlXXconn` unit is in the `uses` clause (nor anywhere in the .lpi), so SQLDB has no `MySQL 5.7` connector registered; the `try ... except` around the assignment hides that at form-load time and it surfaces at `Open`. `grep -rn mysql57conn\|mysql80conn src/ DBDesignerFork.lpi` finds nothing. The same holds for `Oracle`, `MSSQLServer`, `Firebird`, `ODBC`, `PostgreSQL` (none linked).
- Temporary change used for this diagnosis (works against 8.0.46 with `libmysqlclient.so.21`, no `LD_LIBRARY_PATH`): `uses ..., SQLite3Conn, SQLiteLib, MySQL80Conn;` and `NewType := 'MySQL 8.0'`. `mysql57conn` would need `libmysqlclient.so.20`, which is not installed; `mysql80conn` links `libmysqlclient.so.21`. Both units exist in `/usr/lib/fpc/3.2.2/units/x86_64-linux/fcl-db/`. Caching_sha2_password authentication worked out of the box (the client library handles it).
- Fix idea: link `MySQL80Conn` (Linux) and map to `'MySQL 8.0'`; optionally try `mysql57conn` when only `.so.20` exists, the way `sqlitelib.pas` probes library names. Complexity: **small**.

### 2. "List index (3) out of bounds" when opening Reverse Engineering / Synchronisation on a non-empty MySQL database (schema queries return misaligned fields)
- **FIXED**. Cause: bare `NULL AS x` columns come back as `MYSQL_TYPE_NULL`, which `mysqlconn.inc` does not map to a FieldDef, so the dataset silently lost 3/4/4 columns and positional reads misaligned. The three MySQL branches of `TSQLDataSet.SetSchemaInfo` now use `CAST(NULL AS CHAR) AS x`; `tests/TestMySQLShim.pas` asserts 5/14/11 fields and the values by position. The `SHOW KEYS` audit (positional reads after `Packed`) is not done - none of the current reads is after it.
- Severity: **crash** — an exception box, the dialog is abandoned (blocks C, D and the second run of E)
- Repro (with #1 fixed): connect to `dbdtest` (12 tables), Database > Reverse Engineering (or Database Synchronisation when the db already has tables) > choose the connection > Connect. Exception "List index (3) out of bounds. Press OK to ignore..." (`$S/reveng_msg.png`, `$S/sync2_msg.png`); after OK no dialog appears.
- Cause (verified with a standalone program, `$S/TestMySQLShim.pas` -> `$S/shimtest.txt`): the shim's MySQL schema queries (`TSQLDataSet.SetSchemaInfo` in `src/clx_shims/sqlexpr.pas`) pad the dbExpress column layout with bare `NULL AS RECNO, NULL AS CATALOG_NAME, NULL AS SCHEMA_NAME, ... NULL AS COLUMN_SUBTYPE ... NULL AS FILTER`. MySQL types those columns `MYSQL_TYPE_NULL`; FPC's `mysqlconn.inc` `AddFieldDefs` only adds a FieldDef when `MySQLDataType` recognises the type, so those columns are silently **dropped**: `stTables` comes back with 2 fields (TABLE_NAME, TABLE_TYPE) instead of 5, `stColumns` with 10 instead of 14, `stIndexes` with 7 instead of 11. `TDMDB.GetDBTables` (`src/DBDM.pas`, generic branch) reads `Fields[3]` -> out of bounds. Worse, the remaining fields are read from the wrong buffer offsets: `stIndexes` rows come out as `|PRIMARY|BTREE|65|...` (INDEX_NAME in COLUMN_NAME etc.) and iterating `stTables`/`stColumns` rows raised an access violation in the test program. The same connector behaviour hits `SHOW KEYS` (MySQL 8 has a NULL-typed `Packed` column; the test got "List index (14) out of bounds" walking its 15 columns), which `EERMySQLReverseEngineer` and the synchronisation read by position (`Fields[2]`, `Fields[4]` are before `Packed`, so they happened to work here).
- Temporary change used for this diagnosis: in the same three MySQL queries replace every `NULL AS x` with `CAST(NULL AS CHAR) AS x` (this makes all 5/14/11 fields appear and the values line up; `$S/shimtest2.txt`). Reverted afterwards.
- Fix idea: typed NULLs (`CAST(NULL AS CHAR)`, or `0 AS RECNO`) in the MySQL branches of `SetSchemaInfo`; audit every positional `Fields[n]` read after a `SHOW KEYS` / `SHOW TABLE STATUS` for MySQL 8 column changes (`Packed`, `Visible`, `Expression`) and prefer `FieldByName`. Complexity: **small** for the shim, **medium** for the audit.

### 3. "My SQL" create script and synchronisation emit `TYPE=InnoDB` (MySQL 4 syntax); MySQL 8 rejects it
- **FIXED**. Cause: `TEERTable.GetSQLCreateCode` (`src/EERModel.pas`) wrote the pre-5.5 `TYPE=` keyword; the synchronisation uses the same function. Now `ENGINE=InnoDB|MEMORY|MERGE` (HEAP -> MEMORY, BDB -> InnoDB, ISAM -> no clause = default MyISAM). The other table options were left as they are (they loaded fine).
- Severity: **wrong result** (3 of 12 tables missing after stage B; the same 3 fail in every synchronisation run with an error box each)
- Repro: File > Export > SQL Create Script, target "My SQL", save; `webserver`, `webpageclick`, `weblog` (TableType 1 = InnoDB in the model) end with `TYPE=InnoDB;` (`$S/order_mysql.sql` lines 27, 50, 61). `mysql < script`: `ERROR 1064 ... near 'TYPE=InnoDB'` x3 (`$S/roundtrip_A.txt`). Database Synchronisation shows the same statement in "ERROR while executing Query" (`$S/sync_err1.png`).
- Cause: `TEERTable.GetSQLCreateCode` (`src/EERModel.pas` ~9231-9245) writes `TYPE=` + `InnoDB|HEAP|BDB|ISAM|MERGE` when `DatabaseType = 'My SQL'`. `TYPE=` was removed in MySQL 5.5; `ENGINE=` has been accepted since 4.0.18.
- Fix idea: emit `ENGINE=` (and map HEAP -> MEMORY, drop BDB/ISAM which no longer exist). Complexity: **small**. The table options block (`TableOptions`: DelayKeyTblUpdates, PackKeys, RowChecksum, ...) should be checked at the same time; `CHECKSUM=1` did load fine.

### 4. MySQL reverse engineering never sets `AutoInc`
- Severity: **wrong result** (a second export/sync of the recovered model creates the PKs without `AUTO_INCREMENT`)
- Repro: stage C/D. All 5 auto_increment PKs in `dbdtest` (`SHOW COLUMNS` Extra = `auto_increment`) come back with `AutoInc="0"` (`$S/compare.txt`: `creditcard`, `onlinecustomer`, `onlineorder`, `product`, `productgroup`).
- Cause: `TDMDBEER.EERMySQLReverseEngineer` (`src/DBEERDM.pas` ~553-576) reads `SHOW FIELDS` columns 0-4 (Field, Type, Null, Key, Default) and hard-codes `theColumn.AutoInc:=False;` (line ~572); column 5 (`Extra`) is never inspected. The SQLite path (fixed in the last round) does detect it.
- Fix idea: `AutoInc := Pos('auto_increment', LowerCase(Fields[5].AsString)) > 0`. Complexity: **small**.

### 5. MySQL reverse engineering turns UNIQUE indexes into plain indexes
- Severity: **wrong result**
- Repro: stage D; `product_ean` is `UNIQUE KEY` in the db, the recovered model has `IndexKind=1` (INDEX) instead of 2 (UNIQUE) (`$S/compare.txt`, `product ... indexes DIFFER`).
- Cause: `EERMySQLReverseEngineer` `SHOW KEYS` loop (`src/DBEERDM.pas` ~618-660) only distinguishes `PRIMARY` (`Fields[2]`) from everything else (`ik_INDEX`); `Non_unique` (`Fields[1]`) is not read. The `Sub_part` prefix length is not read either (the `(100)` on `info` is lost; `compare_models.py` does not check `LengthParam`, verify by re-exporting).
- Fix idea: `if Fields[1].AsString = '0' then ik_UNIQUE_INDEX`; store `Sub_part` in `LengthParam`; use `FieldByName` because of #2. Complexity: **small**.

### 6. Connection created in the Database Connection Editor is not persisted to `~/.DBDesigner4/DBConn.ini`
- Severity: **wrong result** (the connection disappears on the next start; had to be added by hand for the later stages)
- Repro: Database > Connect to Database > New Database Connection, fill name/host/database/user/password, OK — the selector lists it (`$S/connsel2.png`) and it connects — then kill the app (or close it after the connection was used). `DBConn.ini` still has only `[OrderSQLite]`; `grep -c OrderMySQL` = 0 while the app was running and after `kill <pid>`. Not verified whether a clean File > Exit writes it (my Exit attempt via keyboard did not close the app; see "Not tested"). The SQLite catalog notes the earlier session "never saved one" either.
- Suspects: `TDMDB.StoreDBConns` (`src/DBDM.pas` ~300-350) is only called at shutdown / from the editor's OK path through `UpdateIniFile` (the `--selftest` read-only change in `MainDM`); check that `SettingsReadOnly` is false in normal runs and that the editor's OK calls `StoreDBConns`. Complexity: **small** once located. A hand-written entry that works (`DriverName=MySQL`, `HostName=127.0.0.1`, `Port=3306`, `Database=dbdtest`, `User_Name=bpsa`, `Password=bpsa`) is left in `~/.DBDesigner4/DBConn.ini` as `[OrderMySQL]` (the original file is `$S/DBConn.ini.bak`).

### 7. Relations are guessed by name, FK metadata is ignored (RefDef/ON DELETE/UPDATE lost, self-relation missing)
- Severity: **limitation** (same as SQLite before its native `pragma_foreign_key_list` path)
- Repro: stage D. All 10 recovered relations have `CreateRefDef=1` and `OnDelete=3 OnUpdate=3` (NO ACTION) although `dbdtest` has real FKs with RESTRICT/CASCADE (`$S/roundtrip_fixed.txt`); `forumpost.idforumpost_parent -> forumpost` is not found; "Use Native Selects to retrieve Relations" is greyed out in the dialog.
- Suspects: `EERMySQLReverseEngineer` only calls `EERReverseEngineerMakeRelations` (name/PK heuristic). MySQL 8 offers `information_schema.KEY_COLUMN_USAGE` + `REFERENTIAL_CONSTRAINTS` (the query in `tests/mysql-roundtrip.sh` returns exactly what is needed). Complexity: **medium** (mirror the SQLite implementation with `SkipExisting`).

### 8. Synchronisation continues after a failed CREATE TABLE and shows one modal error box per statement
- Severity: **cosmetic / usability**
- Repro: stage E; each of the 3 InnoDB tables raises an "ERROR while executing Query" box (`$S/sync_err1.png`) that must be dismissed; the progress memo still says "Create non existing table webserver" as if it had worked (`$S/sync4_c.png`); the tables are simply missing afterwards.
- Suspects: `TDMDBEER.EERMySQLSyncDB` (`src/DBEERDM.pas` ~2700-2760) `try ExecSQL except MessageDlg` per statement. Fine once #3 is fixed; a summary line in the progress memo would be enough. Complexity: **small**.

### 9. Tables created by synchronisation lose column and table comments
- Severity: **cosmetic / wrong result (minor)**
- Repro: after stage E `SHOW CREATE TABLE product` in `dbdtest2` has no `COMMENT` clauses, while the same table created from the exported script in `dbdtest` has `COMMENT 'The AutoIncrement ID Field'` etc. (`$S/shimtest.txt` "show create" vs `mysql dbdtest2 -e "SHOW CREATE TABLE product\G"`).
- Suspects: the sync builds its own CREATE/ALTER text in `EERMySQLSyncDB` instead of `GetSQLCreateCode` with comments on. Not checked further. Complexity: **small/medium**.

## Not tested
- Clean File > Exit and whether it writes the new connection (#6); the keyboard path `File` + `End` + `Return` did not close the app.
- Reverse engineering with "Create Standard Inserts from table data", datatype substitution variants, "Based on Tablenames and ID-Fieldnames" relation mode.
- `SHOW TABLE STATUS` / table options round trip (only noticed `Create_time` rendered as `7-9-26`).
- Synchronisation "Apply changes to Model" direction, "Don't delete existing Tables" unchecked (DROP path), "Synchronise Standard Inserts", column rename via `PrevColName`.
- Open from Database / Save in Database (`DBDesigner4` table), Table Data editor in query mode, plugins against MySQL.
- Reserved words (`date`, `action` columns loaded unquoted without error on 8.0.46; `weblog.date` is fine because `date` is non-reserved), `utf8mb4` with non-ASCII data, `BINARY` column attribute round trip (it loads; the model's `BINARY` option was not compared).
- MySQL 5.7 client (`mysql57conn`/`libmysqlclient.so.20`) — not installed.
- Error path when the server is down / wrong password (only the happy path was exercised).

## Driving notes (for the fix agent)
- Database menu opens at client (185,12); keyboard `Down` x1 Connect, x2 Disconnect, x3 Database Synchronisation, x4 Reverse Engineering. Display menu at (115,12), `Down` x2 = Query Mode. File menu: `Down` x1 New, x7 Save As.
- Connection selector at +608+271: rows at y 62/84, Connect (682,237). The connection editor opens with driver MySQL preselected when "All Connections" is selected; port box shows 3306 greyed.
- Reverse Engineering dialog 573x627 at +694+254: Execute (425,573), Close (515,573). Synchronisation dialog 395x518 at +783+308: Execute (250,460).
- Error boxes: parse the *absolute* geometry (last `+x+y` on the `xwininfo -root -tree` line), OK is at (width-55, height-27). `import`/`xwininfo` fail with "Resource temporarily unavailable" while a GTK menu is open.
- `pkill -f "DBDesignerFork <path>"` kills your own `bash -c` (exit 144); use `pgrep -x DBDesignerFork | xargs kill`.
- The app reopens the last file at start (`ReopenLastFile=1`), so File > New before reverse engineering into a fresh model.
