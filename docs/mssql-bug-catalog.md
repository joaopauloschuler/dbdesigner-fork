# MS SQL Server reverse engineering over ODBC: bug catalog

Date: 2026-11-09, branch `dev`, binary built with `lazbuild DBDesignerFork.lpi` on Windows x86_64 (FPC 3.2.2). The database under test was a SQL Server 2008 R2 database reached over ODBC with the SQL Server Native Client 11.0 driver and Windows authentication: about a hundred tables in one schema, a few thousand columns, a table with several hundred columns, tables with and without a primary key, a handful of single-column foreign keys, and column names with German umlauts. Database work was read-only: `SELECT` on catalog views and the catalog procedures `sp_tables`, `sp_columns`, `sp_pkeys`, `sp_fkeys` and `sp_statistics`.

Generated models, logs and the catalog comparison live outside the repository.

Repeatable part: `DBDesignerFork --reveng-mssql=<params> --reveng-out=<file.xml>` runs the real `TDMDBEER.EERMSSQLReverseEngineer` unattended, saves the model through `TEERModel.SaveToFile`, reopens it with `TEERModel.LoadFromFile` and logs object counts to `<file.xml>.log`. `<params>` holds connection parameters separated by semicolons, the same parameters as in the connection editor, for example `Database=MyDataSource;OS Authentication=True`.

## Round-trip status

| Stage | What | Status |
|---|---|---|
| A | Connect through the ODBC connector | **works** after #1, with a data source name and also without one (host name and driver) |
| B | Table list of the MS SQL type | **works** after #2 |
| C | `EERMSSQLReverseEngineer`: columns, datatypes, NOT NULL, identity, defaults | **works** after #3 to #7 |
| D | Primary, unique and non-unique indexes | **works** after #8 |
| E | Relations from declared foreign keys | **works** after #9 |
| F | Save model, reopen with the application's loader | **works** |
| G | Literal-input test of the conversions | `tests/TestMSSQLRevEngUtils.pas`, no connection needed: identity type names (`numeric() identity` and the like), defaults, referential action names, all pass |
| H | Field-by-field comparison of the saved model with `sys.tables`, `sys.columns`, `sys.indexes`, `sys.index_columns`, `sys.identity_columns` and `sys.foreign_keys`: table and column names (umlauts included), column order, NOT NULL, identity, datatype and parameters, defaults, primary keys, index kind and column order, relation direction, column pairs and referential actions | **works**, no differences |

## Entries

### 1. No connector for MS SQL Server
- Status: **FIXED** - `src/clx_shims/sqlexpr.pas` links `odbcconn` and registers `TDBXODBCConnectionDef`, a subclass of the ODBC connection definition. The driver name `MSSQL` (and `openodbc`) selects it through `UpdateConnectorType`.
- Severity: **limitation** (established from the code: no linked connector accepted the driver name)
- Cause: only `SQLite3Conn` and `MySQL80Conn` were linked, and FPC's `mssqlconn` needs the separate DB-Library client (`dblib.dll`).
- The stock ODBC connection definition copies the whole connector parameter list into the ODBC connection string, so the dbExpress-style parameters (`HostName`, `User_Name`, `BlobSize`, `MSSQL TransIsolation`, ...) ended up as ODBC attributes. The new definition reads the parameters it knows and clears the rest. Parameters: `Database` is the data source name when `HostName` is empty; with a host name, a connection without a data source is made through the driver named by `ODBC Driver` (default `SQL Server`); `OS Authentication=True` gives `Trusted_Connection=Yes`. Both shapes were run against the test server.

### 2. `sp_tables` lists support tables as user tables
- Status: **FIXED** - `TDMDB.GetDBTables` skips `dtproperties` (Microsoft-shipped) and `sysdiagrams` (a support table of Management Studio).
- Severity: **wrong result** (extra tables in the dialog and the model)
- `sp_tables @table_type="'TABLE'"`: the double-quoted argument was examined because it should depend on `QUOTED_IDENTIFIER`. It returned the same rows with the setting on, through the same driver from a separate ADO.NET ODBC client, so it was not a defect in this environment; it is still replaced by doubled single quotes (`MSSQLGetTablesSQL`) so that the argument does not depend on the session setting.

### 3. NOT NULL inverted
- Status: **FIXED** - `NULLABLE` of `sp_columns` is 1 for a column that accepts NULL; `NotNull` now follows the value 0.
- Severity: **wrong result** (every nullable column NOT NULL and the other way round)

### 4. Identity columns not detected, and the type lookup missed `int identity` without the substitution list
- Status: **FIXED** - new `TDMDBEER.GetMSSQLDatatype` recognises the ` identity` suffix of `TYPE_NAME`, sets `AutoInc` and looks the type up by its base name.
- Severity: **wrong result** (identity lost; without the substitution list the column fell back to the default datatype silently)

### 5. Datatype mapping
- Status: **FIXED** - `GetMSSQLDatatype` keeps the substitution list, then a table of SQL Server types without a model datatype of the same name, and falls back to `TEXT`.
- Severity: **wrong result** (types silently replaced by the default datatype or by the numeric/text FLOAT, BLOB)
- Mapping used: `int` INTEGER; `smallint`, `bigint`, `bit`, `char`, `varchar`, `text`, `date`, `datetime` by name; `float` DOUBLE; `real` FLOAT; `money`, `smallmoney` DECIMAL(19, 4) and (10, 4); `nvarchar`, `sysname` VARCHAR; `nchar` CHAR; `ntext`, `xml` TEXT; `image` LONGBLOB; `binary`, `timestamp`/`rowversion` CHAR with the BINARY option; `varbinary` VARCHAR with the BINARY option; `uniqueidentifier` CHAR(36); `datetime2`, `datetimeoffset`, `smalldatetime` DATETIME (the offset of `datetimeoffset` is not representable); `sql_variant` BLOB.
- The substitution list `[MSSQL_MySQL_DatatypeSubst]` mapped `money` to FLOAT; it now says DECIMAL.

### 6. Size parameters from `sp_columns`
- Status: **FIXED**
- Severity: **wrong result** (`SMALLINT(5)`, `FLOAT(15)` and the like: sizes the SQL Server type does not have)
- `sp_columns` reports fixed precision and length for the numeric types; only `DECIMAL`/`NUMERIC` keep their precision and scale. `VARCHAR(>255)` was changed to BLOB although the database holds declared lengths up to 8000; the rule now applies only above that. `varchar(max)`, `nvarchar(max)` and `varbinary(max)` are reported by `sp_columns` as `text`, `ntext` and `image`, which map to TEXT and LONGBLOB.

### 7. Column defaults carried SQL Server's parentheses
- Status: **FIXED** - `MSSQLDefaultValue` (`src/MSSQLRevEngUtils.pas`) removes the parentheses enclosing the whole expression. The quotes go only around one complete string literal, whose doubled quotes are reduced (`'it''s'` becomes `it's`); `'a'+'b'` keeps its text. An empty string literal stays as `''`, because an empty `DefaultValue` means no default in the model.
- Severity: **limitation**

### 8. Indexes other than the primary key were not imported
- Status: **FIXED** - `sp_statistics` per table (key columns in key order, included columns are not listed by it, as designed); `ik_UNIQUE_INDEX` or `ik_INDEX`; the primary key row is skipped by the name from `sp_pkeys`. The leftover Oracle SQL inside a comment was replaced.
- Severity: **limitation**

### 9. Relations from `sp_fkeys`
- Status: **FIXED** - rows are grouped by foreign key name and child table, because `sp_fkeys` sorts by key sequence, so the rows of two multi-column keys on one table alternate. Relations go through `TEERModel.NewRelation` (the old code did not show them); source is the primary key table and destination the foreign key table, as in the other routines; `ON DELETE`/`ON UPDATE` into `RefDef` (see #12); identifying relation when all FK columns are in the child's primary key. Self references are skipped, as in the SQLite routine.
- Severity: **wrong result**
- Not exercised against the server: multi-column foreign keys and self references, since the test database has neither.
- `TEERModel.CheckAllRelations` deletes columns flagged `IsForeignKey` that it did not confirm itself, so the routine does not set the flag; `CheckAllRelations` sets it for columns that reference a primary key column.

### 10. Tables in the model were not skipped and table selection was fragile
- Status: **FIXED** - tables whose name is already in the model are skipped and counted in `RevEngSkippedTables`. Selection now also accepts `owner.table` and plain table names, which is what the general ODBC listing would produce. Catalog procedures receive the table owner, as an exact match (escaped for `sp_columns`, see #14), so that the same table name in another schema is not mixed in.
- Severity: **wrong result**
- Not exercised against the server: reverse engineering into a model that already contains tables, and a second schema. The database has one schema, so a merge across schemas could not be reproduced.

### 11. Status label used without a nil check at the end of the routine
- Status: **FIXED** - the unattended run passes no label.
- Severity: **crash**

### 12. `sp_fkeys` hides SET NULL and SET DEFAULT
- Status: **FIXED** - actions are read from `sys.foreign_keys` (`delete_referential_action`, `update_referential_action`), matched by owner and foreign key name.
- Severity: **wrong result**
- On the test server `sp_fkeys` returns `UPDATE_RULE` and `DELETE_RULE` only as 0 (cascade) or 1 (everything else), so `SET NULL` and `SET DEFAULT` would look like `NO ACTION`. When the view cannot be read, the routine falls back to the procedure's values and reports it: `RevEngFKActionsFallback` is set, the status label and the dialog say so, and the unattended run logs a warning.

### 13. `numeric` and `decimal` identity columns became TEXT
- Status: **FIXED** - `sp_datatype_info` lists `numeric() identity` and `decimal() identity`. `MSSQLSplitTypeName` removes the identity suffix and the trailing `()`; precision and scale still come from `sp_columns`.
- Severity: **wrong result**

### 14. `sp_columns` matches the table name and the owner as patterns
- Status: **FIXED** - `MSSQLEscapeLikePattern` wraps `[`, `%` and `_` of the table name and of the owner in brackets for the `sp_columns` call only. `sp_pkeys`, `sp_fkeys` and `sp_statistics` take the plain names; `sp_pkeys` with an owner containing `_` returned no rows, so it does not match patterns there.
- Severity: **wrong result** (a `_` in a table or owner name matches any character, so another table's columns could be returned)
- Tested read-only: a name ending in `_` for a table whose real name has another last character returned that table's columns unescaped and no rows with `[_]`. Bracket escaping returned all columns of real tables with `_` in their name. Backslash escaping returned no rows. `[` and `%` in table names were not exercised, the database has no table names with them. For the owner, an argument of the owner name with its last character replaced by `_` matched unescaped and returned no rows with `[_]`; the real owner has none of the three characters, so its escaped form is unchanged.

## Examined, not defects

- `TablePrefix:=i`: `i` is 0 at that point on every row (the counter of the table-reading loop is only reset), which is the default prefix.
- The options lookup compares the datatype's option names with the second field of `sp_columns` (the owner): it never matches, which leaves the options off, the correct result for SQL Server. The BINARY option is set separately.
- Build Relations by primary key: ignored for MS SQL, relations come only from declared foreign keys.
- Statements now use `EXEC` and named arguments (`@table_name=N'...'`); whether the original positional calls would also have run through the ODBC connector was not tested.

## Not tested

- Foreign keys with `SET NULL` or `SET DEFAULT`, and `numeric`/`decimal` identity columns, against a live server (the test database has none); these conversions ran only with literal inputs in `tests/TestMSSQLRevEngUtils.pas`.
- The fallback when `sys.foreign_keys` cannot be read, and the empty-table-list failure of the unattended run.
- The Reverse Engineering dialog and the connection dialogs by hand on a display: the unattended driver calls `DMDB.ConnectToDB`, `DMDB.GetDBTables` and `EERMSSQLReverseEngineer` directly.
- SQL Server authentication with a user name and password (no credentials were used).
- The general ODBC reverse-engineering method, and an ODBC-type connection to anything else.
- Linux: the shim now links `odbcconn`, which is part of FPC on Linux as well; that build was not compiled.
- `Create standard inserts` was kept off; it reads table rows.

## Limitations

- Datatypes are mapped to the MySQL-style datatypes of the model; the offset of `datetimeoffset` and the difference between `char` and `nchar` are not kept.
- Tables with the same name in different schemas collide in the model (one schema wins, the others are counted as skipped).
- User-defined alias types and CLR types become TEXT unless the substitution list names them.
- `TEERTable.CheckPrimaryIndex` rebuilds the primary index in table column order, so a primary key whose key order differs from the column order keeps the column order.
- A foreign key that references a unique index rather than the primary key gets its relation, but its column is not flagged as a foreign key, because `CheckAllRelations` confirms only primary key columns.
- `SetInterfaceToDatabase` auto-selects the routine only for the driver name `MSSQL`.
- Computed columns appear as plain columns, without their formula.
- An empty string default is stored as the two-character text `''`; with the option that adds quotes to default values, the SQL export quotes that text again.
