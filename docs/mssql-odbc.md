# MS SQL Server over ODBC

DBDesigner Fork can reverse engineer an MS SQL Server database into a model. What has and has not been run is listed under Project Status in the [README](../README.md#project-status).

## Connection

MS SQL Server connections use FPC's ODBC connector, so a SQL Server ODBC driver must be installed. A connection of the type `MSSQL` has these parameters:

| Parameter | Meaning |
|---|---|
| `HostName` | Leave empty to use `Database` as an ODBC data source name; otherwise the server, and a connection is made without a data source |
| `Database` | The ODBC data source name, or the database name when a host name is given |
| `ODBC Driver` | The installed ODBC driver used when a host name is given (default `SQL Server`) |
| `OS Authentication` | `True` logs in with the Windows user (`Trusted_Connection=Yes`). SQL Server logins with `User_Name` and `Password` have not been run |

## What reverse engineering reads

The reverse engineering routine for MS SQL Server reads tables, columns, datatypes, NOT NULL, identity, defaults, primary and other indexes and declared foreign keys, including their actions. It only reads the catalog and does not change the database. It maps SQL Server types to the model's MySQL-style datatypes; see [`mssql-bug-catalog.md`](mssql-bug-catalog.md) for the mapping and what was run.

## Running it unattended

The routine runs without a person at the keyboard. It connects with the given parameters, saves the model and reopens it with the application's loader, writing object counts to `<file.xml>.log`:

```bash
./bin/DBDesignerFork "--reveng-mssql=Database=MyDataSource;OS Authentication=True" "--reveng-out=file.xml"
```
