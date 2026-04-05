
# DBDesigner Fork ![License: GPL v2](https://img.shields.io/badge/License-GPLv2-blue.svg)

**DBDesigner Fork** is an open-source visual database design and modeling tool (Entity-Relationship / EER diagram editor). It is a community fork of the original **DBDesigner 4**, created by fabFORCE (Mike).

![How to Install BPSA](docs/1.jpeg?raw=true)

## Overview

DBDesigner Fork provides a full-featured graphical environment for designing and managing relational database schemas. It allows you to visually create Entity-Relationship diagrams and generate SQL scripts, reverse-engineer existing databases, and much more.

## Key Facts

| Aspect | Details |
|---|---|
| **Language** | Object Pascal (Free Pascal / Lazarus LCL) — originally Delphi 7 / CLX |
| **License** | GNU General Public License v2 (GPLv2) |
| **Original** | DBDesigner 4 (v4.0.2.92) by fabFORCE |
| **Fork versions** | Fork 1.0 (Sep 2006) → Fork 1.5 (Oct 2010) |
| **Original platforms** | Windows (Delphi 7) and Linux (Kylix 3) |
| **Current platform** | Linux (Lazarus/FPC) — Windows and macOS possible |
| **Codebase size** | ~175,000 lines of Pascal source code |

## Features

- **Visual database modeling** — Design Entity-Relationship (EER) diagrams with tables, fields, relations (1:1, 1:n, n:m), regions, notes, and images.
- **SQL script export** — Generate CREATE TABLE scripts from the visual model.
- **Reverse engineering** — Import existing database schemas into visual models.
- **Database connectivity** — Connect to MySQL, Oracle, MS SQL Server, SQLite, and ODBC databases.
- **XML model storage** — Models are saved as XML files; also supports ERwin 4.1 import.
- **Query editor** — Visual SQL query builder with drag-and-drop.
- **Synchronization** — Sync models with live databases.
- **PDF generation** — Embedded PDF export of diagrams.
- **Plugin system** — Extensible via plugins (HTML Report, Data Importer, Simple Web Front-end, etc.).
- **Multi-language support** — Translation files for internationalization.

## Project Structure

```
DBDesignerFork/
├── DBDesignerFork.lpi     # Lazarus project file
├── DBDesignerFork.lpr     # Main program source
├── README.md
├── src/                   # Core application source
│   ├── *.pas, *.lfm, *.xfm   # Main form, EER model engine, editors,
│   │                          #   palettes, options, etc.
│   ├── DBDesigner4.inc        # Shared compiler defines
│   ├── clx_shims/             # CLX → LCL compatibility shim layer
│   └── EmbeddedPDF/           # Built-in PDF document generation library
├── tests/                 # Test programs
│   ├── TestSQLExport.lpi      # Test project file
│   └── Test*.pas              # Unit test sources
├── docs/                  # Documentation
│   ├── port-to-lazarus.md     # Detailed porting guide
│   ├── port-to-lazarus-task-list.md  # Porting task checklist
│   └── *.txt                  # License texts, build instructions
├── Plugins/               # Plugin projects
│   ├── DataImporter/          # Data import tool
│   ├── Demo/                  # Demo/example plugin
│   ├── HTMLReport/            # HTML report generator
│   └── SimpleWebFront/       # Simple web front-end generator
├── bin/                   # Runtime files and compiled binaries
│   ├── Data/                  # Configuration, settings, translations
│   ├── Doc/                   # User documentation (HTML + PDF manual)
│   ├── Examples/              # Example model files (XML)
│   ├── Gfx/                   # Graphics: cursors, icons, splash screen
│   └── dbxoodbc/              # Open ODBC DBExpress driver
├── lib/                   # Compiled unit output directory
├── test-base/             # Test XML models and SQL export reference files
├── SynEdit_clx_original/  # Original Delphi-era SynEdit source (reference only)
└── archive/               # Archived Delphi project files
```

## 🚀 Porting to Free Pascal / Lazarus

**The primary goal of this repository is to port DBDesigner Fork from Delphi/Kylix to [Free Pascal (FPC)](https://www.freepascal.org/) and the [Lazarus IDE](https://www.lazarus-ide.org/).**

### Why?

- **Delphi 7 and Kylix 3 are long discontinued.** Building the original source requires proprietary, legacy tools that are increasingly difficult to obtain and run on modern systems.
- **Free Pascal and Lazarus are free, open-source, and actively maintained.** They support Windows, Linux, macOS, and many other platforms — a natural fit for a GPLv2 project.
- **Preserve and modernize a valuable tool.** DBDesigner Fork remains useful for database design, and porting it ensures it can continue to be built, improved, and used by the community.

### Porting Challenges

The main areas that require attention during the port include:

1. **CLX → LCL migration** — The original project uses Borland's CLX (cross-platform component library). This needs to be migrated to Lazarus's LCL (Lazarus Component Library). Form files (`.xfm`) will need to be converted to Lazarus format (`.lfm`).
2. **Delphi-specific units and APIs** — Some Delphi-specific units (e.g., `DBXpress` database drivers) need to be replaced with FPC/Lazarus equivalents or open-source alternatives.
3. **SynEdit component** — The bundled SynEdit version is Delphi-era; Lazarus ships with its own maintained SynEdit package that should be used instead.
4. **Database connectivity** — The DBExpress driver architecture needs to be replaced (e.g., with SQLDB, ZeosLib, or similar FPC-compatible database access libraries).
5. **Plugin system** — The DLL-based plugin architecture may need adjustments for cross-platform compatibility under FPC.
6. **PDF generation** — The embedded PDF library will need to be reviewed for FPC compatibility.
7. **Platform-specific code** — Any Windows-specific or Kylix-specific code paths need to be updated.

### Porting Status

✅ **All projects compile successfully under Free Pascal / Lazarus!**

| Project | Lines Compiled | Binary |
|---------|---------------|--------|
| Main Application | 56,608 | `bin/DBDesignerFork` |
| Demo Plugin | 21,793 | `bin/DBDplugin_Demo` |
| HTMLReport Plugin | 22,258 | `bin/DBDplugin_HTMLReport` |
| DataImporter Plugin | 8,836 | `bin/DBDplugin_DataImporter` |
| SimpleWebFront Plugin | 40,096 | `bin/DBDplugin_SimpleWebFront` |
| **Total** | **~150,000** | |

### AI-Assisted Porting

The porting of this codebase from Delphi/CLX to Free Pascal/Lazarus has been carried out entirely by Artificial Intelligence, with guidance and review from human developers. This includes the CLX-to-LCL migration, the creation of the compatibility shim layer, form conversions, database driver replacements, and the automated test infrastructure.

This project serves as a real-world benchmark of how far AI-assisted software engineering has evolved — from understanding legacy codebases, to making architectural decisions, to producing working, compilable code across a ~175,000-line project.

### Building with Lazarus

**Requirements:**
- Free Pascal Compiler (FPC) 3.2.2+
- Lazarus 3.0+ (for `lazbuild` command-line tool)
- Required Lazarus packages: `LCL`, `SynEdit`

**Build all projects:**
```bash
# Main application
lazbuild DBDesignerFork.lpi

# Plugins
lazbuild Plugins/Demo/DBDplugin_Demo.lpi
lazbuild Plugins/HTMLReport/DBDplugin_HTMLReport.lpi
lazbuild Plugins/DataImporter/DBDplugin_DataImporter.lpi
lazbuild Plugins/SimpleWebFront/DBDplugin_SimpleWebFront.lpi
```

All binaries are output to the `bin/` directory.

### Porting Approach

The port uses a **compatibility shim layer** ([`src/clx_shims/`](src/clx_shims/)) to minimize changes to original source files:

- **CLX → LCL shims**: Units like `QForms.pas`, `QControls.pas` etc. that re-export LCL equivalents
- **Qt shim** (`qt.pas`): Maps Qt widget types and key constants to LCL equivalents
- **Database shims** (`sqlexpr.pas`, `dbclient.pas`, `provider.pas`): Wrap FPC's SQLDB behind Delphi DBExpress-compatible interfaces
- **XML shims** (`xmlintf.pas`, `xmldoc.pas`, `xmldom.pas`): Wrap `laz2_DOM` behind Delphi XML DOM interfaces

See [`docs/port-to-lazarus.md`](docs/port-to-lazarus.md) for the detailed porting guide and [`docs/port-to-lazarus-task-list.md`](docs/port-to-lazarus-task-list.md) for the task checklist (195/229 tasks complete).

### Runtime Testing Status

✅ **Application launches and passes automated UI self-tests.**

An automated **UI Test Runner** ([`src/UITestRunner.pas`](src/UITestRunner.pas)) is included that programmatically clicks all safe menu items and buttons, catching and reporting any unhandled exceptions with full stack traces.

**Latest self-test results: 63 PASS, 0 FAIL, 79 SKIP (142 components tested)**

To run the self-test:
```bash
# Automated (exits with 0 on success, non-zero on failure)
./bin/DBDesignerFork --selftest

# Results are logged to /tmp/UITestResults.log
```

The self-test covers:
- ✅ Application launch and UI rendering
- ✅ All display/notation/style menu items
- ✅ All toolbar speed buttons (29 tool selectors)
- ✅ Palette show/hide/dock/undock operations
- ✅ Window arrangement (cascade, tile)
- ✅ Design/query mode switching
- ✅ New model creation

Areas still requiring manual or integration testing:
- Database connectivity (MySQL, PostgreSQL — SQLite verified via test programs)
- Model loading/saving through the UI
- PDF export
- Plugin loading
- Print / page setup

## License

This project is licensed under the **GNU General Public License v2**. See [`docs/Copying.txt`](docs/Copying.txt) for the full license text.

## Contributing

Contributions to the FPC/Lazarus port are highly welcome! Whether it's converting a single unit, fixing compilation issues, testing on different platforms, or improving documentation — every bit helps.
