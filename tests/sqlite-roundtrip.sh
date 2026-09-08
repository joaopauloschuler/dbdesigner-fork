#!/usr/bin/env bash
# Stages B and D of the SQLite round trip (docs/sqlite-bug-catalog.md) without a display:
# build a SQLite database from a SQL create script and print a schema summary
# (tables, columns, primary keys, NOT NULL, foreign keys, indexes) that can be
# compared with the model (Examples/order.xml) or with another database.
#
# Usage: tests/sqlite-roundtrip.sh <script.sql> <output.sqlite> [--keep]
#   --keep  do not delete an existing output database first
# Exit code is 0 even when sqlite3 reports statement errors; the errors are
# printed under "== load errors ==" so they can be recorded as export bugs.
set -u
if [ $# -lt 2 ]; then
  echo "usage: $0 <script.sql> <output.sqlite> [--keep]" >&2
  exit 2
fi
script=$1
db=$2
keep=${3:-}
command -v sqlite3 >/dev/null || { echo "sqlite3 CLI not found" >&2; exit 2; }
[ -f "$script" ] || { echo "script not found: $script" >&2; exit 2; }
[ "$keep" = "--keep" ] || rm -f "$db"

echo "== load errors =="
# sqlite3 continues after errors and prints them on stderr.
sqlite3 "$db" < "$script" 2>&1 | sed 's/^/  /'
echo

tables=$(sqlite3 "$db" "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' ORDER BY name")
echo "== tables ($(echo "$tables" | grep -c .)) =="
echo "$tables" | sed 's/^/  /'
echo

for t in $tables; do
  echo "== $t =="
  echo "  columns (cid|name|type|notnull|default|pk):"
  sqlite3 "$db" "PRAGMA table_info('$t')" | sed 's/^/    /'
  fk=$(sqlite3 "$db" "PRAGMA foreign_key_list('$t')")
  if [ -n "$fk" ]; then
    echo "  foreign keys (id|seq|table|from|to|on_update|on_delete|match):"
    echo "$fk" | sed 's/^/    /'
  else
    echo "  foreign keys: none"
  fi
  idx=$(sqlite3 "$db" "PRAGMA index_list('$t')")
  if [ -n "$idx" ]; then
    echo "  indexes (seq|name|unique|origin|partial) + columns:"
    echo "$idx" | while IFS='|' read -r seq name uniq origin partial; do
      cols=$(sqlite3 "$db" "PRAGMA index_info('$name')" | cut -d'|' -f3 | paste -sd, -)
      echo "    $seq|$name|$uniq|$origin|$partial  ($cols)"
    done
  else
    echo "  indexes: none"
  fi
  # AUTOINCREMENT is only visible in the CREATE statement text.
  if sqlite3 "$db" "SELECT sql FROM sqlite_master WHERE type='table' AND name='$t'" | grep -qi autoincrement; then
    echo "  autoincrement: yes"
  else
    echo "  autoincrement: no"
  fi
  echo "  rows: $(sqlite3 "$db" "SELECT count(*) FROM \"$t\"")"
  echo
done
