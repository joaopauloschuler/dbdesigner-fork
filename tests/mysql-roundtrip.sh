#!/usr/bin/env bash
# Stages B and D of the MySQL round trip (docs/mysql-bug-catalog.md) without a display:
# (re)create a MySQL database from a SQL create script and print a schema summary
# (tables, columns, PK, NOT NULL, auto_increment, defaults, foreign keys, indexes,
# row counts) that can be compared with the model (Examples/order.xml).
#
# Usage: tests/mysql-roundtrip.sh <script.sql> <database>
# Env:   MYSQL_USER (bpsa) MYSQL_PASSWORD (bpsa) MYSQL_HOST (127.0.0.1) MYSQL_PORT (3306)
# The database is DROPPED and recreated. Exit code is 0 even when statements fail;
# the errors are printed under "== load errors ==" so they can be recorded as bugs.
set -u
if [ $# -lt 2 ]; then
  echo "usage: $0 <script.sql> <database>" >&2
  exit 2
fi
script=$1
db=$2
: "${MYSQL_USER:=bpsa}" "${MYSQL_PASSWORD:=bpsa}" "${MYSQL_HOST:=127.0.0.1}" "${MYSQL_PORT:=3306}"
command -v mysql >/dev/null || { echo "mysql CLI not found" >&2; exit 2; }
[ -f "$script" ] || { echo "script not found: $script" >&2; exit 2; }
export MYSQL_PWD=$MYSQL_PASSWORD
m() { mysql -u"$MYSQL_USER" -h"$MYSQL_HOST" -P"$MYSQL_PORT" --batch --skip-column-names "$@"; }

m -e "DROP DATABASE IF EXISTS \`$db\`; CREATE DATABASE \`$db\`" || exit 1

echo "== load errors =="
# --force continues after errors; they go to stderr.
m --force "$db" < "$script" 2>&1 | grep -v "^mysql: \[Warning\]" | sed 's/^/  /'
echo

tables=$(m -e "SELECT TABLE_NAME FROM information_schema.TABLES WHERE TABLE_SCHEMA='$db' ORDER BY TABLE_NAME")
echo "== tables ($(echo "$tables" | grep -c .)) =="
echo "$tables" | sed 's/^/  /'
echo

for t in $tables; do
  echo "== $t =="
  echo "  columns (pos|name|type|nullable|default|key|extra):"
  m -e "SELECT ORDINAL_POSITION, COLUMN_NAME, COLUMN_TYPE, IS_NULLABLE, IFNULL(COLUMN_DEFAULT,'<null>'), COLUMN_KEY, EXTRA
        FROM information_schema.COLUMNS WHERE TABLE_SCHEMA='$db' AND TABLE_NAME='$t' ORDER BY ORDINAL_POSITION" \
    | sed 's/\t/|/g; s/^/    /'
  echo "  primary key: $(m -e "SELECT GROUP_CONCAT(COLUMN_NAME ORDER BY SEQ_IN_INDEX) FROM information_schema.STATISTICS WHERE TABLE_SCHEMA='$db' AND TABLE_NAME='$t' AND INDEX_NAME='PRIMARY'")"
  fk=$(m -e "SELECT k.CONSTRAINT_NAME, k.COLUMN_NAME, k.REFERENCED_TABLE_NAME, k.REFERENCED_COLUMN_NAME, r.UPDATE_RULE, r.DELETE_RULE
             FROM information_schema.KEY_COLUMN_USAGE k JOIN information_schema.REFERENTIAL_CONSTRAINTS r
               ON r.CONSTRAINT_SCHEMA=k.CONSTRAINT_SCHEMA AND r.CONSTRAINT_NAME=k.CONSTRAINT_NAME AND r.TABLE_NAME=k.TABLE_NAME
             WHERE k.TABLE_SCHEMA='$db' AND k.TABLE_NAME='$t' AND k.REFERENCED_TABLE_NAME IS NOT NULL
             ORDER BY k.CONSTRAINT_NAME, k.ORDINAL_POSITION" | sed 's/\t/|/g')
  if [ -n "$fk" ]; then
    echo "  foreign keys (name|column|ref_table|ref_column|on_update|on_delete):"
    echo "$fk" | sed 's/^/    /'
  else
    echo "  foreign keys: none"
  fi
  idx=$(m -e "SELECT INDEX_NAME, IF(NON_UNIQUE=0,'unique','nonunique'), INDEX_TYPE,
              GROUP_CONCAT(CONCAT(COLUMN_NAME, IFNULL(CONCAT('(',SUB_PART,')'),'')) ORDER BY SEQ_IN_INDEX)
              FROM information_schema.STATISTICS WHERE TABLE_SCHEMA='$db' AND TABLE_NAME='$t' AND INDEX_NAME<>'PRIMARY'
              GROUP BY INDEX_NAME, NON_UNIQUE, INDEX_TYPE ORDER BY INDEX_NAME" | sed 's/\t/|/g')
  if [ -n "$idx" ]; then
    echo "  indexes (name|unique|type|columns):"
    echo "$idx" | sed 's/^/    /'
  else
    echo "  indexes: none"
  fi
  echo "  engine: $(m -e "SELECT ENGINE FROM information_schema.TABLES WHERE TABLE_SCHEMA='$db' AND TABLE_NAME='$t'")"
  echo "  rows: $(m -e "SELECT COUNT(*) FROM \`$db\`.\`$t\`")"
  echo
done
