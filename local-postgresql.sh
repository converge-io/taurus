#!/bin/zsh

# -------------------------------
# - Local database setup script -
# -------------------------------

set -e;

PG_USER="pg"
PG_PASS="pg"
POLICY_DB="policies"
CONTAINER_NAME="auth-resolver-dev-db"

echo "Starting container"
$(docker container inspect "${CONTAINER_NAME}" >/dev/null 2>&1) \
  && docker start "${CONTAINER_NAME}" \
  || docker run --name "${CONTAINER_NAME}" \
                 -e POSTGRES_PASSWORD="${PG_PASS}" \
                 -e POSTGRES_USER="${PG_USER}" \
                 -d "postgres:14-alpine"

echo "Getting IP address..."
sleep 1;

HOST_IP=$(docker inspect -f '{{.NetworkSettings.IPAddress}}' "${CONTAINER_NAME}")

function exec_pg {
  PGPASSWORD="${PG_PASS}" AUTOCOMMIT="on" psql -h "${HOST_IP}" -p 5432 -U "${PG_USER}" -c "$@"
}

function db_exists {
  echo $(exec_pg "\l $1" | awk '/([0-9]+) row(s?)/ { print $1 }' | tr -d '(')
}

function create_db {
  exec_pg "create database ${1};"
  exec_pg "grant all on database ${1} to ${PG_USER};"
}

function create_user {
  exec_pg "DO \$\$ BEGIN if not exists (select 1 from pg_roles where rolname = '$1') then create role \"$1\"; end if; end \$\$;"
}

echo "Waiting for DB to come up..."
MAX_TRIES=5
i=0
success=0
while [[ ${success} -lt 1 ]]; do
  if [[ $i -gt ${MAX_TRIES} ]]; then
    echo "Timed out waiting for database to come up"
    return 1
  fi
  sleep 0.5
  exec_pg "select 1;" >/dev/null 2>&1 && success=1 || ((i++))
done

echo "Ensuring database exists"
if [[ "$(db_exists ${POLICY_DB})" == "0" ]]; then
  create_db "${POLICY_DB}"
fi

cat << EOF
# Database configuration:
export TAURUS_SOURCE_ADDR=${HOST_IP}
export TAURUS_SOURCE_PORT=5432
export TAURUS_SOURCE_USER=${PG_USER}
export TAURUS_SOURCE_PASS=${PG_PASS}
export TAURUS_SOURCE_DB=${POLICY_DB}
EOF
