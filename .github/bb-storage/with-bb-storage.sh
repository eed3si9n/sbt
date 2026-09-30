#!/usr/bin/env bash
# Usage: BB_STORAGE=<path to bb_storage> with-bb-storage.sh <command> [args...]
set -uo pipefail

config="$(dirname "$0")/storage.jsonnet"
http="${BB_STORAGE_HTTP:-127.0.0.1:8000}"
log="${RUNNER_TEMP:-${TMPDIR:-/tmp}}/bb-storage.log"

"$BB_STORAGE" "$config" > "$log" 2>&1 &
pid=$!
trap 'kill "$pid" 2> /dev/null || true' EXIT

healthy=false
for _ in $(seq 1 30); do
  if curl -sf "http://$http/-/healthy" > /dev/null; then
    healthy=true
    break
  fi
  kill -0 "$pid" 2> /dev/null || break
  sleep 1
done
if [ "$healthy" != true ]; then
  echo "bb-storage did not start"
  cat "$log"
  exit 1
fi

status=0
"$@" || status=$?

if metrics="$(curl -sf "http://$http/metrics")"; then
  echo "bb-storage gRPC calls:"
  grep '^grpc_server_handled_total' <<< "$metrics" | grep -v ' 0$' || true
else
  echo "bb-storage is no longer reachable"
  cat "$log"
  status=1
fi
exit "$status"
