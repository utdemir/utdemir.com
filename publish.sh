#!/bin/sh

set -o errexit
set -o pipefail
set -o nounset

function trace() {
    echo "! $@" >&2;
    $@
}

# peer id of the machine hosting my website.
trace ipfs swarm connect /ipfs/QmUhdP7YFYnBeJHXBMxgVhwLNbZ3uBW6288UJmN7Uxq7ky

build="$(trace nix-build --no-out-link)"
echo "$build"

all_hashes="$(trace ipfs add -r -q "$build"/)"
final_hash="$(echo "$all_hashes" | tail -n 1)"

echo "/ipfs/$final_hash"

sleep 5

gateways=(
  "https://gateway.ipfs.io"
  "https://gateway.pinata.cloud"
  "https://cloudflare-ipfs.com"
)

for gw in "${gateways[@]}"; do
  url="$gw/ipfs/$final_hash/"
  echo "Polling: $url"
  while ! timeout 30s curl -s --fail "$url" >/dev/null; do sleep 5; done
  echo "Found."
done

# TODO: Set DNS
