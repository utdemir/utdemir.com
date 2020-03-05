#!/bin/sh

set -o errexit
set -o pipefail
set -o nounset

function trace() {
    echo "! $@" >&2;
    $@
}

CF_DOMAIN="utdemir.com"

# peer id of the machine hosting my website.
ipfs swarm connect /ipfs/QmUhdP7YFYnBeJHXBMxgVhwLNbZ3uBW6288UJmN7Uxq7ky

build="$(trace nix-build --no-out-link)"
echo "$build"

all_hashes="$(trace ipfs add -r -q "$build"/)"
final_hash="$(echo "$all_hashes" | tail -n 1)"

echo "/ipfs/$final_hash"

sleep 5

echo "Pinning on the home server."
trace ssh utdemir.com \
  ipfs pin add --progress "/ipfs/$final_hash"
echo

echo "Polling IPFS gateways."

gateways=(
  "https://gateway.ipfs.io"
  "https://gateway.pinata.cloud"
  "https://cloudflare-ipfs.com"
)


while [ ${#gateways[@]} -ne 0 ]; do
  for i in "${!gateways[@]}"; do
    gw="${gateways[$i]}"
    url="$gw/ipfs/$final_hash/"
    if timeout 10s curl -s --fail "$url" >/dev/null; then
      echo "Found on: $gw"
      gateways=( "${gateways[@]:0:$i}" "${gateways[@]:$(( $i+1 ))}" )
      break
    fi
  done
done

echo "Updating DNS."

cf_api="https://api.cloudflare.com/client/v4"

cf_zone="$(
    curl -s -H "Authorization: Bearer $CLOUDFLARE_API_TOKEN" -X GET \
        "$cf_api/zones" \
        | jq -r '.result[] | select(.name = "$CF_DOMAIN").id'
)"
cf_rec="$(
    curl -s -H "Authorization: Bearer $CLOUDFLARE_API_TOKEN" -X GET \
    "$cf_api/zones/$cf_zone/dns_records?type=TXT&name=_dnslink.$CF_DOMAIN" \
    | jq -r '.result[].id'
)"

curl -s -H "Authorization: Bearer $CLOUDFLARE_API_TOKEN" -X PATCH \
  -H 'Content-Type: application/json' \
  "$cf_api/zones/$cf_zone/dns_records/$cf_rec" \
  --data "{ \"content\": \"dnslink=/ipfs/$final_hash\" }" \
  | jq -e '.success' >/dev/null

echo "Done."
