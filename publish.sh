#!/bin/sh

set -o errexit
set -o pipefail
set -o nounset

function trace() {
    echo "! $@" >&2;
    $@
}

CF_DOMAIN="utdemir.com"

build="$(trace nix-build --no-out-link)"
echo "$build"

netlify deploy -d "$build" --prod

echo "Done."
