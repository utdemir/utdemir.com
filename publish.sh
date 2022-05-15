#!/bin/sh

set -o errexit
set -o pipefail
set -o nounset

function trace() {
    echo "! $@" >&2;
    $@
}

S3_BUCKET="utdemir-com-src"
CF_DISTRIBUTION="E32F9VIT9BGS79"

trace ./generate.sh
trace aws s3 sync --delete _out s3://"$S3_BUCKET"
trace aws cloudfront create-invalidation --distribution-id "$CF_DISTRIBUTION" --paths "/*"

echo "Done."
