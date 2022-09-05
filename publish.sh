#!/bin/sh

set -o errexit
set -o pipefail
set -o nounset

S3_BUCKET="utdemir-com-src"
CF_DISTRIBUTION="E32F9VIT9BGS79"

set -o xtrace

deno task build
aws s3 sync --delete _site s3://"$S3_BUCKET"
aws cloudfront create-invalidation --distribution-id "$CF_DISTRIBUTION" --paths "/*"
