#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

cd "${0%/*}"

AUTHOR="Utku Demir"
RSS_DESCRIPTION="Posts from Utku Demir's homepage"
BASE="https://utdemir.com"

echo "Creating temporary directories."

tmpdir="$(mktemp -d --suffix '.blog')"
trap "rm -rf '$tmpdir'" EXIT

out="$tmpdir/out"
mkdir "$out"

echo "Reading posts & activities."

echo '$meta-json$' > "$tmpdir/meta-json-template.html"

posts=()
for p in posts/*.md; do
  slug="$(basename "$p" ".md")"
  meta="$(
    cat "$p" \
      | pandoc -f markdown -t html --template "$tmpdir/meta-json-template.html" \
      | jq -c '. + { "file": $file , "slug": $slug, "target": $target, "type": "post"  }' \
          --arg file "$p" \
          --arg slug "$slug" \
          --arg target "posts/$slug.html"
  )"
  posts+=("$meta")
done

readarray -t activities < <(cat ./activity.json | jq -c '.[] | . + { "external": true }')

items=( "${posts[@]}" "${activities[@]}" )
readarray -t items < <(echo "${items[@]}" \
  | jq -sc 'sort_by(.date) | reverse[] | select(.draft | not)')

echo "Writing posts."

mkdir "$out/posts"

for post in "${posts[@]}"; do
  file="$(echo "$post" | jq -r .file)"
  slug="$(echo "$post" | jq -r .slug)"
  target="$(echo "$post" | jq -r .target)"

  post_out="$out/$target"
  mkdir -p "$(dirname "$post_out")"

  cat "$file" \
    | pandoc \
        -f markdown+backtick_code_blocks+fenced_code_attributes \
        -t html \
        --template "templates/post.html" \
        --shift-heading-level-by 2 \
        -V "author=$AUTHOR" \
        -V "root=.." \
    > "$post_out"
done

echo "Writing index."

cat index.md \
  | pandoc -f markdown -t html \
      --template "templates/index.html" \
      --metadata-file=<( echo -n "items: "; echo "${items[@]}" | jq -sc ) \
      -V "author=$AUTHOR" \
      -V "root=." \
  > "$out/index.html"

echo "Writing the RSS feed."

cat <<EOF >> "$out/rss.xml"
<?xml version="1.0" encoding="UTF-8" ?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
<channel>
<title>$AUTHOR</title>
<description>$RSS_DESCRIPTION</description>
<link>$BASE</link>
<atom:link href="$BASE/rss.xml" rel="self" type="application/rss+xml" />
<ttl>1800</ttl>
EOF

for item in "${items[@]:0:10}"; do
 (
   title="$(echo "$item" | jq -r '.title')"
   target="$(echo "$item" | jq -r '.target')"
   date="$(date -d "$(echo "$item" | jq -r '.date')" --rfc-822)"

   if echo "$item" | jq -e '.external' > /dev/null;
   then url="$target"
   else url="$BASE/$target"
   fi

   echo "<item>"
   echo "  <title>$title</title>"
   echo "  <link>$url</link>"
   echo "  <guid>$url</guid>"
   echo "  <pubDate>$date</pubDate>"
   echo "</item>"
 ) >> "$out/rss.xml"
done

cat <<EOF >> "$out/rss.xml"
</channel>
</rss>
EOF

echo "Copying static files."
rsync -r static/ "$out"

echo "Checking link validity."
linkchecker -o csv "$out/" 2>/dev/null \
  | ( grep -v '^#' | grep -v 'urlname' || true )

echo "Saving _out."
rm -rf _out
mv -T "$out" _out

echo "Done."
