#!/bin/sh
set -efu
root="${1?root url needed to scrape}"

subdirectory="$(basename "$root")"

mkdir "$subdirectory" && cd "$subdirectory"

curl "$root" | htmlq 'ul>li>a' --attribute href | while read -r chapter; do
  curl "$root/$chapter" \
    | sed -n '/<hr size="1" color="#808080">/,/<\/hr>/p' \
    | sed 's!src="bilder!src="'$root'/bilder!g' \
    | sed '1d;$d' \
      >> "$subdirectory.html"
done
