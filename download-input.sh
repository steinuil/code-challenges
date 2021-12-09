#!/bin/sh
dir="advent-of-code-$2"
test -e $dir || mkdir $dir
curl "https://adventofcode.com/$2/day/$3/input" --cookie "session=$1" > "$dir/day_`printf '%02d' $3`.input"
