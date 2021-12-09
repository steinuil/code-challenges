: count-floors ( c-addr u -- floors )
  0 rot rot
  0 ?do
    dup i + c@ 40 = if 1 else -1 then
    rot + swap
  loop
  drop ;

: basement-loop ( c-addr i floor -- i )
  2 pick c@ 40 = if 1+ else 1- then
  dup -1 = if rot 2drop 1+ else rot 1+ rot 1+ rot recurse then ;

: basement-entrance ( c-addr -- i )
  0 0 basement-loop ;

include wota.fth

s" day_01.input" snort swap dup rot
count-floors ." Part One: " . cr
basement-entrance ." Part Two: " . cr
