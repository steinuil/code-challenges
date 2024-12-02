def main [] {
  let lists = (
    open day_01.input
    | from ssv
    | transpose
    | each { values | into int | sort }
  )

  let left = $lists.0
  let right = $lists.1

  print -n "Part 1: "
  $left | wrap left | merge ($right | wrap right)
  | each { |l| ($l.left - $l.right) | math abs } | math sum
  | print $in

  print -n "Part 2: "
  $left | each { |n| let count = $right | where { |m| $m == $n } | length; $n * $count } | math sum
  | print $in
}
