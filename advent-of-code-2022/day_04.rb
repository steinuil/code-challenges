input =
  File
    .read(ARGV[0])
    .split("\n")
    .map { |line| line.split(",").map { |range| range.split("-").map(&:to_i) } }

def contains r1, r2
  r1[0] <= r2[0] && r1[1] >= r2[1]
end

part1 = input.filter { |(p1, p2)| contains(p1, p2) || contains(p2, p1) }.size

puts "Part One: #{part1}"

def has_no_overlap r1, r2
  !contains(r1, [r2[0], r2[0]]) &&
  !contains(r1, [r2[1], r2[1]]) &&
  !contains(r2, [r1[0], r1[0]]) &&
  !contains(r2, [r1[1], r1[1]])
end

part2 = input.reject { |(r1, r2)| has_no_overlap(r1, r2) }.size

puts "Part Two: #{part2}"
