input =
  File
    .read("day_01.input")
    .split("\n")
    .chunk_while { |curr, next_| curr != "" }
    .map { |e| e.reject(&:empty?).map(&:to_i) }

part1 = input.map(&:sum).max

puts "Part 1: #{part1}"

part2 = input.map(&:sum).max(3).sum

puts "Part 2: #{part2}"