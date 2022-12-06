input =
  File
    .read(ARGV[0])
    .chomp

part1 = input.chars.each_cons(4).find_index { |c| c.uniq.size == 4 } + 4

puts "Part One: #{part1}"

part2 = input.chars.each_cons(14).find_index { |c| c.uniq.size == 14 } + 14

puts "Part Two: #{part2}"
