require "set"

input =
  File.read("day_03.input")
    .split("\n")

input1 = input.map { |line| [line[..line.size / 2 - 1], line[line.size / 2..]] }

A_LOWERCASE = 'a'.bytes[0]
Z_LOWERCASE = 'z'.bytes[0]
A_UPPERCASE = 'A'.bytes[0]
Z_UPPERCASE = 'Z'.bytes[0]

def priority c
  b = c.bytes[0]
  if b >= A_LOWERCASE
    b - A_LOWERCASE + 1
  else
    b - A_UPPERCASE + 27
  end
end

part1 =
  input1
    .map { |(l, r)| priority (Set.new(l.chars) & Set.new(r.chars)).to_a[0] }
    .sum

puts "Part 1: #{part1}"

input2 = input.each_slice(3)

part2 =
  input2
    .map { |g| priority g.map { |r| Set.new(r.chars) }.inject(&:&).to_a[0] }
    .sum

puts "Part 2: #{part2}"
