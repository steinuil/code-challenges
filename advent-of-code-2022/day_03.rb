input =
  File.read(ARGV[0])
    .split("\n")

input1 = input.map { |line| [line[..line.size / 2 - 1], line[line.size / 2..]] }

A_LOWERCASE = 'a'.bytes[0]
A_UPPERCASE = 'A'.bytes[0]

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
    .map { |(l, r)| priority (l.chars & r.chars)[0] }
    .sum

puts "Part 1: #{part1}"

part2 =
  input.each_slice(3)
    .map { |g| priority g.map(&:chars).inject(&:&)[0] }
    .sum

puts "Part 2: #{part2}"
