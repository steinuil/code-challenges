init, instructions =
  File
    .read(ARGV[0])
    .split("\n\n")

init =
  init.split("\n").map(&:chars).transpose
    .select { |line| line[-1] =~ /\d/ } 
    .map { |line| line.reverse[1..].take_while { |c| c != " " } }

Instruction = Struct.new(:count, :source, :destination)

instructions =
  instructions.split("\n")
    .map { |line|
      m = line.match /move (\d+) from (\d+) to (\d+)/
      Instruction.new(m[1].to_i, m[2].to_i - 1, m[3].to_i - 1)
    }

init1 = init.map(&:dup)

instructions.each do |i|
  i.count.times do
    init1[i.destination] << init1[i.source].pop
  end
end

part1 = init1.map { |stack| stack[-1] }.compact.join

puts "Part One: #{part1}"

init2 = init.map(&:dup)

instructions.each do |i|
  init2[i.destination] += init2[i.source].pop(i.count)
end

part2 = init2.map { |stack| stack[-1] }.compact.join

puts "Part Two: #{part2}"
