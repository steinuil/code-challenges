input =
  File.read("day_02.input")
    .split("\n")
    .map { |line| line.split(" ") }

RPS = [:rock, :paper, :scissors]

def solve a, b
  if [[:rock, :rock], [:paper, :paper], [:scissors, :scissors]].include?([a, b])
    RPS.index(b) + 1 + 3
  elsif [[:rock, :paper], [:paper, :scissors], [:scissors, :rock]].include?([a, b])
    RPS.index(b) + 1 + 6
  else
    RPS.index(b) + 1
  end
end

part1 =
  input
    .map { |(a, b)| [a.bytes[0] - 'A'.bytes[0], b.bytes[0] - 'X'.bytes[0]] }
    .map { |(a, b)| solve(RPS[a], RPS[b]) }
    .sum

puts "Part 1: #{part1}"

CONDITION = [:lose, :draw, :win]

WIN = {
  :rock => :paper,
  :paper => :scissors,
  :scissors => :rock,
}

LOSE = {
  :rock => :scissors,
  :paper => :rock,
  :scissors => :paper,
}

def solve2 o, c
  case c
  when :win
    RPS.index(WIN[o]) + 1 + 6
  when :lose
    RPS.index(LOSE[o]) + 1
  when :draw
    RPS.index(o) + 1 + 3
  end
end

part2 =
  input
    .map { |(a, b)| [RPS[a.bytes[0] - 'A'.bytes[0]], CONDITION[b.bytes[0] - 'X'.bytes[0]]] }
    .map { |(a, b)| solve2(a, b) }
    .sum

puts "Part 2: #{part2}"
