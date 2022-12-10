input = File.read(ARGV[0])
            .split("\n")
            .map { |line|
              instr, arg = line.split " "
              [instr.intern, arg&.to_i]
            }

pc = 0
x = 1
checkpoints = {
  20 => nil,
  60 => nil,
  100 => nil,
  140 => nil,
  180 => nil,
  220 => nil
}
queue = nil
cycle = 0

until cycle == 220
  cycle += 1
  checkpoints[cycle] = x if checkpoints.include? cycle
  if queue.nil?
    instr, v = input[pc]
    case instr
    when :noop
      pc += 1
    when :addx
      queue = v
    end
  else
    x += queue
    queue = nil
    pc += 1
  end
end

part1 = checkpoints.to_a.map { |(c, x)| c * x }.sum

puts "Part One: #{part1}"
