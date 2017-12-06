input = File.read('day-06.input').split(?\t).map(&:to_i)
BLOCKS = 16

cycle = 0
states = []
loop do
  if i = states.index(input.pack "C*")
    puts "Part One: #{cycle}"
    puts "Part Two: #{cycle - i}"
    exit
  end

  states << input.pack("C*")

  start_idx = input.index input.max

  n = input[start_idx]
  input[start_idx] = 0
  idx = start_idx
  n.times do
    idx += 1
    idx = idx - BLOCKS if idx >= BLOCKS
    input[idx] += 1
  end
  cycle += 1
end

puts "Part One: #{cycle}"
