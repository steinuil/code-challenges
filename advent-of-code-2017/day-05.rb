# --- Part One ---
input = File.read("day-05.input").split(?\n).map(&:to_i)
pos = 0
counter = 0

loop do
  jmp = input[pos]
  break unless jmp
  input[pos] += 1
  pos += jmp
  counter += 1
end

puts "Part One: #{counter}"


# --- Part Two ---
input = File.read("day-05.input").split(?\n).map(&:to_i)
pos = 0
counter = 0

loop do
  jmp = input[pos]
  break unless jmp
  input[pos] += (jmp >= 3 ? -1 : 1)
  pos += jmp
  counter += 1
end

puts "Part Two: #{counter}"
