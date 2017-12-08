registers = {}
instructions = []

File.read("day-08.input").split(?\n).map do |l|
  s = l.split " "
  op = case s[1] when "inc" then :+ when "dec" then :- end
  instruction = [s[0], op, s[2].to_i, s[4], s[5], s[6].to_i]
  registers[instruction[0]] = 0
  registers[instruction[3]] = 0
  instructions << instruction
end

max = 0

instructions.each do |i|
  if registers[i[3]].send(i[4], i[5])
    registers[i[0]] = registers[i[0]].send(i[1], i[2])
    max = registers[i[0]] if registers[i[0]] > max
  end
end

puts "Part One: #{registers.values.max}"
puts "Part Two: #{max}"
