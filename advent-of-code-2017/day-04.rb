input = File.read("day-04.input").split(?\n).map { |l| l.split(" ") }


# --- Part One ---
count = input.select do |p|
  p.uniq.length == p.length
end.length

puts "Part One: #{count}"


# --- Part Two ---
valid2 = input.select do |p|
  sorted = p.map { |x| x.chars.sort.join }
  sorted.all? do |x|
    sorted.one? { |y| x == y }
  end
end.length

puts "Part Two: #{valid2}"
