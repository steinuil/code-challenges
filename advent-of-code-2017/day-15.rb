input_a = 722
input_b = 354

factor_a = 16807
factor_b = 48271

div = 2147483647

# --- Part One ---
prev_a = input_a
prev_b = input_b

count = 0
40_000_000.times do
  prev_a = (prev_a * factor_a) % div
  prev_b = (prev_b * factor_b) % div
  count += 1 if prev_a & 0xffff == prev_b & 0xffff
end

puts "Part One: #{count}"


# --- Part Two ---
prev_a = input_a
prev_b = input_b

count = 0
5_000_000.times do
  loop do
    prev_a = (prev_a * factor_a) % div
    break if prev_a & 0b11 == 0
  end
  loop do
    prev_b = (prev_b * factor_b) % div
    break if prev_b & 0b111 == 0
  end
  count += 1 if prev_a & 0xffff == prev_b & 0xffff
end

puts "Part Two: #{count}"
