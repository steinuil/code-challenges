input = "157,222,1,2,177,254,0,228,159,140,249,187,255,51,76,30"


# --- Part One ---
list = Array.new(256) { |i| i }

pos = 0
skip_size = 0
input.split(?,).map(&:to_i).each do |len|
  ls = []
  pos.upto(pos + len - 1) do |i|
    i -= 256 if i > 255
    ls << list[i]
  end

  i = pos
  ls.reverse.each do |el|
    list[i] = el
    i += 1
    i -= 256 if i > 255
  end

  pos += len + skip_size
  pos -= 256 if pos > 255
  pos -= 256 if pos > 255
  skip_size += 1
end

puts "Part One: #{list[0] * list[1]}"


# --- Part Two ---
list = Array.new(256) { |i| i }

pos = 0
skip_size = 0
64.times do
  input.codepoints.+([17, 31, 73, 47, 23]).each do |len|
    ls = []
    pos.upto(pos + len - 1) do  |i|
      i -= 256 if i > 255
      ls << list[i]
    end

    i = pos
    ls.reverse.each do |el|
      list[i] = el
      i += 1
      i -= 256 if i > 255
    end

    pos += len + skip_size
    until pos < 256
      pos -= 256
    end
    skip_size += 1
  end
end

puts "Part Two: #{list.each_slice(16).map { |b| b.reduce(0, :^) }.map { |x| "%02x" % x }.join}"
