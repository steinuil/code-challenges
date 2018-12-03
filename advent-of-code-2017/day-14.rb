input = "xlqgujun"

side = 128

def knot_hash input
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

  list.each_slice(16).map { |b| "%08b" % b.reduce(0, :^) }.join
end

# --- Part One ---
free = 0
$disk = Array.new side
side.times do |n|
  hash = knot_hash "#{input}-#{n}"
  $disk[n] = hash.each_char.map do |ch|
    if ch == "1"
      free += 1
      true
    end
  end
end

puts "Part One: #{free}"


# --- Part Two ---
def walk x, y
  $disk[x][y] = false
  if y > 0 and $disk[x][y - 1]
    walk(x, y - 1)
  end
  if x < 127 and $disk[x + 1][y]
    walk(x + 1, y)
  end
  if y < 127 and $disk[x][y + 1]
    walk(x, y + 1)
  end
  if x > 0 and $disk[x - 1][y]
    walk(x - 1, y)
  end
end

regions = 0

side.times do |x|
  side.times do |y|
    if $disk[x][y]
      regions += 1
      walk x, y
    end
  end
end

puts "Part Two: #{regions}"
