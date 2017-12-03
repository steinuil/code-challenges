input = 312051


# --- Part One ---
ring = catch :found do
  x = 0
  loop do
    if ((x * 2) + 1) ** 2 > input
      throw(:found, x)
    end
    x += 1
  end
end

side = ((ring * 2) + 1)
prev_side = (((ring - 1) * 2) + 1)

half_side = (side / 2.0).floor

top_right = (prev_side ** 2) + (side - 1)
top_left = top_right + (side - 1)
bottom_left = top_left + (side - 1)
bottom_right = bottom_left + (side - 1)

[((prev_side ** 2)..(top_right)),(top_right..top_left), (top_left..bottom_left), (bottom_left..bottom_right)].each do |r|
  if r.include? input
    path =
      case input <=> r.first + half_side
      when 0
        ring
      when 1
        input - (r.first + half_side) + ring
      when -1
        (r.first + half_side) - input + ring
      end
    puts "Part One: #{path}"
    break
  end
end


# --- Part Two ---
last_ring = [ 1, 2, 4, 5, 10, 11, 23, 25 ]
2.upto(Float::INFINITY) do |ring_n|
  side = (ring_n * 2)
  curr_ring = [ last_ring.last + last_ring.first ]
  curr_ring << curr_ring[0] + last_ring.last + last_ring[0..1].sum
  perimeter = side * 4

  edge = 0
  idx = 1
  3.upto(perimeter - 2) do |n|
    x = curr_ring.last
    case (side * edge) + side - n
    when 1
      x += last_ring[idx - 1] + last_ring[idx]
    when 0
      x += last_ring[idx]
    when -1
      x += curr_ring[-2] + last_ring[idx] + last_ring[idx + 1]
      edge += 1
      idx += 1
    else
      x += last_ring[(idx - 1)..(idx + 1)].sum
      idx += 1
    end
    curr_ring << x
  end

  curr_ring << curr_ring.last + curr_ring.first + last_ring[-2..-1].sum
  curr_ring << curr_ring.last + last_ring.last + curr_ring.first

  curr_ring.each do |n|
    if n > input
      puts "Part Two: #{n}"
      exit
    end
  end

  last_ring = curr_ring
end
