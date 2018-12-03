Claim = Struct.new(:id, :x, :y, :w, :h)
Rect = Struct.new(:x1, :y1, :x2, :y2)

claims = []

File.foreach 'day-03.input' do |line|
  match = line.match /^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$/
  claims << Claim.new(
    match[1].to_i,
    match[2].to_i, match[3].to_i,
    match[4].to_i, match[5].to_i
  )
end

def overlapping_squares claims, width
  field = Array.new(width) { Array.new(width, 0) }

  claims.each do |c1|
    c1.x.upto(c1.x + c1.w - 1) do |x|
      c1.y.upto(c1.y + c1.h - 1) do |y|
        field[x][y] += 1
      end
    end
  end

  return field.map { |col| col.select { |x| x > 1 }.count }.sum
end

puts overlapping_squares(claims, 1000)

__END__

def overlap_2d c1x1, c1x2, c2x1, c2x2
  if c1x1 < c2x1 and c1x2 >= c2x1
    return [c2x1, c1x2]
  elsif c2x1 < c1x1 and c2x2 >= c1x1
    return [c1x1, c2x2]
  end
end

raise unless overlap_2d(1, 4, 3, 5) == overlap_2d(3, 5, 1, 4)

raise if overlap_2d(1, 3, 4, 5)
raise if overlap_2d(4, 5, 1, 3)

raise unless overlap_2d(1, 3, 3, 5)
raise unless overlap_2d(3, 5, 1, 3)

raise unless overlap_2d(1, 5, 3, 4)
raise unless overlap_2d(3, 4, 1, 5)

def overlap_area c1, c2
  x1, x2 = overlap_2d(
    c1.x,
    (c1.x + c1.w - 1),
    c2.x,
    (c2.x + c2.w - 1)
  )

  return unless x1

  y1, y2 = overlap_2d(
    c1.y,
    (c1.y + c1.h - 1),
    c2.y,
    (c2.y + c2.h - 1)
  )

  return unless y1

  Rect.new(x1, y1, x2, y2)
end

claim1 = Claim.new(1, 1, 3, 4, 4)
claim2 = Claim.new(2, 3, 1, 4, 4)
claim3 = Claim.new(3, 5, 5, 2, 2)

raise unless overlap_area(claim1, claim2) == overlap_area(claim2, claim1)

ids = claims.map &:id

claims.each do |c1|
  claims.each do |c2|
    if overlap_area c1, c2
      ids.delete c1.id
      ids.delete c2.id
    end
  end
end

puts ids
