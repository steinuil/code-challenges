require 'set'

Point = Struct.new :name, :x, :y
coords = []

letters = Enumerator.new do |en|
  ('a'..'z').each { |l| en.yield l }
  ('A'..'Z').each { |l| en.yield l }
end

File.foreach('day-06.input').with_index do |l, i|
  poi = l.split ', '
  coords << Point.new(letters.drop(i).first, Integer(poi[0]), Integer(poi[1]))
end

grid = Array.new(400) { Array.new(400, nil) }

coords.each do |c|
  grid[c.y][c.x] = c.name
end

def dist p1, p2
  out =
    if p1.x > p2.x
      p1.x - p2.x
    else
      p2.x - p1.x
    end
  
  out +=
    if p1.y > p2.y
      p1.y - p2.y
    else
      p2.y - p1.y
    end

  out
end

grid.each.with_index do |row, y|
  row.each.with_index do |cell, x|
    next if cell
    ps = coords.inject({ d: 1000, points: []}) do |acc, p|
      d = dist p, Point.new(nil, x, y)
      if d < acc[:d]
        { d: d, points: [p] }
      elsif d == acc[:d]
        points = acc[:points]
        points.push p
        { d: d, points: points }
      else
        acc
      end
    end

    if ps[:points].count < 2
      row[x] = ps[:points][0].name
    end
  end
end

non_finite = Set.new

400.times { |x| non_finite << grid.first[x] }
400.times { |x| non_finite << grid.last[x] }
400.times { |y| non_finite << grid[y].first }
400.times { |y| non_finite << grid[y].last }

largest_finite = grid.map.with_object({}) do |row, seen|
  row.map do |p|
    next unless p
    next if non_finite.include? p
    seen[p] ||= 0
    seen[p] += 1
  end
end.max_by { |(k, v)| v }[0]

puts largest_finite

def total_distance coords, x, y
  coords.inject(0) { |acc, c| acc + (x - c.x).abs + (y - c.y).abs }
end

area = 0

3000.times do |y|
  3000.times do |x|
    if total_distance(coords, x - 1000, y - 1000) < 10_000
      area += 1
    end
  end
end

puts area