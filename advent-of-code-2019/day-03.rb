input = File.read 'day-03.input'

#input = <<EOI
#R75,D30,R83,U83,L12,D49,R71,U7,L72
#U62,R66,U55,R34,D71,R55,D58,R83
#EOI

#input = <<EOI
#R8,U5,L5,D3
#U7,R6,D4,L4
#EOI

line1, line2 = input.split(/\R/).map { |line| line.split ',' }

Point = Struct.new :x, :y

def path_from line
  path = [Point.new(0, 0)]

  line.each do |instr|
    dir = instr[0]
    steps = instr[1..-1].to_i

    orig = path[-1]

    case dir
    when "U"
      (orig.y + 1).upto(orig.y + steps).each do |y|
        path.push Point.new(orig.x, y)
      end
    when "D"
      (orig.y - 1).downto(orig.y - steps).each do |y|
        path.push Point.new(orig.x, y)
      end
    when "L"
      (orig.x - 1).downto(orig.x - steps).each do |x|
        path.push Point.new(x, orig.y)
      end
    when "R"
      (orig.x + 1).upto(orig.x + steps).each do |x|
        path.push Point.new(x, orig.y)
      end
    end
  end

  path
end


def manhattan_distance point
  point.x.abs + point.y.abs
end


puts (path_from(line1) & path_from(line2)).drop(1).map { |p| manhattan_distance p }.min


def shortest_path l1, l2
  p1 = path_from l1
  p2 = path_from l2

  (p1 & p2)
    .drop(1)
    .map { |p| p1.index(p) + p2.index(p) }
    .min
end

puts shortest_path(line1, line2)
