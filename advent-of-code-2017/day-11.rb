input = File.read("day-11.input").chomp.split(?,)

class Walker
  def self.steps_to x, y
    x = x.abs; y = y.abs
    if y > x
      x + ((y - x) / 2)
    else
      x
    end
  end

  def initialize
    @x = @y = 0
    @steps = 0
    @max_dist = 0
  end

  attr_reader :steps, :x, :y, :max_x, :max_y, :max_dist

  def n() move y:  2 end
  def s() move y: -2 end
  def ne() move x:  1, y:  1 end
  def se() move x:  1, y: -1 end
  def sw() move x: -1, y: -1 end
  def nw() move x: -1, y:  1 end

  private

  def move x: nil, y: nil
    @x += x if x
    @y += y if y
    @steps += 1
    if @max_dist < (dist = Walker.steps_to @x, @y)
      @max_dist = dist
    end
  end
end

w = Walker.new
input.each { |dir| w.send dir }

puts "Part One: #{Walker.steps_to w.x, w.y}"
puts "Part Two: #{w.max_dist}"
