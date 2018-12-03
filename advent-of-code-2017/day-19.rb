input = File.read("day-19.input").split(?\n)

start = input[0].index ?|

LETTERS = ("A".."Z").to_a

seen_letters = ""

counter = 1
pos = Struct.new(:x, :y).new start, 0
dir = :down
loop do
  case dir
  when :down
    pos.y += 1
  when :up
    pos.y -= 1
  when :right
    pos.x += 1
  when :left
    pos.x -= 1
  end

  case input[pos.y][pos.x]
  when ?+
    if dir == :right or dir == :left
      if    (LETTERS + [?|]).member? input[pos.y + 1][pos.x]
        dir = :down
      elsif (LETTERS + [?|]).member? input[pos.y - 1][pos.x]
        dir = :up
      else
        raise pos.inspect
      end
    else
      if    (LETTERS + [?-]).member? input[pos.y][pos.x + 1]
        dir = :right
      elsif (LETTERS + [?-]).member? input[pos.y][pos.x - 1]
        dir = :left
      else
        raise pos.inspect
      end
    end
  when "A".."Z"
    seen_letters += input[pos.y][pos.x]
  when " "
    puts "Part One: #{seen_letters}"
    puts "Part Two: #{counter}"
    exit
  end

  counter += 1
end
