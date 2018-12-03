require "set"

Disc = Struct.new(:name, :weight, :children)

$discs = {}
held_discs = Set.new

File.read("day-07.input").split(?\n).each do |l|
  fst, snd = l.split(" -> ")

  name, num = fst.split(" (")
  weight = num[0..-2].to_i
  children = snd ? snd.split(", ") : []
  $discs[name] = Disc.new(name, weight, children)
  held_discs.merge children
end


# --- Part One ---
bottom = $discs.keys.find do |x|
  not held_discs.include? x
end

puts "Part One: #{bottom}"


# --- Part Two ---
def tower d
  disc = $discs[d]
  chld = disc.children.map { |x| tower x }
  if chld.uniq.length > 1
    disc.children.map { |x| $discs[x].weight }
  end
  disc.weight + chld.sum
end

tower bottom

# solved by repl
puts "Part Two: #{1275}"
