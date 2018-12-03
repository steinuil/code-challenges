input = File.read("day-13.input").split(?\n).each.with_object([]) do |l, acc|
  idx, lvl = l.split(': ').map &:to_i
  acc[idx] = lvl
end

class Scanner
  def initialize range
    @range = range
    @pos = 0
    @dir = :inc
  end

  attr_reader :range, :pos, :dir

  def inc
    case dir
    when :inc
      @dir = :dec if pos >= range - 1
    when :dec
      @dir = :inc if pos <= 0
    end

    case dir
    when :inc then @pos += 1
    when :dec then @pos -= 1
    end
  end
end

# --- Part One ---
firewall = Array.new input.length do |i|
  Scanner.new input[i] if input[i]
end

severity = 0

input.length.times do |depth|
  if s = firewall[depth] and s.pos == 0
    severity += depth * s.range
  end

  firewall.each { |s| s.inc if s }
end

puts "Part One: #{severity}"


# --- Part Two ---
firewall = Array.new input.length do |i|
  Scanner.new input[i] if input[i]
end

firewall.each.with_index do |s, i|
  i.times { s.inc } if s
end

wait = 0
loop do
  break if firewall.none? { |s| s and s.pos == 0 }
  firewall.each { |s| s.inc if s }
  wait += 1
end

puts "Part Two: #{wait}"
