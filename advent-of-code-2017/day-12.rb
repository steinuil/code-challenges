require "set"

$input = File.read("day-12.input").split(?\n).map do |l|
  n, conns = l.split ' <-> '
  conns.split(', ').map &:to_i
end

# --- Part One ---
$orphaned = Array.new($input.length) { |i| i }
$list = Set.new

def rec inp
  $list << inp
  $orphaned.delete inp
  $input[inp].each do |i|
    rec i unless $list.member? i
  end
end

rec 0

puts "Part One: #{$list.length}"


# --- Part Two ---
groups = 1

until $orphaned.empty?
  rec $orphaned[0]
  groups += 1
end

puts "Part Two: #{groups}"
