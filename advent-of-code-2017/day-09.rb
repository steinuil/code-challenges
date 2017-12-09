$input = File.read "day-09.input"


# --- Part One ---
$groups = []

def parse_group idx, depth
  $groups << depth
  loop do
    idx =
      case $input[idx]
      when nil
        return
      when "{"
        parse_group (idx + 1), depth + 1
      when "}"
        return idx + 1
      when "<"
        parse_garbage (idx + 1)
      else
        idx + 1
      end
  end
end

def parse_garbage idx
  loop do
    idx =
      case $input[idx]
      when "!"
        idx + 2
      when ">"
        return idx + 1
      else
        idx + 1
      end
  end
end


parse_group 0, 0

puts "Part One: #{$groups.sum}"


# --- Part Two ---
$garbage = 0

def parse_garbage_ idx
  loop do
    idx =
      case $input[idx]
      when "!"
        idx + 2
      when ">"
        return idx + 1
      else
        $garbage += 1
        idx + 1
      end
  end
end

def parse_group_ idx
  loop do
    idx =
      case $input[idx]
      when nil
        return
      when "{"
        parse_group_ (idx + 1)
      when "}"
        return idx + 1
      when "<"
        parse_garbage_ (idx + 1)
      else
        idx + 1
      end
  end
end

parse_group_ 0

puts "Part Two: #{$garbage}"
