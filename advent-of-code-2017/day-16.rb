input = File.read("day-16.input").chomp.split(?,).map do |move|
  case move[0]
  when 's'
    [:rotate, -(move[1..-1].to_i)]
  when 'x'
    [:exchange, *move[1..-1].split(?/).map(&:to_i)]
  when 'p'
    [:partner, *move[1..-1].split(?/)]
  end
end

orig_state = ('a'..'p').to_a


# --- Part One ---
programs = Array.new orig_state

input.each do |m|
  move, *args = m
  case move
  when :rotate
    programs.rotate! args[0]
  when :exchange
    a, b = args
    programs[a], programs[b] = programs[b], programs[a]
  when :partner
    a, b = args
    idx_a, idx_b = programs.index(a), programs.index(b)
    programs[idx_a] = b
    programs[idx_b] = a
  end
end

puts "Part One: #{programs.join}"


# --- Part Two ---
programs = Array.new orig_state

times = 0

until times >= 1_000_000_000
  input.each do |m|
    move, *args = m
    case move
    when :rotate
      programs.rotate! args[0]
    when :exchange
      a, b = args
      programs[a], programs[b] = programs[b], programs[a]
    when :partner
      a, b = args
      idx_a, idx_b = programs.index(a), programs.index(b)
      programs[idx_a] = b
      programs[idx_b] = a
    end
  end

  times += 1

  if programs == orig_state
    times = 1_000_000_000 - (1_000_000_000 % times)
  end
end

puts "Part Two: #{programs.join}"
