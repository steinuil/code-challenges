freq = 0

changes = []

File.foreach 'day-01.input' do |change|
  change = Integer(change)
  changes << change
  freq += change
end

puts freq

## Part Two

def reaches_twice change_list
  freq = 0
  seen = {}
  change_list.cycle do |change|
    freq += change
    if seen[freq]
      return freq
    end
    seen[freq] = true
  end
end

puts(reaches_twice changes)
