def checksum str
  letters = {}
  str.each_char do |chr|
    letters[chr] ||= 0
    letters[chr] += 1
  end
  { two: letters.any? { |(k, v)| v == 2 },
    three: letters.any? { |(k, v)| v == 3 } }
end

two = 0
three = 0

ids = []

File.foreach 'day-02.input' do |id|
  ids << id
  sum = checksum id
  two += 1 if sum[:two]
  three += 1 if sum[:three]
end

puts two * three

## Part two

# possible_ids = ids.dup

def difference id1, id2
  diffs = 0
  0.upto(id1.length) do |i|
    diffs += 1 if id1[i] != id2[i]
  end
  return diffs
end

correct_ids = nil

ids.each do |id|
  other = ids.find { |id2| difference(id, id2) == 1 }
  if other
    correct_ids = [id, other]
    break
  end
end

common = correct_ids.map(&:chars).transpose.reject { |(x, y)| x != y }.reduce("") { |acc, (x, _)| acc << x }

puts common
