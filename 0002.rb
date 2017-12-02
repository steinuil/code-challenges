four_million = 4_000_000

puts Enumerator.new { |acc| prev = curr = 1; loop { acc << curr; prev, curr = curr, curr + prev } }.take_while { |n| n < four_million }.select(&:even?).sum
