require 'set'

numbers = 100..999

prods = Set.new

numbers.each do |n|
  numbers.each do |m|
    prods << n * m
  end
end

prods.to_a.sort.reverse.each do |n|
  x = n.to_s
  len = x.length / 2
  if x[0..len] == x[(-(len + 1))..-1].reverse
    puts n
    exit
  end
end
