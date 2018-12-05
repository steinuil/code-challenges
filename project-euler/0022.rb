names = eval('[' + File.read('0022_names.txt') + ']')

scores = names.sort.map.with_index do |name, i|
	(i + 1) * name.each_codepoint.inject(0) { |acc, c| acc + (c - ?A.ord + 1) }
end

puts scores.sum