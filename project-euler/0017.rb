numbers = [
	"",
	"one",
	"two",
	"three",
	"four",
	"five",
	"six",
	"seven",
	"eight",
	"nine",
].map(&:length)

tens = [
	"ten",
	"eleven",
	"twelve",
	"thirteen",
	"fourteen",
	"fifteen",
	"sixteen",
	"seventeen",
	"eighteen",
	"nineteen",
].map(&:length)

ties = [
	"",
	"",
	"twenty",
	"thirty",
	"forty",
	"fifty",
	"sixty",
	"seventy",
	"eighty",
	"ninety",
].map(&:length)

til_10 = numbers.sum
til_19 = tens.sum + til_10

til_99 = 2.upto(9).inject(til_19) do |acc, decimal|
	acc + (ties[decimal] * 10) + til_10
end

til_999 = 1.upto(9).inject(til_99) do |acc, h|
	acc + numbers[h] + "hundred".length +
		(99 * numbers[h]) + (99 * "hundredand".length) + til_99
end

puts til_999 + "onethousand".length