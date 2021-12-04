5.times do |i|
	puts "2#" + (((1 << 5) - 1) << (i * 5)).to_s(2).rjust(25, "0") + ","
end
 
n = 1

4.times do
	n = n << 5
	n |= 1
end

5.times do |i|
	puts "2#" + (n << i).to_s(2).rjust(25, "0") + (i == 4 ? "" : ",")
end
