data = File.readlines('day-01.input').map &:to_i

def fuel_req n
  n / 3 - 2
end

puts data.inject(0) { |acc, n| fuel_req(n) + acc }

def fuel_req2 n
  x = 0
  loop do
    n = fuel_req(n)
	break if n < 0
	x += n
  end
  x
end

puts data.inject(0) { |acc, n| fuel_req2(n) + acc }