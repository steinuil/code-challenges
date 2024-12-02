triangle_numbers = Enumerator.new do |y|
  n = 0
  last = 0

  loop do
    n += 1
    num = last + n
    last = num

    y << num
  end
end

def count_divisors n
  d = 1
  d += 1 unless d == 1

  (2..Math.sqrt(n)).each do |i|
    next unless n % i == 0

    d += 2
  end

  d
end

triangle_numbers.each do |n|
  if count_divisors(n) > 500
    puts n
    break
  end
end
