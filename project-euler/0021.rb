require 'set'

def divisors n
  x = 1

  (2..Math.sqrt(n)).each do |i|
    d, r = n.divmod i
    next unless r == 0
    x += d
    x += i
    # x += 2
  end

  x
end

@cache = []

def d n
  @cache[n] ||= divisors(n)
  @cache[n]
end

s = 0

2.upto(10_000) do |n|
  m = d(n)
  next if n == m
  if d(m) == n
    s += n
    s += m
  end
end

puts s
