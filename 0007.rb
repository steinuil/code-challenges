nth = 10_001

primes = []

BLOCK_SIZE = 10_000

stops = {}

0.upto(Float::INFINITY).each do |block|
  nums = Array.new(BLOCK_SIZE, true)

  offset = block * BLOCK_SIZE

  # Black out the primes that we've already gotten
  primes.each do |p|
    n = stops[p] - offset
    while n < BLOCK_SIZE
      nums[n] = false
      n += p
    end
    stops[p] = n + offset
  end

  BLOCK_SIZE.times do |i|
    next if i + offset < 2
    next if i != 2 and i % 2 == 0
    next unless nums[i]

    prime = i + offset

    primes << prime
    n = (prime * 2) - offset

    while n < BLOCK_SIZE
      nums[n] = false
      n += prime
    end

    stops[prime] = n + offset
  end

  if prime = primes[nth - 1]
    puts prime
    exit
  end
end
