STEP = 60

1.upto(Float::INFINITY) do |i|
  nums = 11.upto(20).to_a
  n = i * STEP
  loop do
    if nums.all? { |x| n % x == 0 }
      puts n
      exit
    end

    n += STEP
  end
end
