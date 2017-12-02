number = 600_851_475_143

1.upto(Math.sqrt number).select { |n| number % n == 0 }.reverse.each do |n|
  if 1.upto(Math.sqrt n  / 2).none? { |x| n % x.*(2).+(1) == 0 }
    puts n
    exit
  end
end
