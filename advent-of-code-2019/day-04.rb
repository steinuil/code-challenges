# input: 134792-675810

range = 134792 ..  675810

def can_be_password? n
  d5 = n % 10
  d4 = n / 10 % 10
  d3 = n / 100 % 10
  d2 = n / 1000 % 10
  d1 = n / 10000 % 10
  d0 = n / 100000 % 10

  increasing = (d5 >= d4) && (d4 >= d3) && (d3 >= d2) && (d2 >= d1) && (d1 >= d0)
  consecutive = (d5 == d4) || (d4 == d3) || (d3 == d2) || (d2 == d1) || (d1 == d0)

  increasing && consecutive
end

puts range.inject(0) { |acc, n| if can_be_password? n then acc + 1 else acc end }

def can_be_password2? n
  d5 = n % 10
  d4 = n / 10 % 10
  d3 = n / 100 % 10
  d2 = n / 1000 % 10
  d1 = n / 10000 % 10
  d0 = n / 100000 % 10

  increasing = (d5 >= d4) && (d4 >= d3) && (d3 >= d2) && (d2 >= d1) && (d1 >= d0)

  consecutive =
    (d5 == d4 && d4 != d3) ||
    (d4 == d3 && d5 != d4 && d3 != d2) ||
    (d3 == d2 && d4 != d3 && d2 != d1) ||
    (d2 == d1 && d3 != d2 && d1 != d0) ||
    (d1 == d0 && d2 != d1)

  increasing && consecutive
end

puts range.inject(0) { |acc, n| if can_be_password2? n then acc + 1 else acc end }
