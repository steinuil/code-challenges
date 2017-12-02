puts (1..100).sum.**(2) - (1..100).inject(0) { |acc, x| acc + (x ** 2) }
