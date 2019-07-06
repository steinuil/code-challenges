fib = 1
fib_prev = 1

idx = 2

loop do
  n = fib + fib_prev
  fib_prev = fib
  fib = n
  idx += 1

  if n > (10 ** 999)
    puts idx
    break
  end
end
