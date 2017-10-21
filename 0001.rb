puts (1.upto(1000 / 3).map { |i| i * 3 } + 1.upto((1000 / 5) - 1).map { |i| i * 5 }).uniq.inject(&:+)
