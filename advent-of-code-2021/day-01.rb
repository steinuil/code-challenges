INPUT = File.read("day-01.input").split("\n").map(&:to_i)

# INPUT = %w{199
# 200
# 208
# 210
# 200
# 207
# 240
# 269
# 260
# 263}.map(&:to_i)

increases = INPUT.each_cons(2).select { |(a, b)| b > a }.count

puts "Part One: %d" % increases

windows = INPUT.each_cons(3).map { |window| window.sum }.each_cons(2).select { |(a, b)| b > a }.count

puts "Part Two: %d" % windows
