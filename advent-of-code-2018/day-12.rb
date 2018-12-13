INPUT = 'day-12.input'

initial = nil
rules = {}

File.open(INPUT) do |f|
  initial = f.readline.match(/^initial state: (.+)$/)[1]
  f.readline

  begin
    loop do
      match, res = f.readline.match(/^(.{5}) => (.)$/)[1..2]
      rules[match] = res
    end
  rescue EOFError
  end
end

generations = [
  (Array.new(5, '.') + initial.chars + Array.new(30, '.'))
]

# def plant_cons(plants)
#   Enumerator.new do |en|
#     en << ['.', '.'] + plants[0..2]
#     en << ['.'] + plants[0..3]
#     plants.each_cons(5) { |st| en << st  }
#     en << plants[-4..-1] + ['.']
#     en << plants[-3..-1] + ['.', '.']
#   end
# end

20.times do
  curr_gen = generations.last
  new_gen = ['.', '.']

  curr_gen.each_cons(5) do |state|
    center = rules[state.join] || '.'
    new_gen << center
  end
  new_gen << '.'
  new_gen << '.'

  generations << new_gen
end

puts generations.map(&:join)

start_num = -3 - 2

res = 0

start_num.upto(generations[0].length + start_num - 1).with_index do |n, i|
  res += case generations.last[i]
    when '.'
      print '.'
      0
    when '#'
      print '#'
      n
    else
      raise StandardError, i
    end
end

puts

start_num.upto(generations[0].length + start_num - 1).with_index do |n, i|
  if generations.last[i] == '#'
    print n
  else
    print ' '
  end
end

puts

puts res