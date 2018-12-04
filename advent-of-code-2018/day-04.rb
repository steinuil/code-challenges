require 'date'

Sleep = Struct.new(:from, :to)

Shift = Struct.new(:guard, :time, :sleeps)

Entry = Struct.new(:time, :what)

entries = []

File.foreach 'day-04.input' do |line|
  matches = line.match /^\[(.+)\] (.+)$/
  time = DateTime.parse(matches[1], '%Y-%m-%d %H:%M').to_time
  what = matches[2]

  entries << Entry.new(time, what)
end

entries = entries.sort_by &:time

shifts = []

i = 0
while i < entries.length
  guard = Integer(entries[i].what.match(/Guard #(\d+) begins shift/)[1]) rescue (raise entries[i].to_s)
  time = entries[i].time
  sleeps = []
  until entries[i + 1].nil? or entries[i + 1].what =~ /Guard/
    i += 1
    raise entries[i].to_s + ', expected falls asleep' unless entries[i].what =~ /falls asleep/
    sleeps << entries[i].time.min
    i += 1
    raise entries[i].to_s + ', expected wakes up' unless entries[i].what =~ /wakes up/
    sleeps << entries[i].time.min
  end
  i += 1

  shifts << Shift.new(guard, time, sleeps)
end

by_guard = shifts.group_by &:guard

x = by_guard.map.max_by do |(k, shifts)|
  mins = 0
  shifts.each do |shift|
    shift.sleeps.each_slice(2) do |(b, e)|
      mins += e - b
    end
  end
  mins
end

a = x[1].each_with_object(Array.new(60, 0)) do |shift, acc|
  shift.sleeps.each_slice(2) do |(b, e)|
    b.upto(e - 1) do |m|
      acc[m] += 1
    end
  end
end

puts x[0] * a.index(a.max)

b = by_guard.map do |(k, shifts)|
  hour = Array.new(60, 0)
  shifts.each do |shift|
    shift.sleeps.each_slice(2) do |(b, e)|
      b.upto(e - 1) do |m|
        hour[m] += 1
      end
    end
  end
  [k, hour.index(hour.max), hour.max]
end

puts(b.max_by { |x| x[2] }.yield_self { |x| x[0] * x[1] })