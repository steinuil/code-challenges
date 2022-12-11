Operation = Struct.new(:op, :coefficient)
Monkey = Struct.new(:id, :items, :operation, :divisible_by, :if_true, :if_false, :inspected_items)

def parse input
  input
    .split("\n\n")
    .map do |m|
      id, starting_items, operation, test, if_true, if_false =
        m.split("\n")

      id = id[7..-2].to_i
      starting_items = starting_items[18..].split(", ").map(&:to_i)
      operation_op, operation_co = operation[23..].split(" ")
      operation = Operation.new operation_op.intern, (operation_co == "old" ? :old : operation_co.to_i)
      divisible_by = test[21..].to_i
      if_true = if_true[29..].to_i
      if_false = if_false[30..].to_i
      Monkey.new(
        id,
        starting_items,
        operation,
        divisible_by,
        if_true,
        if_false,
        0
      )
    end
end

input1 = parse File.read(ARGV[0])

def round1 monkeys
  monkeys.each do |monke|
    items = monke.items
    monke.items = []
    items.each do |worry_level|
      coefficient = monke.operation.coefficient == :old ? worry_level : monke.operation.coefficient
      worry_level = worry_level.send monke.operation.op, coefficient
      worry_level /= 3

      next_id = worry_level % monke.divisible_by == 0 ? monke.if_true : monke.if_false
      monkeys[next_id].items << worry_level
      monke.inspected_items += 1
    end
  end
end

20.times do
  round1 input1
end

part1 = input1.map(&:inspected_items).max(2).reduce(:*)

puts "Part One: #{part1}"

input2 = parse File.read(ARGV[0])

def round2 monkeys
  n = monkeys.map(&:divisible_by).reduce(:*)

  monkeys.each do |monke|
    items = monke.items
    monke.items = []
    items.each do |worry_level|
      coefficient = monke.operation.coefficient == :old ? worry_level : monke.operation.coefficient
      worry_level = worry_level.send monke.operation.op, coefficient

      next_id = worry_level % monke.divisible_by == 0 ? monke.if_true : monke.if_false
      monkeys[next_id].items << worry_level % n
      monke.inspected_items += 1
    end
  end
end

def print_inspections round, monkeys
  puts "== After round #{round} =="
  monkeys.each do |monke|
    puts "Monkey #{monke.id} inspected items #{monke.inspected_items} times."
  end
  puts
end

10000.times do
  round2 input2
end

part2 = input2.map(&:inspected_items).max(2).reduce(:*)

puts "Part Two: #{part2}"
