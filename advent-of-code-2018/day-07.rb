require 'set'

Step = Struct.new(:step, :requires)
steps = []

File.foreach('day-07.input') do |line|
  s1, s2 = line.match(/Step (.) must be finished before step (.) can begin\./)[1..2]
  steps << Step.new(s2, s1)
end

all_steps = steps.each_with_object(Set.new) do |x, acc|
  acc << x.step
  acc << x.requires
end.to_a

rev_deps = all_steps.each_with_object({}) do |step, acc|
  acc[step] = steps.select { |s| s.step == step }.map { |x| x[1] }
end

deps = all_steps.each_with_object({}) do |step, acc|
  acc[step] = steps.select { |s| s.requires == step }.map { |x| x[0] }
end

class Worker
  def initialize
    @time = 0
  end

  def busy?
    @time < 1
  end

  def build letter
    @time = 60 + letter.ord - ?A.ord + 1
  end

  def step
    @time
  end
end

Worker = Struct.new :busy?, :time

class Build
  def initialize deps, rev_deps
    @deps = deps
    @rev_deps = rev_deps

    @queue = rev_deps.each.select { |(k, v)| v.empty? }.map { |x| x[0] }
    @waiting = []
    @order = []
  end

  def next_step
    ok, @waiting = @waiting.partition { |v| @rev_deps[v].all? { |x| @order.member? x }}
    @queue += ok
    return unless @queue
    @queue.sort!
    @queue.shift
  end

  def claim
    letter next_step
  end

  def step_with s
  end

  def do_parallel
    workers = Array.new(5) { Worker.new(false, 0) }

    loop do
      workers.each do |w|
        if w.time < 1

        end

        if w.busy?
          if w.time < 1
            w.busy? = false
          end

          w.time -= 1
        else
        end
      end
    end
  end

  def step
    todo = next_step
    return if @order.member? todo
    return true unless todo
    @order << todo
    deps = @deps[todo]
    return true if deps.empty?
    deps.each do |dep|
      waiting = @rev_deps[dep].reject { |v| @order.member? v }
      if waiting.empty?
        @queue << dep
      else
        @waiting += @rev_deps[dep].reject { |v| @order.member? v }
      end
    end
    nil
  end

  def do
    loop do
      break if step
    end

    @order.join ""
  end
end

b = Build.new deps, rev_deps
puts b.do