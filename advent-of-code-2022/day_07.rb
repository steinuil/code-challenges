FileEntry = Struct.new(:name, :size) do
  def type = :file
end

class DirEntry
  attr_reader :name, :entries

  def initialize name
    @name = name
    @entries = {}
  end

  def type = :directory

  def dig *path
    @entries.dig(*path)
  end

  def size
    @size ||= @entries.values.sum(&:size)
  end
end

Cd = Struct.new(:directory) do
  def command = :cd
end

Ls = Struct.new(:entries) do
  def command = :ls
end

input =
  File
    .read(ARGV[0])
    .split("\n")
    .map do |line|
      case line
      when /^\$ cd /
        dir = line[5..]
        { type: :cd, directory: dir }
      when "$ ls"
        { type: :ls }
      when /^dir /
        dir = line[4..]
        { type: :dir, name: dir }
      else
        size, name = line.split(" ")
        { type: :file, name: name, size: size.to_i }
      end
    end
    .chunk_while { |l, r| l[:type] != :cd && r[:type] != :cd }
    .map do |cmds|
      case cmds[0][:type]
      when :cd
        Cd.new(cmds[0][:directory])
      when :ls
        Ls.new(cmds[1..].map do |e|
          case e[:type]
          when :file
            FileEntry.new(e[:name], e[:size])
          when :dir
            DirEntry.new(e[:name])
          end
        end)
      end
    end

root = {}
path = []

input.each do |cmd|
  case cmd.command
  when :cd
    case cmd.directory
    when "/"
      path = []
    when ".."
      path.pop
    else
      path << cmd.directory
    end
  when :ls
    cwd = path.empty? ? root : root.dig(*path).entries

    cmd.entries.each { |entry| cwd[entry.name] = entry }
  end
end

def small_total_size entries
  entries.values.sum do |e|
    case e.type
    when :directory
      size = e.size
      (size < 100000 ? size : 0) + small_total_size(e.entries)
    else
      0
    end
  end
end

part1 = small_total_size root

puts "Part One: #{part1}"

TOTAL_DISK_SPACE = 70000000
SPACE_REQUIRED = 30000000

SPACE_OCCUPIED = root.values.sum(&:size)
FREE_SPACE = TOTAL_DISK_SPACE - SPACE_OCCUPIED

def directory_to_delete entries
  sizes = []

  entries.values.each do |e|
    case e.type
    when :directory
      size = e.size
      if FREE_SPACE + size >= SPACE_REQUIRED
        sizes << size
        sizes << directory_to_delete(e.entries)
      end
    end
  end

  sizes.min || 70000000
end

part2 = directory_to_delete root

puts "Part Two: #{part2}"
