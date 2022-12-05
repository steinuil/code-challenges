#!/usr/bin/env ruby
require "json"

year = nil
day = nil
lang = nil
mode = nil

mode = ARGV[0]

if mode.nil? || !["run", "test"].include?(mode)
  STDERR.puts "Usage: run.rb [run|test]"
  exit 1
end

opts = ARGV.dup
until opts.empty?
  arg = opts.shift
  case arg
  when "-year"
    year = opts.shift&.to_i
    if year.nil?
      STDERR.puts "No year specified"
      exit 1
    end
  when "-day"
    day = opts.shift&.to_i
    if day.nil?
      STDERR.puts "No day specified"
      exit 1
    end
  when "-lang"
    lang = opts.shift
    if lang.nil?
      STDERR.puts "No language specified"
    end
  end
end

now = Time.now.localtime("-05:00")

if year && !day
  STDERR.puts "You need to specify both the year and the day"
  exit 1
elsif day.nil? && (now.month != 12  || now.day > 25)
  STDERR.puts "Today's not a valid Advent of Code day"
  exit 1
elsif day && year && (year < 2015 || day < 1 || day > 25)
  STDERR.puts "Not a valid Advent of Code day: #{year}-12-#{day}"
  exit 1
end

year ||= now.year
day ||= now.day

config = JSON.load File.read "advent-of-code-templates/config.json"

cfg_by_year ||= config["by_year"][year.to_s]
if cfg_by_year.nil?
  STDERR.puts "No config found for year: #{year}"
  exit 1
end

lang = cfg_by_year["lang"]
separator = cfg_by_year["separator"] || "_"

extension = config["extension"][lang]
runner = config["runner"][lang]

if extension.nil? || runner.nil?
  STDERR.puts "No runner found for language: #{lang}"
  exit 1
end

if runner["change_directory"]
  script_file = "day#{separator}#{"%02d" % [day]}.#{extension}"
  input_file = "day#{separator}#{"%02d" % [day]}.#{mode == "run" ? "input" : "test"}"
else
  script_file = "advent-of-code-#{year}/day#{separator}#{"%02d" % [day]}.#{extension}"
  input_file = "advent-of-code-#{year}/day#{separator}#{"%02d" % [day]}.#{mode == "run" ? "input" : "test"}"
end

cmdline = runner["command"].dup

Dir.chdir "advent-of-code-#{year}" if runner["change_directory"]

cmdline[cmdline.index "<SCRIPT>"] = script_file
cmdline[cmdline.index "<INPUT>"] = input_file if cmdline.include? "<INPUT>"

system *cmdline
