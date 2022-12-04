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

lang ||= config["by_year"][year.to_s]

extension = config["extension"][lang]
runner = config["runner"][lang]

if extension.nil? || runner.nil?
  STDERR.puts "Not a valid language: #{lang}"
  exit 1
end

script_file = "advent-of-code-#{year}/day_#{"%02d" % [day]}.#{extension}"
input_file = "advent-of-code-#{year}/day_#{"%02d" % [day]}.#{mode == "run" ? "input" : "test"}"

cmdline = runner.dup

cmdline[runner.index "<SCRIPT>"] = script_file
cmdline[runner.index "<INPUT>"] = input_file

system *cmdline
