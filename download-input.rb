#!/usr/bin/env ruby
require "net/http"
require "cgi"
require "erb"
require "json"

year = nil
day = nil
lang = nil

opts = ARGV.dup
until opts.empty?
  arg = opts.shift
  case arg
  when "-h"
    puts "Usage: download-input.rb [-h] [-lang <language>] [<year> <day>]"
    exit
  when "-lang"
    lang = opts.shift
    if lang.nil?
      STDERR.puts "No language specified"
      exit 1
    end
  else
    if year.nil?
      year = arg.to_i
    elsif day.nil?
      day = arg.to_i
    else
      STDERR.puts "Argument not recognized: #{arg}"
      exit 1
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

dir = "advent-of-code-#{year}"

uri = URI "https://adventofcode.com/#{year}/day/#{day}/input"

input = Net::HTTP.start(uri.host, uri.port, use_ssl: true) do |http|
  request = Net::HTTP::Get.new uri.request_uri
  request["Cookie"] = CGI::Cookie.new("session", File.read("aoc-cookie.txt")).to_s

  response = http.request request

  if response.code != "200"
    STDERR.puts "Error #{response.code}"
    STDERR.puts response.body
    exit 1
  end

  response.body
end

unless Dir.exist? dir
  Dir.mkdir dir
end

out = File.join(dir, "day_%02d.input" % [day])
File.write out, input
puts "Written #{dir}/day_%02d.input" % [day]

template_config = JSON.load File.read("advent-of-code-templates/config.json")

lang ||= template_config["by_year"][year.to_s].dig("lang")

exit if lang.nil?

extension = template_config["extension"][lang]

if extension.nil?
  STDERR.puts "Not a valid language: #{lang}"
  exit 1
end

out_template = File.join(dir, "day_%02d.#{extension}" % [day])
exit if File.exist? out_template

template = ERB.new File.read("advent-of-code-templates/#{lang}.erb")

File.write out_template, template.result(binding)
puts "Written #{out_template}"
