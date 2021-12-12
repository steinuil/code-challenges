#!/usr/bin/env ruby
require "net/http"
require "cgi"

if ARGV[0] == "-h"
	puts "Usage: download-input.rb [<year> [<date>]]"
	exit
end

year = (ARGV[0] || Time.now.year).to_i
day = (ARGV[1] || Time.now.day).to_i

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

exit unless year == 2021

out_template = File.join(dir, "day_%02d.erl" % [day])
exit if File.exist? out_template

template = <<TEMPLATE
-module(day_#{"%02d" % [day]}).
-export([main/1]).
-mode(compile).


read_line(Line) ->
\tLine.


main([File]) ->
\t{ok, Input} = ekk:read_lines(File, fun read_line/1),
\tio:format("Part One: ~w\\n", [todo]),
\tio:format("Part Two: ~w\\n", [todo).
TEMPLATE

File.write out_template, template
puts "Written #{out_template}"
