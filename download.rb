#!/usr/bin/env ruby
# frozen_string_literal: true

require 'dotenv/load'
require 'net/http'
require 'uri'

session = ENV['AOC_COOKIE']
abort ".env file with entry 'AOC_COOKIE' must be provided" unless session

args = ARGV
abort "expect 2 arguments for year and date" unless ARGV.length == 2
year, date = args

def http_download_uri(uri, filename, session)
  puts "Starting HTTP download for: " + uri.to_s
  http_object = Net::HTTP.new(uri.host, uri.port)
  http_object.use_ssl = true if uri.scheme == 'https'
  begin
    http_object.start do |http|
      request = Net::HTTP::Get.new(
        uri.request_uri,
        { 'Cookie': "session=#{session}"}
      )
      http.read_timeout = 500
      http.request request do |response|
        open filename, 'w' do |io|
          response.read_body do |chunk|
            io.write chunk
          end
        end
      end
    end
  rescue Exception => e
    puts "=> Exception: '#{e}'. Skipping download."
    return
  end
  puts "Stored download as " + filename + "."
end

file_path = "inputs/#{date.rjust(2, '0')}.txt"
api_url = "https://adventofcode.com/#{year}/day/#{date}/input"
http_download_uri(URI.parse(api_url), file_path, session)
