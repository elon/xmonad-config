#! /usr/bin/env ruby

require 'net/http'
require "uri"

uri = URI.parse 'http://rss.wunderground.com/auto/rss_full/AZ/Scottsdale.xml?units=english'
http = Net::HTTP.new(uri.host, uri.port)
response = http.request(Net::HTTP::Get.new(uri.request_uri))
# File.open('/tmp/1','w') { |f| f.write response.body }

body = response.body
current, pattern = body.scan(/Current Conditions : ([0-9]{2}\.[0-9]{1})F, (.*) -/)[0]
high = body.scan(/High of ([0-9]{2})F/)[0][0]
puts "#{current}/#{high} #{pattern}"
