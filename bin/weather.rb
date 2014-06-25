#!/usr/bin/ruby

begin
  require 'open-uri'
  require 'nokogiri'

  url = 'http://www.weather.com/weather/today/Scottsdale+AZ+85255:4:US'
  doc = Nokogiri::HTML(open(url))

  current = doc.css('.wx-temperature')[0].content
  high = doc.css('.wx-temperature')[1].content
  pattern = doc.css('.wx-phrase')[1].content
  sunset = doc.css('.wx-astro-details dl.wx-first dd')[1].content
  sunset = sunset.gsub(' ', '')

  # apparently dzen doesn't like utf-8...
  puts "#{current}/#{high} #{pattern} #{sunset}".encode('US-ASCII', :undef => :replace, :replace => '')
rescue => e
  puts "Error: #{e.to_s}"
end
