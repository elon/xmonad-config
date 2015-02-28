#!/usr/bin/ruby

begin
  require 'open-uri'
  require 'nokogiri'

  # url = 'http://www.weather.com/weather/today/Scottsdale+AZ+85255:4:US'
  url = 'http://www.accuweather.com/en/us/scottsdale-az/85255/current-weather/36679_pc'
  doc = Nokogiri::HTML(open(url))

  current = doc.css('div.forecast div.info span.temp')[0].content rescue '?'
  high = doc.css('div.bg.bg-su div.info strong.temp')[0].content rescue '?'
  sunset = doc.css('div#feature-sun.feature.feature-first p.time-period span.finish')[0].content rescue ''
  sunset = sunset.gsub(' ', '')

  # apparently dzen doesn't like utf-8...
  puts "#{current}/#{high} #{sunset}".encode('US-ASCII', :undef => :replace, :replace => '')
rescue => e
  puts "Error: #{e.to_s}"
end
