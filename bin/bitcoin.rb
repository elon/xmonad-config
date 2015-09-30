#!/usr/bin/ruby

begin
  require 'open-uri'
  require 'json'

  url = 'http://api.coindesk.com/v1/bpi/currentprice.json'

  file = '/tmp/bitcoin_price.json'
  last = File.mtime(file).to_i rescue 0
  interval = 15*60 # 15 minutes
  if (Time.now.to_i > last + interval)
    #puts 'writing cache'
    data = open(url).read
    File.open(file,'w') {|f| f.write(data)}
  else
    #puts 'reading cached'
    data = File.open(file).read
  end
  data = JSON.parse(data)
  rate = data['bpi']['USD']['rate'].strip.to_f.round(2).to_s
  # apparently dzen doesn't like utf-8...
  puts rate.encode('US-ASCII', :undef => :replace, :replace => '')
rescue => e
  puts "Error: #{e.to_s}"
end
