#!/usr/bin/ruby

begin
  require 'tzinfo'
  require 'shell'

  now = Time.now.utc
  def now.to(tz)
    TZInfo::Timezone.get(tz).utc_to_local(self.utc).strftime("%H:%M")
  end

  msg = "World Times\n"
  msg << "  Pacific: #{now.to('America/Los_Angeles')}  \n"
  msg << "  Arizona: #{now.to('America/Phoenix')}  \n"
  msg << "  Eastern: #{now.to('America/New_York')}  "

  sh = Shell.new
  sh.transact do
    sh.echo(msg) | sh.system("dzen2 -p 10 -x 1200 -y '977' -w '200' -l '3' -sa 'c' -ta 'c' -bg '#484840' -fg '#d9d4cc' -e 'onstart=uncollapse;button1=exit;button3=exit'")
  end
rescue => e
  puts e.backtrace
  puts "Error: #{e.to_s}"
end
