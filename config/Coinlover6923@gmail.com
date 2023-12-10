# conkyrc file for dzen
###

background no
out_to_console yes
out_to_x no
override_utf8_locale yes
update_interval 1.5
total_run_times 0
# mpd_host 127.0.0.1
# mpd_port 6600

use_spacer left
pad_percents 3

# see dzen README for escapes
TEXT
# luks-status: single line space separated
${if_running audacious}^fg()Audacious: ^fg(\#909090)${audacious_status}: ${audacious_title 70} [${audacious_position}/${audacious_length}] $endif \
${if_existing  /tmp/luks-status} ^fg()L: ^fg(\#909090)${execi 10 cat /tmp/luks-status} $endif \
^fg()Cpu: ^fg(\#909090)${cpu}% \
^fg()Mem: ^fg(\#909090)${memperc}% \
${if_up wlan0} \
^fg()D: ^fg(\#909090)${downspeedf wlan0}K \
^fg()U: ^fg(\#909090)${upspeedf wlan0}K \
$else \
^fg()D: ^fg(\#909090)${downspeedf eth0}K \
^fg()U: ^fg(\#909090)${upspeedf eth0}K \
$endif \
^fg()W: ^fg(\#909090)${execi 1800 ~/.xmonad/bin/weather.rb} \
^fg()CA: ^fg(\#909090)${tztime America/Los_Angeles %H:%M} \
# /usr/share/zoneinfo
^fg()NY: ^fg(\#909090)${tztime America/New_York %H:%M} \
^fg()    AZ: ^fg(\#909090)${time %a %b %d %H:%M}
