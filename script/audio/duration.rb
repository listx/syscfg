#!/usr/bin/env ruby

# Find the total duration of audio/video files using ffmpeg.

durs = []

ARGV.each do |a|
  durDirty = %x[ffmpeg -i '#{a}' 2>&1 | grep Duration]
  m = /Duration: (\d\d):(\d\d):(\d\d)\.(\d\d)/.match(durDirty)
  hours = m[1].to_i
  mins = m[2].to_i
  secs = m[3].to_i
  hsecs = m[4].to_i

  # To avoid floating point math, we up-convert the regular 'secs' seconds by
  # multiplying by 100. This way, we can just add the 'hsecs' to them and then
  # divide by 100 later.
  durs.push \
    (((hours * 3600 * 100) + (mins * 60 * 100) + (secs * 100) + hsecs)/100)
end

dursTotal = durs.inject(0, :+)

puts Time.at(dursTotal).utc.strftime("%H:%M:%S")
