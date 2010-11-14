#!/usr/bin/ruby
# Author: Linus Arver (C) 2009
# License: GPL3
# Program Name: time.rb
# Description: returns nice timestamps

mode = ARGV.shift.to_i

t = Time.now
s_ymd = t.strftime("%Y%m%d")
s_hm = t.strftime("%H:%M")
s_wn = t.strftime("%U") # week number, with Sunday as the first day of the week
# calculate day of the week letter, in this format: (M W T R F A U) (a = sAt, u = sUn)
d = ""
case t.wday
when 0
    d = "U"
when 1
    d = "M"
when 2
    d = "T"
when 3
    d = "W"
when 4
    d = "R"
when 5
    d = "F"
when 6
    d = "A"
end

# calculate percentage completed of the month, and year, based on the number of days in the month, and the year,
# respectively
days_m = 0
days_y = 365
y = t.year
# take into account the effect of a leap year
leap = ((y % 4 == 0 and y % 100 != 0) or y % 400 == 0) ? true : false
days_y += leap ? 1 : 0
case t.month
when 1,3,5,7,8,10,12
    days_m = 31
when 2
    days_m = 28
    days_m += leap ? 1 : 0
else
    days_m = 30
end
pm = ((t.day/days_m.to_f) * 100).to_i
py = ((t.yday/days_y.to_f) * 100).to_i

case mode
when 0 # full mode
    # YYYYMMDD/[day name letter] [week number] [year %]/[intramonth %] HH:MM
    print "#{s_ymd}/#{d} #{s_wn} #{py}/#{pm} #{s_hm}"
when 1 # shortened mode for just the basics
    print "#{s_ymd}/#{d} #{s_hm}"
end
