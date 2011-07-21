#!/bin/zsh

# launch firefox with a randomized assortment of news sites

# websites to load
news=(www.lemonde.fr www.zeit.de www.nikkei.com www.hani.co.kr www.sfgate.com)
hobs=(www.chessbase.com www.linuxfr.org)

# randomize sites within each category
news_r=$(echo ${(F)news} | sort -R | tr '\n' ' ') # now $news_r is a string, with a space between each element
hobs_r=$(echo ${(F)hobs} | sort -R | tr '\n' ' ')
sites="$news_r $hobs_r" # combine the strings into one string

firefox ${(s: :)sites} & disown # forcefully split the $sites string into separate words on each space character

# vim:syntax=zsh
