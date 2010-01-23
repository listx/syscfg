#!/usr/bin/ruby
# Return a random character within ASCII range each time "ENTER" is pressed

#================================#
# Table of outputted characters: #
# Alphabet, upper and lowercased #
# Numbers 0-9                    #
# ~`!@#$%^&*()-_=+               #
# []{}\|;:'"<>,./?               #
#================================#

# 52 (alphabet) + 10 (numbers) + 32 (punctuation) = 94 total possibilities

map = [
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "0",
    "` (backtick)",
    "~ (tilde)",
    "!",
    "@",
    "#",
    "$",
    "%",
    "^ (caret)",
    "&",
    "*",
    "(",
    ")",
    "_ (underscore)",
    "- (hyphen-minus)",
    "+",
    "=",
    "[",
    "]",
    "{",
    "}",
    "\\ (backslash)",
    "| (pipe)",
    "; (semicolon)",
    ": (colon)",
    "' (apostrophe)",
    "\" (double quote)",
    ", (comma)",
    ". (period)",
    "<",
    ">",
    "/ (forward slash)",
    "?"
]

termsettings = `stty --save`
puts "q    quit"
puts "j    only letters"
puts "k    only punctuation"
puts "else any symbol"
while true
    system("stty -echo -icanon min 1 time 0")
    str = STDIN.getc
    char = str.chr
    system("stty #{termsettings}")
    system("clear")

    # make rand (much) less likely to repeat the same number again
    quint = 1000000000000000000

    case char
    when 'q'
        break
    when 'j'
        puts "magic punctuation => #{map[((rand ((map.size - 62) * quint))/quint) + 62]}"
    when 'k'
        puts "magic letter => #{map[(rand ((map.size - 32) * quint))/quint]}"
    else
        puts "magic symbol => #{map[(rand (map.size * quint))/quint]}"
    end
end
