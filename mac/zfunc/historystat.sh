#!/bin/zsh
# get your top _ commands, default is 10
# 0 = The 0th word or the command word
# use awk to print only column 2
# -c = Precede each output line with the count of the number of times the line occurred in the input
# -n = Compare according to arithmetic value an initial numeric string
# -r = Sort results in descending order

historyStat(){

    history 0 | awk '{print $2}' | sort | uniq -c | sort -n -r | head $1
}
