library(tidyverse)

# reading the input file
boardingpass <- readLines("data/d5_input.txt")
head(boardingpass)

upper <- 127
lower <- 0

pass_sete <- str_sub("BFBFBFBLLR", 1, 7)

# teste


if (str_detect(pass_sete, "^.F")) {
  upper = (upper/2)-0.5
  lower = lower }  else {
    upper = upper
    lower = (upper/2)+0.5
  }
