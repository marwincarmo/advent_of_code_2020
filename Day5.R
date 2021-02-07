library(tidyverse)

# reading the input file
boardingpass <- readLines("data/d5_input.txt")
head(boardingpass)

# this function will check the boarding pass letters and return the correct
# seat id according to the rules

seatid <- function(x) {
  upper <- 127
  lower <- 0
  right <- 7
  left <- 0
  for (i in 1:7) { 
    letra = str_sub(x, i, i)
    if (str_detect(letra, "F")) {
      upper = lower+floor((upper-lower)/2)
      lower = lower }  else {
        upper = upper
        lower = lower+ceiling((upper-lower)/2)
    }
  }
  for(i in 8:10) {
    letra = str_sub(x, i, i)
    if (str_detect(letra, "L")) {
      right = left+floor((right-left)/2)
      left = left }  else {
        right = right
        left = left+ceiling((right-left)/2)
      }
  }
  seat_id <- lower * 8 + left
  seat_id
}

# passing this function to every boarding pass code
bd_ids <- map_dbl(boardingpass, seatid)

# verifying the maximum value
max(bd_ids)

pass_num_id <- tibble(boarding_pass = boardingpass,
                      seat_id = bd_ids)

# Part two ----------------------------------------------------------------

# my seat is not on the list, but the seats with IDs +1 and -1 from it
# exists on the list.
# 
# inspecting the range of values of IDs
range(bd_ids)

# it goes from 55 to 906
# to find the missing seat ID, we need to inspect which value from bd_ids
# is missing from the sequence 55 to 906

seq(55, 906)[which(!55:906 %in% bd_ids)]
# 519
