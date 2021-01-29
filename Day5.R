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
