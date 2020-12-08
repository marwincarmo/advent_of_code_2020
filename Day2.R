# Day 2: Password Philosophy ----------------------------------------------

# Each line gives the password policy and then the password. 
# The password policy indicates the lowest and highest number of times a given 
# letter must appear for the password to be valid
# Goal: Find how many passwords are valid according to their policies.

input <- read.table('d2_input.txt')
dplyr::glimpse(input)

# God help us, there will be some serious regex work on this one

# I hope it is not considered cheating but given that now they gave us a 
# dataframe, I'll summon our beloved tidyverse package to make things a little easier

library(tidyverse)

# First we need to separate the max and minimum values from V1 into different columns
# it can help to rename the columns to give them a proper description
# Also the letters have a colon besides them, which may cause some pain when 
# we search for the content of this column in the password column. Removing it
# will help. I also be passing it into a tibble

new_input <- separate(input, V1, into = c("min", "max"), sep = "-") %>% 
  rename("letter" = V2,
         "password" = V3) %>% 
  mutate(letter = str_remove(letter, ":")) %>% 
  as_tibble()

# Now lets find how many times the given letter really appears on the password

new_input <- new_input %>% 
  mutate(n_letter = str_count(password, letter))

# Cool. But min and max are characters, and we better transform them into real numbers

new_input <- new_input %>% 
  mutate(min = as.double(min),
         max = as.double(max))

# Let's now create a new column 'is_valid' that return TRUE 'n_letter' is greater or equal 'min'
# and lesser or equal 'max':

new_input <- new_input %>% 
  mutate(is_valid = ifelse(n_letter >= min & n_letter <= max, TRUE, FALSE))

# To get the the amount of valid passwords we can simply sum the TRUE values in 'is_valid':

d2p1_answer <- sum(new_input$is_valid)
