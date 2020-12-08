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

# Part two ----------------------------------------------------------------

# The tell us in part two that 'min' and 'max' are now the positions in which 
# the letter must be on the password. Let's rename those columns to their new meaning
# and drop the previous 'is_valid' and 'n_letter' because they no longer serve our goal

part_two <- new_input %>% 
  rename("pos_1" = min,
         "pos_2" = max) %>% 
  select(-c(n_letter, is_valid))

# First we need to create two new columns that extract 'pos_1' and 'pos_2' from password.
# we can name them 'pass_p1' and 'pass_p2'. If letter is equal as pass_p1 and pass_p2,
# 'is_valid' should return a TRUE value, and FALSE if not.

part_two <- part_two %>% 
  mutate(pass_p1 = str_sub(password, start = pos_1, end =  pos_1),
         pass_p2 = str_sub(password, start = pos_2, end = pos_2))


# To be valid, only one of the positions must contain the given letter.
# First we can eliminate the rows in which pass_p1 and pass_p2 are equal,
# since they must not be so to be valid. Then we can test if one of the
# two values are equal to the given letter.

part_two <- part_two %>% 
  filter(pass_p1 != pass_p2) %>% 
  mutate(is_valid = ifelse(letter == pass_p1 | letter == pass_p2, TRUE, FALSE))

# As before, sum the TRUE values in 'is_valid' will give us the right answer:

d2p2_answer <- sum(part_two$is_valid)
