# --- Day 4: Passport Processing --- --------------------------------------
library(tidyverse)

# reading the input file
passports <- readLines("data/d4_input.txt")

# inspecting the index in the passport vector where there are empty lines ("").
# by identifying them, we can learn where the info for a given starts and ends.
blank_index <- which(passports == "")


# pass_index() function gets the indexes for the values of individual passports. 
# Checking the length of blank_index, we find 279 values, so we can presume that 
# there are 279 passports, since they're separated by those empty lines.
# For example, the first passport starts at position 1 in 'passports' and ends at position
# 4 (which corresponds to index 1 in `blank_lines`) - 1.
# Similarly, the last passport starts at the last blank line position + 1 and ends
# at the last line in passports.
# It's arguments are n (index in blank_index vector), blank_vec (the vector with blank lines position),
# and input_vec (the input vector)
pass_index <- function(n, blank_vec, input_vec) {
  if (n == 0) {
    res = 1:(blank_vec[n+1] - 1)
  }
  else if (n == length(blank_vec)) {
    res = (blank_vec[n] + 1):length(input_vec)
  }
  else {
    res = (blank_vec[n] + 1):(blank_vec[n+1] - 1)
  }
  res
}

# saving the indexes for all passports in a list
index_list <- map(seq(0, length(blank_index)), 
                    ~pass_index(.x, blank_index, passports))


# this new function extract the individual passport fields based on the 
# indexes stored at index_list
pass_values <- function(l, indexes, input_vec) {
  y = unlist(strsplit(input_vec[indexes[[l]]], " "))
  z = str_split(y, ":")
  names(z) = lapply(z, function(x) x[1])
  z
}

# storing those values in a list
pass_list <- map(seq_along(index_list), ~pass_values(.x, index_list, passports))

# mapping all the values to a dataframe. we select only the second element since the
# first is the field name, which we already stored on the list
pass_df <- map_dfr(pass_list, ~map(.x, `[`, 2))

# to the the answer, we need only to find how many rows (passports) have NA values
# in any column, except on `cid`
pass_df %>% 
  filter(
    across(
      .cols = c(-cid),
      .fns = ~ !is.na(.x)
    )
  ) %>% 
  nrow(.)
# 202


# Part two ----------------------------------------------------------------


# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
#   If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.
# 
# Your job is to count the passports where all required fields are both present 
# and valid according to the above rules.

# inspecting the passports dataframe, we can see that all the fields are stored
# as character. pid, byr, iyr and eyr are numbers, so we need to convert them from character.

pass_df2 <- pass_df %>% 
  mutate(across(c("pid", "eyr", "iyr", "byr"), parse_number)) %>% 
# in hgt column we need to separate the height value from the scale label
  mutate(hgt_scale = str_extract(hgt, "[a-z]+"), .after = hgt) %>% 
  mutate(hgt = str_extract(hgt, "[0-9]+"))
