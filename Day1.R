
# Day 1: Report Repair ----------------------------------------------------

# Goal: Find the two entries that sum to 2020 and multiply them
input <- read.table("input.txt")
entries <- input$V1

# First subtract 2020 from every number in entries
s <- 2020-entries 

# Now we search for numbers that are present in both s and in entries
# to find which numbers are possible matches for the equation x + y = 2020
sum_to_2020 <- entries[s %in% entries]

# Having them, we can now multiply those numbers to get the answer for the
# first half of Day 1's puzzle
answer_p1 <-  prod(sum_to_2020)


# Part two
# Goal: Find three entries that sum to 2020 and multiply them
 
# We have to sort a slightly more complicated equation now, x + y + z = 2020
# First step is to rearrange it to x + y = 2020 - z
# With this for loop we sum each number to the next ones, excluding itself
# and the others that are already summed up

xy <- list()
for (i in seq_along(entries)) {
  x <- list(entries[i] + entries[-c(1:i)])
  if (!is.na(x[[1]][1])) {
    xy <- c(xy, x)
  }
}

# It's a reeeeally long list, but that's how it's suppose to be, since
# we're summing every number to each other on the list
# We can unlist xy to ease the next step

xy <- unlist(xy)

# Now, for the final step, all we have to do is to find the possible values for z
# which we can obtain by simply moving 2020 to the other side of the equation: z = 2020 - (x + y)

z <- 2020 - xy

# It can be further rewritten as x + y + z = 2020

# Great! Now that we have z and (x + y) and z is interchagable with x and y
# we search for all possible values of z that are in entries. They correspond to
# either x, y or z

sum_p2 <- z[z %in% entries]
answer_p2 <- prod(sum_p2)
