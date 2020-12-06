
# Day 1: Report Repair ----------------------------------------------------

# Goal: Find the two entries that sum to 2020 and multiply them
input <- read.table("input.txt")
nmbr <- input$V1

s <- 2020-nmbr 

sum_to_2020 <- nmbr[s %in% nmbr]
answer_d1 <-  sum_to_2020[1] * sum_to_2020[2]
