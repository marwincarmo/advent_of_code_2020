#### --- Day 6: Custom Customs --- ####

forms <- read.table("data/d6_input.txt",blank.lines.skip = FALSE)
head(forms, 10)
forms_delim <- read.delim("data/d6_input.txt",blank.lines.skip = FALSE) 
head(forms_delim)

# criar id de modo que vá contando 1 ate whitespace, depois 2 ate outro whitespace

sep_pos <- which(forms$V1 == "")
