
# Day 3: Toboggan Trajectory ----------------------------------------------

library(stringr)

caminho <- readLines("./data/d3_input.txt")


# Tiro a primeira linha porque nao vou marcar nada nela
#caminho <- caminho[-1]

# Todas as linhas tem 31 caracteres
nchar(caminho[1])

# caracteres onde eu vou marcar:
# ele vai primeiro do 1 ao 31, depois do 3 ao 30 e do 2 ao 29
# depois repete o ciclo do 1 ao 31
marcar_1 <- cumsum(c(1, rep(3, 10)))
marcar_2 <- cumsum(c(2, rep(3, 9)))
marcar_3 <- cumsum(c(3, rep(3, 9)))
marcar <- c(marcar_1, marcar_3, marcar_2)

# agora substituo a posição das árvores pelo X
substr(caminho, marcar, marcar) <- ifelse(substr(caminho, marcar, marcar) == ".", "0", "X")

# e conto o número de X

sum(str_count(caminho, "X"))

# uma solução mais elegante (e correta) do R-bloggers

input <- readLines("./data/d3_input.txt")
positions <- (3 * (seq_along(input) - 1)) %% nchar(input) + 1
sum(substr(input, positions, positions) == '#')


# Part two ----------------------------------------------------------------

# new slopes to check
# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.

# copiado de r-bloggers
trees <- function(right, down = 1) {
  vertical <- seq(0, length(input) - 1, by = down) + 1
  horizontal <- (right * (seq_along(input) - 1)) %% nchar(input) + 1
  horizontal <- head(horizontal, length(vertical))
  as.double(
    sum(substr(input[vertical], horizontal, horizontal) == '#')
  )
}
trees(1) * trees(3) * trees(5) * trees(7) * trees(1, 2)