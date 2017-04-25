n <- 201
incl <- c(rep("Female", 70), rep("Male", 131))
x <- table(incl)
p <- c(0.5, 0.5)

fem.ex <- 23
mal.ex <- 15
n.ex <- fem.ex + mal.ex
excl <- c(rep("Female", fem.ex), rep("Male", mal.ex))
x2 <- table(excl)

