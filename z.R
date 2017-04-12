c.safe <- sample(c(0, 1), size = 200, prob = c(0.25, 0.75), replace = TRUE)
rr.ref <- sample(c(0, 1), size = 200, prob = c(0.83, 0.17), replace = TRUE)

library(dplyr)
x <- cbind(c.safe, rr.ref)
x.ft <- ftable(x[, 1], x[, 2]) %>% as.matrix()
dimnames(x.ft) <- list()