library(RQDA); openProject("data/RQDA/comps.rqda", updateGUI = FALSE)

s3 <- readLines("zs3.txt")
s4 <- readLines("zs4.txt")

# ys3 <- data.frame()
# ys4 <- data.frame()
ys3 <- vector(mode = "character", length = length(s3))
ys4 <- vector(mode = "character", length = length(s4))

for (i in 1:length(s3)){
    ys3[i] <- RQDAQuery(s3[i])
}

for (i in 1:length(s4)){
    ys4[i] <- as.character(RQDAQuery(s4[i]))
}

ys3df <- data.frame(unlist(ys3))

ys4df <- data.frame(unlist(ys4))
# library(dplyr)
# y$text <- gsub("c\\(\"(.*?)\"", "'\\1'", y$text)
# yy <- unlist(y$text) %>% as.character()
# yydf <- as.data.frame(yy)

write.csv(ys3df, "data/RQDA/RQDAQuery-MAPseltext-s3.csv")
write.csv(ys4df, "data/RQDA/RQDAQuery-MAPseltext-s4.csv")


closeProject()

