source("bibs.R")
x <- read.csv("data/SQL/RQDA-MAP-SQL/selPoints.csv")
x <- dplyr::rename(x, "bibkey" = casename)
xm <- merge(MAP, x)
xm <- xm[, c("caseid", "bibkey", "selfirst", "selend", "scat")]
write.csv(xm, "data/SQL/RQDA-MAP-SQL/MAP-selPoints.csv")
