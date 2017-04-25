# ctbl.gr <- cb[, c("caseid", "bibkey", "scat", "cpv", "journal", "cat", "code", "year")]
tp <- cb[cb$cat == "TOPIC", ]
tp$code <- droplevels(tp$code)
tp$clab <- droplevels(tp$clab)
tp$bibkey <- droplevels(tp$bibkey)

edges.tp1 <- cbind(tp$scat, tp$code)
library(arcdiagram)
arcplot(edges.tp1)

