csid.all <- csid$caseid
incl <- MAP$caseid

csid.excl <- as.numeric(!csid.all %in% incl)
csid.incl <- as.numeric(csid.all %in% MAP$caseid)

csattr.INCL <- data.frame(id = csid.all, name = csid$case, INCL = csid.incl, EXCL = csid.excl)
csattr.INCL$x <- ifelse(csattr.INCL$INCL == 1, "INCLUDE", "X")

# write.csv(csattr.INCL, "data/SQL/RQDA-MAP-SQL/MAP-caseAttr-INCL.csv", row.names = FALSE)

dat <- read.csv("data/SQL/RQDA-MAP-SQL/MAP-caseAttr-INCL.csv")
dat <- dat[, c("SQL.prepend", "INCL", "id", "SQL.append")]
write.csv(dat, "data/SQL/RQDA-MAP-SQL/MAP-caseAttr-INCL-2.csv", row.names = FALSE)
