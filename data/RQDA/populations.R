source("MAPrqda.R")
openProject("data/RQDA/comps.rqda")
caseids2 <-
    RQDAQuery("SELECT `caseid`, `selfirst`, `selend` FROM `caselinkage` ORDER BY `caseid`")
# ctbl2 <- RQDAQuery("SELECT `seltext`, `selfirst`, `selend` FROM `coding` ORDER BY `selfirst`")
# ctbl3 <- merge(ctbl2, cbk, by.x = "id")

# caseids3 <- merge(caseids2, ctbl3, by = c("selfirst", "selend"))


p1.s3 <- data.frame(caseid = c(1, 2, 3, 5, 6, 7, 8, 9, 10, 12, 13,
                               14, 15, 16, 17, 18, 20, 21, 23, 24,
                               25, 26, 27, 28, 29, 30, 31, 33, 34,
                               35, 37, 38),
                     name = c("IPVV", "GENP", "IPVV", "PRAC", "IPVP",
                              "IPVV", "YC", "PRG", "PRG", "IPVP", "PRNT",
                              "GENP", "PRAC", "F", "LAT", "PRG", "AA",
                              "IPVP", "IPVP", "IPVP", "IPVP", "IPVP",
                              "IPVP", "YC", "YC", "IPVV", "IPVP", "PRAC",
                              "SYS", "IPVP", "IPVV", "PRG"))
# p1.s31$name <- as.character(p1.s31$name)
# p1.s32 <- merge(p1.s31, cbk, by.x = "name", by.y = "name")
# p1.s3 <- merge(caseids2, p1.s32, by = "caseid")

p2.s3 <- data.frame(caseid = c(1, 5, 6, 7, 8, 12, 17, 20, 21, 23,
                                24, 27, 29, 31, 33, 34, 35, 37, 38),
                     name = c("F", "YC", "M", "F", "PRNT", "M",
                              "IPVP", "IPVV", "M", "M", "M",
                              "PRAC-IPV", "PRNT", "PRG", "SYS",
                              "PRAC", "M", "F", "IPVP"))
# p2.s31$name <- as.character(p2.s31$name)
# p2.s32 <- merge(p2.s31, cbk, by.x = "name", by.y = "name")
# p2.s3 <- merge(caseids2, p2.s32, by = "caseid")

p3.s3 <- data.frame(caseid = c(8, 17, 20, 24, 29, 38),
                     name = c("F", "M", "F", "F", "F", "M"))
# p3.s31$name <- as.character(p3.s31$name)
# p3.s32 <- merge(p3.s31, cbk, by.x = "name", by.y = "name")
# p3.s3 <- merge(caseids2, p3.s32, by = "caseid")

p4.s3 <- data.frame(caseid = c(8, 38),
                     name = c("PRG", "PRAC-IPV"))
# p4.s31$name <- as.character(p4.s31$name)
# p4.s32 <- merge(p4.s31, cbk, by.x = "name", by.y = "name")
# p4.s3 <- merge(caseids2, p4.s32, by = "caseid")

p5.s3 <- data.frame(caseid = c(38), name = c("SYS"))
# p5.s31$name <- as.character(p5.s31$name)
# p5.s32 <- merge(p5.s31, cbk, by.x = "name", by.y = "name")
# p5.s3 <- merge(caseids2, p5.s32, by = "caseid")

p.s31 <- rbind(p1.s3, p2.s3, p3.s3, p4.s3, p5.s3)
p.s31$name <- as.character(p.s31$name)
p.s32 <- merge(p.s31, cbk, by = "name")
p.s3 <- merge(caseids2, p.s32, by = "caseid")

write.csv(p.s3, "data/sql/p_s3.csv")


p1.s4 <- data.frame(caseid = c(42, 43, 44, 45, 46, 49, 50, 52, 53, 54, 55, 56, 57, 59, 60, 61, 62, 66, 67, 68, 69, 70, 71, 73, 74, 75, 76, 77, 78, 81, 83, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 97, 98, 99, 102, 103, 104, 105, 106),
name = c("SML", "SML", "SML", "SM", "ASA", "PRNT", "PRNT", "PRACIPV", "SML", "SMG", "IPVV", "CLG", "SML", "IPVV", "SML", "GRD", "SML", "CLG", "CLG", "SM", "SM", "SMG", "SM", "SMG", "SML", "F", "ASA", "M", "M", "GENP", "SM", "SMG", "GENP", "SM", "CRMV", "IPVV", "YC", "SML", "YC", "IPVV", "IPVP", "SML", "YC", "SYS", "IPVV", "IPVV", "YC", "SYS", "SML"))

p2.s4 <- data.frame(caseid = c(42, 43, 44, 45, 46, 47, 49, 50, 53, 54, 57, 59, 60, 61, 62, 66, 68, 69, 70, 71, 73, 74, 76, 77, 78, 83, 85, 91, 92, 93, 97, 99, 102, 103, 106),
name = c("F", "CPL", "F", "YC", "F", "RM", "F", "F", "F", "SML", "F", "SML", "HET", "PRAC", "F", "F", "SMT", "CLG", "AA", "CLG", "YC", "F", "SMG", "SMG", "SMG", "YC", "SML", "CPL", "IPVP", "SML", "F", "PRAC", "SML", "SM", "SMB"))

p3.s4 <- data.frame(caseid = c(43, 46, 47, 49, 50, 54, 59, 60, 66, 68, 69, 70, 73, 76, 77, 78, 91, 92, 93, 97, 102, 103, 106),
name = c("F", "SML", "DA", "SML", "SML", "AR", "F", "F", "HET", "CIS", "IPVV", "CPL", "M", "RM", "SMB", "SMB", "IPVV", "AR", "F", "IPVP", "F", "HET", "F"))

p4.s4 <- data.frame(caseid = c(46, 47, 49, 59, 70, 76, 77, 91, 103),
name = c("SMQ", "SM", "SMB", "PRACIPV", "RM", "M", "HET", "F", "URBN"))

p5.s4 <- data.frame(caseid = c(46, 47, 70),
name = c("RM", "CLG", "M"))

p.s41 <- rbind(p1.s4, p2.s4, p3.s4, p4.s4, p5.s4)
p.s41$name <- as.character(p.s41$name)
p.s42 <- merge(p.s41, cbk, by = "name")
p.s4 <- merge(caseids2, p.s42, by = "caseid")

write.csv(p.s4, "data/sql/p_s4.csv")
