## AnalyticApproaches - S3 ========================================================
rumptz1991 <- data.frame(bibkey = "rumptz1991ecological", code = c("QTDESC"))
gondolf1999 <- data.frame(bibkey = "gondolf1999comparison", code = c("QTDESC", "X2D", "LGR"))
thompson2000 <- data.frame(bibkey = "thompson2000identification", code = c("QTDESC", "X2D", "TTEST", "ANOVA", "LNR", "CORR", "LGR", "OR"))
sullivan2002 <- data.frame(bibkey = "sullivan2002findings", code = c("QTDESC", "TTEST", "X2D", "MANCOVA", "ANCOVA", "RMANOVA", "MLTV"))
gregory2002 <- data.frame(bibkey = "gregory2002effects", code = c("CNTA"))
foshee2004 <- data.frame(bibkey = "foshee2004assessing", code = c("OR", "CORR", "MLTV", "LGR", "MOD"))
hendricks2006 <- data.frame(bibkey = "hendricks2006recidivism", code = c("QTDESC", "ANOVA", "X2D", "TTEST"))
silvergleid2006 <- data.frame(bibkey = "silvergleid2006batterer", code = c("GT-QL", "THMA"))
hovell2006 <- data.frame(bibkey = "hovell2006evaluation", code = c("X2D", "OR", "QTDESC"))
contrino2007 <- data.frame(bibkey = "contrino2007compliance", code = c("QTDESC", "CORR", "TTEST"))
muftic2007 <- data.frame(bibkey = "muftic2007evaluation", code = c("QTDESC", "X2D", "TTEST", "LGR"))
roffman2008 <- data.frame(bibkey = "roffman2008mens", code = c("QTDESC"))
gillum2008 <- data.frame(bibkey = "gillum2008benefits", code = c("CNTA", "GT-QL", "THMA", "XCASE"))
price2009 <- data.frame(bibkey = "price2009batterer", code = c("QTDESC"))
feder2011 <- data.frame(bibkey = "feder2011need", code = c("NOTAPPLICABLE"))
ermentrout2014 <- data.frame(bibkey = "ermentrout2014this", code = c("GT-QL", "THMA"))
boal2014 <- data.frame(bibkey = "boal2014barriers", code = c("THMA", "CNTA"))
kan2014 <- data.frame(bibkey = "kan2014can", code = c("MLM", "MLTV", "MEM", "MOD"))
boal2014 <- data.frame(bibkey = "boal2014impact", code = c("QTDESC", "MANOVA", "MLTV", "ANOVA"))
howell2015 <- data.frame(bibkey = "howell2015strengthening", code = c("QTDESC", "CORR", "MULTV", "LNR"))
sargent2016 <- data.frame(bibkey = "sargent2016evaluating", code = c("QTDESC", "TTEST", "X2D", "ANOVA", "MOD"))
enriquez2010 <- data.frame(bibkey = "enriquez2010development", code = c("QTDESC", "TTEST", "THMA"))
welland2010 <- data.frame(bibkey = "welland2010culturally", code = c("CCA", "THMA", "QTDESC"))
potter2011 <- data.frame(bibkey = "potter2011bringing", code = c("QTDESC", "TTEST", "OLSRG", "X2D", "ANOVA", "CNTA"))
portwood2011 <- data.frame(bibkey = "portwood2011evaluation", code = c("QTDESC", "MLM", "ANOVA", "PHCOMP", "THMA"))

s3codes <- rbind(rumptz1991, gondolf1999, thompson2000, sullivan2002, gregory2002, foshee2004, hendricks2006, silvergleid2006, hovell2006, contrino2007, muftic2007, roffman2008, gillum2008, price2009, feder2011, ermentrout2014, boal2014, kan2014, boal2014, howell2015, sargent2016, enriquez2010, welland2010, potter2011, portwood2011)

# write.csv(s3codes, file = "data/AnalyticApproaches-s3.csv")

## AnalyticApproaches - S4 ========================================================

balsam2005 <- data.frame(bibkey = "balsam2005relationship", code = c("QTDESC", "CORR", "PATH"))
blosnich2009 <- data.frame(bibkey = "blosnich2009comparisons", code = c("QTDESC", "X2D", "OR", "LGR", "WLSE"))
oswald2010 <- data.frame(bibkey = "oswald2010lesbian", code = c("QLDESC", "THMA", "THMA-GRP"))
mustanski2014 <- data.frame(bibkey = "mustanski2014syndemic", code = c("SEM", "OR", "MGM", "RRR", "CORR", "LVM", "CFA", "IRT", "WLSE", "MLR", "DESC", "X2D"))
lewis2014 <- data.frame(bibkey = "lewis2014sexual", code = c("DESC", "CFA", "BOOT", "MLE", "SEM", "SEM-FULL", "MED", "INDEFF"))
edwards2016 <- data.frame(bibkey = "edwards2016college", code = c("QTDESC", "CORR", "ANOVA", "X2D", "PHCOMP"))
glass2008 <- data.frame(bibkey = "glass2008risk", code = c("QLDESC", "GT-QL", "THMA", "CORR", "RRR", "QTDESC", "LGR"))
sylaska2015 <- data.frame(bibkey = "sylaska2015disclosure", code = c("QTDESC", "TTEST", "QLDESC", "THMA", "CNTA"))

s4codes <- rbind(balsam2005, blosnich2009, oswald2010, mustanski2014, lewis2014, edwards2016, glass2008, sylaska2015)
# write.csv(s4codes, file = "data/AnalyticApproaches-s4.csv")

## selpoints ========================================================

x <- read.csv("data/SQL/RQDA-MAP-SQL/selPoints.csv")
x <- dplyr::rename(x, "bibkey" = casename)
xs3 <- merge(s3codes, x, all = FALSE)
xs4 <- merge(s4codes, x, all = FALSE)
xs34 <- rbind(xs3, xs4)
xs34 <- xs34[, c("caseid", "bibkey", "scat", "code", "selfirst", "selend")]

# write.csv(xs34, file = "data/analyticApproaches-codings.csv", row.names = FALSE)

codings <- read.csv("data/analyticApproaches-codings.csv")
codes <- read.csv("data/analysisCodes.csv")
x <- merge(codings, codes, all = FALSE)
write.csv(x, file = "data/analyticApproaches-codings-2.csv", row.names = FALSE)
## RQDA ========================================================

# library(RQDA); openProject("data/RQDA/comps.rqda", updateGUI = FALSE)
#
# # ys3 <- data.frame()
# # ys4 <- data.frame()
# ys3 <- vector(mode = "character", length = length(s3))
# ys4 <- vector(mode = "character", length = length(s4))
#
# for (i in 1:length(s3)){
#     ys3[i] <- RQDAQuery(s3[i])
# }
#
# for (i in 1:length(s4)){
#     ys4[i] <- as.character(RQDAQuery(s4[i]))
# }
#
# ys3df <- data.frame(unlist(ys3))
#
# ys4df <- data.frame(unlist(ys4))
# # library(dplyr)
# # y$text <- gsub("c\\(\"(.*?)\"", "'\\1'", y$text)
# # yy <- unlist(y$text) %>% as.character()
# # yydf <- as.data.frame(yy)
#
# write.csv(ys3df, "data/RQDA/RQDAQuery-MAPseltext-s3.csv")
# write.csv(ys4df, "data/RQDA/RQDAQuery-MAPseltext-s4.csv")
#
#
# closeProject()
