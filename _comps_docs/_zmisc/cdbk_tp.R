source("MAPrqda.R")
cdbk_tp <- codecats[codecats$cat == "TOPIC", ]

Rdt(cdbk_tp[order(cdbk_tp$clab), c("cid", "clab")])

tp_filter <- c(25, 100, 11, 10, 99, 23, 18, 20, 19, 95,
               24, 96, 14, 6, 15, 94, 97, 21, 22, 16, 17)
cdbk_tpFilter <- cdbk_tp[cdbk_tp$cid %in% tp_filter, ]
