catt <- Rtdf(cdbk$catlab, names = c("Information Category", "$N_{sub-codes}$"))

cdbk.ft1 <- with(cdbk, {
    ftable(clab, catlab) %>% data.frame()
})

cdbk.ft1$Freq <- ifelse(cdbk.ft1$Freq == 0, NA, cdbk.ft1$Freq)
cdbk.ft <- na.omit(cdbk.ft1)[, 1:2]
rownames(cdbk.ft) <- NULL
cdbk.ft$catlab <- as.character(cdbk.ft$catlab)
cdbk.ft$catlab <- gsub("^0(\\d\\.)", "\\1", cdbk.ft$catlab)
cdbk.ft$catlab <- ifelse(duplicated(cdbk.ft$catlab), NA, paste0("**", cdbk.ft$catlab, "**"))
cdbk.ft <- cdbk.ft[, c("catlab", "clab"), drop = FALSE]

# kable(cdbk.ft)

cats <- unique(cdbk.ft$catlab) %>% as.character()
# ctime <- paste0("cat_", seq(1:length(cats)))

# cdbk.ft$catlab <- as.character(cdbk.ft$catlab)
# cdbk.ft$code <- as.character(cdbk.ft$code)
# y <- vector(mode = "list", length = length(cats))
# names(y) <- cats
# # for (i in length(cats)) {
#     y[i] <- cdbk.ft[cdbk.ft$catlab == cats[i], "clab"]
# #     }
#
# x <- vector(mode = "list", length = length(cats))
# names(x) <- cats
#
# for (i in length(cats)) {
#     x <- dplyr::filter(cdbk.ft, catlab == cats[i])[, "clab"]
# }
