
# SOLUTION ----------------------------------------------------------------

## FINALLY FOUND ON SO: http://stackoverflow.com/a/19998876/5944560

x <- MAPtl$year
sequence(rle(x)$lengths)


# OTHER ATTEMPTS ----------------------------------------------------------


# MAPtl$pos2 <- ifelse(duplicated(MAPtl$year), jitter(MAPtl$pos2, factor = 2.5), MAPtl$pos2)

# MAPtl <- MAPtl[order(MAPtl$year), , drop = FALSE]# %>% data.frame()
# x <- MAPtl$year
# xtbl <- Rtdf(x, names = c("year", "yrcnt"))
# x[duplicated(x)]
# MAPtl2 <- merge(MAPtl, xtbl, by = "year", all = TRUE)
# MAPtl2$yrcnt <- ifelse(MAPtl2$yrcnt > 1, MAPtl2$yrcnt + 1, MAPtl2$yrcnt)
# yrdup <- duplicated(x)
# yrdupsum <- yrdup %>% sum()
# xd <- x[yrdup]

# y <- vector(mode = mode(x), length = length(x))

# repeat{
#     for (i in 2:length(x)) {
#         if (x[i] != x[i - 1]) {
#             y[i] <- x[i]
#         } else {
#             if (x[i] == x[i - 1]) {
#                 y[i] <- x[i - 1] + 1
#             } #else {
#                # if (sum(duplicated(x) == 0)) {
#                 #    break
#                 }
#             }
#         # }
#     # }
# # }
#
# for (i in 2:length(x)) {
#         repeat {
#             if(x[i] == x[i-1]) {y[i] <- x[i-1] + 1
#             } else {
#
#             if (sum(duplicated(x) == 0)) break
#         }
#         }
# }
# # if(x[i] != x[i - 1]) next
# #     }
#     #; if (x[i] != x[i - 1]) break}
# # }
#
# # ifelse(x[14] == lag(x, 1)[14-1], x[14] + 1, x[i])
#
# MAPtl$y <- group_by(MAPtl, year) %>% tally()
# yc <- count
# if (x[i] == x[i-1]) {
# y[i] <- y[i-1] + 1
# }

# y[i] <- ifelse(


# MAPtl$pos <- sample(seq(1, nrow(MAPtl), by = 1), size = nrow(MAPtl), replace = FALSE)
# posmu <- mean(MAPtl$pos)
# MAPtl$pos <- ifelse(MAPtl$cpv == "Violence", MAPtl$pos*-1, MAPtl$pos)
# MAPtl$pos <- ifelse(MAPtl$cpv == "Violence" & MAPtl$pos > posmu, MAPtl$pos-posmu, MAPtl$pos)
MAPtl$pos2 <- ifelse(MAPtl$cpv == "Violence", MAPtl$pos1*-1, MAPtl$pos1)
# pos2mu <- mean(MAPtl$pos2)

