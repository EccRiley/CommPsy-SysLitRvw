# DEPRECATED - litSummaries.R ---------------------------------------------------------


# print("## Location(s)\n\n"); pander(sf.locations); print("\n\n")
# print("## Level-1 Frame:\n\n"); print(L1.sf); print("\n\n")
#
# print("## Level-2 Frame:\n\n"); print(L2.sf); print("\n\n")
#
# print("## Level-3 Frame:\n\n"); print(L3.sf); print("\n\n")
# print("## Level-2 Frame Includes:\n\n"); print(L2.sf.incl); print("\n\n")
# print("## Level-2 Frame Excludes:\n\n"); print(L2.sf.excl); print("\n\n")
# print("## Level-3 Frame Includes:\n\n"); print(L3.sf.incl); print("\n\n")
# print("## Level-3 Frame Excludes:\n\n"); print(L3.sf.excl); print("\n\n")
#'
# L1.criteria <- c("Complies with state BIP standards", "collaborates with victim services", "empoloys cognitive-behavioral approach according to evidence-based standards", "40-50+ referrals per month", "operating for 5+ years", "provide training and supervision to new programs")
# L1.scriteria2 <- c("", "Range of Intervention System Components")
# L1.sm <- list("L1-Sampling Methods" = L1.smethods,
#               # "L1-Program Criteria" = L1.criteria,
#               "L1-Intervention Components Continuum" = L1.scontinuum) %>% stack()
#
# L2.scriteria <- c("First 20-25 men appearing for intake each month at each of the four research sites")
# L2.sm <- list("L2-Sampling Methods" = L2.smethods)
#               # "L2-Participant Criteria" = L2.scriteria) %>% stack()
# L3.scriteria <- c("L3-sampling probability depends on L2 participants")

# L3.sm <- list("L3-Sampling Methods" = L3.smethods)
#               # "L3-Participant Criteria" = L3.scriteria) %>% stack()

# methods.w <- reshape(methods, v.names = c("value"), idvar = c("L"), timevar = "V", direction = "wide")
# methods.ww <- reshape(methods.w, v.names = c("value.L1", "value.L2", "value.L3"), idvar = "id", timevar = "V", direction = "wide")

# methods <- cbind(id = seq(1:nrow(methods)), methods[, c(2, 1)])

# vars <- gsub("L\\d\\-(\\w+)", "\\1", vars, perl = TRUE)
# vars <- gsub("[ ]+", ".", vars, perl = TRUE)
#
# slvls <- methods$var
# slvls <- gsub("[ ]+", "", slvls, perl = TRUE)
# slvls <- gsub("-\\w+", "", slvls, perl = TRUE)
#
# methods$V <- vars
# methods$L <- slvls

# kable(sf[, c(2, 1)], align = c("r", "l"), col.names = c(names(sf)[1], "S[note]") %>% add_footnote("'S' = Sub-Samples")

# kable(sf.incl[, c(2, 1)], align = c("r", "l"), format = 'latex', booktabs = TRUE, escape = FALSE, caption = "Populations Included in Sampling Frame") %>% kable_styling(full_width = TRUE, latex_options = c("scale_down"))

# kable(sf.excl[, c(2, 1)], align = c("r", "l"), format = 'latex', booktabs = TRUE, escape = FALSE, caption = "Populations Excluded from Sampling Frame") %>% kable_styling(full_width = TRUE, latex_options = c("scale_down"))

# kable(sf[sf$S == samples[2], c(2, 1)], align = c("r", "l"), caption = paste0("'", samples[2], "' Sub-Sample Definition"))
#
# kable(sf[sf$S == samples[3], c(2, 1)], align = c("r", "l"), caption = paste0("'", samples[3], "' Sub-Sample Definition"))
#
# ### LOOP - sf[2:3, ] kables (no footnotes) ####
# for (i in 1:length(samples[-1])) {
#     print(kable(sf[sf$S == samples[-1][i], c(2, 1)], align = c("r", "l"), caption = paste0("'", samples[-1][i], "' Sub-Sample Definition")))
# }

#' `r tufte::newthought("States:")`
#'
#' `r unique(sf.locations[sf.locations$ind == "State", "values"]) %>% pander()`
#'
#' `r tufte::newthought("Counties:")`
#'
#' `r sf.locations[sf.locations$ind == "County", "values"] %>% pander()`
#'
#' `r tufte::newthought("Cities:")`
#'
#' `r sf.locations[sf.locations$ind == "City", "values"] %>% pander()`
#'
#'
# for (i in 1:length(samples)) {
#     print(
#         kable(sf.incl[sf.incl$S == samples[i], c(2, 1)], align = c("r", "l"),
#               caption = paste0("Populations \\textit{Included} in '\\textbf{",
#                                samples[i], "}' Sub-Sample"),
#               format = 'latex', booktabs = TRUE, escape = FALSE, row.names = FALSE) %>%
#             kable_styling(full_width = TRUE, latex_options = c("scale_down"))
#           )
# }


# DEPRECATED - gondolf-crosstab.R ------------------------------------

# ra.PB <- c(rep(1, reassault[1]), rep(0, noReassault[1]))
# ra.DL <- c(rep(1, reassault[2]), rep(0, noReassault[2]))
# ra.HT <- c(rep(1, reassault[3]), rep(1, noReassault[3]))
# ra.DV <- c(rep(1, reassault[4]), rep(1, noReassault[4]))

# n <- list(PT = 180, Dallas = 144, Houston = 160, Denver = 174)

# f <- function(x) {n - x}
# reassault <- list(PB = 63, DL = 52, HT = 48, DV = 47)
# noReassault <- lapply(reassault, f)
# severe <- list(PB = 41, DL = 38, HT = 33, DV = 21)
# noSevere <- n - severe
# repeated <- list(PB = 43, DL = 34, HT = 25, DV = 19)
# noRepeated <- n - repeated
# injury<- list(PB = 35, DL = 37, HT = 33, DV = 23)
# noInjury <- n - injury
# controlling <- list(PB = 78, DL = 67, HT = 75, DV = 79)
# noControlling <- n - controlling
# verbal <- list(PB = 140, DL = 101, HT = 102, DV = 117)
# noVerbal <- n - verbal
# threats <- list(PB = 81, DL = 60, HT = 72, DV = 70)
# noThreats <- n - threats


# ra.PB <- c(ra.PB, rep(NA, maxN - length(ra.PB)))

# frq.s3 <- seq(0, max(hist(cpv.s3$year, plot = FALSE)$counts), by = 2)
# frq.s3 <- frq.s3[-length(frq.s3)]
# hist(MAP$year[MAP$scat == "S3"], col = pal_my.a75[16], border = pal_my[19],
# main = "IPV Interventions Research", xlab = " ", ylab = expression(N[Articles]),
# lwd = .5, right = F)#, breaks = seq(1990, 2018, by = 4));
# lines(density(cpv.s3$year), lwd = 2, col = pal_my[19], lty = 3);
# axis(side = 2, at = axTicks(side = 2), labels = frq.s3);

# frq.s4 <- seq(0,max(hist(cpv.s4$year, plot = FALSE)$counts),by = 6)
# hist(MAP$year[MAP$scat == "S4"], col = pal_my.a75[12], border = pal_my[19],
#      main = "LGBTQ-Specific IPV Research", xlab = " ", ylab = " ",
#      lwd = .5, right = F)
# lines(density(cpv.s4$year), lwd = 2, col = pal_my[19], lty = 3);
# axis(side = 2, at = c(0, .02, .04, .06, .8), labels = frq.s4)

#+ yr_hist1, echo=FALSE, fig.fullwidth=TRUE, fig.width=7, fig.height=5
# publication years ============================================================
### PLOT - year ####

# par(cex = 0.9)
# frq <- seq(0, max(hist(MAP$year, plot = FALSE)$counts), by = 5)[1:4]
# hist(MAP$year, freq = FALSE, col = pal_my.a75[17], border = pal_my[18],
#      main = " ", xlab = "Year Published", ylab = expression(N[Articles]),
#      lwd = .5, yaxt = 'n', right = T, breaks = seq(1990, 2018, by = 4));
# lines(density(MAP$year), lwd = 2, col = pal_my[18], lty = 3);
# axis(side = 2, at = axTicks(side = 2), labels = frq)


# LitMap.R -----------------------------------------------------------

# states2 <- merge(states, bibState, by.x = "id", by.y = "state", all = TRUE)
# states2.t <- Rtdf(states2$id)

# bibSt$Freq <- sapply(bibSt$Freq, Rna)


# scale_colour_gradientn(colours = grad(frq.st, p = grays2),
# na.value = pal_my[20], guide = FALSE)
# divngrp <- list()
# for (i in 1:nrow(divn)) {divngrp <- paste0(divn[i, 1], ".", seq(1:divn[i, 2]))}


# bibs.R -------------------------------------------------------------

# DEPRECATED ---------------------------------------------------------


# yr <- cbind(density(MAP$year)$x, density(MAP$year)$y*350)
# hist(MAP$year, col = pal_my.a50[17], border = pal_my[18], main = " ", xlab = "Year Published", lwd = .5); lines(yr, lwd = 2, col = pal_my[18], lty = 3)

# hist(cpv.s3$year, col = pal_my.a50[16], border = pal_my[18], lwd = .5, main = "IPV Interventions Research", xlab = "Year Published"); lines(yr.s3, lwd = 2, col = pal_my[18], lty = 3)

# hist(cpv.s4$year, col = pal_my.a50[16], border = pal_my[18], main = "LGBTQ-Specific IPV Research", xlab = "Year Published", lwd = .5); lines(yr.s4, lwd = 2, col = pal_my[18], lty = 3)

# wt$n <- Rdich(wt$n, min = 1)

# table(ct.mo$code[21 | 20 | 17 | 14 | 5])
# scale_x_discrete(breaks = c("code", "scat"),
# labels = c("Primary Topics", "Category")) +

# wt <- group_by(ct.top, case) %>% dplyr::count()
# names(wt) <- c("scat", "wt")
# wt2 <- group_by(ct.top, scat) %>% dplyr::count()
# names(wt2) <- c("scat", "wt2")
# ct.top <- merge(ct.top, wt, by = "case")
# ct.top <- merge(ct.top, wt2, by = "scat")
# library(kableExtra)
# kable(yrt, align = rep('c', ncol(yrt)),
#       caption = "Summary of Number of Articles Published Per Year",
#       format = 'latex', booktabs = TRUE, escape = FALSE) %>%
#     kable_styling(full_width = TRUE)
#
#
# kable(yrt.s3, align = rep('c', ncol(yrt)), caption = "Summary Statistics for Amount of IPV Interventions Research Articles Published Each Year", format = 'latex', booktabs = TRUE) %>%
#     kable_styling(position = "float_left")
# kable(yrt.s4, align = rep('c', ncol(yrt)), caption = "Summary Statistics for Amount of LGBTQ-Specific IPV Research Articles Published Each Year", format = 'latex', booktabs = TRUE) %>%
#     kable_styling(position = "float_right")
# t.top <- Rtdf(ctbl.m[ctbl.m$cat == "TOPIC", "code"] %>%
#                   droplevels(),
#               names = c("Topic", "$N_{Articles}$"))
# ftm.top <- ftable(ctbl.m[ctbl.m$cat == "TOPIC",
#                          c("code", "scat")] %>%
#                       droplevels(),
#                   row.vars = 1) %>%
#     matrix(nrow = nrow(t.top),
#            byrow = FALSE)
# dimnames(ftm.top) <-
#     list(Topic = levels(ctbl.m[ctbl.m$cat == "TOPIC", "code"] %>% droplevels()),
#          c("IPV Interventions", "LGBTQ-IPV Research"))
# ftm.top <- ifelse(ftm.top == 0, NA, ftm.top)
#
# t.top

# t.s3bibkey <- Rtdf(cpv.s3$bibkey)
# ft.s3jrnl <- with(cpv.s3, {
#     ftable(bibkey, jrnl) %>%
#         matrix(nrow = nrow(t.s3bibkey),
#                byrow = FALSE)
# })
# dimnames(ft.s3jrnl) <- list("Case" = levels(cpv.s3$bibkey), "Publication Title" = levels(cpv.s3$jrnl))
# ft.s3jrnl <- ifelse(ft.s3jrnl == 0, NA, ft.s3jrnl)
# ft.s3jrnl %>% pander(caption = "Publication Title per Case")
# t.s4bibkey <- Rtdf(cpv.s4$bibkey)
# ft.s4jrnl <- with(cpv.s4, {
#     ftable(bibkey, jrnl) %>%
#         matrix(nrow = nrow(t.s4bibkey),
#                byrow = FALSE)
# })
# dimnames(ft.s4jrnl) <- list("Case" = levels(cpv.s4$bibkey), "Publication Title" = levels(cpv.s4$jrnl))
# ft.s4jrnl
# yr <- Rmsmm(MAP$year) %>% t() %>% as.data.frame()
#
# yrt <- Rtdf(MAP$year)[, 2] %>% Rmsmm() %>% t() %>% as.data.frame()
# yrt[, c(1, 3:4)] <- apply(yrt[, c(1, 3:4)], 2, round, digits = 0)
# yrt$M <- paste0(yrt$M, " (\\textit{", yrt$SD, "})")
# yrt <- yrt[c(1, 3:4)]
# names(yrt) <- c("Mean (\\textit{SD})", "Minimum", "Maximum")
# #'
# #+ yrt, echo=FALSE
# yrt.s3 <- Rtdf(cpv.s3$year)[, 2] %>% Rmsmm() %>% t() %>% as.data.frame()
# yrt.s3[, c(1, 3:4)] <- apply(yrt.s3[, c(1, 3:4)], 2, round, digits = 0)
# yrt.s3$M <- paste0(yrt.s3$M, " (\\textit{", yrt.s3$SD, "})")
# yrt.s3 <- yrt.s3[c(1, 3:4)]
# names(yrt.s3) <- c("Mean (\\textit{SD})", "Minimum", "Maximum")
#
# yrt.s4 <- Rtdf(cpv.s4$year)[, 2] %>% Rmsmm() %>% t() %>% as.data.frame()
# yrt.s4[, c(1, 3:4)] <- apply(yrt.s4[, c(1, 3:4)], 2, round, digits = 0)
# yrt.s4$M <- paste0(yrt.s4$M, " (\\textit{", yrt.s4$SD, "})")
# yrt.s4 <- yrt.s4[c(1, 3:4)]
# names(yrt.s4) <- c("Mean (\\textit{SD})", "Minimum", "Maximum")

#'
### PLOT - populations - 3 ####

# cutoff <- group_by(ct.pop, code) %>% dplyr::count()
# names(cutoff) <- c("code", "cutoff.score")
# ct.pop <- merge(ct.pop, cutoff, by = "code")
# ct.pop <- ct.pop[ct.pop$cutoff.score > 1, ]
# cutoff2 <- with(ct.pop, {ftable(code, scat)}) %>% data.frame()
# names(cutoff2)[3] <- "co2.freq"
# ct.pop <- merge(ct.pop, cutoff2, by = c("code", "scat"))
# ct.pop <- ct.pop[ct.pop$cutoff.score > 1, ]
# ct.pop <- ct.pop[ct.pop$co2.freq > 2, ]
# ct.pop$code <- droplevels(ct.pop$code)
# nlabs <- length(unique(ct.pop$code))
# ppop <- mpal(1:(length(unique(ct.pop$code))), p = sci)
# pscat <- pal_my[c(2, 17)]
# ct.pop <- rename(ct.pop, c(code = "Included Populations",
#                            scat = "Research Category"))
#
# pop.ps2 <- ggparset2(list("Included Populations",
#                           "Research Category"),
#                     data = ct.pop, method = "parset", label = TRUE,
#                     label.size = 3.5, text.angle = 0, order = c(1,-1)) +
#     scale_fill_manual(values = c(adjustcolor(ppop, alpha.f = 0.55), pscat),
#                       guide = FALSE) +
#     scale_colour_manual(values = c(ppop, pscat), guide = FALSE) +
#     thm_Rtft(ticks = FALSE, ytext = FALSE)
# pop.ps2

### PLOT - mixed-methods - 1 ####

# mosaic(
#     ftm.mm,
#     labeling_args = list(
#         gp_labels = gpar(fontsize = 7),
#         gp_varnames = gpar(fontsize = 10, fontface = 2),
#         pos_labels = c("center", "center"),
#         just_labels = c("center", "right")
#     ),
#     rot_varnames = c(0, -90, 0, -90),
#     rot_labels = rep(0, 4),
#     alternate_labels = c(F, F),
#     tl_labels = c(T, F),
#     tl_varnames = c(F, T),
#     highlighting = 2,
#     highlighting_fill = catpal,
#     margins = unit(5, "lines"),
#     main = "Mixed-Methods by Research Category"
# )

### PLOT - quantitative - 1 ####

# mosaic(ftm.qt,
#        labeling_args = list(gp_labels = gpar(fontsize = 7),
#                             gp_varnames = gpar(fontsize = 10,
#                                                fontface = 2),
#                             pos_labels = c("center", "center"),
#                             just_labels = c("center", "right")),
#        rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4),
#        alternate_labels = c(F, F), tl_labels = c(T, F),
#        tl_varnames = c(F, T), highlighting = 2,
#        highlighting_fill = catpal, margins = unit(5, "lines"))


### PLOT - qualitative - 1 ####

# mosaic(ftm.ql,
#        labeling_args = list(gp_labels = gpar(fontsize = 7),
#                             gp_varnames = gpar(fontsize = 10,
#                                                fontface = 2),
#                             pos_labels = c("center", "center"),
#                             just_labels = c("center", "right")),
#        rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4),
#        alternate_labels = c(F, F), tl_labels = c(T, F),
#        tl_varnames = c(F, T), highlighting = 2, highlighting_fill = catpal,
#        margins = unit(5, "lines"))
#
# mosaic(ftm.mo,
#        labeling_args = list(gp_labels = gpar(fontsize = 9),
#                             gp_varnames = gpar(fontsize = 11,
#                                                fontface = 2),
#                             pos_labels = c("center", "center"),
#                             just_labels = c("center", "right"),
#                             rot_varnames = c(0, -90, 0, -90),
#                             rot_labels = rep(0, 4),
#                             alternate_labels = c(F, F),
#                             tl_labels = c(T, F),
#                             tl_varnames = c(F, T)),
#        highlighting = 2, highlighting_fill = catpal)

# map.v <- MAP[vlc && MAP$scat == "S3", , drop = FALSE]

# map.v <- map.v[!map.v$scat == "S4", ]
# levels(map.v$journal) <- c(levels(map.v$journal),
#                            j.v[!j.v %in% levels(map.v$journal)])

# Ms3 <- MAP[cp | vlc, , drop = FALSE]
# Ms4 <- MAP[cp, , drop = FALSE]


# mpyr$scat <- factor(mpyr$scat) %>% as.integer()
# mpyr$scat <- sapply(mpyr$scat, Rna, v = 0)
# mpyr$scat <- ifelse(mpyr$scat == 1, -1, mpyr$scat)
# mpyr$scat <- ifelse(mpyr$scat == 2, 1, mpyr$scat)
# ggyrt + geom_freqpoly(aes(y = yrFrq), stat = "identity")
# geom_line(aes(y = yrFrq))
# ggyr <- ggplot(MAP, aes(x = year, colour = scat), alpha = 0.6) + thm_Rtft() +
#     geom_freqpoly(binwidth = 2, closed = "left", size = 1) +
#     scale_colour_manual(values = catpal,
#                         breaks = c("S3", "S4"),
#                         labels = c("IPV Interventions Research",
#                                    "LGBTQ-IPV CP Research"))
# ggyr
#
# yrts3 <- Rtdf(MAP[MAP$scat == "S3", "year"], names = c("Year", "Freq"))
# yrts3$scat <- "S3"
# yrts4 <- Rtdf(MAP[MAP$scat == "S4", "year"], names = c("Year", "Freq"))
# yrts4$scat <- "S4"
# yrt2 <- rbind(yrts3, yrts4)

# yrt <- Rtdf(MAP$year, names = c("year", "Frq"))
# yrt$year <- as.character(yrt$year) %>% as.integer()
# yrtyr <- yrt$year %>% range()
# yr.seq <- data.frame(year = seq(yrtyr[1], yrtyr[2]))
# yrt <- merge(yrt, yr.seq, by = "year", all = TRUE)
# yrt$Frq <- sapply(yrt$Frq, Rna)
#
# mpyr <- merge(yrt, MAP, by = "year", all = TRUE)[, c("year", "Frq", "scat", "bibkey", "jrnl")]
#
#
#
# # catpal2 <- c(sci(20)[2], pal_my[20], sci(20)[10])
# catpal2 <- c(sci(20)[c(2, 10)])
#
# mpyr$Frq <- ifelse(mpyr$Frq == 0, NA, mpyr$Frq)
# ggyrt <- ggplot(mpyr, aes(x = year, fill = factor(scat)), na.rm = TRUE) + thm_Rtft() + scale_fill_manual(values = catpal2)
# ggyrt + geom_area(aes(y = (Frq)), alpha = 0.5, color = pal_my[20], size = 0.25)
# ggyrt + geom_ribbon(aes(ymin = 0, ymax = Frq),
#                     alpha = 0.5, color = pal_my[20])
#
# ggyrt +
#     geom_ribbon(aes(ymin = Frq, ymax = Frq + 1), fill = pal_my[2]) +
#     geom_line(aes(y = Frq)) + thm_Rtft()
# hist(MAP$year, freq = FALSE, col = pal_my.a75[17], border = pal_my[18],
#      main = " ", xlab = "Year Published", ylab = expression(N[Articles]),
#      lwd = .5, yaxt = 'n', right = T, breaks = seq(1990, 2018, by = 4));

# #+ wt
# scattdf <- Rtdf(MAP$scat)
# ps4 <- scattdf[2,2]/sum(scattdf[, 2])
# ps3 <- scattdf[1,2]/sum(scattdf[, 2])
# wts4 <- ps4
# wts3 <- ps4/ps3
# ctbl.m$wt <- ifelse(ctbl.m$scat == "LGBTQ-IPV Research", 1, wts3-1)

#+ populations2, fig.fullwidth=TRUE
### PLOT - populations - 2 ####

# cutoff <- group_by(ct.pop, code) %>% dplyr::count()
# names(cutoff) <- c("code", "cutoff.score")
# ct.pop <- merge(ct.pop, cutoff, by = "code")
# ct.pop <- ct.pop[ct.pop$cutoff.score > 3, ]
# nlabs <- length(unique(ct.pop$code))
# ppop <- mpal(1:(length(unique(ct.pop$code))), p = sci)
#
# ct.pop <-
#     rename(ct.pop,
#            c(code = "Sampling_Frame", scat = "Category"))
#
# pop.ps <- ggparset2(list("Category", "Sampling_Frame"),
#                    data = ct.pop,
#                    method = "parset", label = TRUE,
#                    label.size = 3.5, text.angle = 0, order = c(-1,1)) +
#     scale_fill_manual(values = c(pscat, adjustcolor(ppop, alpha.f = 0.55)),
#                       guide = FALSE) +
#     scale_colour_manual(values = c(pscat, ppop), guide = FALSE) +
#     thm_Rtft(ticks = FALSE, ytext = FALSE)
# pop.ps


# cutoff <- group_by(ct.mm, code) %>% dplyr::count()
# names(cutoff) <- c("code", "cutoff.score")
# ct.mm <- merge(ct.mm, cutoff, by = "code")
# ct.mm <- ct.mm[ct.mm$cutoff.score > 1, ]


# ### PLOT - topic - 2 ####
# library(ggparallel)
# ct.tp <- ctbl.m[ctbl.m$cat == "TOPIC", ] %>% droplevels()
# cutoff <- group_by(ct.tp, code) %>% dplyr::count()
# names(cutoff) <- c("code", "cutoff.score")
# ct.tp <- merge(ct.tp, cutoff, by = "code")
# # ct.tp <- ct.tp[ct.tp$cutoff > 2, ]
# # ct.tp <- ct.tp[ct.tp$cutoff > mean(ct.tp$cutoff), ]
#
# nlabs <- length(unique(ct.tp$code))
# ptop <- mpal(1:(length(unique(ct.tp$code))), p = sci)
# pscat <- c("#a6afbb", pal_my[17])
#
# library(reshape)
# ct.tp <- rename(ct.tp, c(scat = "Category", code = "Topics"))
#
# top.ps <- ggparset2(list("Category", "Topics"),
#                     data = ct.tp,
#                     method = "parset", label = TRUE,
#                     label.size = 3.5, text.angle = 0, order = c(1, 1)) +
#     scale_fill_manual(values = c(pscat, adjustcolor(ptop, alpha.f = 0.55)),
#                       guide = FALSE) +
#     scale_colour_manual(values = c(pscat, ptop), guide = FALSE) +
#     thm_Rtft(ticks = FALSE, ytext = FALSE)
# # top.ps

# catpal <- c(pal_my[16], pal_my[18])
# catpal <- colorRampPalette(pal_my[c(2, 16)])(20)[c(12, 20)]



# MAPrqda.R ----------------------------------------------------------

#+ echo=FALSE

# DEPRECATED ---------------------------------------------------------


# knitr::opts_chunk$set(echo = FALSE)
# library(vcd);
# library(kableExtra);
# library(dplyr)
# #'
# #' `r tufte::newthought("\\large{Search Categories}")`
# #'
# ct.scat$scat <- ifelse(ct.scat$scat == "S3", "IPV Interventions", "LGBTQ-IPV Research")
#
# t.scat <- Rtdf(ct.scat$scat, names = c("Category", "$N_{Articles}$"))
# t.scat
# scat.t <- table(ct.scat$scat)
# prop.test(scat.t)
#'
#'
#+ out.width=".5\\linewidth"
# # catpal <- c(adjustcolor(pal_my[16], alpha.f = 0.8), adjustcolor(pal_my[18], alpha.f = 0.9)) ## muted dark blue and muted medium gray ##
#
# barplot(t.scat[, 2], names.arg = t.scat[, 1], pch = 19, col = catpal, ylab = "Frequency", xlab = "Category")
#'
#' \newpage
#' `r tufte::newthought("\\large{Topics}")`
#'
# labs <- list(
#     top = c("Approach Eval.", "Community Capacity", "CCR", "IPV-Consequences", "IPV-Dynamics", "Help-Seeking", "Int. - General", "Int. - Descriptions", "Int. - Efficay", "Int. - Proposal", "Measures", "Eval. Methods", "Perp. Char.", "Program/Policy Devel.", "Outsiders' Persp.", "Key Stakeholders' Persp.", "Victims' Persp.", "Program Eval.", "Protective Factors", "Policy", "Prevalence", "Risk Factors", "System Response"),
#     mo = c("MTA", "MM", "QL", "QT"),
#     ql = c("Case Study", "Focus Groups", "Group Interviews", "1-on-1 Interviews", "Multiple QL Methods", "Participant Obsv.", "QL Survey"),
#     qt = c("Secondary Data", "Experimental", "Longitudinal", "Multiple QT Methods", "Client Records", "Police Records", "QT Survey", "Cross-Sectional"),
#     mm = c("Experimental", "Focus Groups", "1-on-1 Interviews", "Longitudinal", "QL Survey", "QT Survey", "Cross-Sectional"),
#     pop = c("African Americans", "'At Risk' Populations", "Asian Americans", "Cis-Gender", "College Students", "Couples", "Non-IPV Crime Victims", "Disabled  Persons", "Female/Women/Girls", "General Population", "Graduate Students", "Heterosexuals", "IPV-Perpetratords", "IPV-Victims/Survivors", "Latin*/Hispanic", "Males/Men/Boys", "CB Practitioners", "CB Practitioners - IPV", "Int. Programs" , "Parents", "Racial Minorities", "Sexual Minorities (SM)", "SM - Bisexuals", "SM - Gay", "SM - Lesbian", "SM - Queer", "SM - Transgender", "System Entities", "Urban-Specific", "Children/Youth")
# )
# labs2 <- list(
#     top = c("Intervention Approach Evaluation (Eval.)", "Community Capacity", "Coordinated Community Response (CCR)", "IPV Consequences", "IPV Dynamics", "Help-Seeking", "IPV Interventions (Int.) - General", "IPV Interventions (Int.) - Description", "IPV Interventions (Int.) - Efficay", "IPV Interventions (Int.) - Proposal", "Measures", "Program Evaluation (Eval.) Methods", "Perpetrator (Perp.) Characteristics (Char.)", "Program/Policy Development (Devel.)", "Outsiders' Perspectives (Persp.)", "Key Stakeholders' Perspectives (Persp.)", "Victims' Perspectives (Persp.)", "Program/Policy Evaluation (Eval.) - General", "Protective Factors", "Policy", "IPV Prevalence", "Risk Factors", "System Response"),
#     mo = c("Meta-Analysis (MTA)", "Mixed-Methods (MM)", "Qualitative (QL)", "Quantitative (QT)"),
#     ql = c("Case Study", "Focus Groups", "Group Interviews", "1-on-1 Interviews", "Multiple Qualitative (QL) Methods", "Participant Observation (Obsv.)", "Qualitative (QL) Survey"),
#     qt = c("Secondary Data", "Experimental", "Longitudinal", "Multiple Quantitative (QT) Methods", "Client Records", "Police Records", "Quantitative (QT) Survey", "Cross-Sectional"),
#     mm = c("Experimental", "Focus Groups", "1-on-1 Interviews", "Longitudinal", "Qualitative (QL) Survey", "Quantitative (QT) Survey", "Cross-Sectional"),
#     pop = c("African Americans", "'At Risk' Populations", "Asian Americans", "Cis-Gender", "College Students", "Couples", "Non-IPV Crime Victims", "Disabled  Persons", "Female/Women/Girls", "General Population", "Graduate Students", "Heterosexuals", "IPV-Perpetrators", "IPV-Victims/Survivors", "Latinos/Latinas and/or Hispanic-Americans (Latin*/Hispanic)", "Males/Men/Boys", "Community-Based (CB) Practitioners", "CB Practitioners - IPV-Specific", "IPV Intervention (Int.) Programs" , "Parents", "Racial Minorities", "Sexual Minorities (SM)", "SM - Bisexuals", "SM - Gay", "SM - Lesbian", "SM - Queer", "SM - Transgender", "System Entities", "Urban-Specific", "Children/Youth")
# )
# ct.top <- ctbl[ctbl$cat == "TOPIC",]
# t.top <- Rtdf(ct.top$code, names = c("Topic", "Frequency"))
# t.top[, 1] <- labs2$top
# kable(t.top, booktabs = T, format = "latex") %>% kable_styling(position = "float_right")
#
# ft.top <- ftable(ct.top[, c("code", "scat")], row.vars = 1)
# ftm.top <- matrix(ft.top, nrow = length(unique(t.top[, 1])), byrow = FALSE)
# dimnames(ftm.top) <- list(Topic = labs$top, scat = c("IPV Interventions", "LGBTQ-IPV Research"))
# #'
# #+ out.width=".55\\linewidth"
# # dotchart(t.top[, 2], labels = labs$top, pch = 19, lcolor = pal_my[20], xlab = "Frequency", cex = 0.8, xlim = c(1, 24))
#
# Rdotchart(ftm.top, labels = labs$top, pch = 19, gcolor = pal_my[20], xlab = "Frequency", cex = 0.7, gcex = 0.75, gfont = 2, pt.cex = 1.125, color = c(rep(catpal[1], nrow(ftm.top)), rep(catpal[2], nrow(ftm.top))))
#
# #'
# #' \newpage
# #' `r tufte::newthought("\\large{Overarching Methododology}")`
# #'
# ct.mo <- ctbl[ctbl$cat == "M-OVERALL", ]
# t.mo <- Rtdf(ct.mo$code, names = c("Method(s)", "Frequency"))
# t.mo[, 1] <- labs2$mo
# kable(t.mo, align = c("l", "r"))
#
# ft.mo <- ftable(ct.mo[, c("code", "scat")], row.vars = 1)
# ftm.mo <- matrix(ft.mo, nrow = length(unique(t.mo[, 1])), byrow = FALSE)
# dimnames(ftm.mo) <- list(Methodology = labs$mo, scat = c("IPV Interventions", "LGBTQ-IPV Research"))
#
# # barplot(t.mo[, 2], names.arg = labs$mo, pch = 19, main = "Overarching Methodology", cex.names = 0.9)
#
# mosaic(ftm.mo, labeling_args = list(gp_labels = gpar(fontsize = 9), gp_varnames = gpar(fontsize = 11, fontface = 2), set_varnames = c(scat = "Search Category"), pos_labels = c("center", "center"), just_labels = c("center", "right")), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T), highlighting = 2, highlighting_fill = catpal)#, margins = unit(6, "lines"))
# #'
# #' \newpage
# #' `r tufte::newthought("\\large{Qualitative Methods}")`
# #'
# ct.ql <- ctbl[ctbl$cat == "M-QL", ]
# t.ql <- Rtdf(ct.ql$code, names = c("Qualitative Method", "Frequency"))
# ft.ql <- ftable(ct.ql[, c("code", "scat")], row.vars = 1)
# ftm.ql <- matrix(ft.ql, nrow = length(unique(t.ql[, 1])), byrow = FALSE)
# dimnames(ftm.ql) <- list("Qualitative Method(s)" = labs$ql, scat = c("IPV Interventions", "LGBTQ-IPV Research"))
#
# t.ql[, 1] <- labs2$ql
# t.ql
#
# mosaic(ftm.ql, labeling_args = list(gp_labels = gpar(fontsize = 7), gp_varnames = gpar(fontsize = 10, fontface = 2), set_varnames = c(scat = "Search Category"), pos_labels = c("center", "center"), just_labels = c("center", "right")), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T), highlighting = 2, highlighting_fill = catpal, margins = unit(6, "lines"))
# #'
# #' \newpage
# #' `r tufte::newthought("\\large{Quantitative Methods}")`
# #'
# ct.qt <- ctbl[ctbl$cat == "M-QT", ]
# t.qt <- Rtdf(ct.qt$code, names = c("Quantitative Method", "Frequency"))
# ft.qt <- ftable(ct.qt[, c("code", "scat")], row.vars = 1)
# ftm.qt <- matrix(ft.qt, nrow = length(unique(t.qt[, 1])), byrow = FALSE)
# dimnames(ftm.qt) <- list("Quantitative Method(s)" = labs$qt, scat = c("IPV Interventions", "LGBTQ-IPV Research"))
#
# t.qt[, 1] <- labs2$qt
# t.qt
#
# mosaic(ftm.qt, labeling_args = list(gp_labels = gpar(fontsize = 7), gp_varnames = gpar(fontsize = 10, fontface = 2), set_varnames = c(scat = "Search Category"), pos_labels = c("center", "center"), just_labels = c("center", "right")), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T), highlighting = 2, highlighting_fill = catpal, margins = unit(5, "lines"))
# #'
# #' \newpage
# #' `r tufte::newthought("\\large{Mixed-Methods}")`
# #'
# ct.mm <- ctbl[ctbl$cat == "M-MM", ]
# t.mm <- Rtdf(ct.mm$code, names = c("Methods", "Frequency"))
# ft.mm <- ftable(ct.mm[, c("code", "scat")], row.vars = 1)
# ftm.mm <- matrix(ft.mm, nrow = length(unique(t.mm[, 1])), byrow = FALSE)
# dimnames(ftm.mm) <- list("Mixed-Methods" = labs$mm, scat = c("IPV Interventions", "LGBTQ-IPV Research"))
#
# t.mm[, 1] <- labs2$mm
# t.mm
#
# mosaic(ftm.mm, labeling_args = list(gp_labels = gpar(fontsize = 7), gp_varnames = gpar(fontsize = 10, fontface = 2), set_varnames = c(scat = "Search Category"), pos_labels = c("center", "center"), just_labels = c("center", "right")), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T), highlighting = 2, highlighting_fill = catpal, margins = unit(5, "lines"))
# #'
# #' \newpage
# #' `r tufte::newthought("\\large{Populations}")`
# #'
# ct.pop <- ctbl[ctbl$cat == "POPULATION", ]
# t.pop <- Rtdf(ct.pop$code, names = c("Population", "Frequency"))
# ft.pop <- ftable(ct.pop[, c("code", "scat")], row.vars = 1)
# ftm.pop <- matrix(ft.pop, nrow = length(unique(t.pop[, 1])), byrow = FALSE)
# dimnames(ftm.pop) <- list("Populations" = labs$pop, scat = c("IPV Interventions", "LGBTQ-IPV Research"))
#
# t.pop[, 1] <- labs2$pop
# t.pop
# Rdotchart(ftm.pop, labels = labs$pop, pch = 19, gcolor = pal_my[20], xlab = "Frequency", cex = 0.7, gcex = 0.75, gfont = 2, pt.cex = 1.125, color = c(rep(catpal[1], nrow(ftm.pop)), rep(catpal[2], nrow(ftm.pop))))

# pop.df <- ft.pop %>% as.data.frame

# lrm <- glm(Freq ~ scat + , data = pop.df)
# dat.p1 <- with(pop.df,
#                data.frame(Freq = mean(Freq), scat = factor(scat)))
# # dat.p1
# dat.p1$frqP <- predict(lrm, newdata = dat.p1, type = "response")
# # dat.p1
# dat.p2 <- with(pop.df, data.frame(Freq = rep(seq(from = 0, to = 65, length.out = 30), 2), scat = factor(scat)))
# dat.p3 <- cbind(dat.p2, predict(lrm, newdata = dat.p2, type = "link", se = TRUE))
# dat.p3 <- within(dat.p3, {
#     PredictedProb <- plogis(fit)
#     LL <- plogis(fit - (1.96 * se.fit))
#     UL <- plogis(fit + (1.96 * se.fit))
# })
# library(ggplot2); library(ggthemes)
# ggplot(dat.p3, aes(x = Freq, y = PredictedProb)) +
#     geom_ribbon(aes(ymin = LL, ymax = UL, fill = scat), alpha = 0.2) +
#     geom_line(aes(colour = scat), size = 1) + scale_fill_manual(values = catpal) + scale_colour_manual(values = catpal) + thm_Rtft()
#
#'
#'
#'

