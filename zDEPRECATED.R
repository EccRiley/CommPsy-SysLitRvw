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
