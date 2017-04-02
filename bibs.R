#' ---
#' title: "MAP - Literature Description"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
# SETUP --------------------------------------------------------------

source("../SETUP.R")
knitr::opts_chunk$set(
    tidy = TRUE,
    echo = TRUE,
    fig.keep = 'high',
    fig.show = 'hold',
    results = 'asis',
    tidy.opts = list(comment = FALSE),
    echoRule = NULL
)
rpm()
# options(warn = -1)
RtCap <- function(x) {
    s0 <- strsplit(x, " ")[[1]]
    nocap <- c("a", "the", "to", "at", "in", "with", "and", "but", "or", "of")
    s1 <- ifelse(!s0 %in% nocap, toupper(substring(s0, 1,1)), s0)
    # s2 <- toupper(substring(s[!s %in% nocap], 1,1))
    s2 <- ifelse(!s0 %in% nocap, substring(s0, 2), "")
    s <- paste(s1, s2, sep="", collapse=" ")
    return(s)
}
Rabbr <- function(x) {
    s0 <- strsplit(x, " ")[[1]]
    ex <- c("a", "the", "to", "at", "in", "with", "and", "but", "or", "of", "&")
    s1 <- s0[!s0 %in% ex]
    s2 <- substring(s1, 1,1)
    s <- paste(s2, sep = "", collapse = "")
    s <- toupper(s)
    return(s)
}
#'
#' \Frule
#'
#+ journals
# journals -----------------------------------------------------------

jdat <- read.csv("data/ipvJournalsSearch.csv")[c(-1, -6, -7), ]
## Exclude Theses and Dissertations ##

m.cnt <- mean(jdat[, 2])
s.cnt <- sd(jdat[, 2])
jdat$j <- as.integer(jdat$journal)

jv.sum <- sum(jdat$count)
jdat$prop <- jdat$count/jv.sum
jfv.n <- jdat[jdat$j == 59, 2]
jiv.n <- jdat[jdat$j == 61, 2]
vaw.n <- jdat[jdat$j == 94, 2]
jvv.n <- jdat[jdat$j == 96, 2]
jv.n <- rbind(jiv.n, jfv.n, vaw.n, jvv.n)
sum(jv.n)

jdat.m <- jdat[jdat[,2] >= m.cnt, ]
jdat.s <- jdat[jdat[,2] >= s.cnt, ]

# source("journals.R", echo = TRUE)
# jdat$journal <- sapply(jdat$journal, tolower) ## "jdat" created in "journals.R" ##

j.v <- c("Journal of Interpersonal Violence",
         "Violence Against Women",
         "Violence and Victims") %>%
    tolower()
#, "Journal of Family Violence", "Aggression and Violent Behavior", "Partner Abuse", "Trauma, Violence, \\& Abuse", "Psychology of Violence", "Journal of Emotional Abuse", "Response to the Victimization of Women \\& Children")

j.cp <- c("Action Research",
          "American Journal of Community Psychology",
          "American Journal of Health Promotion",
          "American Journal of Orthopsychiatry",
          "American Journal of Preventive Medicine",
          "American Journal of Public Health",
          "Australian Community Psychologist",
          "Community Development",
          "Community Development Journal",
          "Community Mental Health Journal",
          "Community Psychology in Global Perspective",
          "Cultural Diversity \\& Ethnic Minority Psychology",
          "Global Journal of Community Psychology Practice",
          "Health Education \\& Behavior",
          "Health Promotion Practice",
          "Journal of Applied Social Psychology",
          "Journal of Community \\& Applied Social Psychology",
          "Journal of Community Practice",
          "Journal of Community Psychology",
          "Journal of Health \\& Social Behavior",
          "Journal of Prevention \\& Intervention",
          "Journal of Primary Prevention",
          "Journal of Rural Community Psychology",
          "Journal of Social Issues",
          "Psychiatric Rehabilitation Journal",
          "Psychology of Women Quarterly",
          "Social Science \\& Medicine",
          "The Community Psychologist",
          "Transcultural Psychiatry",
          "Progress in Community Health Partnerships") %>%
    tolower()
#'
#+ pander_journals, echo=FALSE
names(jdat.m[, 1:2]) <- c("Journal", "Count")
rownames(jdat.m) <- NULL
pander(jdat.m[, 1:2],
       justify = c("left", "right"),
       caption = "Journals with article counts greater than or equal to the mean of all journal article counts in the 'broad-strokes' database search results set")

names(jdat.s[, 1:2]) <- c("Journal", "Count")
rownames(jdat.s) <- NULL
pander(jdat.s[, 1:2],
       justify = c("left", "right"),
       caption = "Journals with article counts greater than or equal one standard deviation of the distribution for all journal article counts in the 'broad-strokes' database search results set")
#'
#' -----
#'
#+ bibdf
# bibdf --------------------------------------------------------------

library(bib2df)
bib <- readLines("MAP.bib")
keys <- bib[grep("\\@.*?\\{.*?,", bib, perl = TRUE)]
keys <- gsub("\\@\\w+\\{(.*?)", "\\1", keys, perl = TRUE)
keys <- keys[!grepl("\\%.*?,", keys, perl = TRUE)]
keys <- gsub(" ", NA_character_, keys)
BIBKEYS <- gsub(",", "", keys) %>% data.frame
names(BIBKEYS) <- "bibkey"
bibdf <- bib2df("MAP.bib")

MAP.au <- bibdf$AUTHOR ## bibdf[,2] ##
MAP.abs <- bibdf$ABSTRACT ## bibdf[, 8] ##

bibdf.s <- bibdf[, c("YEAR", "TITLE", "JOURNAL", "VOLUME"),
                 drop = FALSE] ## bibdf[3:6] ##
names(bibdf.s) <- tolower(names(bibdf.s))
bibdf.s$journal <- sapply(bibdf.s$journal, tolower)
#'
#'
#+ MAP, results='hide', fig.keep='none', fig.show='none'
# MAP ----------------------------------------------------------------

MAP <- data.frame(BIBKEYS, bibdf.s)
source("MAPrqda.R", echo = FALSE)
#'
#'
#+ MAP_RQDA
# MAP-RQDA ===========================================================

csid <- caseids[, c("caseid", "case", "RM", "scat")] ## caseids[, -3] ##
csid$case <- factor(csid$case)

MAP <- merge(MAP, csid, by.x = "bibkey", by.y = "case")

MAP$RM <- as.integer(MAP$RM)
MAP$RM <- ifelse(MAP$RM == 1, NA_character_, 0)
apply(MAP, 2, function(x){sum(is.na(x))})
MAP <- na.omit(MAP)
# MAP <- MAP[, -7, drop = FALSE]

#'
#+ MAP_CPV
# MAP-CPV ============================================================

MAP$journal <- sapply(MAP$journal, tolower)

### map.cp & map.v ############################################################
MAP$journal <- factor(MAP$journal)
Rtdf(MAP$journal)
ftable(MAP$journal, MAP$scat)

cp <- MAP$journal %in% j.cp
# sum(cp)
map.cp <- MAP[cp, , drop = FALSE]
# nrow(map.cp)
map.cp$journal <- factor(map.cp$journal)
levels(map.cp$journal) <- c(levels(map.cp$journal),
                            j.cp[!j.cp %in% levels(map.cp$journal)])
levels(map.cp$journal) <- sapply(levels(map.cp$journal), RtCap)

vlc <- MAP$journal %in% j.v
# sum(vlc)

map.v <- MAP[vlc, , drop = FALSE]
# nrow(map.v)
levels(map.v$journal) <- c(levels(map.v$journal),
                           j.v[!j.v %in% levels(map.v$journal)])
levels(map.v$journal) <- sapply(levels(map.v$journal), RtCap)

### MAP.cpv ############################################################

MAP.cpv <- MAP[cp | vlc, , drop = FALSE]
nrow(MAP.cpv)

MAP.cpv$journal <- factor(MAP.cpv$journal)
levels(MAP.cpv$journal) <- sapply(levels(MAP.cpv$journal), RtCap)
MAP.cpv$journal <- droplevels(MAP.cpv$journal)
#'
#' \newpage
#'
#' # \LARGE{\textsc{Reviewed Literature Descriptives}}
#'
#' \Frule
#'
#' # General Research Categories
#'
# dESCRIPTIVES ---------------------------------------------------------------

# SEARCH CATEGORIES ============================================================

Rtdf(MAP.cpv$scat, names = c(" ", "$N_{Articles}$"))
#'
#' # Publication Titles
#'
#+ pub_titles
# PUB TITLES ============================================================
t.jrnl <- Rtdf(MAP.cpv$journal)
with(MAP.cpv, {ftable(journal, scat) %>% matrix(nrow = length(unique(t.jrnl[, 1])), byrow = FALSE)})
cpv.s3 <- MAP.cpv[MAP.cpv$scat == "S3", ]
cpv.s4 <- MAP.cpv[MAP.cpv$scat == "S4", ]
#'
#' # Publication Years
#'
#+ yr_hist1, echo=FALSE, fig.fullwidth=TRUE, fig.width=7, fig.height=5
# PUB YEARS ============================================================

par(cex = 0.9)
frq <- seq(0, max(hist(MAP.cpv$year, plot = FALSE)$counts), by = 5)[1:4]
hist(MAP.cpv$year, freq = FALSE, col = mypal.a75[17], border = mypal[18],
     main = " ", xlab = "Year Published", ylab = expression(N[Articles]),
     lwd = .5, yaxt = 'n', right = T, breaks = seq(1990, 2018, by = 4));
lines(density(MAP.cpv$year), lwd = 2, col = mypal[18], lty = 3);
axis(side = 2, at = axTicks(side = 2), labels = frq)
# yr <- cbind(density(MAP.cpv$year)$x, density(MAP.cpv$year)$y*350)
# hist(MAP.cpv$year, col = mypal.a50[17], border = mypal[18], main = " ", xlab = "Year Published", lwd = .5); lines(yr, lwd = 2, col = mypal[18], lty = 3)
#'
#' \tufteskip
#' \begin{fullwidth}\begin{centering}
#'
#+ yr_hist2, echo=FALSE, fig.width=7, fig.height=4, out.width="0.85\\linewidth"
par(mfrow = c(1, 2), cex = 0.8)

frq.s3 <- seq(0, max(hist(cpv.s3$year, plot = FALSE)$counts), by = 2)
frq.s3 <- frq.s3[-length(frq.s3)]
hist(cpv.s3$year, freq = FALSE, col = mypal.a75[16], border = mypal[19],
     main = "IPV Interventions Research", xlab = " ", ylab = expression(N[Articles]),
     lwd = .5, yaxt = 'n', right = F, breaks = seq(1990, 2018, by = 4));
lines(density(cpv.s3$year), lwd = 2, col = mypal[19], lty = 3);
axis(side = 2, at = axTicks(side = 2), labels = frq.s3);
# hist(cpv.s3$year, col = mypal.a50[16], border = mypal[18], lwd = .5, main = "IPV Interventions Research", xlab = "Year Published"); lines(yr.s3, lwd = 2, col = mypal[18], lty = 3)
frq.s4 <- seq(0,max(hist(cpv.s4$year, plot = FALSE)$counts),by = 5)
hist(cpv.s4$year, freq = FALSE, col = mypal.a75[12], border = mypal[19],
     main = "LGBTQ-Specific IPV Research", xlab = " ", ylab = " ",
     lwd = .5, yaxt = 'n', right = F, breaks = seq(1990, 2018, by = 4));
lines(density(cpv.s4$year), lwd = 2, col = mypal[19], lty = 3);
axis(side = 2, at = axTicks(side = 2), labels = frq.s4)
# hist(cpv.s4$year, col = mypal.a50[16], border = mypal[18], main = "LGBTQ-Specific IPV Research", xlab = "Year Published", lwd = .5); lines(yr.s4, lwd = 2, col = mypal[18], lty = 3)

par(mfrow = c(1, 1))

yr <- Rmsmm(MAP.cpv$year) %>% t() %>% as.data.frame()

yrt <- Rtdf(MAP.cpv$year)[, 2] %>% Rmsmm() %>% t() %>% as.data.frame()
yrt[, c(1, 3:4)] <- apply(yrt[, c(1, 3:4)], 2, round, digits = 0)
yrt$M <- paste0(yrt$M, " (\\textit{", yrt$SD, "})")
yrt <- yrt[c(1, 3:4)]
names(yrt) <- c("Mean (\\textit{SD})", "Minimum", "Maximum")
#'
#' \end{centering}\end{fullwidth}\newpage
#'
#+ yrt, echo=FALSE

kable(yrt, caption = "Summary Statistics for Amount of Articles Published Each Year")

yrt.s3 <- Rtdf(cpv.s3$year)[, 2] %>% Rmsmm() %>% t() %>% as.data.frame()
yrt.s3[, c(1, 3:4)] <- apply(yrt.s3[, c(1, 3:4)], 2, round, digits = 0)
yrt.s3$M <- paste0(yrt.s3$M, " (\\textit{", yrt.s3$SD, "})")
yrt.s3 <- yrt.s3[c(1, 3:4)]
names(yrt.s3) <- c("Mean (\\textit{SD})", "Minimum", "Maximum")

kable(yrt.s3, caption = "Summary Statistics for Amount of IPV Interventions Research Articles Published Each Year")

yrt.s4 <- Rtdf(cpv.s4$year)[, 2] %>% Rmsmm() %>% t() %>% as.data.frame()
yrt.s4[, c(1, 3:4)] <- apply(yrt.s4[, c(1, 3:4)], 2, round, digits = 0)
yrt.s4$M <- paste0(yrt.s4$M, " (\\textit{", yrt.s4$SD, "})")
yrt.s4 <- yrt.s4[c(1, 3:4)]
names(yrt.s4) <- c("Mean (\\textit{SD})", "Minimum", "Maximum")

kable(yrt.s4, caption = "Summary Statistics for Amount of LGBTQ-Specific IPV Research Articles Published Each Year")
#'
#' \tufteskip
#'
#+ ctbl_m
# CTBL ---------------------------------------------------------------

ctbl.m <- merge(MAP.cpv, ctbl, by = c("caseid", "scat"))

clabs <- c("African Americans", "Approach Evaluation", "'At Risk' Populations", "Secondary/Archival Data", "Asian Americans", "Community Capacity", "Coordinated Community Response", "Cis-Gender", "College Students", "IPV Consequences", "Couples", "Non-IPV Crime Victims", "Case Study", "Disabled Persons", "IPV Dynamics", "Experimental Design", "Females/Women/Girls", "Focus Groups", "General Population", "Graduate Students", "Heterosexuals", "Help-Seeking", "IPV Interventions (Int.) - General", "Int. - Description", "Int. - Proposal", "IPV-Perpetrators", "IPV-Victims/Survivors", "Group Interviews", "1-on-1 Interviews", "Latinos/Latinas or Hispanic-Americans", "Longitudinal", "Males/Men/Boys", "Measures", "Multiple Qualitative Methods", "Multiple Quantitative Methods", "Mixed-Methods", "Program Evaluation Methods", "Perpetrator Characteristics", "Program/Policy Development", "Outsiders' Perspectives", "Key Stakeholders' Persepctives", "Victims'/Survivors' Perspectives", "Program/Policy Evaluation - General", "Protective Factors", "Public Policy", "Community-Based Practitioners", "IPV-Specific Community-Based Practitioners", "IPV Prevalence", "IPV Intervention Programs", "Parents", "Qualitative", "Quantitative", "Client Records", "Police Records", "Risk Factors", "Racial Minorities", "Sexual Minorities (SM)", "SM - Bisexuals", "SM - Gay", "SM - Lesbian", "SM - Queer", "SM - Transgender", "Qualitative Survey", "Quantitative Survey", "System Entities", "System Response", "Urban-Specific", "Cross-Sectional", "Children/Youth")

ctbl.m <- within(ctbl.m, {
    journal <- droplevels(journal)
    jrnl <- sapply(as.character(journal), Rabbr)
    code <- gsub("FG-\\w+", "FG", code)
    code <- gsub("EXP-\\w+", "EXP", code)
    code <- gsub("LT-\\w+", "LT", code)
    code <- gsub("SVY-QL-MM", "SVY-QL", code)
    code <- gsub("SVY-QT-MM", "SVY-QT", code)
    code <- gsub("XS-\\w+", "XS", code)
    code <- gsub("IVW-QL", "IVW", code)
    code <- gsub("IVW-MM", "IVW", code)
    code <- factor(code, labels = clabs)
    scat <- factor(scat, labels = c("IPV Interventions", "LGBTQ-IPV Research"))
    # jrnl <- paste0("\\footnotesize{", jrnl, "}")
    # code <- paste0("\\footnotesize{", code, "}")
    # jrnl <- sapply(jrnl, noquote)
    # code <- sapply(code, noquote)
})
#'
#+ echo=FALSE
# NO MORE ECHO -------------------------------------------------------

knitr::opts_chunk$set(echo = FALSE)
#'

#'
#' \newpage
#'
#' # Research Topics, Sampling Frames, and Methodologies
#'
#' ## Primary Topics
#'
ct.top <- ctbl.m[ctbl.m$cat == "TOPIC", ] %>% droplevels()
t.top <- Rtdf(ct.top$code, names = c("Topic", "$N_{Articles}$"))
t.top
ft.top <- ftable(ct.top[, c("code", "scat")], row.vars = 1)
ftm.top <- matrix(ft.top, nrow = nrow(t.top), byrow = FALSE)
dimnames(ftm.top) <- list(Topic = levels(ct.top$code), c("IPV Interventions", "LGBTQ-IPV Research"))
ftm.top
Rdotchart(ftm.top, labels = labs$top, pch = 19, gcolor = mypal[20], xlab = expression(N[Articles]), cex = 0.7, gcex = 0.75, gfont = 2, pt.cex = 1.125, color = c(rep(catpal[1], nrow(ftm.top)), rep(catpal[2], nrow(ftm.top))))
#'
#' ## Methodologies
#'
ct.mo <- ctbl.m[ctbl.m$cat == "M-OVERALL", ] %>% droplevels()
t.mo <- Rtdf(ct.mo$code, names = c("Method(s)", "$N_{Articles}$"))
t.mo
ft.mo <- ftable(ct.mo[, c("code", "scat")], row.vars = 1)
ftm.mo <- matrix(ft.mo, nrow = nrow(t.mo), byrow = FALSE)
dimnames(ftm.mo) <- list(Methodology = levels(ct.mo$code), Category = c("IPV Interventions", "LGBTQ-IPV Research"))
ftm.mo
mosaic(ftm.mo, labeling_args = list(gp_labels = gpar(fontsize = 9), gp_varnames = gpar(fontsize = 11, fontface = 2), pos_labels = c("center", "center"), just_labels = c("center", "right"), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T)), highlighting = 2, highlighting_fill = catpal)
#'
#' \newpage
#' `r tufte::newthought("\\large{Qualitative Methods}")`
#'
ct.ql <- ctbl.m[ctbl.m$cat == "M-QL", ] %>% droplevels()
t.ql <- Rtdf(ct.ql$code, names = c("Qualitative Method(s)", "$N_{Articles}$"))
ft.ql <- ftable(ct.ql[, c("code", "scat")], row.vars = 1)
ftm.ql <- matrix(ft.ql, nrow = nrow(t.ql), byrow = FALSE)
dimnames(ftm.ql) <- list("Qualitative Method(s)" = levels(ct.ql$code), Category = c("IPV Interventions", "LGBTQ-IPV Research"))
t.ql

mosaic(ftm.ql, labeling_args = list(gp_labels = gpar(fontsize = 7), gp_varnames = gpar(fontsize = 10, fontface = 2), pos_labels = c("center", "center"), just_labels = c("center", "right")), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T), highlighting = 2, highlighting_fill = catpal, margins = unit(5, "lines"))
#'
#' \newpage
#' `r tufte::newthought("\\large{Quantitative Methods}")`
#'
ct.qt <- ctbl.m[ctbl.m$cat == "M-QT", ] %>% droplevels()
t.qt <- Rtdf(ct.qt$code, names = c("Quantitative Method", "$N_{Articles}$"))
ft.qt <- ftable(ct.qt[, c("code", "scat")], row.vars = 1)
ftm.qt <- matrix(ft.qt, nrow = nrow(t.qt), byrow = FALSE)
dimnames(ftm.qt) <- list("Quantitative Method(s)" = levels(ct.qt$code), Category = c("IPV Interventions", "LGBTQ-IPV Research"))
t.qt

mosaic(ftm.qt, labeling_args = list(gp_labels = gpar(fontsize = 7), gp_varnames = gpar(fontsize = 10, fontface = 2), pos_labels = c("center", "center"), just_labels = c("center", "right")), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T), highlighting = 2, highlighting_fill = catpal, margins = unit(5, "lines"))
#'
#' \newpage
#' `r tufte::newthought("\\large{Mixed-Methods}")`
#'
ct.mm <- ctbl.m[ctbl.m$cat == "M-MM", ] %>% droplevels()
t.mm <- Rtdf(ct.mm$code, names = c("Methods", "$N_{Articles}$"))
ft.mm <- ftable(ct.mm[, c("code", "scat")], row.vars = 1)
ftm.mm <- matrix(ft.mm, nrow = nrow(t.mm), byrow = FALSE)
dimnames(ftm.mm) <- list("Mixed-Methods" = levels(ct.mm$code), scat = c("IPV Interventions", "LGBTQ-IPV Research"))
t.mm

mosaic(ftm.mm, labeling_args = list(gp_labels = gpar(fontsize = 7), gp_varnames = gpar(fontsize = 10, fontface = 2), pos_labels = c("center", "center"), just_labels = c("center", "right")), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T), highlighting = 2, highlighting_fill = catpal, margins = unit(5, "lines"))
#'
#' \newpage
#' `r tufte::newthought("\\large{Populations}")`
#'
ct.pop <- ctbl.m[ctbl.m$cat == "POPULATION", ] %>% droplevels()
t.pop <- Rtdf(ct.pop$code, names = c("Population", "$N_{Articles}$"))
ft.pop <- ftable(ct.pop[, c("code", "scat")], row.vars = 1)
ftm.pop <- matrix(ft.pop, nrow = nrow(t.pop), byrow = FALSE)
dimnames(ftm.pop) <- list("Populations" = levels(ct.pop$code), scat = c("IPV Interventions", "LGBTQ-IPV Research"))
t.pop

Rdotchart(ftm.pop, labels = labs$pop, pch = 19, gcolor = mypal[20], xlab = expression(N[Articles]), cex = 0.7, gcex = 0.75, gfont = 2, pt.cex = 1.125, color = c(rep(catpal[1], nrow(ftm.pop)), rep(catpal[2], nrow(ftm.pop))))
#'
#' ## IPV Interventions Research
#'
s3 <- ctbl.m[ctbl.m$scat == "IPV Interventions", ]
#'
s3.top <- s3[s3$cat == "TOPIC", ] %>% droplevels()
# ftable(s3.top$code, s3.top$jrnl)
Rtdf(s3.top$code, names = c(" ", "$N_{Articles}$")) %>% kable(caption = "Primary Topics", align = c("l", "r"))
#'
s3.pop <- s3[s3$cat == "POPULATION", ] %>% droplevels()
# ftable(s3.pop$code, s3.pop$jrnl)
Rtdf(s3.pop$code, names = c(" ", "$N_{Articles}$")) %>% kable(caption = "Sampling Frames", align = c("l", "r"))
#'
s3.mo <- s3[s3$cat == "M-OVERALL", ] %>% droplevels()
# ftable(s3.mo$code, s3.mo$jrnl)
Rtdf(s3.mo$code, names = c(" ", "$N_{Articles}$")) %>% kable(caption = "Overarching Methodology", align = c("l", "r"))
#'
s3.ql <- s3[s3$cat == "M-QL", ] %>% droplevels()
# ftable(s3.ql$code, s3.ql$jrnl)
Rtdf(s3.ql$code, names = c(" ", "$N_{Articles}$")) %>% kable(caption = "Qualitative Methods", align = c("l", "r"))
#'
s3.qt <- s3[s3$cat == "M-QT", ] %>% droplevels()
# ftable(s3.qt$code, s3.qt$jrnl)
Rtdf(s3.qt$code, names = c(" ", "$N_{Articles}$")) %>% kable(caption = "Quantitative Methods", align = c("l", "r"))
#'
s3.mm <- s3[s3$cat == "M-MM", ] %>% droplevels()
# ftable(s3.mm$code, s3.mm$jrnl)
Rtdf(s3.mm$code, names = c(" ", "$N_{Articles}$")) %>% kable(caption = "Mixed-Methods", align = c("l", "r"))
#'
#' \newpage
#'
#' ## LGBTQ-IPV Research
#'
s4 <- ctbl.m[ctbl.m$scat == "LGBTQ-IPV Research", ]
#'
s4.top <- s4[s4$cat == "TOPIC", ] %>% droplevels()
# ftable(s4.top$code, s4.top$jrnl)
Rtdf(s4.top$code, names = c(" ", "$N_{Articles}$")) %>% kable(caption = "Primary Topics", align = c("l", "r"))
#'
s4.pop <- s4[s4$cat == "POPULATION", ] %>% droplevels()
# ftable(s4.pop$code, s4.pop$jrnl)
Rtdf(s4.pop$code, names = c(" ", "$N_{Articles}$")) %>% kable(caption = "Populations Included", align = c("l", "r"))
#'
s4.mo <- s4[s4$cat == "M-OVERALL", ] %>% droplevels()
# ftable(s4.mo$code, s4.mo$jrnl)
Rtdf(s4.mo$code, names = c(" ", "$N_{Articles}$")) %>% kable(caption = "Overarching Methodology", align = c("l", "r"))
#'
s4.ql <- s4[s4$cat == "M-QL", ] %>% droplevels()
# ftable(s4.ql$code, s4.ql$jrnl)
Rtdf(s4.ql$code, names = c(" ", "$N_{Articles}$")) %>% kable(caption = "Qualitative Methods", align = c("l", "r"))
#'
s4.qt <- s4[s4$cat == "M-QT", ] %>% droplevels()
# ftable(s4.qt$code, s4.qt$jrnl)
Rtdf(s4.qt$code, names = c(" ", "$N_{Articles}$")) %>% kable(caption = "Quantitative Methods", align = c("l", "r"))
#'
s4.mm <- s4[s4$cat == "M-MM", ] %>% droplevels()
# ftable(s4.mm$code, s4.mm$jrnl)
Rtdf(s4.mm$code, names = c(" ", "$N_{Articles}$")) %>% kable(caption = "Mixed-Methods", align = c("l", "r"))
#'
