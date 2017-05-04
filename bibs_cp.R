#' ---
#' title: "MAP - Bibliography (CP-Only)"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
# SETUP --------------------------------------------------------------

source("../SETUP.R")
knitr::opts_chunk$set(
    tidy = TRUE,
    echo = FALSE,
    fig.keep = 'high',
    fig.show = 'asis',
    results = 'asis',
    tidy.opts = list(comment = FALSE),
    echoRule = NULL,
    echoRuleb = NULL,
    fig.height = 5,
    fig.path = "graphics/bibs_cp/rplot-")#, dev = 'png')
panderOptions("table.emphasize.rownames", FALSE)
# fig.retina = 4
# rpm()
#'
#' \Frule
#'
#' \newpage
#'
#'
#+ journals
# JOURNALS -----------------------------------------------------------
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

jdat.m <- jdat[jdat[,2] >= m.cnt, ]
jdat.s <- jdat[jdat[,2] >= s.cnt, ]

j.vpr <-c("Journal of Interpersonal Violence",
          "Violence Against Women",
          "Violence and Victims",
          "Journal of Family Violence")

j.v <- sapply(j.vpr, tolower, USE.NAMES = FALSE)

j.cppr <- c("Action Research",
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
            "Cultural Diversity and Ethnic Minority Psychology",
            "Global Journal of Community Psychology Practice",
            "Health Education and Behavior",
            "Health Promotion Practice",
            "Journal of Applied Social Psychology",
            "Journal of Community and Applied Social Psychology",
            "Journal of Community Practice",
            "Journal of Community Psychology",
            "Journal of Health and Social Behavior",
            "Journal of Prevention and Intervention",
            "Journal of Primary Prevention",
            "Journal of Rural Community Psychology",
            "Journal of Social Issues",
            "Journal of Community Psychology",
            "Psychiatric Rehabilitation Journal",
            "Psychology of Women Quarterly",
            "Social Science and Medicine",
            "The Community Psychologist",
            "Transcultural Psychiatry",
            "Progress in Community Health Partnerships")

j.cp <- sapply(j.cppr, tolower, USE.NAMES = FALSE)
#'
#'
#+ pander_journals, echo=FALSE

### FUN - 'RtCap()' ####
RtCap <- function(x) {
    s0 <- strsplit(x, " ")[[1]]
    nocap <- c("a", "the", "to", "at", "in", "with", "and", "but", "or", "of")
    s1 <- ifelse(!s0 %in% nocap, toupper(substring(s0, 1,1)), s0)
    # s2 <- toupper(substring(s[!s %in% nocap], 1,1))
    s2 <- ifelse(!s0 %in% nocap, substring(s0, 2), "")
    s <- paste(s1, s2, sep="", collapse=" ")
    return(s)
}

j.cpp <- sapply(j.cp, RtCap, USE.NAMES = FALSE)
cat(tufte::newthought("Community-psychology journals"), "included in database searches:\n\n")
j.cpp %>% as.list() %>% pander()
#'
#' \newpage
#'
#' # \LARGE{\textsc{Results of Systematic Database Searches:}}
#'
#' \Frule
#'
#+ bibdf
### FUN - 'Rbibkeys()' ####
Rbibkeys <- function(bib) {
    keys <- bib[grep("\\@.*?\\{.*?,", bib, perl = TRUE)]
    keys <- gsub("\\@\\w+\\{(.*?)", "\\1", keys, perl = TRUE)
    keys <- keys[!grepl("\\%.*?,", keys, perl = TRUE)]
    keys <- gsub(" ", NA_character_, keys)
    keys <- gsub(",", "", keys)
    return(keys)
}

# BIB --------------------------------------------------------------

bib <- readLines("MAP.bib")
BIBKEY <- Rbibkeys(bib)

library(bib2df)
bibdf <- bib2df("MAP.bib")

### n.init ####
n.init <- nrow(bibdf)

ID <- seq(1:nrow(bibdf))
MAP.au <- cbind(BIBKEY, bibdf[, "AUTHOR"])
## bibdf[,2] ##
#'
#+ MAP
# MAP ----------------------------------------------------------------
MAP <- cbind(ID,
             BIBKEY,
             bibdf[, c("YEAR", "TITLE", "JOURNAL", "ABSTRACT")]) %>%
    as.data.frame()
## bibdf[c(3:5, 8)] ##
names(MAP)[-1] <- tolower(names(MAP)[-1])
#'
#+ MAP_RQDA, results='hide', fig.keep='none', fig.show='none'
# MAP-RQDA ----------------------------------------------------------------
source("MAPrqda.R", echo = FALSE)
#'
csid <- caseids[, c("caseid", "case", "RM", "scat")]
## caseids[, -3] ##
csid$case <- factor(csid$case)

MAP <- merge(MAP, csid, by.x = "bibkey", by.y = "case")
#'
#'
#+ MAP_CPV

# MAP-CPV ----------------------------------------------------------------

## (map.cp & map.v) ============================================================

MAP$journal <- sapply(MAP$journal, tolower)
MAP$journal <- gsub(" & ", " and ", MAP$journal)
MAP$journal <- factor(MAP$journal)

cp <- MAP$journal %in% j.cp
map.cp <- MAP[cp, , drop = FALSE]
levels(map.cp$journal) <- c(levels(map.cp$journal),
                            j.cp[!j.cp %in% levels(map.cp$journal)])
levels(map.cp$journal) <- sapply(levels(map.cp$journal), RtCap)

MAP <- map.cp
#'
#'
MAP$RM <- ifelse(MAP$RM == 1, NA_character_, 0)
MAPrm <- MAP[is.na(MAP$RM), "bibkey"] %>% droplevels() %>% as.character()
MAP <- na.omit(MAP)
MAP <- droplevels(MAP)
#'
#'
## map-jrnl ============================================================
MAP$journal <- factor(MAP$journal)
levels(MAP$journal) <- sapply(levels(MAP$journal), RtCap)
MAP$journal <- droplevels(MAP$journal)

### FUN - 'Rabbr()' ####
Rabbr <- function(x) {
    s0 <- strsplit(x, " ")[[1]]
    ex <- c("a", "the", "to", "at", "in", "with", "and", "but", "or", "of", "\\&")
    s1 <- s0[!s0 %in% ex]
    s2 <- substring(s1, 1,1)
    s <- paste(s2, sep = "", collapse = "")
    s <- toupper(s)
    return(s)
}

MAP$jrnl <- sapply(as.character(MAP$journal), Rabbr)
#'
#'
#+ cb

# CTBL ---------------------------------------------------------------

cb <- merge(MAP, ctbl, by = c("caseid", "scat"))
cb <- within(cb, {
    journal <- droplevels(journal)
    jrnl <- sapply(as.character(journal), Rabbr)
    code <- gsub("FG-\\w+", "FG", code)
    # code <- gsub("EXP-\\w+", "EXP", code)
    # code <- gsub("LT-\\w+", "LT", code)
    code <- gsub("SVY-QL-MM", "SVY-QL", code)
    code <- gsub("SVY-QT-MM", "SVY-QT", code)
    # code <- gsub("XS-\\w+", "XS", code)
    code <- gsub("IVW-\\w+", "IVW", code)
    code <- ifelse(code == "HET", NA, code) ## have not coded all cases for this ##
    code <- ifelse(code == "F", NA, code) ## have not coded all cases for this ##
    code <- ifelse(code == "M", NA, code) ## have not coded all cases for this ##
    # code <- gsub("SMIN-\\w+", NA, code) ## previously done to save space
    scat <- factor(scat, labels = c("IPV Interventions", "SMW-Inclusive Research"))
})
cb <- na.omit(cb) %>% droplevels()

# cbk$clab <- ifelse(cbk$code %in% cb$code, cbk$clab, NA)
# cbk <- na.omit(cbk) %>% droplevels()
cbk <- within(cbk, {
    clab <- ifelse(code == "SMIN-L", "Sexual Minorities - Lesbian", clab)
    clab <- ifelse(code == "SMIN-G", "Sexual Minorities - Gay", clab)
    clab <- ifelse(code == "SMIN-B", "Sexual Minorities - Bisexual", clab)
    clab <- ifelse(code == "SMIN-T", "Sexual Minorities - Transgender", clab)
    clab <- ifelse(code == "SMIN-Q", "Sexual Minorities - Queer", clab)
})
cb <- merge(cb, cbk, by = "code")
cb$code <- factor(cb$code)
cb$clab <- factor(cb$clab)
#'
#'
#+ FUN_Rftm

### FUN - 'Rftm()' ####

Rftm <- function(x1, x2, dnn = NULL, zero.action = NA, zero.qt = FALSE) {
    if (!is.null(dnn)) {
        tx <- Rtdf(x1, names = dnn[[1]])
        ftm <- ftable(x1, x2, row.vars = 1) %>%
            matrix(nrow = nrow(tx), byrow = FALSE)
        dimnames(ftm) <- list(levels(x1), dnn[[2]])
    } else {
        tx <- Rtdf(x1)
        ftm <- ftable(x1, x2, row.vars = 1) %>%
            matrix(nrow = nrow(tx), byrow = FALSE)
        dimnames(ftm) <- list(levels(x1))
    }
    if (!is.null(zero.action)) {
        if (zero.qt == 0 | zero.qt == 1) {
            zero.qt <- as.logical(zero.qt)
        }
        if (zero.qt == TRUE) {
            ftm <- ifelse(ftm == 0, quote(zero.action), ftm)
        } else {
            ftm <- ifelse(ftm == 0, noquote(zero.action), ftm)
        }
    }
    y <- list(tx, ftm)
    names(y) <- c(paste0("Tabulation of ", deparse(substitute(x1))),
                  paste0("Cross-Tabulation of ",
                         deparse(substitute(x1)),
                         " & ",
                         deparse(substitute(x2))))
    return(y)
}
#'
#' # General Research Categories
#'
#' \Frule
#'
#+ desc
### catpal ####

catpal <- c(adjustcolor(pal_my[16], alpha.f = 0.9), adjustcolor(pal_my[5], alpha.f = 0.9))

# DESCRIPTIVES ---------------------------------------------------------------

## search categories ============================================================

### [MAP-abstracts-S3.csv] ####
cpv.s3 <- MAP[MAP$scat == "S3", ]; ## write.csv(cpv.s3[order(cpv.s3$year), c("bibkey", "year", "title", "journal", "abstract")], "data/MAP-abstracts-S3.csv", row.names = FALSE)

### [MAP-abstracts-S4.csv] ####
cpv.s4 <- MAP[MAP$scat == "S4", ]; ## write.csv(cpv.s4[order(cpv.s4$year), c("bibkey", "year", "title", "journal", "abstract")], "data/MAP-abstracts-S4.csv", row.names = FALSE)

ct.scat <- within(MAP, {
    scat <- ifelse(scat == "S3", "IPV Interventions", "SMW-Inclusive Research")
})

t.scat <- Rtdf(ct.scat$scat, names = c("Category", "N"))
## "ct.scat" created in "MAPrqda.R" ##
t.scat %>% kable()
scat.t <- table(ct.scat$scat)
scat.bn <- Rbinom(scat.t)
scat.bn %>% pander(caption = "Binomial Test of the Difference in Search Category Proportions")

t.scat$prop = t.scat$N / sum(t.scat$N)
t.scat <- t.scat[order(t.scat$prop), ]
t.scat$ymax <- cumsum(t.scat$prop)
t.scat$ymin <- c(0, head(t.scat$ymax, n = -1))

### PLOT - scat ####
library(ggplot2)
scat.p <- ggplot(t.scat, aes(fill = Category,
                             ymax = ymax,
                             ymin = ymin,
                             xmax = 4,
                             xmin = 3)) +
    scale_fill_manual(values = catpal) +
    geom_rect() +
    coord_polar(theta = "y") +
    xlim(c(0, 4)) +
    annotate("text", x = 0, y = 0, label = "Category Proportions") +
    labs(title = "") +
    thm_Rtft(ytitle = FALSE) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = rel(1)),
          legend.key.width = unit(0.5, "cm"),
          legend.key.height = unit(0.5, "cm"))
scat.p
#'
#'
#' \newpage
#'
#' # Publication Titles
#' \Frule
#'
#+ pub_titles
## publication titles ============================================================
t.jrnl <- Rtdf(MAP$journal)
ft.jrnl <- with(MAP, {
    ftable(journal, scat) %>%
        matrix(nrow = nrow(t.jrnl),
               byrow = FALSE)
})
dimnames(ft.jrnl) <- list("Publication Title" = levels(MAP$journal),
                          Category = c("IPV Interventions", "SMW-Inclusive Research"))
ft.jrnl <- ifelse(ft.jrnl == 0, NA, ft.jrnl)
# ft.jrnl %>% pander(caption = "Number of Publications in Each Research Category per Journal",
#                    justify = c("left", "right", "right"))

cpv.s3$jrnl <- sapply(as.character(cpv.s3$journal), Rabbr) %>% factor()
cpv.s4$jrnl <- sapply(as.character(cpv.s4$journal), Rabbr) %>% factor()
j.cp <- sapply(j.cp, Rabbr, USE.NAMES = FALSE)
#'
#' <!--## Research Category by Journal & Journal Category-->
#'
#+ scat_x_journal, fig.height=4, fig.fullwidth=TRUE
cj.dnn <- c("Journal", "$N_{Articles}$")
sj.dnn <- c("IPV Interventions", "SMW-Inclusive Research")
journals <- Rftm(MAP$journal, MAP$scat, dnn = list(cj.dnn, sj.dnn))
t.j <- journals[[1]]
ftm.j <- journals[[2]]

ftm.j2 <- Rna(ft.jrnl)
sum.j <- apply(ftm.j2, 1, sum)
ftm.jp <- cbind(ft.jrnl, "**Total**" = sum.j)
ftm.jp %>% pander(justify = c("left", "right", "right", "right"),
                  caption = "$N_{articles}$ in Each Research Category per Journal")
Rdotchart(
    ftm.j,
    pch = 19,
    gcolor = pal_my[20],
    xlab = expression(N[Articles]),
    cex = 0.7,
    gcex = 0.75,
    gfont = 2,
    pt.cex = 1.125,
    color = c(rep(catpal[1], nrow(ftm.j)),
              rep(catpal[2], nrow(ftm.j))),
    xaxt = 'n'
); axis(1, at = seq(range(ftm.j, na.rm = TRUE)[1],
                    range(ftm.j, na.rm = TRUE)[2], by = 1))

MAP.j <- MAP[, c("scat", "journal")]
names(MAP.j) <- c("Category", "Journal")
MAP.j$Category <- ifelse(MAP.j$Category == "S3",
                         "IPV Interventions",
                         "SMW-Inclusive Research")
pj <- mpal(1:length(unique(MAP$jrnl)), p = sci)
library(ggparallel)
pscat <- c("#a6afbb", pal_my[17])

j.ps <- ggparset2(list("Category", "Journal"),
                  data = MAP.j,
                  method = "parset", label = TRUE,
                  label.size = 2.75, text.angle = 0, order = c(1, 1)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pj, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pj), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
# j.ps

#'
#' \tufteskip
#'
#'
#+ echo=FALSE
#### NO MORE ECHO ####
## knitr::opts_chunk$set(echo = FALSE)
#'
#' \newpage
#'
#' # Research Topics, Sampling Frames, and Methodologies
#'
#' \Frule
#'
#' ## Primary Topics
#'
#+ topics, fig.fullwidth=TRUE, fig.height = 7.5

## topics ============================================================
# cb$clab <- factor(cb$clab)
codes.tp <- cb[cb$cat == "TOPIC", "clab"] %>% droplevels()
ctp.dnn <- c("Topic", "$N_{Articles}$")
scats.tp <- cb[cb$cat == "TOPIC", "scat"] %>% droplevels()
stp.dnn <- c("IPV Interventions", "SMW-Inclusive Research")
topics <- Rftm(codes.tp, scats.tp, dnn = list(ctp.dnn, stp.dnn))
t.tp <- topics[[1]]
ftm.tp <- topics[[2]]
ftm.tp2 <- Rna(ftm.tp)
sum.tp <- apply(ftm.tp2, 1, sum)
ftm.tpp <- cbind(ftm.tp, "**Total**" = sum.tp)
ftm.tpp %>% pander(justify = c("left", "right", "right", "right"),
                   caption = "Research Topics")

### PLOT - topic - 1 ####
Rdotchart(
    ftm.tp,
    pch = 19,
    gcolor = pal_my[20],
    xlab = expression(N[Articles]),
    cex = 0.7,
    gcex = 0.75,
    gfont = 2,
    pt.cex = 1.125,
    color = c(rep(catpal[1], nrow(ftm.tp)), rep(catpal[2], nrow(ftm.tp))))
## xaxt = 'n'
#; axis(1, at = seq(range(ftm.tp, na.rm = TRUE)[1],
## range(ftm.tp, na.rm = TRUE)[2], by = 3))
#'
#' \newpage
#'
#+ topics2, fig.fullwidth=TRUE
dfm.tp2 <- data.frame(ftm.tp)
names(dfm.tp2) <- c("s3", "s4")
top.s3 <- data.frame(dfm.tp2$s3, row.names = rownames(dfm.tp2))
top.s3 <- na.omit(top.s3)
# top.s3
top.s4 <- data.frame(dfm.tp2$s4, row.names = rownames(dfm.tp2))
top.s4 <- na.omit(top.s4)
# top.s4

tp.s3 <- cb[cb$scat == levels(cb$scat)[1] &
                cb$cat == "TOPIC", ] %>%
    droplevels()
tp.s4 <- cb[cb$scat == levels(cb$scat)[2] &
                cb$cat == "TOPIC", ] %>%
    droplevels()
#'
#' \newpage
#' ## Research Designs
#'
#+ designs, fig.fullwidth=TRUE
## designs ============================================================

ct.d <- cb[cb$cat == "DESIGN", ] %>% droplevels()
t.d <- Rtdf(ct.d$clab)
ct.d$clab <- gsub(" Design", "", ct.d$clab) %>% factor()
ft.d <- ftable(ct.d[, c("clab", "scat")], row.vars = 1)
ftm.d <- matrix(ft.d, nrow = nrow(t.d), byrow = FALSE)
dimnames(ftm.d) <- list(Design = levels(ct.d$clab),
                        Category = c("IPV Interventions",
                                     "SMW-Inclusive Research"))
sum.d <- apply(ftm.d, 1, sum)
ftm.d <- ifelse(ftm.d == 0, NA, ftm.d)
ftm.d <- cbind(ftm.d, "**Total**" = sum.d)
ftm.d %>% pander(justify = c("left", "right", "right", "right"),
                 caption = "Overarching Research Designs")

nlabs <- length(unique(ct.d$clab))
pmo <- mpal(1:length(unique(ct.d$clab)), p = sci)

ct.d <- dplyr::rename(ct.d, "Category" = scat, "Design" = clab)

### PLOT - designs ####

library(ggparallel)
pscat <- c("#a6afbb", pal_my[17])
d.ps <- ggparset2(list("Category", "Design"),
                  data = ct.d,
                  method = "parset", label = TRUE,
                  label.size = 3.5, text.angle = 0, order = c(1, 1)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pmo, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pmo), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
d.ps
#'
#' \newpage
#' `r tufte::newthought("\\Large{Experimental Research Designs}")`
#'
#+ exp, fig.fullwidth=TRUE
## experimental designs ============================================================

ct.exp <- cb[cb$cat == "DESIGN-EXP", ] %>% droplevels()
t.exp <- Rtdf(ct.exp$clab)
ct.exp$clab <- gsub(" Design", "", ct.exp$clab) %>% factor()
ct.exp$clab <- gsub(" \\(", " \n\\(", ct.exp$clab) %>% factor()
ft.exp <- ftable(ct.exp[, c("clab", "scat")], row.vars = 1)
ftm.exp <- matrix(ft.exp, nrow = nrow(t.exp), byrow = FALSE)
dimnames(ftm.exp) <- list("Experimental Design" = levels(ct.exp$clab),
                          Category = c("IPV Interventions",
                                       "SMW-Inclusive Research"))
sum.exp <- apply(ftm.exp, 1, sum)
ftm.exp <- ifelse(ftm.exp == 0, NA, ftm.exp)
ftm.exp <- cbind(ftm.exp, "**Total**" = sum.exp)
ftm.exp %>% pander(justify = c("left", "right", "right", "right"),
                   caption = "Experimental Research Designs")

nlabs <- length(unique(ct.exp$clab))
pmo <- mpal(1:length(unique(ct.exp$clab)), p = sci)

ct.exp <- dplyr::rename(ct.exp, "Category" = scat, "Experimental Design" = clab)

### PLOT - experimental designs ####

library(ggparallel)
pscat <- c("#a6afbb", pal_my[17])
exp.ps <- ggparset2(list("Category", "Experimental Design"),
                    data = ct.exp,
                    method = "parset", label = TRUE,
                    label.size = 3, text.angle = 0, order = c(1, 1)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pmo, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pmo), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
exp.ps
#'
#' \newpage
#' ## Data Collection Methodologies
#'
#+ methodologies, fig.fullwidth=TRUE
## methodologies ============================================================

### PLOT - methodologies - 1 ####

ct.mo <- cb[cb$cat == "METHODS", ] %>% droplevels()
t.mo <- Rtdf(ct.mo$clab)

ft.mo <- ftable(ct.mo[, c("clab", "scat")], row.vars = 1)
ftm.mo <- matrix(ft.mo, nrow = nrow(t.mo), byrow = FALSE)
dimnames(ftm.mo) <- list(Methodology = levels(ct.mo$clab),
                         Category = c("IPV Interventions",
                                      "SMW-Inclusive Research"))
sum.mo <- apply(ftm.mo, 1, sum)
ftm.mo <- ifelse(ftm.mo == 0, NA, ftm.mo)
ftm.mo <- cbind(ftm.mo, "**Total**" = sum.mo)
ftm.mo %>% pander(justify = c("left", "right", "right", "right"),
                  caption = "Methodologies")

### PLOT - methodologies ####

nlabs <- length(unique(ct.mo$clab))
pmo <- mpal(1:length(unique(ct.mo$clab)), p = sci)

ct.mo <- dplyr::rename(ct.mo, "Category" = scat, "Methodology" = clab)

library(ggparallel)
pscat <- c("#a6afbb", pal_my[17])
mo.ps <- ggparset2(list("Category", "Methodology"),
                   data = ct.mo,
                   method = "parset", label = TRUE,
                   label.size = 3.5, text.angle = 0, order = c(1, 1)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pmo, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pmo), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
mo.ps
#'
#' \newpage
#' `r tufte::newthought("\\large{QuaLitative \\textit{Methods}}")`
#'
#+ qual, fig.fullwidth=TRUE
## qualitative ============================================================

ct.ql <- cb[cb$cat == "M-QL", ] %>% droplevels()
t.ql <- Rtdf(ct.ql$clab)
ft.ql <- ftable(ct.ql[, c("clab", "scat")], row.vars = 1)
ftm.ql <- matrix(ft.ql, nrow = nrow(t.ql), byrow = FALSE)
dimnames(ftm.ql) <- list("Qua**L**itative Method(s)" = levels(ct.ql$clab),
                         Category = c(#"IPV Interventions",
                                      "SMW-Inclusive Research"))
sum.ql <- apply(ftm.ql, 1, sum)
ftm.ql <- ifelse(ftm.ql == 0, NA, ftm.ql)
ftm.ql <- cbind(ftm.ql, "**Total**" = sum.ql)
ftm.ql %>% pander(justify = c("left", "right", "right"),
                  caption = "Qua**L**itative Method(s)")

### PLOT - qualitative ####
pql <- mpal(1:length(unique(ct.ql$clab)), p = sci)

ct.ql <- dplyr::rename(ct.ql, "QuaLitative Methods" = clab, "Category" = scat)


ql.ps <- ggparset2(list("Category", "QuaLitative Methods"),
                   data = na.omit(ct.ql),
                   method = "parset", label = TRUE,
                   label.size = 3.5, text.angle = 0, order = c(1, 1)) +
    scale_fill_manual(values = c(pscat[1], adjustcolor(pql, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat[1], pql), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
ql.ps
#'
#' \newpage
#'
#' `r tufte::newthought("\\Large{QuaNTitative Research \\textit{Designs}}")`
#'
#+ quaNT_designs, fig.fullwidth=TRUE
ct.dqt <- cb[cb$cat == "D-QT", ] %>% droplevels()
t.dqt <- Rtdf(ct.dqt$clab)
ft.dqt <- ftable(ct.dqt[, c("clab", "scat")], row.vars = 1)
ftm.dqt <- matrix(ft.dqt, nrow = nrow(t.dqt), byrow = FALSE)
dimnames(ftm.dqt) <- list("Qua**NT**itative Design" = levels(ct.dqt$clab),
                          Category = c("IPV Interventions",
                                       "SMW-Inclusive Research"))
# t.dqt
sum.dqt <- apply(ftm.dqt, 1, sum)
ftm.dqt <- ifelse(ftm.dqt == 0, NA, ftm.dqt)
ftm.dqt <- cbind(ftm.dqt, "**Total**" = sum.dqt)
ftm.dqt %>% pander(justify = c("left", "right", "right", "right"),
                   caption = "Qua**NT**itative Designs")

### PLOT - quantitative ####

nlabs <- length(unique(ct.dqt$clab))
pqt <- mpal(1:length(unique(ct.dqt$clab)), p = sci)

ct.dqt <- dplyr::rename(ct.dqt, "QuaNTitative Design" = clab, "Category" = scat)


dqt.ps <- ggparset2(list("Category", "QuaNTitative Design"),
                    data = ct.dqt,
                    method = "parset", label = TRUE,
                    label.size = 3.5, text.angle = 0, order = c(1, 1)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pqt, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pqt), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
dqt.ps
#'
#'
#' \newpage
#' `r tufte::newthought("\\large{QuaNTitative \\textit{Methods}}")`
#'
#+ quant, fig.fullwidth=TRUE, fig.height=6
### quantitative ============================================================

ct.qt <- cb[cb$cat == "M-QT", ] %>% droplevels()
t.qt <- Rtdf(ct.qt$clab)
ft.qt <- ftable(ct.qt[, c("clab", "scat")], row.vars = 1)
ftm.qt <- matrix(ft.qt, nrow = nrow(t.qt), byrow = FALSE)
dimnames(ftm.qt) <- list("Qua**NT**itative Method" = levels(ct.qt$clab),
                         Category = c("IPV Interventions",
                                      "SMW-Inclusive Research"))
# t.qt
sum.qt <- apply(ftm.qt, 1, sum)
ftm.qt <- ifelse(ftm.qt == 0, NA, ftm.qt)
ftm.qt <- cbind(ftm.qt, "**Total**" = sum.qt)
ftm.qt %>% pander(justify = c("left", "right", "right", "right"),
                  caption = "Qua**NT**itative Methods")

### PLOT - quantitative ####

nlabs <- length(unique(ct.qt$clab))
pqt <- mpal(1:length(unique(ct.qt$clab)), p = sci)

ct.qt <- dplyr::rename(ct.qt, "QuaNTitative Methods" = clab, "Category" = scat)


qt.ps <- ggparset2(list("Category", "QuaNTitative Methods"),
                   data = ct.qt,
                   method = "parset", label = TRUE,
                   label.size = 3.5, text.angle = 0, order = c(1, 1)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pqt, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pqt), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
qt.ps
#'
#' \newpage
#' `r tufte::newthought("\\large{Archival/Secondary Data Sources}")`
#'
#+ m_rcrd, fig.fullwidth=TRUE
### archival/secondary data sources ####
ct.rcrd <- cb[cb$cat == "M-RCRD", ] %>% droplevels()
t.rcrd <- Rtdf(ct.rcrd$clab)
ft.rcrd <- ftable(ct.rcrd[, c("clab", "scat")], row.vars = 1)
ftm.rcrd <- matrix(ft.rcrd, nrow = nrow(t.rcrd), byrow = FALSE)
dimnames(ftm.rcrd) <- list("Archival Data Source" = levels(ct.rcrd$clab),
                           Category = c(#"IPV Interventions",
                                        "SMW-Inclusive Research"))
# t.rcrd
sum.rcrd <- apply(ftm.rcrd, 1, sum)
ftm.rcrd <- ifelse(ftm.rcrd == 0, NA, ftm.rcrd)
ftm.rcrd <- cbind(ftm.rcrd, "**Total**" = sum.rcrd)
ftm.rcrd %>% pander(justify = c("left", "right", "right"),
                    caption = "Archival Data Sources")

### PLOT - archival/secondary data sources ####

nlabs <- length(unique(ct.rcrd$clab))
prcrd <- mpal(1:length(unique(ct.rcrd$clab)), p = sci)

ct.rcrd <- dplyr::rename(ct.rcrd, "Archival Data Source" = clab, "Category" = scat)


rcrd.ps <- ggparset2(list("Category", "Archival Data Source"),
                     data = ct.rcrd,
                     method = "parset", label = TRUE,
                     label.size = 3.5, text.angle = 0, order = c(1, 1)) +
    scale_fill_manual(values = rev(c(rev(pscat), adjustcolor(prcrd, alpha.f = 0.55))),
                      guide = FALSE) +
    scale_colour_manual(values = rev(c(rev(pscat), prcrd)), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
rcrd.ps
#'
#' \newpage
#'
#' `r tufte::newthought("\\Large{Mixed-Methodological \\textit{Designs}}")`
#'
#+ mm_designs, fig.fullwidth=TRUE
## mixed-methods designs ========================================================

ct.dmm <- cb[cb$cat == "D-MM", ] %>% droplevels()
t.dmm <- Rtdf(ct.dmm$clab)
ft.dmm <- ftable(ct.dmm[, c("clab", "scat")], row.vars = 1)
ftm.dmm <- matrix(ft.dmm, nrow = nrow(t.dmm), byrow = FALSE)
dimnames(ftm.dmm) <- list("Mixed-Methodological Design" = levels(ct.dmm$clab),
                          scat = c(#"IPV Interventions",
                                   "SMW-Inclusive Research"))
# t.dmm
sum.dmm <- apply(ftm.dmm, 1, sum)
ftm.dmm <- ifelse(ftm.dmm == 0, NA, ftm.dmm)
ftm.dmm <- cbind(ftm.dmm, "**Total**" = sum.dmm)
ftm.dmm %>% pander(justify = c("left", "right", "right"),
                   caption = "Mixed-Methodological Designs")

### PLOT - mixed-methods ####

nlabs <- length(unique(ct.dmm$clab))
pmm <- mpal(1:length(unique(ct.dmm$clab)), p = sci)

ct.dmm <- dplyr::rename(ct.dmm, "Mixed-Methodological Design" = clab, "Category" = scat)


dmm.ps <- ggparset2(list("Category", "Mixed-Methodological Design"),
                    data = ct.dmm,
                    method = "parset", label = TRUE,
                    label.size = 3.5, text.angle = 0, order = c(-1,-1)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pmm, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pmm), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
dmm.ps
#' \newpage
#' `r tufte::newthought("\\large{Mixed (QuaLitative \\& QuaNTitative) \\textit{Methods}}")`
#'
#+ mm_methods, fig.fullwidth=TRUE
## mixed-methods ============================================================

ct.mm <- cb[cb$cat == "M-MM", ] %>% droplevels()
t.mm <- Rtdf(ct.mm$clab)
ft.mm <- ftable(ct.mm[, c("clab", "scat")], row.vars = 1)
ftm.mm <- matrix(ft.mm, nrow = nrow(t.mm), byrow = FALSE)
dimnames(ftm.mm) <- list("Mixed-Methods" = levels(ct.mm$clab),
                         scat = c(#"IPV Interventions",
                                  "SMW-Inclusive Research"))
# t.mm
sum.mm <- apply(ftm.mm, 1, sum)
ftm.mm <- ifelse(ftm.mm == 0, NA, ftm.mm)
ftm.mm <- cbind(ftm.mm, "**Total**" = sum.mm)
ftm.mm %>% pander(justify = c("left", "right", "right"),
                  caption = "Research Topics")

### PLOT - mixed-methods ####

nlabs <- length(unique(ct.mm$clab))
pmm <- mpal(1:length(unique(ct.mm$clab)), p = sci)

ct.mm <- dplyr::rename(ct.mm, "Mixed-Methods" = clab, "Category" = scat)


mm.ps <- ggparset2(list("Category", "Mixed-Methods"),
                   data = na.omit(ct.mm),
                   method = "parset", label = TRUE,
                   label.size = 3.5, text.angle = 0, order = c(-1,-1)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pmm, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pmm), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
mm.ps
#'
#' \newpage
#' ## Target Populations & Sampling Frames
#'
#+ populations, fig.fullwidth=TRUE, fig.height = 7.5
## populations ============================================================

ct.pop <- cb[cb$cat == "POPULATION", ] %>% droplevels()
t.pop <- Rtdf(ct.pop$clab)
ft.pop <- ftable(ct.pop[, c("clab", "scat")], row.vars = 1)
ftm.pop <- matrix(ft.pop, nrow = nrow(t.pop), byrow = FALSE)
dimnames(ftm.pop) <-
    list(
        "Populations" = levels(ct.pop$clab),
        scat = c("IPV Interventions", "SMW-Inclusive Research")
    )

sum.pop <- apply(ftm.pop, 1, sum)
ftm.pop <- ifelse(ftm.pop == 0, NA, ftm.pop)
ftm.popp <- cbind(ftm.pop, "**Total**" = sum.pop)
ftm.popp %>% pander(justify = c("left", "right", "right", "right"),
                    caption = "Research Topics")

### PLOT - populations - 1 ####

Rdotchart(
    ftm.pop,
    pch = 19,
    gcolor = pal_my[20],
    xlab = expression(N[Articles]),
    cex = 0.7,
    gcex = 0.75,
    gfont = 2,
    pt.cex = 1.125,
    color = c(rep(catpal[1], nrow(ftm.pop)), rep(catpal[2], nrow(ftm.pop))))
#'
#' \newpage
#'
#' # References`r Rcite_r(file = "../auxDocs/REFs.bib", footnote = TRUE)`
#'
#'
#' \parindent=-1.75em
#'
#' \setlength{\parskip}{0.25\baselineskip}
#'
