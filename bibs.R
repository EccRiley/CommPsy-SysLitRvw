#' ---
#' title: "MAP - Bibliography"
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
    echoRule = NULL,
    echoRuleb = TRUE
)
rpm()
# options(warn = -1)
# FUN - 'RtCap()' -------------------------------------------------------
RtCap <- function(x) {
    s0 <- strsplit(x, " ")[[1]]
    nocap <- c("a", "the", "to", "at", "in", "with", "and", "but", "or", "of")
    s1 <- ifelse(!s0 %in% nocap, toupper(substring(s0, 1,1)), s0)
    # s2 <- toupper(substring(s[!s %in% nocap], 1,1))
    s2 <- ifelse(!s0 %in% nocap, substring(s0, 2), "")
    s <- paste(s1, s2, sep="", collapse=" ")
    return(s)
}
# FUN - 'Rabbr()' -------------------------------------------------------
Rabbr <- function(x) {
    s0 <- strsplit(x, " ")[[1]]
    ex <- c("a", "the", "to", "at", "in", "with", "and", "but", "or", "of", "\\&")
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

j.v <- c("Journal of Interpersonal Violence",
         "Violence Against Women",
         "Violence and Victims",
         "Journal of Family Violence") %>%
    tolower()
#, "Aggression and Violent Behavior", "Partner Abuse", "Trauma, Violence, \\& Abuse", "Psychology of Violence", "Journal of Emotional Abuse", "Response to the Victimization of Women \\& Children")

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
# FUN - 'Rbibkeys()' -------------------------------------------------------
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
ID <- seq(1:nrow(bibdf))
MAP.au <- cbind(BIBKEY, bibdf[, "AUTHOR"]) ## bibdf[,2] ##
#'
#+ MAP
# MAP ----------------------------------------------------------------
MAP <- cbind(ID,
             BIBKEY,
             bibdf[, c("YEAR", "TITLE", "JOURNAL", "ABSTRACT")]) %>%
    as.data.frame()## bibdf[c(3:5, 8)] ##
names(MAP)[-1] <- tolower(names(MAP)[-1])
# names(MAP)[2]
#'
#+ MAP_RQDA, results='hide', fig.keep='none', fig.show='none'
# MAP-RQDA ----------------------------------------------------------------
source("MAPrqda.R", echo = FALSE)

csid <- caseids[, c("caseid", "case", "RM", "scat")] ## caseids[, -3] ##
csid$case <- factor(csid$case)

MAP <- merge(MAP, csid, by.x = "bibkey", by.y = "case")
#'
#+ MAP_CPV

# MAP-CPV ----------------------------------------------------------------

# (map.cp & map.v) ============================================================
MAP$journal <- sapply(MAP$journal, tolower)
MAP$journal <- factor(MAP$journal)
Rtdf(MAP$journal)
ftable(MAP$journal, MAP$scat)

cp <- MAP$journal %in% j.cp
map.cp <- MAP[cp, , drop = FALSE]
levels(map.cp$journal) <- c(levels(map.cp$journal),
                            j.cp[!j.cp %in% levels(map.cp$journal)])
levels(map.cp$journal) <- sapply(levels(map.cp$journal), RtCap)

vlc <- MAP$journal %in% j.v
map.v <- MAP[vlc, , drop = FALSE]
levels(map.v$journal) <- c(levels(map.v$journal),
                           j.v[!j.v %in% levels(map.v$journal)])
levels(map.v$journal) <- sapply(levels(map.v$journal), RtCap)

MAP <- MAP[cp | vlc, , drop = FALSE]

# map-na.omit ============================================================
MAP$RM <- ifelse(MAP$RM == 1, NA_character_, 0)
apply(MAP, 2, Risna)
MAP <- na.omit(MAP)
MAP <- droplevels(MAP)
#'
#+ echo=FALSE
paste0("\\[ N_{Articles} = ", nrow(MAP), " \\]")
#'
# map-jrnl ============================================================
MAP$journal <- factor(MAP$journal)
levels(MAP$journal) <- sapply(levels(MAP$journal), RtCap)
MAP$journal <- droplevels(MAP$journal)
MAP$jrnl <- sapply(as.character(MAP$journal), Rabbr)
#'
#' \newpage
#'
#+ ctbl_m

# CTBL ---------------------------------------------------------------

ctbl.m <- merge(MAP, ctbl, by = c("caseid", "scat"))

clabs <- c("African Americans", "Approach Evaluation", "'At Risk' Populations", "Secondary/Archival Data", "Asian Americans", "Community Capacity", "Coordinated Community Response", "Cis-Gender", "College Students", "IPV Consequences", "Couples", "Non-IPV Crime Victims", "Case Study", "Disabled Persons", "IPV Dynamics", "Experimental Design", "Females/Women/Girls", "Focus Groups", "General Population", "Graduate Students", "Heterosexuals", "Help-Seeking", "IPV Interventions (Int.) - General", "Int. - Description", "Int. - Proposal", "IPV-Perpetrators", "IPV-Victims/Survivors", "Group Interviews", "1-on-1 Interviews", "Latinos/Latinas or Hispanic-Americans", "Longitudinal", "Males/Men/Boys", "Measures", "Multiple Qualitative Methods", "Multiple Quantitative Methods", "Mixed-Methods", "Program Evaluation Methods", "Perpetrator Characteristics", "Program/Policy Development", "Outsiders' Perspectives", "Intervention Providers' Perspectives", "Key Stakeholders' Persepctives", "Victims'/Survivors' Perspectives", "Program/Policy Evaluation - General", "Protective Factors", "Public Policy", "Community-Based Practitioners", "IPV-Specific Community-Based Practitioners", "IPV Prevalence", "IPV Intervention Programs", "Parents", "Qualitative", "Quantitative", "Client Records", "Police Records", "Risk Factors", "Racial Minorities", "Sexual Minorities (SM)", "SM - Bisexuals", "SM - Gay", "SM - Lesbian", "SM - Queer", "SM - Transgender", "Qualitative Survey", "Quantitative Survey", "System Entities", "System Response", "Urban-Specific", "Cross-Sectional", "Children/Youth")

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
#' # \LARGE{\textsc{Reviewed Literature Descriptives}}
#'
#' \Frule
#'
#' # General Research Categories
#'
#+ desc
### catpal ####
catpal <- c(adjustcolor(pal_my[16], alpha.f = 0.8), adjustcolor(pal_my[18], alpha.f = 0.9))

# DESCRIPTIVES ---------------------------------------------------------------

# search categories ============================================================

### [MAP-abstracts-S3.csv] ####
cpv.s3 <- MAP[MAP$scat == "S3", ]
# write.csv(cpv.s3[order(cpv.s3$year), c("bibkey", "year", "title", "journal", "abstract")], "data/MAP-abstracts-S3.csv", row.names = FALSE)

### [MAP-abstracts-S4.csv] ####
cpv.s4 <- MAP[MAP$scat == "S4", ]
# write.csv(cpv.s4[order(cpv.s4$year), c("bibkey", "year", "title", "journal", "abstract")], "data/MAP-abstracts-S4.csv", row.names = FALSE)

ct.scat <- within(MAP, {
    scat <- ifelse(scat == "S3", "IPV Interventions", "LGBTQ-IPV Research")
})

t.scat <- Rtdf(ct.scat$scat, names = c("Category", "N"))
## "ct.scat" created in "MAPrqda.R" ##
t.scat
scat.t <- table(ct.scat$scat)
prop.test(scat.t)

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
#' # Publication Titles
#'
#+ pub_titles
# publication titles ============================================================
t.jrnl <- Rtdf(MAP$journal)
ft.jrnl <- with(MAP, {
    ftable(journal, scat) %>%
        matrix(nrow = nrow(t.jrnl),
               byrow = FALSE)
})
dimnames(ft.jrnl) <- list("Publication Title" = levels(MAP$journal), Category = c("IPV Interventions", "LGBTQ-IPV Research"))
ft.jrnl

t.s3bibkey <- Rtdf(cpv.s3$bibkey)
cpv.s3$jrnl <- sapply(as.character(cpv.s3$journal), Rabbr) %>% factor()
ft.s3jrnl <- with(cpv.s3, {
    ftable(bibkey, jrnl) %>%
        matrix(nrow = nrow(t.s3bibkey),
               byrow = FALSE)
})
dimnames(ft.s3jrnl) <- list("Case" = levels(cpv.s3$bibkey), "Publication Title" = levels(cpv.s3$jrnl))
ft.s3jrnl

t.s4bibkey <- Rtdf(cpv.s4$bibkey)
cpv.s4$jrnl <- sapply(as.character(cpv.s4$journal), Rabbr) %>% factor()
ft.s4jrnl <- with(cpv.s4, {
    ftable(bibkey, jrnl) %>%
        matrix(nrow = nrow(t.s4bibkey),
               byrow = FALSE)
})
dimnames(ft.s4jrnl) <- list("Case" = levels(cpv.s4$bibkey), "Publication Title" = levels(cpv.s4$jrnl))
ft.s4jrnl


#'
#' \newpage
#'
#' # Publication Years
#'
#+ yr_hist1, echo=FALSE, fig.fullwidth=TRUE, fig.width=7, fig.height=5
# publication years ============================================================

par(cex = 0.9)
frq <- seq(0, max(hist(MAP$year, plot = FALSE)$counts), by = 5)[1:4]
hist(MAP$year, freq = FALSE, col = pal_my.a75[17], border = pal_my[18],
     main = " ", xlab = "Year Published", ylab = expression(N[Articles]),
     lwd = .5, yaxt = 'n', right = T, breaks = seq(1990, 2018, by = 4));
lines(density(MAP$year), lwd = 2, col = pal_my[18], lty = 3);
axis(side = 2, at = axTicks(side = 2), labels = frq)
#'
#' \tufteskip
#' \begin{fullwidth}\begin{centering}
#'
#+ yr_hist2, echo=FALSE, fig.width=7, fig.height=4, out.width="0.85\\linewidth"
### PLOT - year ####

par(mfrow = c(1, 2), cex = 0.8)

frq.s3 <- seq(0, max(hist(cpv.s3$year, plot = FALSE)$counts), by = 2)
frq.s3 <- frq.s3[-length(frq.s3)]
hist(cpv.s3$year, freq = FALSE, col = pal_my.a75[16], border = pal_my[19],
     main = "IPV Interventions Research", xlab = " ", ylab = expression(N[Articles]),
     lwd = .5, yaxt = 'n', right = F, breaks = seq(1990, 2018, by = 4));
lines(density(cpv.s3$year), lwd = 2, col = pal_my[19], lty = 3);
axis(side = 2, at = axTicks(side = 2), labels = frq.s3);

frq.s4 <- seq(0,max(hist(cpv.s4$year, plot = FALSE)$counts),by = 5)
hist(cpv.s4$year, freq = FALSE, col = pal_my.a75[12], border = pal_my[19],
     main = "LGBTQ-Specific IPV Research", xlab = " ", ylab = " ",
     lwd = .5, yaxt = 'n', right = F, breaks = seq(1990, 2018, by = 4));
lines(density(cpv.s4$year), lwd = 2, col = pal_my[19], lty = 3);
axis(side = 2, at = axTicks(side = 2), labels = frq.s4)

par(mfrow = c(1, 1))

yr <- Rmsmm(MAP$year) %>% t() %>% as.data.frame()

yrt <- Rtdf(MAP$year)[, 2] %>% Rmsmm() %>% t() %>% as.data.frame()
yrt[, c(1, 3:4)] <- apply(yrt[, c(1, 3:4)], 2, round, digits = 0)
yrt$M <- paste0(yrt$M, " (\\textit{", yrt$SD, "})")
yrt <- yrt[c(1, 3:4)]
names(yrt) <- c("Mean (\\textit{SD})", "Minimum", "Maximum")
#'
#' \end{centering}\end{fullwidth}
#'
#+ yrt, echo=FALSE
yrt.s3 <- Rtdf(cpv.s3$year)[, 2] %>% Rmsmm() %>% t() %>% as.data.frame()
yrt.s3[, c(1, 3:4)] <- apply(yrt.s3[, c(1, 3:4)], 2, round, digits = 0)
yrt.s3$M <- paste0(yrt.s3$M, " (\\textit{", yrt.s3$SD, "})")
yrt.s3 <- yrt.s3[c(1, 3:4)]
names(yrt.s3) <- c("Mean (\\textit{SD})", "Minimum", "Maximum")

yrt.s4 <- Rtdf(cpv.s4$year)[, 2] %>% Rmsmm() %>% t() %>% as.data.frame()
yrt.s4[, c(1, 3:4)] <- apply(yrt.s4[, c(1, 3:4)], 2, round, digits = 0)
yrt.s4$M <- paste0(yrt.s4$M, " (\\textit{", yrt.s4$SD, "})")
yrt.s4 <- yrt.s4[c(1, 3:4)]
names(yrt.s4) <- c("Mean (\\textit{SD})", "Minimum", "Maximum")
#'
#' \tufteskip
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
#+ FUN_Rftm
# FUN - 'Rftm()' -------------------------------------------------------
Rftm <- function(x1, x2, dnn = NULL, zero.action = NA, zero.qt = FALSE) {
    if (!is.null(dnn)) {
        tx <- Rtdf(x1, names = dnn[[1]])
        ftm <- ftable(x1, x2, row.vars = 1) %>% matrix(nrow = nrow(tx), byrow = FALSE)
        dimnames(ftm) <- list(levels(x1), dnn[[2]])
    } else {
        tx <- Rtdf(x1)
        ftm <- ftable(x1, x2, row.vars = 1) %>% matrix(nrow = nrow(tx), byrow = FALSE)
        dimnames(ftm) <- list(levels(x1))
    }
    if (!is.null(zero.action)) {
        if (zero.qt == 0 | zero.qt == 1) {
            zero.qt <- as.logical(zero.qt)
        } else {
            if (!is.logical(zero.qt)) {
                stop("'zero.qt' must be either logical ('TRUE'/'FALSE') or interpretable as logical ('0'/'1'")
            }
        }
        if (zero.qt == TRUE) {
            ftm <- ifelse(ftm == 0, quote(zero.action), ftm)
        } else {
            ftm <- ifelse(ftm == 0, noquote(zero.action), ftm)
        }
    }
    y <- list(tx, ftm)
    names(y) <- c(paste0("Tabulation of ", deparse(substitute(x1))), paste0("Cross-Tabulation of ",
                                                                            deparse(substitute(x1)), " & ", deparse(substitute(x2))))
    return(y)
}


#' ## Primary Topics
#'
#+ topics, fig.fullwidth=TRUE

# topics ============================================================

codes.tp <- ctbl.m[ctbl.m$cat == "TOPIC", "code"] %>% droplevels()
ctp.dnn <- c("Topic", "$N_{Articles}$")
scats.tp <- ctbl.m[ctbl.m$cat == "TOPIC", "scat"] %>% droplevels()
stp.dnn <- c("IPV Interventions", "LGBTQ-IPV Research")
topics <- Rftm(codes.tp, scats.tp, dnn = list(ctp.dnn, stp.dnn))
t.tp <- topics[[1]]
ftm.tp <- topics[[2]]

ftm.tp %>% pander

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
    color = c(rep(catpal[1], nrow(ftm.tp)), rep(catpal[2], nrow(ftm.tp))),
    xaxt = 'n'
); axis(1, at = seq(range(ftm.tp, na.rm = TRUE)[1], range(ftm.tp, na.rm = TRUE)[2], by = 3))

dfm.tp2 <- data.frame(ftm.tp)
names(dfm.tp2) <- c("s3", "s4")
top.s3 <- data.frame(dfm.tp2$s3, row.names = rownames(dfm.tp2))
top.s3 <- na.omit(top.s3)
top.s3
top.s4 <- data.frame(dfm.tp2$s4, row.names = rownames(dfm.tp2))
top.s4 <- na.omit(top.s4)
top.s4

### PLOT - topic - 2 ####
library(ggparallel)
ct.tp <- ctbl.m[ctbl.m$cat == "TOPIC", ] %>% droplevels()
cutoff <- group_by(ct.tp, code) %>% dplyr::count()
names(cutoff) <- c("code", "cutoff.score")
ct.tp <- merge(ct.tp, cutoff, by = "code")
ct.tp <- ct.tp[ct.tp$cutoff > 2, ]
nlabs <- length(unique(ct.tp$code))
ptop <- mpal(1:(length(unique(ct.tp$code))), p = sci)
pscat <- pal_my[c(2, 17)]

library(reshape)
ct.tp <- rename(ct.tp, c(scat = "Category"))

source("ggparset2.R")
top.ps <- ggparset2(list("code", "Category"),
                    data = ct.tp, #weight = "wt",
                    method = "parset", label = FALSE,
                    text.angle = 0, order = c(1,-1)) +
    scale_fill_manual(values = c(pscat, ptop)) +
    scale_colour_manual(values = c(pscat, ptop)) +
    guides(fill = guide_legend(override.aes = list(alpha = 0))) +
    thm_Rtft() +
    theme(legend.text = element_text(size = rel(.8)),
          legend.key.height = unit(0.5, "cm"))
top.ps
#'
#'
#' ## Methodologies
#'
#+ methodologies, fig.fullwidth=TRUE
# methodologies ============================================================

library(vcd);

### PLOT - methodologies - 1 ####

ct.mo <- ctbl.m[ctbl.m$cat == "M-OVERALL", ] %>% droplevels()
t.mo <- Rtdf(ct.mo$code, names = c("Method(s)", "$N_{Articles}$"))

ft.mo <- ftable(ct.mo[, c("code", "scat")], row.vars = 1)
ftm.mo <- matrix(ft.mo, nrow = nrow(t.mo), byrow = FALSE)
dimnames(ftm.mo) <- list(Methodology = levels(ct.mo$code), Category = c("IPV Interventions", "LGBTQ-IPV Research"))
t.mo
ftm.mo

mosaic(ftm.mo, labeling_args = list(gp_labels = gpar(fontsize = 9), gp_varnames = gpar(fontsize = 11, fontface = 2), pos_labels = c("center", "center"), just_labels = c("center", "right"), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T)), highlighting = 2, highlighting_fill = catpal)

### PLOT - methodologies - 2 ####

ct.mo$code <- as.numeric(ct.mo$code) %>% factor()
cutoff <- group_by(ct.mo, code) %>% dplyr::count()
names(cutoff) <- c("code", "cutoff.score")
ct.mo <- merge(ct.mo, cutoff, by = "code")
ct.mo <- ct.mo[ct.mo$cutoff.score > 1, ]
nlabs <- length(unique(ct.mo$code))
pmo <- mpal(1:(length(unique(ct.mo$code))), p = sci)
pscat <- pal_my[c(2, 17)]

library(reshape)
ct.mo <- rename(ct.mo, c(code = "Methodology", scat = "Research Category"))

source("ggparset2.R")
mo.ps <- ggparset2(list("Methodology", "Research Category"),
                   data = ct.mo, #weight = "wt",
                   method = "parset", label = FALSE,
                   text.angle = 0, order = c(1,-1)) +
    scale_fill_manual(values = c(pmo, pscat),
                      breaks = c(rep("", nlabs),
                                 "Research Category:IPV Interventions",
                                 "Research Category:LGBTQ-IPV Research"),
                      labels = c(rep("", nlabs),
                                 "IPV Interventions", "LGBTQ-IPV\nResearch")) +
    scale_colour_manual(values = c(pmo, pscat), guide = FALSE) +
    guides(fill = guide_legend(override.aes = list(alpha = 0))) +
    thm_Rtft(lpos = "right", ldir = "vertical") +
    theme(legend.text = element_text(size = rel(1)),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.width = unit(0, "cm"),
          legend.key.height = unit(1, "cm"),
          legend.position = c(0.91, 0.625),
          legend.justification = c(0.95, 0.45),
          legend.background = element_rect(fill="transparent"))
mo.ps


#'
#' \newpage
#' `r tufte::newthought("\\large{Qualitative Methods}")`
#'
#+ qual, fig.fullwidth=TRUE
# qualitative ============================================================

ct.ql <- ctbl.m[ctbl.m$cat == "M-QL", ] %>% droplevels()
t.ql <- Rtdf(ct.ql$code, names = c("Qualitative Method(s)", "$N_{Articles}$"))
ft.ql <- ftable(ct.ql[, c("code", "scat")], row.vars = 1)
ftm.ql <- matrix(ft.ql, nrow = nrow(t.ql), byrow = FALSE)
dimnames(ftm.ql) <- list("Qualitative Method(s)" = levels(ct.ql$code), Category = c("IPV Interventions", "LGBTQ-IPV Research"))
t.ql

### PLOT - qualitative - 1 ####

mosaic(ftm.ql, labeling_args = list(gp_labels = gpar(fontsize = 7), gp_varnames = gpar(fontsize = 10, fontface = 2), pos_labels = c("center", "center"), just_labels = c("center", "right")), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T), highlighting = 2, highlighting_fill = catpal, margins = unit(5, "lines"))

### PLOT - qualitative - 2 ####
ct.ql$code <- as.numeric(ct.ql$code) %>% factor()
cutoff <- group_by(ct.ql, code) %>% dplyr::count()
names(cutoff) <- c("code", "cutoff.score")
ct.ql <- merge(ct.ql, cutoff, by = "code")
ct.ql <- ct.ql[ct.ql$cutoff.score > 1, ]
nlabs <- length(unique(ct.ql$code))
pql <- mpal(1:(length(unique(ct.ql$code))), p = sci)
pscat <- pal_my[c(2, 17)]

library(reshape)
ct.ql <- rename(ct.ql, c(code = "Qualitative Methods", scat = "Research Category"))

source("ggparset2.R")
ql.ps <- ggparset2(list("Qualitative Methods", "Research Category"),
                   data = ct.ql, #weight = "wt",
                   method = "parset", label = FALSE,
                   text.angle = 0, order = c(1,-1)) +
    scale_fill_manual(values = c(pql, pscat),
                      breaks = c(rep("", nlabs),
                                 "Research Category:IPV Interventions",
                                 "Research Category:LGBTQ-IPV Research"),
                      labels = c(rep("", nlabs),
                                 "IPV Interventions", "LGBTQ-IPV\nResearch")) +
    scale_colour_manual(values = c(pql, pscat), guide = FALSE) +
    guides(fill = guide_legend(override.aes = list(alpha = 0))) +
    thm_Rtft(lpos = "right", ldir = "vertical") +
    theme(legend.text = element_text(size = rel(1)),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.width = unit(0, "cm"),
          legend.key.height = unit(1, "cm"),
          legend.position = c(0.91, 0.61),
          legend.justification = c(0.95, 0.45),
          legend.background = element_rect(fill="transparent"))
ql.ps

#'
#' \newpage
#' `r tufte::newthought("\\large{Quantitative Methods}")`
#'
#+ quant, fig.fullwidth=TRUE
# quantitative ============================================================

ct.qt <- ctbl.m[ctbl.m$cat == "M-QT", ] %>% droplevels()
t.qt <- Rtdf(ct.qt$code, names = c("Quantitative Method", "$N_{Articles}$"))
ft.qt <- ftable(ct.qt[, c("code", "scat")], row.vars = 1)
ftm.qt <- matrix(ft.qt, nrow = nrow(t.qt), byrow = FALSE)
dimnames(ftm.qt) <- list("Quantitative Method(s)" = levels(ct.qt$code), Category = c("IPV Interventions", "LGBTQ-IPV Research"))
t.qt

### PLOT - quantitative - 1 ####

mosaic(ftm.qt, labeling_args = list(gp_labels = gpar(fontsize = 7), gp_varnames = gpar(fontsize = 10, fontface = 2), pos_labels = c("center", "center"), just_labels = c("center", "right")), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T), highlighting = 2, highlighting_fill = catpal, margins = unit(5, "lines"))

### PLOT - quantitative - 2 ####

ct.qt$code <- as.numeric(ct.qt$code) %>% factor()
cutoff <- group_by(ct.qt, code) %>% dplyr::count()
names(cutoff) <- c("code", "cutoff.score")
ct.qt <- merge(ct.qt, cutoff, by = "code")
ct.qt <- ct.qt[ct.qt$cutoff.score > 1, ]
nlabs <- length(unique(ct.qt$code))
pqt <- mpal(1:(length(unique(ct.qt$code))), p = sci)
pscat <- pal_my[c(2, 17)]

library(reshape)
ct.qt <- rename(ct.qt, c(code = "Quantitative Methods", scat = "Research Category"))

source("ggparset2.R")
qt.ps <- ggparset2(list("Quantitative Methods", "Research Category"),
                   data = ct.qt, #weight = "wt",
                   method = "parset", label = FALSE,
                   text.angle = 0, order = c(1,-1)) +
    scale_fill_manual(values = c(pqt, pscat),
                      breaks = c(rep("", nlabs),
                                 "Research Category:IPV Interventions",
                                 "Research Category:LGBTQ-IPV Research"),
                      labels = c(rep("", nlabs),
                                 "IPV Interventions", "LGBTQ-IPV\nResearch")) +
    scale_colour_manual(values = c(pqt, pscat), guide = FALSE) +
    guides(fill = guide_legend(override.aes = list(alpha = 0))) +
    thm_Rtft(lpos = "right", ldir = "vertical") +
    theme(legend.text = element_text(size = rel(1)),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.width = unit(0, "cm"),
          legend.key.height = unit(1, "cm"),
          legend.position = c(0.91, 0.55),
          legend.justification = c(0.95, 0.45),
          legend.background = element_rect(fill="transparent"))
qt.ps

#'
#' \newpage
#' `r tufte::newthought("\\large{Mixed-Methods}")`
#'
#+ mixed, fig.fullwidth=TRUE
# mixed-methods ============================================================

ct.mm <- ctbl.m[ctbl.m$cat == "M-MM", ] %>% droplevels()
t.mm <- Rtdf(ct.mm$code, names = c("Methods", "$N_{Articles}$"))
ft.mm <- ftable(ct.mm[, c("code", "scat")], row.vars = 1)
ftm.mm <- matrix(ft.mm, nrow = nrow(t.mm), byrow = FALSE)
dimnames(ftm.mm) <- list("Mixed-Methods" = levels(ct.mm$code), scat = c("IPV Interventions", "LGBTQ-IPV Research"))
t.mm

### PLOT - mixed-methods - 1 ####

mosaic(ftm.mm, labeling_args = list(gp_labels = gpar(fontsize = 7), gp_varnames = gpar(fontsize = 10, fontface = 2), pos_labels = c("center", "center"), just_labels = c("center", "right")), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T), highlighting = 2, highlighting_fill = catpal, margins = unit(5, "lines"))

### PLOT - mixed-methods - 2 ####

ct.mm$code <- as.numeric(ct.mm$code) %>% factor()
cutoff <- group_by(ct.mm, code) %>% dplyr::count()
names(cutoff) <- c("code", "cutoff.score")
ct.mm <- merge(ct.mm, cutoff, by = "code")
ct.mm <- ct.mm[ct.mm$cutoff.score > 1, ]
nlabs <- length(unique(ct.mm$code))
pmm <- mpal(1:(length(unique(ct.mm$code))), p = sci)
pscat <- pal_my[c(2, 17)]

library(reshape)
ct.mm <- rename(ct.mm, c(code = "Mixed-Methods", scat = "Research Category"))

source("ggparset2.R")
mm.ps <- ggparset2(list("Mixed-Methods", "Research Category"),
                   data = ct.mm, #weight = "wt",
                   method = "parset", label = FALSE,
                   text.angle = 0, order = c(1,-1)) +
    scale_fill_manual(values = c(pmm, pscat),
                      breaks = c(rep("", nlabs),
                                 "Research Category:IPV Interventions",
                                 "Research Category:LGBTQ-IPV Research"),
                      labels = c(rep("", nlabs),
                                 "IPV Interventions", "LGBTQ-IPV\nResearch")) +
    scale_colour_manual(values = c(pmm, pscat), guide = FALSE) +
    guides(fill = guide_legend(override.aes = list(alpha = 0))) +
    thm_Rtft(lpos = "right", ldir = "vertical") +
    theme(legend.text = element_text(size = rel(1)),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.width = unit(0, "cm"),
          legend.key.height = unit(1, "cm"),
          legend.position = c(0.91, 0.755),
          legend.justification = c(0.95, 0.45),
          legend.background = element_rect(fill="transparent"))
mm.ps


#'
#' \newpage
#' `r tufte::newthought("\\large{Populations}")`
#'
#+ populations, fig.fullwidth=TRUE
# populations ============================================================

ct.pop <- ctbl.m[ctbl.m$cat == "POPULATION", ] %>% droplevels()
t.pop <- Rtdf(ct.pop$code, names = c("Population", "$N_{Articles}$"))
ft.pop <- ftable(ct.pop[, c("code", "scat")], row.vars = 1)
ftm.pop <- matrix(ft.pop, nrow = nrow(t.pop), byrow = FALSE)
dimnames(ftm.pop) <- list("Populations" = levels(ct.pop$code), scat = c("IPV Interventions", "LGBTQ-IPV Research"))
t.pop

### PLOT - populations - 1 ####

Rdotchart(ftm.pop, pch = 19, gcolor = pal_my[20], xlab = expression(N[Articles]), cex = 0.7, gcex = 0.75, gfont = 2, pt.cex = 1.125, color = c(rep(catpal[1], nrow(ftm.pop)), rep(catpal[2], nrow(ftm.pop))))

### PLOT - populations - 2 ####

ct.pop$code <- as.numeric(ct.pop$code) %>% factor()
cutoff <- group_by(ct.pop, code) %>% dplyr::count()
names(cutoff) <- c("code", "cutoff.score")
ct.pop <- merge(ct.pop, cutoff, by = "code")
ct.pop <- ct.pop[ct.pop$cutoff.score > 3, ]
nlabs <- length(unique(ct.pop$code))
ppop <- mpal(1:(length(unique(ct.pop$code))), p = sci)
pscat <- pal_my[c(2, 17)]

library(reshape)
ct.pop <- rename(ct.pop, c(code = "Included Populations", scat = "Research Category"))

source("ggparset2.R")
pop.ps <- ggparset2(list("Included Populations", "Research Category"),
                    data = ct.pop, #weight = "wt",
                    method = "parset", label = FALSE,
                    text.angle = 0, order = c(1,-1)) +
    scale_fill_manual(values = c(ppop, pscat),
                      breaks = c(rep("", nlabs),
                                 "Research Category:IPV Interventions",
                                 "Research Category:LGBTQ-IPV Research"),
                      labels = c(rep("", nlabs),
                                 "IPV Interventions", "LGBTQ-IPV\nResearch")) +
    scale_colour_manual(values = c(ppop, pscat), guide = FALSE) +
    guides(fill = guide_legend(override.aes = list(alpha = 0))) +
    thm_Rtft(lpos = "right", ldir = "vertical") +
    theme(legend.text = element_text(size = rel(1)),
          legend.key = element_rect(colour = "transparent", fill = "transparent"),
          legend.key.width = unit(0, "cm"),
          legend.key.height = unit(1, "cm"),
          legend.position = c(0.91, 0.65),
          legend.justification = c(0.95, 0.45),
          legend.background = element_rect(fill="transparent"))
pop.ps

ct.pop <- ctbl.m[ctbl.m$cat == "POPULATION", ] %>% droplevels()
t.pop <- Rtdf(ct.pop$code, names = c("Population", "$N_{Articles}$"))
ft.pop <- ftable(ct.pop[, c("code", "scat")], row.vars = 1)
ftm.pop <- matrix(ft.pop, nrow = nrow(t.pop), byrow = FALSE)
dimnames(ftm.pop) <- list("Populations" = levels(ct.pop$code), scat = c("IPV Interventions", "LGBTQ-IPV Research"))

### PLOT - populations - 3 ####

# ct.pop$code <- as.numeric(ct.pop$code) %>% factor()
cutoff <- group_by(ct.pop, code) %>% dplyr::count()
names(cutoff) <- c("code", "cutoff.score")
ct.pop <- merge(ct.pop, cutoff, by = "code")
ct.pop <- ct.pop[ct.pop$cutoff.score > 1, ]
cutoff2 <- with(ct.pop, {ftable(code, scat)}) %>% data.frame()
names(cutoff2)[3] <- "co2.freq"
ct.pop <- merge(ct.pop, cutoff2, by = c("code", "scat"))
ct.pop <- ct.pop[ct.pop$cutoff.score > 1, ]
ct.pop <- ct.pop[ct.pop$co2.freq > 2, ]
ct.pop$code <- droplevels(ct.pop$code)
nlabs <- length(unique(ct.pop$code))
ppop <- mpal(1:(length(unique(ct.pop$code))), p = sci)
pscat <- pal_my[c(2, 17)]
ct.pop <- rename(ct.pop, c(code = "Included Populations", scat = "Research Category"))

pop.ps2 <- ggparset2(list("Included Populations", "Research Category"),
                    data = ct.pop, #weight = "wt",
                    method = "parset", label = TRUE,
                    label.size = 3, text.angle = 0, order = c(1,-1)) +
    scale_fill_manual(values = c(ppop, pscat), guide = FALSE) +
    scale_colour_manual(values = c(ppop, pscat), guide = FALSE) +
    # guides(fill = guide_legend(override.aes = list(alpha = 0))) +
    thm_Rtft(lpos = "bottom", ldir = "horizontal")
pop.ps2


#'
#' \newpage
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
