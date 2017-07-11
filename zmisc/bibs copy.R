#' ---
#' title: "MAP - Bibliography"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' fignos-plus-name: "Figure "
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
    fig.path = "graphics/bibs/rplot-")
    #fignos = TRUE)#, dev = 'png',
    # fig.retina = 6)
# panderOptions("table.emphasize.rownames", "FALSE")
# rpm()


knitr::opts_template$set(invisible = list(echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE, fig.keep='none', fig.show='none'))

## FOR DOCX/HTML OUTPUT ##

# knit_hooks$set(plot = function(x, options) {
#     if (!is.null(options$fig.lab) {
#     paste('![', options$fig.cap, '](',
#           opts_knit$get('base.url'), paste(x, collapse = '.'),
#           '){#fig:', options$fig.lab, "}",
#           sep = '')
#         }
# })
# knitr::opts_template$set(docx = list(plot = TRUE, dev = 'png', fig.retina = 4))
# fig.retina = 4)
#'
#' \Frule
#'
#' \newpage
#'
#' # \LARGE{\textsc{Systematic Database Search}}
#'
#' \Frule
#'
#' Six separate literature searches were conducted using the [_PsycINFO_](http://www.apa.org/pubs/databases/psycinfo/) and [_Web of Science_](http://wokinfo.com) online citation indexing databases via the [Portland State University library website](library.pdx.edu)^[Note that (1) "intimate partner violence" included "domestic violence" and "partner abuse", (2) "same-sex" included "same-gender", and (3) the results ranges provided after each search description listed reflect the minimum and maximum number of results returned across the two databases searched.]:
#'
#' <!-- _C. Key search terms_ -->

#'
#' 1. _Intimate Partner Violence - General_
#' 2. _Intimate Partner Violence Interventions_
#' 3. _Intimate Partner Violence Intervention Evaluations_
#' 4. _Female Same-Sex Intimate Partner Violence - General_
#' 5. _Female Same-Sex Intimate Partner Violence Interventions_
#' 6. _Female Same-Sex Intimate Partner Violence Intervention Evaluations_
#'
#' \Frule
#'
#+ src_dbsrch, opts.label='invisible'

# dbsrch ------------------------------------------------------------------

source("dbsrch.R")
#'
#+ dbsrch, results='asis'
pander(dbsrch, justify = c("right", "left", "centre"), caption = "Descriptions of database searches conducted with corresponding ranges of the number of results returned")
#'
#' \newpage
#'
#' # Community Psychology Publications & Closely-Related Violence-Specific Publications
#'
#+ src_journals, opts.label='invisible'

# JOURNALS -----------------------------------------------------------
source("journals.R")
#'
#+ journals, results='asis'
cat(tufte::newthought("Community-psychology journals"), "included in database searches:\n\n")
j.cpp %>% as.list() %>% pander()

cat(tufte::newthought("Violence-specific journals"), " selected for inclusion in database searches.")
j.vp %>% as.list() %>% pander()

#'
#' \newpage
#'
#' # Results of Systematic Database Searches:
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
MAP.au <- cbind(BIBKEY, bibdf[, "AUTHOR"]) ## bibdf[,2] ##
#'
#+ MAP1

# MAP1 ----------------------------------------------------------------
MAP1 <- cbind(ID,
             BIBKEY,
             bibdf[, c("YEAR", "TITLE", "JOURNAL", "ABSTRACT")]) %>%
    as.data.frame() ## bibdf[c(3:5, 8)] ##
names(MAP1)[-1] <- tolower(names(MAP1)[-1])

# KEYSv1 -------------------------------------------------------------------

KEYSv0 <- as.character(MAP1$bibkey)

#'
#+ src_MAPrqda, opts.label='invisible'

# MAP-RQDA ----------------------------------------------------------------

source("MAPrqda.R", echo = FALSE)
#'
#+ csid, results='asis'
csid <- caseids[, c("caseid", "case", "RM", "scat")] ## caseids[, -3] ##
csid$case <- factor(csid$case)

# MAP2 --------------------------------------------------------------------

MAP2 <- merge(MAP1, csid, by.x = "bibkey", by.y = "case")

n.inits3 <- MAP2[MAP2$scat == "S3", ] %>% nrow()
n.inits4 <- MAP2[MAP2$scat == "S4", ] %>% nrow()
#'
#+ ctblz1

# ctbl.z1 -----------------------------------------------------------------

ctbl.z1 <- merge(ctbl.z1, cbk, all.x = TRUE, all.y = FALSE) ## from MAPrqda.R ##
ctbl.z1$clab <- factor(ctbl.z1$clab) %>% droplevels()
t.excl <- Rtdf(ctbl.z1$clab, names = c("Reason for Exclusion", "$N_{excluded~articles}$"))

v1v2 <- paste0("@", as.character(ctbl.z1[, "case"]))

cat(tufte::newthought(paste0("$N = ", length(v1v2), "$ items excluded ")), "after restricting results to only _U.S.-based empirical_ studies.")
# v1v2 %>% sort() %>% as.list() %>% pander()

t.excl %>% kable(caption = "Number of Articles Removed per Exclusion Criteria",
                 align = c('l', 'r'))
#'
#'
#+ MAP2rm

# MAP2$RM ------------------------------------------------------------------

MAP2$RM <- ifelse(MAP2$RM == 1, NA, 0)
MAP2rm <- MAP2[is.na(MAP2$RM), "bibkey"] %>% droplevels() %>% as.character()
MAP2 <- na.omit(MAP2)
MAP2 <- droplevels(MAP2)
KEYSv1 <- as.character(MAP2$bibkey)
#'
#+ MAP_CPV

# MAP-CPV ----------------------------------------------------------------
MAP2$journal <- sapply(MAP2$journal, tolower)
MAP2$journal <- gsub(" & ", " and ", MAP2$journal)

cp <- MAP2$journal %in% j.cp
map.cp <- MAP2[cp, ] %>% data.frame()


vlc <- MAP2$journal %in% j.v
map.v <- MAP2[vlc, ] %>% data.frame()
map.v <- map.v[map.v$scat == "S3", , drop = FALSE]
map.v$journal <- sapply(map.v$journal, RtCap)

j.cpv <- c(j.v, j.cp)
cpv <- MAP2$journal %in% j.cpv

map.cpv <- MAP2[cpv, ] %>% data.frame()

map.cpv$rms4 <- ifelse(map.cpv$journal %in% j.v & map.cpv$scat == "S4", NA, map.cpv$scat)
map.cpv <- na.omit(map.cpv)

MAP3 <- MAP2[vlc | cp, ] %>% data.frame()
MAP3$bibkey <- as.character(MAP3$bibkey)

# KEYSv2 -------------------------------------------------------------------

KEYSv2 <- as.character(MAP3$bibkey)
v1v2 <- KEYSv1[!KEYSv1 %in% KEYSv2]

cat(tufte::newthought(paste0("$N = ", length(v1v2), "$ items excluded after restricting search results")), " to only those published in community-psychology specific journals and the _four selected_ violence-related journals (i.e., ", paste0("_", j.vp[1:(length(j.vp)-1)], "_, "), "and ", paste0("_", j.vp[length(j.vp)], "_)."))
# v1v2 <- paste0("@", as.character(v1v2))
# v1v2 %>% as.list() %>% pander()

MAP3$rms4 <- ifelse(MAP3$journal %in% j.v & MAP3$scat == "S4", NA, MAP3$scat)
MAP <- na.omit(MAP3)

# KEYSv3 - FINAL --------------------------------------------------------

KEYSv3 <- as.character(MAP$bibkey)

v2v3 <- KEYSv2[!KEYSv2 %in% KEYSv3]
#'
#+ v2v3_cat
cat(tufte::newthought(paste0("$N = ", length(v2v3), "$ items excluded after restricting ")), "SMW-inclusive search results to only include items published in community-psychology specific journals.")
# v2v3 %>% as.list() %>% pander()
#'
#' ------
#'
#+ tempPanderOpts
#, opts.label="invisible"
panderOptions("p.wrap", "")
panderOptions("p.sep", "; ")
panderOptions("p.copula", "; ")

#' `r tufte::newthought(paste0("$N = ", length(KEYSv3), "$ items included in the formal literature review"))` [`r paste0("@", KEYSv3) %>% pander()`]
#'
#'
#+ resetPanderOpts
#, opts.label="invisible"
panderOptions("p.wrap", "_")
panderOptions("p.sep", ", ")
panderOptions("p.copula", ", and ")
#'
#'
#'
#+ map_jrnl
## map-jrnl ============================================================

MAP <- merge(MAP, jdat, by = "journal", all.x = TRUE)
MAP$journal <- factor(MAP$journal)
levels(MAP$journal) <- sapply(levels(MAP$journal), RtCap)
MAP$jrnl <- sapply(as.character(MAP$journal), Rabbr)
#'
#' \newpage
#'
#' # \LARGE{\textsc{Systematically-Reviewed Literature}}
#'
#' \Frule
#'
#' `r tufte::newthought("The resulting selection of empirical literature")`, representing a community-psychology-focused subset of the U.S.-based IPV-related literature, was reviewed using a `primarily deductive` _qualitative comparative analytic approach_ [_QCA_; @leech2007array; @onwuegbuzie2017framework]. This approach was conducted as part of an initial data reduction and organization process in which the reviewed literature was categorized according to the commonalities in overarching research topics, target populations, sampling frames, sampling and data collection methodologies, and data analytic approaches. In addition, the QCA approach served as a systematic method for examining the similarities, differences, and anomalies within the groups identified in the initial data reduction and organization process [@onwuegbuzie2017framework; @onwuegbuzie2009qualitative]. The qualitative comparative analysis of the reviewed literature was aided by the _`RQDA`_ package created for use with the _`R` Statistical Programming Language and Environment_ [@R-RQDA; @R-base].
#'
#+ cb

# CTBL/CB ---------------------------------------------------------------

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
    code <- gsub("SMIN-\\w+", NA, code)
    code <- gsub("HET", NA, code)
    scat <- factor(scat, labels = c("IPV Interventions", "SMW-Inclusive Research"))
})
cb <- na.omit(cb) %>% droplevels()
cb <- cb[, c("caseid", "scat", "journal", "bibkey", "year", "RM", "rms4", "j.loc", "j.year", "SJR", "Hindex", "jrnl", "case", "cid", "code", "catid", "cat")]
# cbk$clab <- ifelse(cbk$code %in% cb$code, cbk$clab, NA)
# cbk <- na.omit(cbk) %>% droplevels()

cb <- merge(cb, cbk, by = "code")
cb$code <- factor(cb$code)
cb$clab <- factor(cb$clab)
#'
#' ## Codebook
#'
#+ cdbk
catt <- Rtdf(cdbk$catlab, names = c("Information Category", "$N_{sub-codes}$"))

cdbk.ft1 <- with(cdbk, {
    ftable(clab, catlab) %>% data.frame()
})

cdbk.ft1$Freq <- ifelse(cdbk.ft1$Freq == 0, NA, cdbk.ft1$Freq)
cdbk.ft <- na.omit(cdbk.ft1)[, 1:2]
rownames(cdbk.ft) <- NULL
cdbk.ft$catlab <- as.character(cdbk.ft$catlab)
cdbk.ft$catlab <- ifelse(duplicated(cdbk.ft$catlab), NA, paste0("**", cdbk.ft$catlab, "**"))
cdbk.ft <- cdbk.ft[, c("catlab", "clab"), drop = FALSE]

kable(cdbk.ft, col.names = c("\\textbf{Information Category}", "Codes"), caption = "Codebook Constructed from the Discrete Summative Data Compiled Across the Formally Reviewed Literature {#tbl:cdbk}", align = c("r", "l"))


#'
#' \newpage
#'
#' # General Research Categories
#' \Frule
#'
#+ desc, fig.cap="Proportions of reviewed articles in each of the two overarching research categories: IPV interventions research, and SMW-inclusive IPV research", fig.lab="desc"

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
scat.bn %>% kable(caption = "Binomial Test of the Difference in Search Category Proportions", col.names = c("Alternative", "Null Value ($\\mathpzc{\\pi_{0}}$)", "Parameter", "Estimate", "$\\mathpzc{\\chisq}$", "$\\mathpzc{p}$-value", "CI"), format = 'latex', booktabs = T, escape = FALSE)

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
ft.jrnl %>% kable(caption = "Number of Publications in Each Research Category per Journal",
                   align = c("l", "r", "r"))

cpv.s3$jrnl <- sapply(as.character(cpv.s3$journal), Rabbr) %>% factor()
cpv.s4$jrnl <- sapply(as.character(cpv.s4$journal), Rabbr) %>% factor()
j.cp <- sapply(j.cp, Rabbr, USE.NAMES = FALSE)
cb$cpv <- ifelse(cb$jrnl %in% j.cp, "CP", "V")

j.v <- sapply(j.v, Rabbr, USE.NAMES = FALSE)
pr.jv <- length(j.v)/length(j.cp)
pr.jcp <- 1 - pr.jv
pr.j <- c(pr.jv, pr.jcp)
MAP$cpv <- ifelse(MAP$jrnl %in% j.cp, "CP", "V")

# cpv.bn1 <- table(MAP$cpv) %>% Rbinom()
    ### H1: unequal proportions (H0: pi_0 = 0.5) ##
# cpv.bn2 <- table(MAP$cpv) %>% rev() %>% Rbinom(pi0 = pr.jv)
    ## H1: V is less than CP (based on N_{journals}
        ## per V & CP included in db searches;
    ### H0: pi_0 = 0.129) ##

# cpv.bn1 %>% pander(caption = "Binomial Test of $N_{articles}$ per Journal Category (Violence vs. Community Psychology ($\\pi_{0} = 0.5$).")
# cpv.bn2 %>% pander(caption = paste0("Binomial Test of $N_{articles}$ per Journal Category (Violence vs. Community Psychology [$\\pi_{0} = ", round(pr.jv, 3), "$ (based on proportion of $N_{journals}$ per journal category included in database searches; $n_{journals_{V}} = ", length(j.v), "$; $n_{journals_{CP}} = ", length(j.cp), "$)]."))
#'
#' \newpage
#'
#' ## Research Category by Journal & Journal Category
#'
#+ scat_x_journal
ftm.j <- Rna(ft.jrnl)
sum.j <- apply(ftm.j, 1, sum)
ftm.j <- ifelse(ftm.j == 0, NA, ftm.j)
ftm.jp <- cbind(ft.jrnl, "**Total**" = sum.j)
ftm.jp %>% kable(align = rep("r", 3),
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
    color = c(rep(catpal[1], nrow(ftm.j)), rep(catpal[2], nrow(ftm.j))),
    xaxt = 'n'
); axis(1, at = seq(range(ftm.j, na.rm = TRUE)[1],
                    range(ftm.j, na.rm = TRUE)[2], by = 3))

MAP.jrnl <- MAP[, c("scat", "journal")]
names(MAP.jrnl) <- c("Category", "Journal")
MAP.jrnl$Category <- ifelse(MAP.jrnl$Category == "S3",
                         "IPV Interventions",
                         "SMW-Inclusive Research")
pj <- mpal(1:length(unique(MAP$jrnl)), p = sci)
library(ggparallel)
pscat <- c("#a6afbb", pal_my[17])


t.jrnl$log <- log(t.jrnl[, 2])+1
lj <- c(3.5, 3.5, t.jrnl[, 3])
parset.jrnl <- ggparset2(list("Category", "Journal"),
                  data = MAP.jrnl,
                  method = "parset", label = TRUE,
                  label.size = lj, text.angle = 0, order = c(0, 0)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pj, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pj), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.jrnl


MAP.jrnl <- MAP[, c("scat", "journal", "cpv", "jrnl")]
pcpv <- mpal(1:2)

clr.cpv1 <- levels(factor(MAP.jrnl$journal))
clr.cpv <- ifelse(clr.cpv1 %in% j.cpp, pcpv[1], pcpv[2])

MAP.jrnl$cpv <- ifelse(MAP.jrnl$cpv == "CP", "Community Psychology", "Violence-Specific")
MAP.jrnl$scat <- ifelse(MAP.jrnl$scat == "S3", "IPV Interventions", "SMW-Inclusive Research")

names(MAP.jrnl) <- c("Category", "Journal", "Discipline", "jrnl")

parset.jrnl2 <- ggparset2(list("Category", "Journal", "Discipline"),
                   data = MAP.jrnl,
                   method = "parset", label = TRUE,
                   label.size = 2.75, text.angle = 0, order = c(1, 0, 0)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pcpv, alpha.f = 0.85),
                                 adjustcolor(clr.cpv, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, adjustcolor(pcpv, alpha.f = 0.85),
                                   adjustcolor(clr.cpv, alpha.f = 0.85)),
                        guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE); parset.jrnl2
#'
#' \tufteskip
#'
#' # Publication Years
#' \Frule
#'
#+ yr_hist, echo=FALSE, fig.fullwidth=TRUE, fig.cap="Publication Years Grouped by Research Category", fig.height = 4

### PLOT - year/scat ####
s3hist <- hist(MAP$year[MAP$scat == "S3"], plot = FALSE, right = F, breaks = 25)
s4hist <- hist(MAP$year[MAP$scat == "S4"], plot = FALSE, right = F, breaks = s3hist$breaks)
## col = pal_my.a75[12], border = pal_my[19], lwd = .5

plot(s4hist, col = pal_sci[8], border = pal_my[19], density = 50, lwd = .25,
     main = " ", xlab = "Year Published", ylab = expression(N[Articles]),
     ylim = c(0, max(s3hist$counts))); plot(s3hist, col = pal_my[16], border = pal_my[19], density = 50, angle = -45, lwd = .25, add = TRUE); legend(x = 1990, y = 3.75, legend = c("IPV Research Specifically Inclusive of Sexual Minority Women", "IPV Interventions Research (general)"),
       fill = c(pal_sci[8], pal_my[16]), density = 50, angle = c(45, -45),
       border = NA, bty = 'n', cex = 0.8, text.font = 3,
       trace = F)
#'
#+ echo=FALSE
#### NO MORE ECHO ####
## knitr::opts_chunk$set(echo = FALSE)
#'
#' \newpage
#'
#' # Research Topics, Sampling Frames, and Methodologies
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
ftm.tpp %>% kable(align = rep("r", 3),
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
# ct.d <- ct.d[!duplicated(ct.d), ]
# x <- ct.d[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

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
ftm.d %>% kable(align = rep("r", 3),
                 caption = "Research Designs")

nlabs <- length(unique(ct.d$clab))
pd <- mpal(1:length(unique(ct.d$clab)), p = sci)

ct.d <- dplyr::rename(ct.d, "Category" = scat, "Design" = clab)

### PLOT - designs ####

library(ggparallel)
pscat <- c("#a6afbb", pal_my[17])
t.d$log <- log(t.d[, 2])+1
ld <- c(3.5, 3.5, t.d[, 3])
parset.dsgn <- ggparset2(list("Category", "Design"),
                  data = ct.d,
                  method = "parset", label = TRUE,
                  label.size = ld, text.angle = 0, order = c(0, 0)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pd, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pd), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.dsgn
#'
#' \newpage
#' `r tufte::newthought("\\Large{Experimental Research Designs}")`
#'
#+ exp, fig.fullwidth=TRUE
## experimental designs ============================================================

ct.exp <- cb[cb$cat == "DESIGN-EXP", ] %>% droplevels()
ct.exp <- ct.exp[!duplicated(ct.exp), ]
# x <- ct.exp[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

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
ftm.expp <- cbind(ftm.exp, "**Total**" = sum.exp)
rownames(ftm.expp) <- gsub("\\n", "", rownames(ftm.expp))
ftm.expp %>% kable(align = rep("r", 3),
                   caption = "Experimental Research Designs")

nlabs <- length(unique(ct.exp$clab))
pexp <- mpal(1:length(unique(ct.exp$clab)), p = sci)

ct.exp <- dplyr::rename(ct.exp, "Category" = scat, "Experimental Design" = clab)

### PLOT - experimental designs ####

library(ggparallel)
pscat <- c("#a6afbb", pal_my[17])
t.exp$log <- log(t.exp[, 2])+1
lexp <- c(3.5, 3.5, t.exp[, 3])

parset.exp <- ggparset2(list("Category", "Experimental Design"),
                    data = ct.exp,
                    method = "parset", label = TRUE,
                    label.size = lexp, text.angle = 0, order = c(0, 0)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pexp, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pexp), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.exp
#'
#' \newpage
#' ## Data Collection Methodologies
#'
#+ methodologies, fig.fullwidth=TRUE
## methodologies ============================================================

### PLOT - methodologies - 1 ####

ct.mo <- cb[cb$cat == "METHODS", ] %>% droplevels()
ct.mo <- ct.mo[!duplicated(ct.mo), ]
# x <- ct.mo[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

t.mo <- Rtdf(ct.mo$clab)

ft.mo <- ftable(ct.mo[, c("clab", "scat")], row.vars = 1)
ftm.mo <- matrix(ft.mo, nrow = nrow(t.mo), byrow = FALSE)
dimnames(ftm.mo) <- list(Methodology = levels(ct.mo$clab),
                         Category = c("IPV Interventions",
                                      "SMW-Inclusive Research"))
sum.mo <- apply(ftm.mo, 1, sum)
ftm.mo <- ifelse(ftm.mo == 0, NA, ftm.mo)
ftm.mo <- cbind(ftm.mo, "**Total**" = sum.mo)
ftm.mo %>% kable(align = rep("r", 3),
                  caption = "Methodologies")

### PLOT - methodologies ####

nlabs <- length(unique(ct.mo$clab))
pmo <- mpal(1:length(unique(ct.mo$clab)), p = sci)

ct.mo <- dplyr::rename(ct.mo, "Category" = scat, "Methodology" = clab)

t.mo$log <- log(t.mo[, 2])+1
lmo <- c(3.5, 3.5, t.mo[, 3])

pscat <- c("#a6afbb", pal_my[17])
parset.mo <- ggparset2(list("Category", "Methodology"),
                   data = ct.mo,
                   method = "parset", label = TRUE,
                   label.size = lmo, text.angle = 0, order = c(0, 0)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pmo, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pmo), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.mo# + coord_flip()
#'
#' \newpage
#'
#'
#' `r tufte::newthought("\\Large{QuaLitative Research \\textit{Designs}}")`
#'
#+ quaL_designs, fig.fullwidth=TRUE
ct.dql <- cb[cb$cat == "D-QL", ] %>% droplevels()
ct.dql <- ct.dql[!duplicated(ct.dql), ]
# x <- ct.dql[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

t.dql <- Rtdf(ct.dql$clab)
ft.dql <- ftable(ct.dql[, c("clab", "scat")], row.vars = 1)
ftm.dql <- matrix(ft.dql, nrow = nrow(t.dql), byrow = FALSE)
dimnames(ftm.dql) <- list("Qua**L**itative Design" = levels(ct.dql$clab),
                          Category = c("IPV Interventions",
                                       "SMW-Inclusive Research"))
# t.dql
sum.dql <- apply(ftm.dql, 1, sum)
ftm.dql <- ifelse(ftm.dql == 0, NA, ftm.dql)
ftm.dql <- cbind(ftm.dql, "**Total**" = sum.dql)
ftm.dql %>% kable(align = rep("r", 3),
                   caption = "Qua**L**itative Designs")

### PLOT - quaLitative ####

nlabs <- length(unique(ct.dql$clab))
pdql <- mpal(1:length(unique(ct.dql$clab)), p = sci)

ct.dql <- dplyr::rename(ct.dql, "QuaLitative Design" = clab, "Category" = scat)

t.dql$log <- log(t.dql[, 2])+2
ldql <- c(3.5, 3.5, t.dql[, 3])

parset.dql <- ggparset2(list("Category", "QuaLitative Design"),
                        data = ct.dql,
                        method = "parset", label = TRUE,
                        label.size = ldql, text.angle = 0, order = c(0, 0)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pdql, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pdql), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.dql
#'
#'  \newpage
#' `r tufte::newthought("\\large{QuaLitative \\textit{Methods}}")`
#'
#+ qual, fig.fullwidth=TRUE
## qualitative ============================================================

ct.ql <- cb[cb$cat == "M-QL", ] %>% droplevels()
ct.ql <- ct.ql[!duplicated(ct.ql), ]
# x <- ct.ql[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

t.ql <- Rtdf(ct.ql$clab)
ft.ql <- ftable(ct.ql[, c("clab", "scat")], row.vars = 1)
ftm.ql <- matrix(ft.ql, nrow = nrow(t.ql), byrow = FALSE)
dimnames(ftm.ql) <- list("Qua**L**itative Method(s)" = levels(ct.ql$clab),
                         Category = c("IPV Interventions", "SMW-Inclusive Research"))
sum.ql <- apply(ftm.ql, 1, sum)
ftm.ql <- ifelse(ftm.ql == 0, NA, ftm.ql)
ftm.ql <- cbind(ftm.ql, "**Total**" = sum.ql)
ftm.ql %>% kable(align = rep("r", 3),
                  caption = "Qua**L**itative Method(s)")

### PLOT - qualitative ####
pql <- mpal(1:length(unique(ct.ql$clab)), p = sci)

ct.ql <- dplyr::rename(ct.ql, "QuaLitative Methods" = clab, "Category" = scat)

t.ql$log <- log(t.ql[, 2])+2
lql <- c(3.5, 3.5, t.ql[, 3])

parset.ql <- ggparset2(list("Category", "QuaLitative Methods"),
                   data = ct.ql,
                   method = "parset", label = TRUE,
                   label.size = lql, text.angle = 0, order = c(0, 0)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pql, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pql), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.ql
#'
#' \newpage
#'
#' `r tufte::newthought("\\Large{QuaLitative Data Analytic Approaches}")`
#'
#+ aql, fig.fullwidth=TRUE, fig.height=7
## QL Analytic Approaches ========================================================

ct.aql <- cb[cb$cat == "A-QL", ] %>% droplevels()
ct.aql <- ct.aql[!duplicated(ct.aql), ]
# x <- ct.aql[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

t.aql <- Rtdf(ct.aql$clab)
ft.aql <- ftable(ct.aql[, c("clab", "scat")], row.vars = 1)
ft.aql2 <- ftable(ct.aql[, c("code", "scat")], row.vars = 1)
ftm.aql <- matrix(ft.aql, nrow = nrow(t.aql), byrow = FALSE)
dimnames(ftm.aql) <- list("QuaLitative Data Analytic Approaches" = levels(ct.aql$clab),
                          scat = c("IPV Interventions",
                                   "SMW-Inclusive Research"))
sum.aql <- apply(ftm.aql, 1, sum)
ftm.aql <- ifelse(ftm.aql == 0, NA, ftm.aql)
ftm.aqlp <- cbind(ftm.aql, "**Total**" = sum.aql)
rownames(ftm.aqlp) <- gsub("\\n", "", rownames(ftm.aqlp))
ftm.aqlp %>% kable(align = rep("r", 3),
                    caption = "QuaLitative Analytic Approaches")

### PLOT - QL analytic approaches ####

nlabs <- length(unique(ct.aql$clab))
paql <- mpal(1:length(unique(ct.aql$clab)), p = sci)

ct.aqlps <- dplyr::rename(ct.aql, "QuaLitative Analytic Approaches" = clab, "Category" = scat)

aql.s3 <- ftm.aql[, 1]
aql.s4 <- ftm.aql[, 2]

Rdotchart(
    ftm.aql,
    pch = 19,
    gcolor = pal_my[20],
    xlab = expression(N[Articles]),
    cex = 0.7,
    gcex = 0.75,
    gfont = 2,
    pt.cex = 1.125,
    main = "SMW-Inclusive IPV Research",
    color = c(rep(catpal[1], nrow(ftm.aql)), rep(catpal[2], nrow(ftm.aql))))

t.aql$log <- log(t.aql[, 2])+2
laql <- c(3.5, 3.5, t.aql[, 3])

parset.aql <- ggparset2(list("Category", "QuaLitative Analytic Approaches"),
                        data = ct.aqlps,
                        method = "parset", label = TRUE,
                        label.size = laql, text.angle = 0, asp=1.25, text.offset = 0, order = c(0, 0),
                        label.hjust = c(rep(0.5, 2), rep(0.55, nrow(t.aql)))) +
    scale_fill_manual(values = c(pscat, adjustcolor(paql, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, paql), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.aql
#'
#'
#' \newpage
#'
#' `r tufte::newthought("\\Large{QuaNTitative Research \\textit{Designs}}")`
#'
#+ quaNT_designs, fig.fullwidth=TRUE
ct.dqt <- cb[cb$cat == "D-QT", ] %>% droplevels()
ct.dqt <- ct.dqt[!duplicated(ct.dqt), ]
# x <- ct.dqt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

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
ftm.dqt %>% kable(align = rep("r", 3),
                   caption = "Qua**NT**itative Designs")

### PLOT - quantitative ####

nlabs <- length(unique(ct.dqt$clab))
pdqt <- mpal(1:length(unique(ct.dqt$clab)), p = sci)

ct.dqt <- dplyr::rename(ct.dqt, "QuaNTitative Design" = clab, "Category" = scat)

t.dqt$log <- log(t.dqt[, 2])+1
ldqt <- c(3.5, 3.5, t.dqt[, 3])

parset.dqt <- ggparset2(list("Category", "QuaNTitative Design"),
                    data = ct.dqt,
                    method = "parset", label = TRUE,
                    label.size = ldqt, text.angle = 0, order = c(0, 0)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pdqt, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pdqt), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.dqt
#'
#'
#' \newpage
#' `r tufte::newthought("\\large{QuaNTitative \\textit{Methods}}")`
#'
#+ quant, fig.fullwidth=TRUE, fig.height=6
### quantitative ============================================================

ct.qt <- cb[cb$cat == "M-QT", ] %>% droplevels()
ct.qt <- ct.qt[!duplicated(ct.qt), ]
# x <- ct.qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

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
ftm.qt %>% kable(align = rep("r", 3),
                  caption = "Qua**NT**itative Methods")

### PLOT - quantitative ####

nlabs <- length(unique(ct.qt$clab))
pqt <- mpal(1:length(unique(ct.qt$clab)), p = sci)

ct.qt <- dplyr::rename(ct.qt, "QuaNTitative Methods" = clab, "Category" = scat)

t.qt$log <- log(t.qt[, 2])+1
lqt <- c(3.5, 3.5, t.qt[, 3])

parset.qt <- ggparset2(list("Category", "QuaNTitative Methods"),
                   data = ct.qt,
                   method = "parset", label = TRUE,
                   label.size = lqt, text.angle = 0, order = c(0, 0)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pqt, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pqt), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.qt
#'
#' \newpage
#'
#' `r tufte::newthought("\\Large{QuaNTitative Data Analytic Approaches}")`
#'
#+ aqt, fig.fullwidth=TRUE, fig.height=7
## Analytic Approaches ========================================================

ct.aqt <- cb[cb$cat == "A-QT", ] %>% droplevels()
ct.aqt <- ct.aqt[!duplicated(ct.aqt), ]
# x <- ct.aqt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

t.aqt <- Rtdf(ct.aqt$clab)
ft.aqt <- ftable(ct.aqt[, c("clab", "scat")], row.vars = 1)
ft.aqt2 <- ftable(ct.aqt[, c("code", "scat")], row.vars = 1)
ftm.aqt <- matrix(ft.aqt, nrow = nrow(t.aqt), byrow = FALSE)
dimnames(ftm.aqt) <- list("QuaNTitative Data Analytic Approaches" = levels(ct.aqt$clab),
                          scat = c("IPV Interventions",
                                   "SMW-Inclusive Research"))

sum.aqt <- apply(ftm.aqt, 1, sum)
ftm.aqt <- ifelse(ftm.aqt == 0, NA, ftm.aqt)
ftm.aqtp <- cbind(ftm.aqt, "**Total**" = sum.aqt)
ftm.aqtp %>% kable(align = rep("r", 3),
                    caption = "Qua**NT**itative Analytic Approaches")

### PLOT - QT analytic approaches ####

nlabs <- length(unique(ct.aqt$clab))
paqt <- mpal(1:length(unique(ct.aqt$clab)), p = sci)

ct.aqtps <- dplyr::rename(ct.aqt, "QuaNTitative Analytic Approaches" = clab, "Category" = scat)

aqt.s3 <- ftm.aqt[, 1]
aqt.s4 <- ftm.aqt[, 2]

Rdotchart(
    ftm.aqt,
    pch = 19,
    gcolor = pal_my[20],
    xlab = expression(N[Articles]),
    cex = 0.7,
    gcex = 0.75,
    gfont = 2,
    pt.cex = 1.125,
    main = "SMW-Inclusive IPV Research",
    color = c(rep(catpal[1], nrow(ftm.aqt)), rep(catpal[2], nrow(ftm.aqt))))

t.aqt$log <- log(t.aqt[, 2])*1.65
laqt <- c(3.5, 3.5, t.aqt[, 3])

parset.aqt <- ggparset2(list("Category", "QuaNTitative Analytic Approaches"),
                        data = ct.aqtps,
                        method = "parset", label = TRUE,
                        label.size = laqt, text.angle = 0, order = c(0, 0),
                        label.hjust = c(rep(0.5, 2), rep(0.55, nrow(t.aqt))))+
    scale_fill_manual(values = c(pscat, adjustcolor(paqt, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, paqt), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.aqt
#'
#'
#' \newpage
#' `r tufte::newthought("\\large{Archival/Secondary Data Sources}")`
#'
#+ m_rcrd, fig.fullwidth=TRUE
### archival/secondary data sources ####
ct.rcrd <- cb[cb$cat == "M-RCRD", ] %>% droplevels()
ct.rcrd <- ct.rcrd[!duplicated(ct.rcrd), ]
# x <- ct.rcrd[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

t.rcrd <- Rtdf(ct.rcrd$clab)
ft.rcrd <- ftable(ct.rcrd[, c("clab", "scat")], row.vars = 1)
ftm.rcrd <- matrix(ft.rcrd, nrow = nrow(t.rcrd), byrow = FALSE)
dimnames(ftm.rcrd) <- list("Archival Data Source" = levels(ct.rcrd$clab),
                           Category = c("IPV Interventions",
                                        "SMW-Inclusive Research"))
# t.rcrd
sum.rcrd <- apply(ftm.rcrd, 1, sum)
ftm.rcrd <- ifelse(ftm.rcrd == 0, NA, ftm.rcrd)
ftm.rcrd <- cbind(ftm.rcrd, "**Total**" = sum.rcrd)
ftm.rcrd %>% kable(align = rep("r", 3),
                    caption = "Archival Data Sources")

### PLOT - archival/secondary data sources ####

nlabs <- length(unique(ct.rcrd$clab))
prcrd <- mpal(1:length(unique(ct.rcrd$clab)), p = sci)

ct.rcrd <- dplyr::rename(ct.rcrd, "Archival Data Source" = clab, "Category" = scat)

t.rcrd$log <- log(t.rcrd[, 2])+2
lrcrd <- c(3.5, 3.5, t.rcrd[, 3])

parset.rcrd <- ggparset2(list("Category", "Archival Data Source"),
                     data = ct.rcrd,
                     method = "parset", label = TRUE,
                     label.size = lrcrd, text.angle = 0, order = c(0, 0)) +
    scale_fill_manual(values = rev(c(rev(pscat), adjustcolor(prcrd, alpha.f = 0.55))),
                      guide = FALSE) +
    scale_colour_manual(values = rev(c(rev(pscat), prcrd)), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.rcrd
#'
#' \newpage
#'
#' `r tufte::newthought("\\Large{Mixed-Methodological \\textit{Designs}}")`
#'
#+ mm_designs, fig.fullwidth=TRUE
## mixed-methods designs ========================================================

ct.dmm <- cb[cb$cat == "D-MM", ] %>% droplevels()
ct.dmm <- ct.dmm[!duplicated(ct.dmm), ]
# x <- ct.dmm[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
t.dmm <- Rtdf(ct.dmm$clab)
ft.dmm <- ftable(ct.dmm[, c("clab", "scat")], row.vars = 1)
ftm.dmm <- matrix(ft.dmm, nrow = nrow(t.dmm), byrow = FALSE)
dimnames(ftm.dmm) <- list("Mixed-Methodological Design" = levels(ct.dmm$clab),
                          scat = c("IPV Interventions",
                                   "SMW-Inclusive Research"))
# t.dmm
sum.dmm <- apply(ftm.dmm, 1, sum)
ftm.dmm <- ifelse(ftm.dmm == 0, NA, ftm.dmm)
ftm.dmm <- cbind(ftm.dmm, "**Total**" = sum.dmm)
ftm.dmm %>% kable(align = rep("r", 3),
                   caption = "Mixed-Methodological Designs")

### PLOT - mixed-methods ####

nlabs <- length(unique(ct.dmm$clab))
pmm <- mpal(1:length(unique(ct.dmm$clab)), p = sci)

ct.dmm <- dplyr::rename(ct.dmm, "Mixed-Methodological Design" = clab, "Category" = scat)

t.dmm$log <- log(t.dmm[, 2])+3
ldmm <- c(3.5, 3.5, t.dmm[, 3])

parset.dmm <- ggparset2(list("Category", "Mixed-Methodological Design"),
                    data = ct.dmm,
                    method = "parset", label = TRUE,
                    label.size = ldmm, text.angle = 0, order = c(0, 0)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pmm, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pmm), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.dmm
#'
#' \newpage
#' `r tufte::newthought("\\large{Mixed (QuaLitative \\& QuaNTitative) \\textit{Methods}}")`
#'
#+ mm_methods, fig.fullwidth=TRUE
## mixed-methods ============================================================

ct.mm <- cb[cb$cat == "M-MM", ] %>% droplevels()
ct.mm <- ct.mm[!duplicated(ct.mm), ]
# x <- ct.mm[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
t.mm <- Rtdf(ct.mm$clab)
ft.mm <- ftable(ct.mm[, c("clab", "scat")], row.vars = 1)
ftm.mm <- matrix(ft.mm, nrow = nrow(t.mm), byrow = FALSE)
dimnames(ftm.mm) <- list("Mixed-Methods" = levels(ct.mm$clab),
                         scat = c("IPV Interventions",
                                  "SMW-Inclusive Research"))
# t.mm
sum.mm <- apply(ftm.mm, 1, sum)
ftm.mm <- ifelse(ftm.mm == 0, NA, ftm.mm)
ftm.mm <- cbind(ftm.mm, "**Total**" = sum.mm)
ftm.mm %>% kable(align = rep("r", 3),
                  caption = "Mixed-Methods")

### PLOT - mixed-methods ####

nlabs <- length(unique(ct.mm$clab))
pmm <- mpal(1:length(unique(ct.mm$clab)), p = sci)

ct.mm <- dplyr::rename(ct.mm, "Mixed-Methods" = clab, "Category" = scat)

t.mm$log <- log(t.mm[, 2])+2
lmm <- c(3.5, 3.5, t.mm[, 3])

parset.mm <- ggparset2(list("Category", "Mixed-Methods"),
                   data = ct.mm,
                   method = "parset", label = TRUE,
                   label.size = lmm, text.angle = 0, order = c(0, 0)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pmm, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pmm), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.mm
#'
#' \newpage
#' ## Target Populations & Sampling Frames
#'
#+ populations, fig.fullwidth=TRUE, fig.height = 7.5
## populations ============================================================

ct.pop <- cb[cb$cat == "POPULATION", ] %>% droplevels()
ct.pop <- ct.pop[!duplicated(ct.pop), ]
# x <- ct.pop[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
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
ftm.popp %>% kable(align = rep("r", ncol(ftm.popp)), caption = "Populations Included in Sampling Frame")

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
#' ## Sampling Settings
#'
#+ settings, fig.fullwidth=TRUE, fig.height = 7.5
## settings ============================================================

ct.set <- cb[cb$cat == "M-SETTINGS", ] %>% droplevels()
ct.set <- ct.set[!duplicated(ct.set), ]
# x <- ct.set[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
ct.set <- ct.set[!ct.set$code == "CJ-POLICE", ] %>% droplevels()
t.set <- Rtdf(ct.set$clab)
ft.set <- ftable(ct.set[, c("clab", "scat")], row.vars = 1)
ftm.set <- matrix(ft.set, nrow = nrow(t.set), byrow = FALSE)
dimnames(ftm.set) <-
    list(
        "Sampling Settings" = levels(ct.set$clab),
        scat = c("IPV Interventions", "SMW-Inclusive Research")
    )

sum.set <- apply(ftm.set, 1, sum)
ftm.set <- ifelse(ftm.set == 0, NA, ftm.set)
ftm.setp <- cbind(ftm.set, "**Total**" = sum.set)
ftm.setp %>% kable(align = rep("r", ncol(ftm.setp)), caption = "Sampling Settings")

### PLOT - settings - 1 ####

Rdotchart(
    ftm.set,
    pch = 19,
    gcolor = pal_my[20],
    xlab = expression(N[Articles]),
    cex = 0.7,
    gcex = 0.75,
    gfont = 2,
    pt.cex = 1.125,
    color = c(rep(catpal[1], nrow(ftm.set)), rep(catpal[2], nrow(ftm.set))))

### PLOT - settings - 1 ####

sets <- c(191, 196, 198, 202, 203, 206, 207, 208, 209, 210, 211, 212, 213, 216, 218, 219)
ct.set2 <- ct.set[ct.set$cid %in% sets, ] %>% droplevels()
ct.set2$clab <- droplevels(ct.set2$clab)
t.set2 <- Rtdf(ct.set2$clab)

nlabs <- length(unique(ct.set2$clab))
pset2 <- mpal(1:length(unique(ct.set2$clab)), p = sci)

ct.set2 <- dplyr::rename(ct.set2, "Sampling Settings" = clab, "Category" = scat)


t.set2$log <- log(t.set2[, 2])+1
lset <- c(3.5, 3.5, t.set2[, 3])

parset.set <- ggparset2(list("Category", "Sampling Settings"),
                        data = ct.set2,
                        method = "parset", label = TRUE,
                        label.size = lset, text.angle = 0, order = c(0, 0)) +
    scale_fill_manual(values = c(pscat, adjustcolor(pset2, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pset2), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.set
#'
#' \newpage
#' ## Sampling Methods
#'
#+ samplingMethods, fig.fullwidth=TRUE, fig.height = 7.5
## samplingMethods ============================================================

ct.smthds <- cb[cb$cat == "M-SAMPLING", ] %>% droplevels()
ct.smthds <- ct.smthds[!duplicated(ct.smthds), ]
# x <- ct.smthds[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
t.smthds <- Rtdf(ct.smthds$clab)
ft.smthds <- ftable(ct.smthds[, c("clab", "scat")], row.vars = 1)
ftm.smthds <- matrix(ft.smthds, nrow = nrow(t.smthds), byrow = FALSE)
dimnames(ftm.smthds) <-
    list(
        "Populations" = levels(ct.smthds$clab),
        scat = c("IPV Interventions", "SMW-Inclusive Research")
    )

sum.smthds <- apply(ftm.smthds, 1, sum)
ftm.smthds <- ifelse(ftm.smthds == 0, NA, ftm.smthds)
ftm.smthdsp <- cbind(ftm.smthds, "**Total**" = sum.smthds)
ftm.smthdsp %>% kable(align = rep("r", ncol(ftm.smthdsp)), caption = "Sampling Methods")

### PLOT - samplingMethods - 1 ####

Rdotchart(
    ftm.smthds,
    pch = 19,
    gcolor = pal_my[20],
    xlab = expression(N[Articles]),
    cex = 0.7,
    gcex = 0.75,
    gfont = 2,
    pt.cex = 1.125,
    color = c(rep(catpal[1], nrow(ftm.smthds)), rep(catpal[2], nrow(ftm.smthds))))

### PLOT - samplingMethods - 1 ####

nlabs <- length(unique(ct.smthds$clab))
psmthds <- mpal(1:length(unique(ct.smthds$clab)), p = sci)

ct.smthds <- dplyr::rename(ct.smthds, "Levels of Analysis" = clab, "Category" = scat)

t.smthds$log <- log(t.smthds[, 2])+1
lsmthds <- c(3.5, 3.5, t.smthds[, 3])

parset.smthds <- ggparset2(list("Category", "Levels of Analysis"),
                           data = ct.smthds,
                           method = "parset", label = TRUE,
                           label.size = lsmthds, text.angle = 0, order = c(0, 0)) +
    scale_fill_manual(values = c(pscat, adjustcolor(psmthds, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, psmthds), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.smthds

#'
#' \newpage
#'
#' # \LARGE{\textsc{Ecological Levels of Analysis}}
#'
#' \Frule
#'
#'
#+ echo=FALSE

knitr::opts_chunk$set(fig.path = "graphics/EcoLvls/rplot-")

#+ ecoLvls, fig.fullwidth=TRUE
## eco levels of analysis ========================================================

ct.eco <- cb[cb$cat == "ECO", ] %>% droplevels()
ct.eco <- ct.eco[!duplicated(ct.eco), ]
# x <- ct.eco[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

t.eco <- Rtdf(ct.eco$clab)
ft.eco <- ftable(ct.eco[, c("clab", "scat")], row.vars = 1)
ftm.eco <- matrix(ft.eco, nrow = nrow(t.eco), byrow = FALSE)
dimnames(ftm.eco) <- list("Ecological Levels of Analysis" = levels(ct.eco$clab),
                          scat = c("IPV Interventions",
                                   "SMW-Inclusive Research"))
# t.eco
sum.eco <- apply(ftm.eco, 1, sum)
ftm.eco <- ifelse(ftm.eco == 0, NA, ftm.eco)
ftm.eco <- cbind(ftm.eco, "**Total**" = sum.eco)
ftm.eco %>% kable(align = rep("r", 3),
                   caption = "Mixed-Methodological Designs")

### PLOT - eco levels ####

nlabs <- length(unique(ct.eco$clab))
peco <- mpal(1:length(unique(ct.eco$clab)), p = sci)

ct.eco <- dplyr::rename(ct.eco, "Levels of Analysis" = clab, "Category" = scat)

t.eco$log <- log(t.eco[, 2])+1
leco <- c(3.5, 3.5, t.eco[, 3])

parset.eco <- ggparset2(list("Category", "Levels of Analysis"),
                        data = ct.eco,
                        method = "parset", label = TRUE,
                        label.size = leco, text.angle = 0, order = c(0, 0)) +
    scale_fill_manual(values = c(pscat, adjustcolor(peco, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, peco), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
parset.eco
#'
#' \newpage
#'
#+ ecoLvlsData
## SEE "MAPlvls.R" FOR MORE DETAILED COMMENTARY ON THE REMAINING CODE BELOW ##

# DATA - ECO-LEVELS -------------------------------------------------------


mpeco0 <- read.csv("data/mapeco.csv")
mplvls0 <- read.csv("data/maplvls.csv")
mplvls1 <- within(mplvls0, {
    micro <- ifelse(l1 | l2 == 1, 1, 0)
    meso_exo <- ifelse(l3 == 1, 1, 0)
    exo_macro <- ifelse(l4 == 1, 1, 0)
})
mplvls1$nlvls <- apply(mplvls1[, 2:5], 1, sum)
mplvls1$nsys <- apply(mplvls1[, 6:8], 1, sum)

rownames(mplvls1) <- mplvls1[, 1]
mplvls <- mplvls1[, -1, drop = FALSE]

#'
#' # Levels of Analysis
#'
#+ clrs_labs

# COLORS & LABELS ---------------------------------------------------------

vclrs <- rev(grad(1:10, p = nord_polar))[4:7]
vtclrs <- rev(grad(1:10, p = nord_polar))[6:9]

catpal85 <- adjustcolor(catpal, alpha.f = 0.85)
lcvclrs <- c("1 = vclrs[1]; 2 = vclrs[2]; 3 = vclrs[3]; 4 = vclrs[4]; 'S3' = catpal85[1]; 'S4' = catpal85[2]")
ltclrs <- c("1 = vtclrs[1]; 2 = vtclrs[2]; 3 = vtclrs[3]; 4 = vtclrs[4]")

llabs1 <- c("1 = 'Individual'; 2 = 'Relationship'; 3 = 'Community'; 4 = 'Societal'")
sclabs <- c("'S3' = 'IPV Interventions Research'; 'S4' = 'SMW-Inclusive IPV Research'")
#'
#+ l_MAP

# MERGE - MAP+l -------------------------------------------------------------

l <- mplvls[, 1:4]
l$id <- rownames(l)

mpjscat <- MAP[, c("bibkey", "scat", "journal")]
l <- merge(l, mpjscat, by.x = "id", by.y = "bibkey", all = TRUE)

# `llong` -------------------------------------------------------------------

llong0 <- reshape(l, varying = 2:5, direction = 'long', sep = "")

llong1 <- dplyr::rename(llong0, lvl = time, ynlvl = l)
rownames(llong1) <- NULL
llong1$ynlvl <- ifelse(llong1$ynlvl == 0, NA, llong1$ynlvl)

llong2 <- na.omit(llong1)[, c("id", "scat", "journal", "lvl")]
llong2$lvclr <- car::recode(llong2$lvl, lcvclrs)
llong2$cvclr <- car::recode(llong2$scat, lcvclrs)

llongv1 <- Rtdf(llong2[, "id"], names = c("x", "Freq"))
llongv1 <- merge(llongv1, llong2[, c("id", "cvclr")], all = FALSE, by.x = "x", by.y = "id")

llongv2 <- Rtdf(llong2[, "lvl"], names = c("x", "Freq"))
llongv2$cvclr <- rep(NA, nrow(llongv2))

llongv0 <- rbind(llongv1, llongv2)

llongv01 <- within(llongv0, {
    vclr <- car::recode(x, lcvclrs)
    x <- car::recode(x, llabs1)
})

kindex <- nrow(llongv01)-4

llongv01$vclr[1:kindex] <- gsub("\\w+\\d{4}\\w+", NA, llongv01$vclr[1:kindex])
llongv01$vclr <- as.character(llongv01$vclr)

llongv01$cvclr[-1:-kindex] <- pal_my[18]

llongv <- llongv01[!duplicated(llongv01), ]

llong <- within(llong2[, c("id", "lvl")], {
    lvl <- car::recode(lvl, llabs1)
})

llongbi1 <- dplyr::rename(llong, "from" = id, "to" = lvl)
llongbi2 <- llongbi1[, c(2, 1)] %>% dplyr::rename("from" = to, "to" = from)

#+ llongg_net

# GRAPH DATA - LVLS -------------------------------------------------------

library(igraph)
llongg <- graph_from_data_frame(llong, directed = F, vertices = llongv)

lvnames0 <- vertex_attr(llongg, "name")
lvnames1 <- gsub("(\\w+)\\d{4}\\w+", "\\1", lvnames0)

lvnames <- sapply(lvnames1, RtCap, USE.NAMES = FALSE)

V(llongg)$name <- lvnames

V(llongg)$size <- V(llongg)$Freq+1
V(llongg)$color <- adjustcolor(V(llongg)$vclr, alpha.f = 0.5)
V(llongg)$frame.color <- adjustcolor(V(llongg)$vclr, alpha.f = 0.85)
E(llongg)$width <- 0.35

kindex.g <- V(llongg)$name %>% length()-4
lblsize <- c(log(V(llongg)$size[1:kindex.g])*0.35, log(V(llongg)$size[-1:-kindex.g])*0.125)
#'
#+ keysnet_lvls, fig.fullwidth=TRUE
# PLOT - `llongg` ---------------------------------------------------------
lfr <- layout_with_fr(llongg) %>% norm_coords()
plot(llongg, rescale = T, layout = lfr, vertex.label.color = V(llongg)$cvclr, vertex.label.cex = lblsize)

#+ lvls_lnet

# `lnet` --------------------------------------------------------------------

lnet12 <- within(l, {
    from <- Rdich(l1, values = c(NA, "l1"))
    to <- Rdich(l2, values = c(NA, "l2"))
})
lnet12 <- na.omit(lnet12)

lnet13 <- within(l, {
    from <- Rdich(l1, values = c(NA, "l1"))
    to <- Rdich(l3, values = c(NA, "l3"))
})
lnet13 <- na.omit(lnet13)

lnet14 <- within(l, {
    from <- Rdich(l1, values = c(NA, "l1"))
    to <- Rdich(l4, values = c(NA, "l4"))
})
lnet14 <- na.omit(lnet14)

lnet23 <- within(l, {
    from <- Rdich(l2, values = c(NA, "l2"))
    to <- Rdich(l3, values = c(NA, "l3"))
})
lnet23 <- na.omit(lnet23)

lnet24 <- within(l, {
    from <- Rdich(l2, values = c(NA, "l2"))
    to <- Rdich(l4, values = c(NA, "l4"))
})
lnet24 <- na.omit(lnet24)

lnet34 <- within(l, {
    from <- Rdich(l3, values = c(NA, "l3"))
    to <- Rdich(l4, values = c(NA, "l4"))
})
lnet34 <- na.omit(lnet34)

lnet0 <- rbind(lnet12, lnet13, lnet14, lnet23, lnet24, lnet34)[, c("id", "from", "to")]
lnet <- lnet0[, -1]

#+ lnet_llabs

# llabs -------------------------------------------------------------------

library(car)
llabs <- c("'l1' = 'Individual'; 'l2' = 'Relationship'; 'l3' = 'Community'; 'l4' = 'Societal'")
lnet$from <- car::recode(lnet$from, llabs)
lnet$to <- car::recode(lnet$to, llabs)

#+ lfrq

# lfrq --------------------------------------------------------------------

lfrq1 <- lnet[, 1]
lfrq2 <- lnet[, 2]
lfrq3 <- c(lfrq1, lfrq2)
lfrq <- Rtdf(lfrq3, names = c("lvl", "Freq"))

# `lnetg` ------------------------------------------------------------------

library(igraph)
lnetg <- graph_from_data_frame(lnet, directed = FALSE, vertices = lfrq)
V(lnetg)$size <- V(lnetg)$Freq*1.5
lnetcol <- mpal(lfrq, p = nord_aurora, a = 0.8)
V(lnetg)$color <- lnetcol
E(lnetg)$width <- 0.25
#'

#+ lnetg_gem
# PLOTS - `lnetg` (layout-0 & layout-2) -----------------------------------

ll3 <- layout.gem(lnetg)
ll3n <- norm_coords(ll3)
plot(lnetg, rescale = T, layout = ll3n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA)
#'
#' \newpage
#'
#+ lnetft

# `lnetft` -----------------------------------------------------------------


lnetft <- ftable(lnet)
llvls <- c("Individual", "Relationship", "Community", "Societal")

lnetp <- within(lnet, {
    from <- factor(from, ordered = FALSE)
    to <- factor(to, ordered = FALSE)
    levels(from) <- c(levels(from), llvls[!llvls %in% levels(from)])
    levels(to) <- c(levels(to), llvls[!llvls %in% levels(to)])
})

lnetftp <- ftable(lnetp) %>% matrix(nrow = nrow(lfrq), byrow = FALSE)
dimnames(lnetftp) <- list("Level-1" = levels(lnetp$to), "Level-2" = levels(lnetp$from))
lnetftp <- ifelse(lnetftp == 0, NA, lnetftp)

kable(lnetftp, align = rep("c", ncol(lnetftp)))

#'
library(arcdiagram)

ledges <- cbind(lnet$from, lnet$to)
lvals <- V(lnetg)$Freq
ldeg <- degree(lnetg)
larcs <- .35*(lnetft %>% matrix())
larcs <- ifelse(larcs == 0, NA, larcs) %>% na.omit()

#+ arc_lvls, fig.height=3.75, fig.fullwidth=TRUE

# PLOT - `lnet - arcplot()` -----------------------------------------------

arcplot(ledges, col.arcs = hsv(0, 0, 0.1, 0.075), pch.nodes = 21, bg.nodes = adjustcolor(lnetcol, alpha.f = 0.75), cex.nodes = log(ldeg[c("Individual", "Relationship", "Community", "Societal")])*0.37, col.nodes = lnetcol, lwd.nodes = 0.75, lwd.arcs = larcs, line = 1.25, cex.labels = 0.5, font = 1, col.labels = pal_my[20])

#'
#+ cbanl
cbanl <- cb[cb$cat == "A-QT" | cb$cat == "A-QL",
            c("bibkey", "scat", "cat", "code", "clab")] %>% droplevels()
cbaqt <- cbanl[cbanl$cat == "A-QT", ] %>% droplevels()
cbaql <- cbanl[cbanl$cat == "A-QL", ] %>% droplevels()

alabs <- paste0(seq(1:length(levels(cbanl$clab))), " = ", levels(cbanl$clab))
alabsqt <- paste0(seq(1:length(levels(cbaqt$clab))), " = ", levels(cbaqt$clab))
alabsql <- paste0(seq(1:length(levels(cbaql$clab))), " = ", levels(cbaql$clab))

kysanl <- cbanl[, c("bibkey", "clab")] %>% dplyr::rename("from" = bibkey, "to" = clab)
kysaqt <- cbaqt[, c("bibkey", "clab")] %>% dplyr::rename("from" = bibkey, "to" = clab)
kysaql <- cbaql[, c("bibkey", "clab")] %>% dplyr::rename("from" = bibkey, "to" = clab)

#'
#' \newpage
#'
#' # Ecological Systems
#'
#+ sysnet

# `snet` -----------------------------------------------------------------

sys <- names(mplvls[, 5:7])
slabs <- c("'micro' = 'Micro-system'; 'meso_exo' = 'Meso- & Exo-system'; 'exo_macro' = 'Exo- & Macro-system'")
slabs2 <- c("'micro' = 'Micro'; 'meso_exo' = 'Meso-Exo'; 'exo_macro' = 'Exo-Macro'")

s <- mplvls[, 5:7]
s$id <- rownames(s)
## exo_macro, meso_exo, micro
snet12 <- within(s, {
    from <- Rdich(micro, values = c(NA, "micro"))
    to <- Rdich(micro, values = c(NA, "micro"))
})
snet12 <- na.omit(snet12)

snet13 <- within(s, {
    from <- Rdich(micro, values = c(NA, "micro"))
    to <- Rdich(meso_exo, values = c(NA, "meso_exo"))
})
snet13 <- na.omit(snet13)

snet14 <- within(s, {
    from <- Rdich(micro, values = c(NA, "micro"))
    to <- Rdich(exo_macro, values = c(NA, "exo_macro"))
})
snet14 <- na.omit(snet14)

snet23 <- within(s, {
    from <- Rdich(micro, values = c(NA, "micro"))
    to <- Rdich(meso_exo, values = c(NA, "meso_exo"))
})
snet23 <- na.omit(snet23)

snet24 <- within(s, {
    from <- Rdich(micro, values = c(NA, "micro"))
    to <- Rdich(exo_macro, values = c(NA, "exo_macro"))
})
snet24 <- na.omit(snet24)

snet34 <- within(s, {
    from <- Rdich(meso_exo, values = c(NA, "meso_exo"))
    to <- Rdich(exo_macro, values = c(NA, "exo_macro"))
})
snet34 <- na.omit(snet34)

snet0 <- rbind(snet12, snet13, snet14, snet23, snet24, snet34)[, c("id", "from", "to")]
snet <- snet0[, -1]

snet$from <- car::recode(snet$from, slabs2)
snet$to <- car::recode(snet$to, slabs2)


# `sfrq` ------------------------------------------------------------------

sfrq1 <- snet[, 1]
sfrq2 <- snet[, 2]
sfrq3 <- c(sfrq1, sfrq2)
sfrq <- Rtdf(sfrq3, names = c("lvl", "Freq"))


# GRAPH DATA - ECO SYS ------------------------------------------------------------------


library(igraph) ## not necessary - but included as reminder and in case I move/copy the code later ##

snetg <- graph_from_data_frame(snet, directed = FALSE, vertices = sfrq)
V(snetg)$size <- V(snetg)$Freq*1.5
# V(snetg)$label.size <- (V(snetg)$Freq*0.85
snetcol <- mpal(sfrq, p = nord_aurora, a = 0.8)
V(snetg)$color <- snetcol
E(snetg)$width <- 0.25
#'
#'
#+ snetg_fr
# PLOT - `snetg` -----------------------------------

ls1 <- layout.fruchterman.reingold(snetg)
ls1n <- norm_coords(ls1)
plot(snetg, rescale = T, layout = ls1n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA)
#'
#'
#' \newpage
#'
#+ snetft

# `snetft` ----------------------------------------------------------------

snetft <- ftable(snet) %>% matrix(nrow = nrow(sfrq), byrow = FALSE)
dimnames(snetft) <- list(levels(factor(snet$to)), levels(factor(snet$from)))

snetp <- within(snet, {
    from <- factor(from, ordered = FALSE)
    to <- factor(to, ordered = FALSE)
    levels(from) <- c(levels(to)[!levels(to) %in% levels(from)], levels(from))
    levels(to) <- c(levels(from)[!levels(from) %in% levels(to)], levels(to))
})

snetftp <- ftable(snetp) %>% matrix(nrow = nrow(sfrq), byrow = FALSE)
dimnames(snetftp) <- list("Ecological System-1" = levels(snetp$to), "Ecological System-2" = levels(snetp$from))
snetftp <- ifelse(snetft == 0, NA, snetft)
# pander(snetftp)


sedges <- cbind(snet$from, snet$to)
svals <- V(snetg)$Freq
sdeg <- degree(snetg)
sarcs <- .15*(snetft %>% matrix())
sarcs <- ifelse(sarcs == 0, NA, sarcs) %>% na.omit()

#+ arc_sys, fig.height=3.5, fig.fullwidth=TRUE

# PLOT - `snetft-arcplot` -------------------------------------------------

arcplot(sedges, col.arcs = hsv(0, 0, 0.1, 0.06), pch.nodes = 21, bg.nodes = adjustcolor(snetcol, alpha.f = 0.5), cex.nodes = log(sdeg[c("Micro", "Meso-Exo", "Exo-Macro")])*0.3, col.nodes = snetcol, lwd.nodes = 0.75, lwd.arcs = sarcs, line = 1.25, cex.labels = 0.5, font = 1, col.labels = pal_my[20])
#'
#' \newpage
#'
#' # Data Analytic Approaches by Ecological Levels of Analysis
#'
cba <- cb[cb$cat == "A-QT" | cb$cat == "A-QL",
          c("bibkey", "scat", "cat", "code", "clab")] %>% droplevels()
cbaqt <- cba[cba$cat == "A-QT", ] %>% droplevels()
cbaql <- cba[cba$cat == "A-QL", ] %>% droplevels()

alabs <- paste0(seq(1:length(levels(cba$clab))), " = ", levels(cba$clab))
alabsqt <- paste0(seq(1:length(levels(cbaqt$clab))), " = ", levels(cbaqt$clab))
alabsql <- paste0(seq(1:length(levels(cbaql$clab))), " = ", levels(cbaql$clab))

kysanl <- cba[, c("bibkey", "clab")] %>% dplyr::rename("from" = bibkey, "to" = clab)
kysaqt <- cbaqt[, c("bibkey", "clab")] %>% dplyr::rename("from" = bibkey, "to" = clab)
kysaql <- cbaql[, c("bibkey", "clab")] %>% dplyr::rename("from" = bibkey, "to" = clab)

la <- mplvls[, 1:4]
la$id <- rownames(la)

lcba <- merge(la, cba, by.x = "id", by.y = "bibkey", all = FALSE) ## feder2011need is removed because there is not any applicable analytic approach codes for that article ##

lcba.a <- lcba[, c("code", "l1", "l2", "l3", "l4")] %>% dplyr::rename("id" = code)
lcba.a <- lcba.a[!duplicated(lcba.a), ]


alnet1 <- lcba.a[, 1:2] %>% Rtdf(names = c(names(lcba.a[,1:2]), "Freq"))
alnet1$l1 <- as.numeric(alnet1$l1)
alnet1$l1 <- ifelse(alnet1$l1 == 1, NA, "l1")
alnet1$Freq <- ifelse(alnet1$Freq == 0, NA, alnet1$Freq)
alnet1 <- na.omit(alnet1)
# alnet1 <- alnet1[!duplicated(alnet1$id), ]
names(alnet1) <- c("from", "to", "Freq")
alnet2 <- lcba.a[, c(1, 3)] %>% Rtdf(names = c(names(lcba.a[, c(1, 3)]), "Freq"))
alnet2$l2 <- as.numeric(alnet2$l2)
alnet2$l2 <- ifelse(alnet2$l2 == 1, NA, "l2")
alnet2$Freq <- ifelse(alnet2$Freq == 0, NA, alnet2$Freq)
alnet2 <- na.omit(alnet2)
names(alnet2) <- c("from", "to", "Freq")

alnet3 <- lcba.a[, c(1, 4)] %>% Rtdf(names = c(names(lcba.a[,c(1, 4)]), "Freq"))
alnet3$l3 <- as.numeric(alnet3$l3)
alnet3$l3 <- ifelse(alnet3$l3 == 1, NA, "l3")
alnet3$Freq <- ifelse(alnet3$Freq == 0, NA, alnet3$Freq)
alnet3 <- na.omit(alnet3)
names(alnet3) <- c("from", "to", "Freq")

alnet4 <- lcba.a[, c(1, 5)] %>% Rtdf(names = c(names(lcba.a[,c(1, 5)]), "Freq"))
alnet4$l4 <- as.numeric(alnet4$l4)
alnet4$l4 <- ifelse(alnet4$l4 == 1, NA, "l4")
alnet4$Freq <- ifelse(alnet4$Freq == 0, NA, alnet4$Freq)
alnet4 <- na.omit(alnet4)
names(alnet4) <- c("from", "to", "Freq")

alnet0 <- rbind(alnet1, alnet2, alnet3, alnet4)
alnet01 <- merge(alnet0, lcba[, c("code", "clab")], by.x = "from", by.y = "code")
alnet <- alnet01[!duplicated(alnet01), c("from", "to", "clab", "Freq")]

avert <- alnet[, 3:4]
avert <- avert[!duplicated(avert), ]

#+ alnet_llabs
library(car)
llabs <- c("'l1' = '.Individual'; 'l2' = '.Relationship'; 'l3' = '.Community'; 'l4' = '.Societal'")
alnet$to <- car::recode(alnet$to, llabs)

#+ alfrq

# alfrq --------------------------------------------------------------------

alfrq1 <- alnet[, 1] %>% as.character()
alfrq2 <- alnet[, 2]
alfrq3 <- c(alfrq1, alfrq2)
alfrq <- Rtdf(alfrq3, names = c("lvl", "Freq"))

av1 <- alnet[, 3] %>% as.character()
av2 <- alnet[, 2]
av3 <- c(av1, av2)
av <- Rtdf(av3, names = c("id", "Freq"))
av[, 1] <- as.character(av[, 1])
# `alnetg` ------------------------------------------------------------------

library(igraph)
alnetg <- graph_from_data_frame(alnet[, 1:2], directed = FALSE, vertices = alfrq)
V(alnetg)$size <- V(alnetg)$Freq*1.5
alnetcol <- mpal(alfrq, p = nord_aurora, a = 0.8)[-1:-4] %>% adjustcolor(alpha.f = 0.5)
avclrs <- c(adjustcolor(vclrs[1:4], alpha.f = 0.65), alnetcol)
V(alnetg)$color <- avclrs
E(alnetg)$width <- 0.25
V(alnetg)$name[-1:-4] <- seq(1:length(av[-1:-4, 1]))
V(alnetg)$name[1:4] <- gsub("\\.", "", V(alnetg)$name[1:4])
aindex.g <- V(alnetg)$name %>% length()
alblsize <- c(log(V(alnetg)$size[1:4])*0.125, log(V(alnetg)$size[5:aindex.g])*0.325)
#'
#'
#+ echo=FALSE
# panderOptions("p.wrap", "")
alabs <- gsub("\\n", "", alabs)
alabs <- gsub("&", "\\\\&", alabs)
# alabs1 <- paste0(alabs[1:length(alabs)-1], sep = ", ")
# deparse(alabs1)
alabs <- paste(alabs, collapse = ", ")

alnetg_cap <- paste0("Network Diagram Showing Relations among Analytic Approaches (numbered graph nodes) used and Ecological Levels of Analysis (named graph nodes) Invovled among the Reviewed Literature: \\textit{", alabs, "}")
# PLOTS - `alnetg` (layout-0 & layout-2) -----------------------------------

#'
#+ alnetg_fr, out.height='4in', fig.cap=alnetg_cap
lal3 <- layout_with_fr(alnetg) %>% norm_coords()
plot(alnetg, rescale = T, layout = lal3, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = c(vclrs[1:4], rep(NA, length(alnetcol))), vertex.label.cex = alblsize)
#'
#'
#' \newpage
#'
alnet.pr <- ftable(alnet[, 2:3], row.vars = 2) %>% as.matrix() #(nrow = nrow(alfrq), byrow = FALSE)
dimnames(alnet.pr) <- list(levels(factor(alnet$clab)), gsub("\\.", "", levels(factor(alnet$to))))
alnet.pr <- ifelse(alnet.pr >= 1, "\\checkmark", "$\\cdot$")
rownames(alnet.pr) <- gsub("\\n", "", rownames(alnet.pr))
rownames(alnet.pr) <- gsub("&", "\\\\&", rownames(alnet.pr))
#'
kable(alnet.pr, caption = "Analytic approaches used across ecological levels of analysis", escape = F)
#'
#' \newpage
#'
#+ anetg_arc, echo=FALSE
knitr::include_graphics("graphics/inputs/arc_analysEs.pdf")
#'
#' \newpage\onehalfspacing
#'
#' # References`r Rcite_r(file = "../auxDocs/REFs.bib", footnote = TRUE)`
#'
#' \refs
#'
