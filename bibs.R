#' ---
#' title: "Appendix C: Results from Qualitative Comparitive Analyses"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' fignos-plus-name: "Figure "
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
# SETUP ----------------

source("../SETUP.R") ## SEE EccRiley.github.io/Rrscs/SETUP.R ##
options(width = 75)
knitr::opts_chunk$set(
    tidy = TRUE,
    echo = TRUE,
    fig.keep = 'high',
    fig.show = 'asis',
    results = 'asis',
    tidy.opts = list(comment = FALSE),
    # echoRule = NULL,
    # echoRuleb = NULL,
    fig.height = 5,
    fig.path = "graphics/bibs/rplot-",
    fignos = TRUE,
    dev = 'pdf',
    # fig.retina = 6,
    Rplot = TRUE,
    Rplot_whbg = NULL,
    hideCap = FALSE,
    figPath = FALSE,
    fpath = NULL)

knitr::opts_hooks$set(fpath = function(options)
{
    if (options$figPath == TRUE)
    {
        options$hideCap <- TRUE
        options$fig.path <- "graphics/inputs/"
    }
    options
})

op <- par(no.readonly = TRUE) ## current values for settable plotting params (see "?par") ##
pmar <- par(mar = c(5, 4, 4, 2))
sci <- colorRampPalette(pal_sci[c(1, 2, 4, 5, 8)])
# panderOptions("table.emphasize.rownames", "FALSE")

# rpm()

knitr::opts_template$set(invisible = list(echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE, fig.keep='none', fig.show='none'))


hideLabs <- function(x, labs) {
    if (knitr::opts_chunk$get("hideCap") == FALSE) {
        x + labs
    } else {
        x
    }
}

## FOR DOCX/HTML OUTPUT ##

# knit_hooks$set(plot = function(x, options) {
#     if (!is.null(options$fig.lab) {
#     paste('![', options$fig.cap, '](',
#           opts_knit$get('base.url'), paste(x, collapse = '.'),
#           '){#fig:', options$fig.lab, "}",
#           sep = '')
#         }
# })
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
#' 1. _**IPV - General**_
#' 2. --- _IPV Interventions_
#' 3. --- _IPV Intervention Evaluations_
#' 4. _**Female Same-Sex/Same-Gender IPV (FSSIPV) - General**_
#' 5. --- _FSSIPV Interventions_
#' 6. --- _FSSIPV Intervention Evaluations_
#'
#' \Frule
#'
#+ src_dbsrch, opts.label='invisible'

# dbsrch ----------------

source("dbsrch.R")
#'
#+ dbsrch, results='asis',echo=FALSE
# pander(dbsrch, justify = c("right", "left", "centre"), caption = "Descriptions of database searches conducted with corresponding ranges of the number of results returned")
library(kableExtra)
kable(dbsrch, caption = "Descriptions of database searches conducted with corresponding ranges of the number of results returned {#tbl:dbsrch}", justify = c("r", "l", "l")) %>%
    add_footnote(c("Note: For each database search, multiple search terms were included for the subject/keywords parameters to represent intimate partner violence", "'PI' = PsycINFO; 'WoS' = Web of Science", threeparttable = TRUE))
#'
#' \newpage
#'
#' # Community Psychology Publications & Closely-Related Violence-Specific Publications
#'
#+ src_journals, opts.label='invisible'

# JOURNALS ----------------
source("journals.R")
#'
#' `r tufte::newthought("Community-psychology journals")` included in database searches:
#'
#+ journals, results='asis', echo=FALSE
j.cpp %>% as.list() %>% pander()
#'
#' `r tufte::newthought("Violence-specific journals")` selected for inclusion in database searches.
#'
#+ journals2, results='asis', echo=FALSE
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

# BIB ----------------

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

# MAP1 ----------------
MAP1 <- cbind(ID,
              BIBKEY,
              bibdf[, c("YEAR", "TITLE", "JOURNAL", "ABSTRACT")]) %>%
    as.data.frame() ## bibdf[c(3:5, 8)] ##
names(MAP1)[-1] <- tolower(names(MAP1)[-1])

# KEYSv1 ----------------

KEYSv0 <- as.character(MAP1$bibkey)

#'
#+ src_MAPrqda, opts.label='invisible'

# MAP-RQDA ----------------

source("MAPrqda.R", echo = FALSE)
#'
#+ csid, results='asis'
csid <- caseids[, c("caseid", "case", "RM", "scat")] ## caseids[, -3] ##
csid$case <- factor(csid$case)

# MAP2 ----------------

MAP2 <- merge(MAP1, csid, by.x = "bibkey", by.y = "case")

n.inits3 <- MAP2[MAP2$scat == "S3", ] %>% nrow()
n.inits4 <- MAP2[MAP2$scat == "S4", ] %>% nrow()
#'
#+ ctblz1

# ctbl.z1 ----------------

ctbl.z1 <- merge(ctbl.z1, cbk, all.x = TRUE, all.y = FALSE) ## from MAPrqda.R ##
ctbl.z1$clab <- factor(ctbl.z1$clab) %>% droplevels()
t.excl <- Rtdf(ctbl.z1$clab, names = c("Reason for Exclusion", "$N_{excluded~articles}$"))

v1v2 <- paste0("@", as.character(ctbl.z1[, "case"]))
#'
#' `r tufte::newthought(paste0("$N = ", length(v1v2), "$ items excluded "))` after restricting results to only _U.S.-based empirical_ studies.
#'
#+ texcl, echo=FALSE
# v1v2 %>% sort() %>% as.list() %>% pander()
t.excl %>% kable(caption = "Number of Articles Removed per Exclusion Criteria",
                 align = c('l', 'r'))
#'
#'
#+ MAP2rm

# MAP2$RM ----------------

MAP2$RM <- ifelse(MAP2$RM == 1, NA, 0)
MAP2rm <- MAP2[is.na(MAP2$RM), "bibkey"] %>% droplevels() %>% as.character()
MAP2 <- na.omit(MAP2)
MAP2 <- droplevels(MAP2)
KEYSv1 <- as.character(MAP2$bibkey)
#'
#+ MAP_CPV

# MAP-CPV ----------------
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

MAP3 <- MAP2[cpv, ] %>% data.frame()

## !!! 2017-07-14: DECIDED TO FILTER BASED ON TOPIC CODES RATHER THAN JOURNAL CATEGORY (DUH! SHOULDA DONE THAT TO BEGIN WITH!!) !!! ##
##
# map.cpv$rms4 <- ifelse(map.cpv$journal %in% j.v & map.cpv$scat == "S4", NA, map.cpv$scat)
# map.cpv <- na.omit(map.cpv)

# MAP3 <- MAP2[vlc | cp, ] %>% data.frame()
MAP3$bibkey <- as.character(MAP3$bibkey)

# KEYSv2 ----------------

KEYSv2 <- as.character(MAP3$bibkey)
v1v2 <- KEYSv1[!KEYSv1 %in% KEYSv2]
# v1v2 <- paste0("@", as.character(v1v2))
# v1v2 %>% as.list() %>% pander()

#' `r tufte::newthought(paste0("$N = ", length(v1v2), "$ items excluded after restricting search results"))` to only those published in community-psychology specific journals or the _four selected_ violence-related journals (i.e., `r paste0("_", j.vp[1:(length(j.vp)-1)], "_, ")` and `r paste0("_", j.vp[length(j.vp)], "_")`.
#'
# MAP3$rms4 <- ifelse(MAP3$journal %in% j.v & MAP3$scat == "S4", NA, MAP3$scat)
# MAP <- na.omit(MAP3)

# KEYSv3 - FINAL ----------------

MAP <- MAP3[MAP3$bibkey %in% keys_tpFilter, ] %>% droplevels()

KEYSv3 <- as.character(MAP$bibkey)

v2v3 <- KEYSv2[!KEYSv2 %in% KEYSv3]
#'
#' `r tufte::newthought(paste0("$N = ", length(v2v3), "$ items excluded after restricting "))` After having the substantive focus results to only include items directly applicable or relevant to IPV interventions and intervention research (e.g., program evaluation research, measures, evaluation research methods, etc.).
#'
#+ v2v3_cat
# v2v3 %>% as.list() %>% pander()
#'

#+ tempPanderOpts,echo=FALSE
#, opts.label="invisible"
panderOptions("p.wrap", "")
panderOptions("p.sep", "; ")
panderOptions("p.copula", "; ")

#' `r tufte::newthought(paste0("$N = ", length(KEYSv3), "$ items included in the formal literature review"))`:
#'
#' `r paste0("@", KEYSv3) %>% as.list %>% pander()`
#'
#'
#+ resetPanderOpts,echo=FALSE
#, opts.label="invisible"
panderOptions("p.wrap", "_")
panderOptions("p.sep", ", ")
panderOptions("p.copula", ", \\& ")
#'
#'
#'
#+ map_jrnl
## map-jrnl ================

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

# CTBL/CB ----------------

cb <- merge(MAP, ctbl, by = c("caseid", "scat"))
cb <- within(cb, {
    # journal <- droplevels(journal)
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
cb <- cb[, c("caseid", "scat", "journal", "bibkey", "year", "RM", "j.loc", "j.year", "SJR", "Hindex", "jrnl", "case", "cid", "code", "catid", "cat")]
# cbk$clab <- ifelse(cbk$code %in% cb$code, cbk$clab, NA)
# cbk <- na.omit(cbk) %>% droplevels()

cb <- merge(cb, cbk, by = "code")
cb$code <- factor(cb$code)
cb$clab <- factor(cb$clab)
#'
#'
#' \newpage
#'
#' ## Codebook
#'
#+ cdbk
catt <- Rtdf(cdbk$catlab, names = c("Information Category", "$N_{sub-codes}$"))

cdbk.ft1 <- with(cdbk, {
    ftable(clab, catlab) %>% data.frame()
})

cdbk.ft1$Freq <- ifelse(cdbk.ft1$Freq == 0, NA, cdbk.ft1$Freq)
## filter out unused codes ##
cdbk.ft <- na.omit(cdbk.ft1)[, 1:2] ## rm "Freq" column after filtering, and other oclumns not needed right here ##
rownames(cdbk.ft) <- NULL
cdbk.ft$catlab <- as.character(cdbk.ft$catlab)
cdbk.ft$catlab <- ifelse(duplicated(cdbk.ft$catlab), NA, paste0("**", cdbk.ft$catlab, "**"))
cdbk.ft <- cdbk.ft[, c("catlab", "clab"), drop = FALSE]
#'
#' \newpage
#'
#+ echo=FALSE
kable(cdbk.ft, col.names = c("\\textbf{Information Category}", "Codes"), caption = "Codebook Constructed from the Discrete Summative Data Compiled Across the Formally Reviewed Literature {#tbl:cdbk}", align = c("r", "l"))
#'
#' \newpage
#'
#' # General Research Categories
#' \Frule
#'
#+ scat

### catpal ####

catpal <- c(pal_my[16], pal_sci[8])

# DESCRIPTIVES ----------------

## search categories ================

### [MAP-abstracts-S3.csv] ####
cpv.s3 <- MAP[MAP$scat == "S3", ]; ## write.csv(cpv.s3[order(cpv.s3$year), c("bibkey", "year", "title", "journal", "abstract")], "data/MAP-abstracts-S3.csv", row.names = FALSE)

### [MAP-abstracts-S4.csv] ####
cpv.s4 <- MAP[MAP$scat == "S4", ]; ## write.csv(cpv.s4[order(cpv.s4$year), c("bibkey", "year", "title", "journal", "abstract")], "data/MAP-abstracts-S4.csv", row.names = FALSE)

ct.scat <- within(MAP, {
    scat <- ifelse(scat == "S3", "IPV Interventions", "SMW-Inclusive Research")
})
t.scat <- Rtdf(ct.scat$scat, names = c("Category", "N"))
## "ct.scat" created in "MAPrqda.R" ##
t.scat %>% kable(caption = "Count of Articles in Each Research Category")

scat.t <- table(ct.scat$scat)
scat.bn <- Rbinom(scat.t)

scat.bn %>% kable(caption = "Binomial Test of the Difference in Search Category Proportions", col.names = c("Alternative", "Null Value ($\\mathpzc{\\pi_{0}}$)", "Parameter", "Estimate", "$\\mathpzc{\\chisq}$", "$\\mathpzc{p}$-value", "CI"), format = 'latex', booktabs = T, escape = FALSE)

N.MAP <- nrow(MAP)
options(Rperc_n = N.MAP)
t.scat$prop = t.scat$N / N.MAP
t.scat <- t.scat[order(t.scat$prop), ]
t.scat$ymax <- cumsum(t.scat$prop)
t.scat$ymin <- c(0, head(t.scat$ymax, n = -1))
t.scat$perc <- paste0((round(t.scat$prop, digits = 1)*100), "%")
# t.scat$labs <- paste("n = ", t.scat$N, " (", t.scat$perc, ")", sep = "")

#+ donut_scat, fig.cap="Proportions of reviewed articles in each of the two overarching research categories: IPV interventions research, and SMW-inclusive IPV research", fig.lab="donut_scat", fig.fullwidth=TRUE
### PLOT - scat - donut ####

library(ggplot2)
scat.p <- ggplot(t.scat, aes(fill = Category,
                             ymax = ymax,
                             ymin = ymin,
                             xmax = 4,
                             xmin = 3)) +
    scale_fill_manual(values = catpal,
                      breaks = rev(levels(t.scat$Category)),
                      labels = paste0(t.scat$Category, " (n = ", t.scat$N, ")")
    ) +
    geom_rect() +
    coord_polar(theta = "y") +
    xlim(c(0, 4)) +
    annotate("text", x = 0, y = 0, label = "Category Proportions", family = "serif", size = 5, fontface = "italic") +
    geom_text(aes(x = 3.5, y = c(0.1, 0.60), label = t.scat$perc, #angle = c(-35, -35),
                  fontface = "bold"), colour = pal_my[2], size = 5.5, family = "serif", vjust = 0.3) +
    # labs(title = "", fill = "Research Category") +
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
## publication titles ================
t.jrnl <- Rtdf(MAP$journal)
ft.jrnl <- with(MAP, {
    ftable(journal, scat) %>%
        matrix(nrow = nrow(t.jrnl),
               byrow = FALSE)
})
dimnames(ft.jrnl) <- list("Publication Title" = levels(MAP$journal),
                          Category = levels(MAP$scat))
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
#+ dot_scatXjournal
### PLOT - scat-x-journal - dotchart ####
ftm.j <- Rna(ft.jrnl)
sum.j <- apply(ftm.j, 1, sum)
ftm.j <- ifelse(ftm.j == 0, NA, ftm.j)
ftm.jp <- cbind(ft.jrnl, "**Total**" = sum.j)
ftm.jp %>% kable(align = rep("r", 3),
                 caption = "$N_{articles}$ in Each Research Category per Journal")

Rdotchart(main = expression(paste(italic(N[Articles]), italic(" per Publication"))),
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

#+ parset_scatXjournal, fig.fullwidth=TRUE
### PLOT - scat-x-journal - parset ####

MAP.jrnl <- MAP[, c("scat", "journal", "jrnl")]
names(MAP.jrnl) <- c("Category", "J", "Journal")
MAP.jrnl$Category <- ifelse(MAP.jrnl$Category == "S3",
                            "IPV Interventions",
                            "SMW-Inclusive Research")
pj <- mpal(seq_along(unique(MAP$jrnl)), p = ppal)
library(ggparallel)
pscat <- c("#a6afbb", pal_my[17])
pscat.a <- adjustcolor(pscat, alpha.f = 0.5)

al.j <- c(rep(90, length(unique(MAP.jrnl$Journal))), rep(0, length(unique(MAP.jrnl$Category))))
t.jrnl$log <- log(t.jrnl[, 2]) + 3
lj <- c(t.jrnl[, 3], rep(3.50, length(unique(MAP.jrnl$Category))))
parset.jrnl <- ggparset2(list("Journal", "Category"),
                         data = MAP.jrnl,
                         method = "adj.angle", label = TRUE,
                         label.size = lj, text.angle = al.j, order = c(0, 0)) +
     scale_fill_manual(values = c(pscat.a, adjustcolor(pj, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pj), guide = FALSE) + coord_flip() +
    thm_Rtft(ticks = FALSE, ytext = TRUE, xtext = FALSE) #+ scale_y_discrete(labels = c("Category" = "Category", "J" = "Journal"))

hideLabs(x = parset.jrnl, labs = labs(subtitle = "Journals"))

MAP.jrnl <- MAP[, c("scat", "journal", "cpv", "jrnl")]


pcpv <- pp[c(11, 30)]

clr.cpv1 <- levels(factor(MAP.jrnl$journal))
clr.cpv <- ifelse(clr.cpv1 %in% j.cpp, pcpv[1], pcpv[2])

MAP.jrnl$cpv <- ifelse(MAP.jrnl$cpv == "CP", "Community Psychology", "Violence-Specific")
MAP.jrnl$scat <- ifelse(MAP.jrnl$scat == "S3", "IPV Interventions", "SMW-Inclusive Research")

names(MAP.jrnl) <- c("Category", "J", "Discipline", "Journal")

# al.cj <- c(rep(0, length(unique(MAP.jrnl$Category))), rep(90, length(unique(MAP.jrnl$Journal))), rep(0, length(unique(MAP.jrnl$Discipline))))

al.cj <- c(rep(90, length(unique(MAP.jrnl$J))),
           rep(0, length(unique(MAP.jrnl$Category))),
           rep(0, length(unique(MAP.jrnl$Discipline))))

ll.cj <- c(t.jrnl[, 3],
           rep(3.50, length(unique(MAP.jrnl$Category))),
           rep(3.50, length(unique(MAP.jrnl$Discipline))))

parset.jrnl2 <- ggparset2(list("Journal", "Category", "Discipline"),
                          data = MAP.jrnl, method = "adj.angle",
                          label = TRUE, label.size = ll.cj,
                          label.face = "bold",
                          text.angle = al.cj, order = c(0, 0, 0)) +
     scale_fill_manual(values = c(pscat.a,
                                  adjustcolor(pcpv, alpha.f = 0.75),
                                  adjustcolor(clr.cpv, alpha.f = 0.55)),
                       guide = FALSE) +
    scale_colour_manual(values = c(pscat.a,
                                   adjustcolor(pcpv, alpha.f = 0.85),
                                   adjustcolor(clr.cpv, alpha.f = 0.85)),
                        guide = FALSE) +
    coord_flip() +
    thm_Rtft(ticks = FALSE, ytext = TRUE, xtext = FALSE);
hideLabs(x = parset.jrnl2, labs = labs(subtitle = "Journals & Research Disciplines by Research Category"))


#'
#'
#' # Publication Years
#' \Frule
#'
#+ hist_yrXscat, echo=FALSE, fig.fullwidth=TRUE, fig.cap="Publication Years Grouped by Research Category", fig.height = 4, figPath=TRUE
par(pmar)
### PLOT - year-X-scat - hist ####
s3hist <- hist(MAP$year[MAP$scat == "S3"], plot = FALSE, right = F, breaks = 25)
s4hist <- hist(MAP$year[MAP$scat == "S4"], plot = FALSE, right = F, breaks = s3hist$breaks)
## col = pal_my.a75[12], border = pal_my[19], lwd = .5

plot(s3hist, col = catpal[1], border = pal_my[19], density = 50, lwd = .25,
     main = " ", xlab = "Year Published", ylab = expression(N[Articles]),
     ylim = c(0, max(s3hist$counts)), yaxt = "n");

plot(s4hist, col = catpal[2], border = pal_my[19], density = 50, angle = -45, lwd = .25, yaxt = "n", add = TRUE);

axis(2, at = 0:max(s3hist$counts));

legend(x = 1990, y = 2.75, legend = c("SMW-Inclusive Research", "IPV Interventions Research (general)"), fill = c(pal_sci[8], pal_my[16]), density = 50, angle = c(45, -45), border = NA, bty = 'n', cex = 0.8, text.font = 3, trace = F)
#'
#+ noEcho, echo=FALSE
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
#+ topics

## topics ================
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
#'
#+ dot_topics, fig.fullwidth=TRUE, fig.height = 7.5, figPath=TRUE
### PLOT - topics - dotchart ####
Rdotchart(main = "Research Topics",
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
#+ designs
## designs ================

ct.d <- cb[cb$cat == "DESIGN", ] %>% droplevels()
# ct.d <- ct.d[!duplicated(ct.d), ]
# x <- ct.d[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

t.d <- Rtdf(ct.d$clab)
ct.d$clab <- gsub(" Design", "", ct.d$clab) %>% factor()
ft.d <- ftable(ct.d[, c("clab", "scat")], row.vars = 1)
ftm.d <- matrix(ft.d, nrow = nrow(t.d), byrow = FALSE)
dimnames(ftm.d) <- list(Design = levels(ct.d$clab),
                        Category = levels(ct.d$scat))
sum.d <- apply(ftm.d, 1, sum)
ftm.d <- ifelse(ftm.d == 0, NA, ftm.d)
ftm.d <- cbind(ftm.d, "**Total**" = sum.d)
ftm.d %>% kable(align = rep("r", 3),
                caption = "Research Designs")

nlabs <- length(unique(ct.d$clab))
pd <- mpal(seq_along(unique(ct.d$clab)), p = ppal)

ct.d <- dplyr::rename(ct.d, "Category" = scat, "Design" = clab)

#+ parset_designs, fig.fullwidth=TRUE
### PLOT - designs - parset ####

library(ggparallel)
pscat <- c("#a6afbb", pal_my[17])
t.d$log <- log(t.d[, 2]) + 2
al.d0 <- ifelse(t.d[, 2] < 3, 90, 0)

al.d <- c(al.d0, rep(0, length(unique(ct.d$Category))))
ld <- c(t.d[, 3], rep(3.50, length(unique(ct.d$Category))))
parset.dsgn <- ggparset2(list("Design", "Category"),
                         data = ct.d,
                         method = "adj.angle", label = TRUE,
                         label.size = ld, text.angle = al.d, order = c(0, 0)) +
     scale_fill_manual(values = c(pscat.a, adjustcolor(pd, alpha.f = 0.45)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pd), guide = FALSE) + coord_flip() +
    thm_Rtft(ticks = FALSE, ytext = TRUE, xtext = FALSE)

hideLabs(x = parset.dsgn, labs = labs(subtitle = "Research Designs"))
#'
#' \newpage
#' `r tufte::newthought("\\Large{Experimental Research Designs}")`
#'
#+ exp
## experimental designs ================

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
                          Category = levels(ct.exp$scat))
sum.exp <- apply(ftm.exp, 1, sum)
ftm.exp <- ifelse(ftm.exp == 0, NA, ftm.exp)
ftm.expp <- cbind(ftm.exp, "**Total**" = sum.exp)
# rownames(ftm.expp) <- gsub("\\n", "", rownames(ftm.expp))
ftm.expp %>% kable(align = rep("r", 3),
                   caption = "Experimental Research Designs")

nlabs <- length(unique(ct.exp$clab))
pexp <- mpal(seq_along(unique(ct.exp$clab)), p = ppal)

ct.exp <- dplyr::rename(ct.exp, "Category" = scat, "Experimental Design" = clab)

#+ parset_exp, fig.fullwidth = TRUE
### PLOT - experimental designs - parset ####

library(ggparallel)
pscat <- c("#a6afbb", pal_my[17])
t.exp$log <- log(t.exp[, 2]) + 2
lexp <- c(t.exp[, 3], rep(3.50, length(unique(ct.exp$Category))))

parset.exp <- ggparset2(list("Experimental Design",
                             "Category"),
                        data = ct.exp,
                        method = "adj.angle", label = TRUE,
                        label.size = lexp, text.angle = 0, order = c(0, 0)) +
     scale_fill_manual(values = c(pscat.a, adjustcolor(pexp, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pexp), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
hideLabs(x = parset.exp, labs = labs(subtitle = "Experimental Designs"))
#'
#' \newpage
#' ## Data Collection Methodologies
#'
#+ methodologies
## methodologies ================

ct.mo <- cb[cb$cat == "METHODS", ] %>% droplevels()
ct.mo <- ct.mo[!duplicated(ct.mo), ]
# x <- ct.mo[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

t.mo <- Rtdf(ct.mo$clab)

ft.mo <- ftable(ct.mo[, c("clab", "scat")], row.vars = 1)
ftm.mo <- matrix(ft.mo, nrow = nrow(t.mo), byrow = FALSE)
dimnames(ftm.mo) <- list(Methodology = levels(ct.mo$clab),
                         Category = levels(ct.mo$scat))
sum.mo <- apply(ftm.mo, 1, sum)
ftm.mo <- ifelse(ftm.mo == 0, NA, ftm.mo)
ftm.mo <- cbind(ftm.mo, "**Total**" = sum.mo)
ftm.mo %>% kable(align = rep("r", 3),
                 caption = "Methodologies")

#+ parset_methodologies, fig.fullwidth=TRUE
### PLOT - methodologies - parset ####

nlabs <- length(unique(ct.mo$clab))
pmo <- mpal(seq_along(unique(ct.mo$clab)), p = ppal)

ct.mo <- dplyr::rename(ct.mo, "Category" = scat, "Methodology" = clab)

t.mo$log <- log(t.mo[, 2]) + 2
lmo <- c(t.mo[, 3], rep(3.50, length(unique(ct.mo$Category))))

pscat <- c("#a6afbb", pal_my[17])
parset.mo <- ggparset2(list("Methodology", "Category"),
                       data = ct.mo,
                       method = "adj.angle", label = TRUE,
                       label.size = lmo, text.angle = 0, order = c(0, 0)) +
     scale_fill_manual(values = c(pscat.a, adjustcolor(pmo, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pmo), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = TRUE, xtext = FALSE) #+ coord_flip()
hideLabs(x = parset.mo, labs = labs(subtitle = "Methodologies"))
#'
#' \newpage
#'
#'
#' `r tufte::newthought("\\Large{QuaLitative Research \\textit{Designs}}")`
#'
#+ qlDesigns
ct.dql <- cb[cb$cat == "D-QL", ] %>% droplevels()
ct.dql <- ct.dql[!duplicated(ct.dql), ]
# x <- ct.dql[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

t.dql <- Rtdf(ct.dql$clab)
ft.dql <- ftable(ct.dql[, c("clab", "scat")], row.vars = 1)
ftm.dql <- matrix(ft.dql, nrow = nrow(t.dql), byrow = FALSE)
dimnames(ftm.dql) <- list("Qua**L**itative Design" = levels(ct.dql$clab),
                          Category = levels(ct.dql$scat))
# t.dql
sum.dql <- apply(ftm.dql, 1, sum)
ftm.dql <- ifelse(ftm.dql == 0, NA, ftm.dql)
ftm.dql <- cbind(ftm.dql, "**Total**" = sum.dql)
ftm.dql %>% kable(align = rep("r", 3),
                  caption = "Qua**L**itative Designs")

#+ parset_qlDesigns, fig.fullwidth=TRUE
### PLOT - qual designs - parset ####

nlabs <- length(unique(ct.dql$clab))
pdql <- mpal(seq_along(unique(ct.dql$clab)), p = ppal)

ct.dql <- dplyr::rename(ct.dql, "QuaLitative Design" = clab, "Category" = scat)

t.dql$log <- log(t.dql[, 2]) + 2
ldql <- c(rep(3.50, length(unique(ct.dql$Category))), t.dql[, 3])

parset.dql <- ggparset2(list("QuaLitative Design", "Category"),
                        data = ct.dql,
                        method = "adj.angle", label = TRUE,
                        label.size = ldql, text.angle = 0, order = c(0, 0)) +
     scale_fill_manual(values = c(pscat.a, adjustcolor(pdql, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pdql), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
hideLabs(x = parset.dql, labs = labs(subtitle = "Qualitative Research Designs"))
#'
#'  \newpage
#' `r tufte::newthought("\\large{QuaLitative \\textit{Methods}}")`
#'
#+ qlMethods
## qual methods ================

ct.ql <- cb[cb$cat == "M-QL", ] %>% droplevels()
ct.ql <- ct.ql[!duplicated(ct.ql), ]
# x <- ct.ql[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

t.ql <- Rtdf(ct.ql$clab)
ft.ql <- ftable(ct.ql[, c("clab", "scat")], row.vars = 1)
ftm.ql <- matrix(ft.ql, nrow = nrow(t.ql), byrow = FALSE)
dimnames(ftm.ql) <- list("Qua**L**itative Method(s)" = levels(ct.ql$clab),
                         Category = levels(ct.ql$scat))

sum.ql <- apply(ftm.ql, 1, sum)
ftm.ql <- ifelse(ftm.ql == 0, NA, ftm.ql)
ftm.ql <- cbind(ftm.ql, "**Total**" = sum.ql)
ftm.ql %>% kable(align = rep("r", 3),
                 caption = "Qua**L**itative Method(s)")

#+ parset_qlMethods, fig.fullwidth=TRUE
### PLOT - qual methods - parset ####
pql <- mpal(seq_along(unique(ct.ql$clab)), p = ppal)

ct.ql <- dplyr::rename(ct.ql, "QuaLitative Methods" = clab, "Category" = scat)

t.ql$log <- log(t.ql[, 2]) + 2
lql <- c(rep(3.50, length(unique(ct.ql$Category))), t.ql[, 3])

parset.ql <- ggparset2(list("QuaLitative Methods", "Category"),
                       data = ct.ql,
                       method = "adj.angle", label = TRUE,
                       label.size = lql, text.angle = 0, order = c(0, 0)) +
     scale_fill_manual(values = c(pscat.a, adjustcolor(pql, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pql), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
hideLabs(x = parset.ql, labs = labs(subtitle = "Qualitative Research Methods"))
#'
#' \newpage
#'
#' `r tufte::newthought("\\Large{QuaLitative Data Analytic Approaches}")`
#'
#+ qlAnalytics
## QL Analytic Approaches ================

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
# rownames(ftm.aqlp) <- gsub("\\n", "", rownames(ftm.aqlp))
ftm.aqlp %>% kable(align = rep("r", 3),
                   caption = "QuaLitative Analytic Approaches")

#+ dot_qlAnalytics, fig.fullwidth=TRUE, fig.height=7
### PLOT - QL analytic approaches - dotchart ####

nlabs <- length(unique(ct.aql$clab))
paql <- mpal(seq_along(unique(ct.aql$clab)), p = ppal)

al.cj <- c(rep(90, length(unique(ct.aql$clab))),
           rep(0, length(unique(ct.aql$scat))))

os.cj <- c(rep(0.07, length(unique(ct.aql$clab))),
           rep(0, length(unique(ct.aql$scat))))

t.aql$log <- log(t.aql[, 2]) + 2
laql <- c(t.aql[, 3], rep(3.50, length(unique(ct.aql$scat))))

ct.aqlps <- dplyr::rename(ct.aql, "QuaLitative Analytic Approaches" = clab, "Category" = scat)

aql.s3 <- ftm.aql[, 1]
aql.s4 <- ftm.aql[, 2]

Rdotchart(main = "QuaLitative Analytic Approaches",
          ftm.aql,
          pch = 19,
          gcolor = pal_my[20],
          xlab = expression(N[Articles]),
          cex = 0.7,
          gcex = 0.75,
          gfont = 2,
          pt.cex = 1.125,
          color = c(rep(catpal[1], nrow(ftm.aql)), rep(catpal[2], nrow(ftm.aql))))

# laql <- c(rep(3.50, length(unique(ct.aqlps$Category))), t.aql[, 3])

#+ parset_qlAnalytics, fig.fullwidth=TRUE, figPath=TRUE
### PLOT - QL analytic approaches - parset ####

# parset.aql <- ggparset2(list("Category", "QuaLitative Analytic Approaches"),
#                         data = ct.aqlps,
#                         method = "adj.angle", label = TRUE,
#                         label.size = laql, text.angle = 0, asp=1.25, text.offset = 0, order = c(0, 0),
#                         label.hjust = c(rep(0.5, 2), rep(0.55, nrow(t.aql)))) +
#      scale_fill_manual(values = c(pscat.a, adjustcolor(paql, alpha.f = 0.55)),
#                       guide = FALSE) +
#     scale_colour_manual(values = c(pscat, paql), guide = FALSE) +
#     thm_Rtft(ticks = FALSE, ytext = FALSE)

parset.aql <- ggparset2(rev(list("Category", "QuaLitative Analytic Approaches")),
                        data = ct.aqlps, method = "adj.angle",
                        order = c(0, 0), #asp=1.25,
                        text.angle = 0, #text.offset = os.cj,
                        label = TRUE, label.size = laql,
                        label.hjust = 0.5) +
     scale_fill_manual(values = c(pscat.a, adjustcolor(paql, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, paql), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE) #+ coord_flip()
hideLabs(x = parset.aql, labs = labs(subtitle = "QuaLitative Analytics"))


#'
#'
#' \newpage
#'
#' `r tufte::newthought("\\Large{QuaNTitative Research \\textit{Designs}}")`
#'
#+ qtDesigns
ct.dqt <- cb[cb$cat == "D-QT", ] %>% droplevels()
ct.dqt <- ct.dqt[!duplicated(ct.dqt), ]
# x <- ct.dqt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

t.dqt <- Rtdf(ct.dqt$clab)
ft.dqt <- ftable(ct.dqt[, c("clab", "scat")], row.vars = 1)
ftm.dqt <- matrix(ft.dqt, nrow = nrow(t.dqt), byrow = FALSE)
dimnames(ftm.dqt) <- list("Qua**NT**itative Design" = levels(ct.dqt$clab),
                          Category = levels(ct.dqt$scat))
# t.dqt
sum.dqt <- apply(ftm.dqt, 1, sum)
ftm.dqt <- ifelse(ftm.dqt == 0, NA, ftm.dqt)
ftm.dqt <- cbind(ftm.dqt, "**Total**" = sum.dqt)
ftm.dqt %>% kable(align = rep("r", 3),
                  caption = "Qua**NT**itative Designs")

#+ parset_qtDesigns, fig.fullwidth=TRUE
### PLOT - quant designs - parset ####

nlabs <- length(unique(ct.dqt$clab))
pdqt <- mpal(seq_along(unique(ct.dqt$clab)), p = ppal)

ct.dqt <- dplyr::rename(ct.dqt, "QuaNTitative Design" = clab, "Category" = scat)

t.dqt$log <- log(t.dqt[, 2]) + 2
ldqt <- c(t.dqt[, 3], rep(3.50, length(unique(ct.dqt$Category))))

parset.dqt <- ggparset2(list("QuaNTitative Design",
                             "Category"),
                        data = ct.dqt,
                        method = "adj.angle", label = TRUE,
                        label.size = ldqt, text.angle = 0, order = c(0, 0)) +
     scale_fill_manual(values = c(pscat.a, adjustcolor(pdqt, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pdqt), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
hideLabs(x = parset.dqt, labs = labs(subtitle = "QuaNTitative Research Designs"))
#'
#'
#' \newpage
#' `r tufte::newthought("\\large{QuaNTitative \\textit{Methods}}")`
#'
#+ qtMethods
### quant methods ================

ct.qt <- cb[cb$cat == "M-QT", ] %>% droplevels()
ct.qt <- ct.qt[!duplicated(ct.qt), ]
# x <- ct.qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

t.qt <- Rtdf(ct.qt$clab)
ft.qt <- ftable(ct.qt[, c("clab", "scat")], row.vars = 1)
ftm.qt <- matrix(ft.qt, nrow = nrow(t.qt), byrow = FALSE)
dimnames(ftm.qt) <- list("Qua**NT**itative Method" = levels(ct.qt$clab),
                         Category = levels(ct.qt$scat))
# t.qt
sum.qt <- apply(ftm.qt, 1, sum)
ftm.qt <- ifelse(ftm.qt == 0, NA, ftm.qt)
ftm.qt <- cbind(ftm.qt, "**Total**" = sum.qt)
ftm.qt %>% kable(align = rep("r", 3),
                 caption = "Qua**NT**itative Methods")

#+ parset_qtMethods, fig.fullwidth=TRUE, fig.height=6
### PLOT - quant Methods - parset ####

nlabs <- length(unique(ct.qt$clab))
pqt <- mpal(seq_along(unique(ct.qt$clab)), p = ppal)

ct.qt <- dplyr::rename(ct.qt, "QuaNTitative Methods" = clab, "Category" = scat)

t.qt$log <- log(t.qt[, 2]) + 2
lqt <- c(t.qt[, 3], rep(3.50, length(unique(ct.qt$Category))))

parset.qt <- ggparset2(list("QuaNTitative Methods",
                            "Category"),
                       data = ct.qt,
                       method = "adj.angle", label = TRUE,
                       label.size = lqt, text.angle = 0, order = c(0, 0)) +
     scale_fill_manual(values = c(pscat.a, adjustcolor(pqt, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pqt), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
hideLabs(x = parset.qt, labs = labs(subtitle = "Quantitative Research Methods"))
#'
#' \newpage
#'
#' `r tufte::newthought("\\Large{QuaNTitative Data Analytic Approaches}")`
#'
#+ qtAnalytics
## quant analytics ================

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

#+ dot_qtAnalytics, fig.fullwidth=TRUE, fig.height=7
### PLOT - quant analytics - dotchart ####

nlabs <- length(unique(ct.aqt$clab))
paqt <- mpal(seq_along(unique(ct.aqt$clab)), p = ppal)

ct.aqtps <- dplyr::rename(ct.aqt, "QuaNTitative Analytic Approaches" = clab, "Category" = scat)

aqt.s3 <- ftm.aqt[, 1]
aqt.s4 <- ftm.aqt[, 2]

Rdotchart(main = "QuaNTitative Analytics",
          ftm.aqt,
          pch = 19,
          gcolor = pal_my[20],
          xlab = expression(N[Articles]),
          cex = 0.7,
          gcex = 0.75,
          gfont = 2,
          pt.cex = 1.125,
          color = c(rep(catpal[1], nrow(ftm.aqt)), rep(catpal[2], nrow(ftm.aqt))))


#+ parset_qtAnalytics, fig.fullwidth=TRUE, fig.height=7,figPath=TRUE
### PLOT-qtAnalytics-parset ####
t.aqt$log <- log(t.aqt[, 2]) + 1.5
# laqt <- c(rep(3.50, length(unique(ct.aqtps$Category))), t.aqt[, 3])
laqt <- rev(c(rep(3.50, length(unique(ct.aqtps$Category))), rev(t.aqt[, 3])))

parset.aqt <- ggparset2(rev(list("Category",
                                 "QuaNTitative Analytic Approaches")),
                        data = ct.aqtps, method = "adj.angle",
                        order = c(0, 0), #asp=1.25,
                        text.angle = 0, #text.offset = os.cj,
                        label = TRUE, label.size = laqt,
                        label.hjust = 0.5) +
     scale_fill_manual(values = c(pscat.a, adjustcolor(paqt, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, paqt), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE) #+ coord_flip()
hideLabs(x = parset.aqt, labs = labs(subtitle = "QuaNTitative Analytics"))
# parset.aqt <- ggparset2(list("Category", "QuaNTitative Analytic Approaches"),
#                         data = ct.aqtps,
#                         method = "parset", label = TRUE,
#                         label.size = laqt, text.angle = 0, order = c(0, 0),
#                         label.hjust = c(rep(0.5, 2), rep(0.55, nrow(t.aqt))))+
#      scale_fill_manual(values = c(pscat.a, adjustcolor(paqt, alpha.f = 0.55)),
#                       guide = FALSE) +
#     scale_colour_manual(values = c(pscat, paqt), guide = FALSE) +
#     thm_Rtft(ticks = FALSE, ytext = FALSE)
# hideLabs(x = parset.aqt, labs = labs(subtitle = "Quantitative Analytics"))
#'
#'
#' \newpage
#' `r tufte::newthought("\\large{Archival/Secondary Data Sources}")`
#'
#+ archivalMethods
### archival/secondary data sources ####
ct.rcrd <- cb[cb$cat == "M-RCRD", ] %>% droplevels()
ct.rcrd <- ct.rcrd[!duplicated(ct.rcrd), ]
# x <- ct.rcrd[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

t.rcrd <- Rtdf(ct.rcrd$clab)
ft.rcrd <- ftable(ct.rcrd[, c("clab", "scat")], row.vars = 1)
ftm.rcrd <- matrix(ft.rcrd, nrow = nrow(t.rcrd), byrow = FALSE)
dimnames(ftm.rcrd) <- list("Archival Data Source" = levels(ct.rcrd$clab),
                           Category = levels(ct.rcrd$scat))
# t.rcrd
sum.rcrd <- apply(ftm.rcrd, 1, sum)
ftm.rcrd <- ifelse(ftm.rcrd == 0, NA, ftm.rcrd)
ftm.rcrd <- cbind(ftm.rcrd, "**Total**" = sum.rcrd)
ftm.rcrd %>% kable(align = rep("r", 3),
                   caption = "Archival Data Sources")

#+ parset_archivalDataSrcs, fig.fullwidth=TRUE
### PLOT - archival/secondary data sources - parset####

nlabs <- length(unique(ct.rcrd$clab))
prcrd <- mpal(seq_along(unique(ct.rcrd$clab)), p = ppal)

ct.rcrd <- dplyr::rename(ct.rcrd, "Archival Data Source" = clab, "Category" = scat)

t.rcrd$log <- log(t.rcrd[, 2]) + 3
lrcrd <- c(t.rcrd[, 3], rep(3.50, length(unique(ct.rcrd$Category))))

parset.rcrd <- ggparset2(list("Archival Data Source",
                              "Category"),
                         data = ct.rcrd,
                         method = "adj.angle", label = TRUE,
                         label.size = lrcrd, text.angle = 0, order = c(0, 0)) +
    scale_fill_manual(values = c(adjustcolor(prcrd, alpha.f = 0.55), pscat.a),
                      guide = FALSE) +
    scale_colour_manual(values = c(prcrd, pscat), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
hideLabs(x = parset.rcrd, labs = labs(subtitle = "Archival/Secondary Data Sources"))
#'
#' \newpage
#'
#' `r tufte::newthought("\\Large{Mixed-Methodological \\textit{Designs}}")`
#'
#+ mmDesigns
## mixed-methods designs ================

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

#+ parset_mixedMethods, fig.fullwidth=TRUE
### PLOT - mm designs - parset ####

nlabs <- length(unique(ct.dmm$clab))
pmm <- mpal(seq_along(unique(ct.dmm$clab)), p = ppal)

ct.dmm <- dplyr::rename(ct.dmm, "Mixed-Methodological Design" = clab, "Category" = scat)

t.dmm$log <- log(t.dmm[, 2]) + 3
ldmm <- c(rep(3.50, length(unique(ct.dmm$Category))), t.dmm[, 3])

parset.dmm <- ggparset2(list("Mixed-Methodological Design",
                             "Category"),
                        data = ct.dmm,
                        method = "adj.angle", label = TRUE,
                        label.size = ldmm, text.angle = 0, order = c(0, 0)) +
     scale_fill_manual(values = c(pscat.a, adjustcolor(pmm, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pmm), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
hideLabs(x = parset.dmm, labs = labs(subtitle = "Mixed-Methods Research Designs"))
#'
#' \newpage
#' `r tufte::newthought("\\large{Mixed (QuaLitative \\& QuaNTitative) \\textit{Methods}}")`
#'
#+ mmMethods
## mixed-methods methods ================

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

#+ parset_mmMethods, fig.fullwidth = TRUE
### PLOT - mm methods - parset ####

nlabs <- length(unique(ct.mm$clab))
pmm <- mpal(seq_along(unique(ct.mm$clab)), p = ppal)

ct.mm <- dplyr::rename(ct.mm, "Mixed-Methods" = clab, "Category" = scat)

t.mm$log <- log(t.mm[, 2]) + 3
lmm <- c(t.mm[, 3], rep(3.50, length(unique(ct.mm$Category))))

parset.mm <- ggparset2(list("Mixed-Methods",
                            "Category"),
                       data = ct.mm,
                       method = "adj.angle", label = TRUE,
                       label.size = lmm, text.angle = 0, order = c(0, 0)) +
     scale_fill_manual(values = c(pscat.a, adjustcolor(pmm, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pmm), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
hideLabs(x = parset.mm, labs = labs(subtitle = "Mixed-Methods"))
#'
#' \newpage
#' ## Target Populations & Sampling Frames
#'
#+ populations
## populations ================

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

#+ dot_populations, fig.fullwidth=TRUE, fig.height = 7.5, figPath=TRUE
### PLOT - populations - 1 ####

Rdotchart(main = "Populations",
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
#+ settings
## settings ================

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

#+ dot_settings, fig.fullwidth=TRUE, fig.height = 7.5, figPath=TRUE
### PLOT - settings - dotchart ####

Rdotchart(main = "Sampling Settings",
          ftm.set,
          pch = 19,
          gcolor = pal_my[20],
          xlab = expression(N[Articles]),
          cex = 0.7,
          gcex = 0.75,
          gfont = 2,
          pt.cex = 1.125,
          color = c(rep(catpal[1], nrow(ftm.set)), rep(catpal[2], nrow(ftm.set))))

#+ parset_settings, fig.fullwidth=TRUE, fig.height = 7.5
### PLOT - settings - parset ####

sets <- c(191, 196, 198, 202, 203, 206, 207, 208, 209, 210, 211, 212, 213, 216, 218, 219)
ct.set2 <- ct.set[ct.set$cid %in% sets, ] %>% droplevels()
ct.set2$clab <- droplevels(ct.set2$clab)
t.set2 <- Rtdf(ct.set2$clab)

nlabs <- length(unique(ct.set2$clab))
pset2 <- mpal(seq_along(unique(ct.set2$clab)), p = ppal)

ct.set2 <- dplyr::rename(ct.set2, "Sampling Settings" = clab, "Category" = scat)


t.set2$log <- log(t.set2[, 2]) + 2
lset <- c(t.set2[, 3], rep(3.50, length(unique(ct.set2$Category))))

parset.set <- ggparset2(list("Sampling Settings",
                             "Category"),
                        data = ct.set2,
                        method = "adj.angle", label = TRUE,
                        label.size = lset, text.angle = 0, order = c(0, 0)) +
     scale_fill_manual(values = c(pscat.a, adjustcolor(pset2, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pset2), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
hideLabs(x = parset.set, labs = labs(subtitle = "Sampling Settings"))
#'
#' \newpage
#' ## Sampling Methods
#'
#+ samplingMethods
## samplingMethods ================

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

#+ dot_sampling, fig.fullwidth=TRUE, figPath=TRUE
### PLOT - samplingMethods - dotchart ####

Rdotchart(main = "Sampling Methods",
          ftm.smthds,
          pch = 19,
          gcolor = pal_my[20],
          xlab = expression(N[Articles]),
          cex = 0.7,
          gcex = 0.75,
          gfont = 2,
          pt.cex = 1.125,
          color = c(rep(catpal[1], nrow(ftm.smthds)), rep(catpal[2], nrow(ftm.smthds))))

#+ parset_sampling, fig.fullwidth=TRUE
### PLOT - samplingMethods - parset ####

nlabs <- length(unique(ct.smthds$clab))
psmthds <- mpal(seq_along(unique(ct.smthds$clab)), p = ppal)

ct.smthds <- dplyr::rename(ct.smthds, "Sampling Methods" = clab, "Category" = scat)

t.smthds$log <- log(t.smthds[, 2]) + 2
lsmthds <- c(t.smthds[, 3], rep(3.50, length(unique(ct.smthds$Category))))

parset.smthds <- ggparset2(list("Sampling Methods", "Category"),
                           data = ct.smthds,
                           method = "adj.angle", label = TRUE,
                           label.size = lsmthds, text.angle = 0, order = c(0, 0)) +
     scale_fill_manual(values = c(pscat.a, adjustcolor(psmthds, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, psmthds), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
hideLabs(x = parset.smthds, labs = labs(subtitle = "Sampling Methods"))

#'
#' \newpage
#'
#' # \textsc{Ecological Network Analysis: Data Analytic Approaches}
#'
#' \Frule
#'
#'
#+ echo=FALSE

knitr::opts_chunk$set(fig.path = "graphics/EcoLvls/rplot-")

#+ ecoLvls
## eco levels of analysis ================

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

#+ parset_ecoLvls, fig.fullwidth=TRUE
### PLOT - eco levels - parset ####

nlabs <- length(unique(ct.eco$clab))
peco <- mpal(seq_along(unique(ct.eco$clab)), p = ppal)

ct.eco <- dplyr::rename(ct.eco, "Levels of Analysis" = clab, "Category" = scat)

t.eco$log <- log(t.eco[, 2]) + 2
leco <- c(t.eco[, 3], rep(3.50, length(unique(ct.eco$Category))))

parset.eco <- ggparset2(list("Levels of Analysis",
                             "Category"),
                        data = ct.eco,
                        method = "adj.angle", label = TRUE,
                        label.size = leco, text.angle = 0, order = c(0, 0)) +
     scale_fill_manual(values = c(pscat.a, adjustcolor(peco, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, peco), guide = FALSE) +
    thm_Rtft(ticks = FALSE, ytext = FALSE)
hideLabs(x = parset.eco, labs = labs(subtitle = "Ecological Levels of Analysis"))

#'
#' \newpage
#'
#' # \textsc{Ecological Network Analysis}
#'
#+ ecoLvlsData
## SEE "MAPlvls.R" FOR MORE DETAILED COMMENTARY ON THE REMAINING CODE BELOW ##

# DATA - ECO-LEVELS ----------------


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
#' # Ecological Levels of Analysis
#'
#+ clrs_labs

# COLORS & LABELS ----------------

vclrs <- rev(grad(1:10))[4:7]
vtclrs <- rev(grad(1:10))[6:9]

catpal85 <- adjustcolor(catpal, alpha.f = 0.85)
lcvclrs <- c("1 = vclrs[1]; 2 = vclrs[2]; 3 = vclrs[3]; 4 = vclrs[4]; 'S3' = catpal[1]; 'S4' = catpal[2]")
ltclrs <- c("1 = vtclrs[1]; 2 = vtclrs[2]; 3 = vtclrs[3]; 4 = vtclrs[4]")

llabs1 <- c("1 = 'Individual'; 2 = 'Relationship'; 3 = 'Community'; 4 = 'Societal'")
sclabs <- c("'S3' = 'IPV Interventions Research'; 'S4' = 'SMW-Inclusive IPV Research'")
#'
#+ l_MAP

# MERGE - MAP+l ----------------

l <- mplvls[, 1:4]
l$id <- rownames(l)

mpjscat <- MAP[, c("bibkey", "scat", "journal")]
l <- merge(l, mpjscat, by.x = "id", by.y = "bibkey", all = TRUE)

# `llong` ----------------

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

llongv01$vclr[1:kindex] <- gsub("\\w+\\d{4}\\w+", NA, llongv01$cvclr[1:kindex])
llongv01$vclr <- as.character(llongv01$vclr)

llongv01$cvclr[-1:-kindex] <- pal_my[18]

llongv <- llongv01[!duplicated(llongv01), ]

llong <- within(llong2[, c("id", "lvl")], {
    lvl <- car::recode(lvl, llabs1)
})

llongbi1 <- dplyr::rename(llong, "from" = id, "to" = lvl)
llongbi2 <- llongbi1[, c(2, 1)] %>% dplyr::rename("from" = to, "to" = from)

#+ llongg_net

# GRAPH DATA - LVLS ----------------

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
lblsize <- c(log(V(llongg)$size[1:kindex.g])*0.5, log(V(llongg)$size[-1:-kindex.g])*0.2)
#'
#+ net_lvls_bibkeys, fig.fullwidth=TRUE, figPath=TRUE
# PLOT - `llongg` ----------------
par(mar = rep(0, 4))
lfr <- layout_with_fr(llongg) %>% norm_coords()
plot(llongg, rescale = T, layout = lfr, vertex.label.color = V(llongg)$cvclr, vertex.label.cex = lblsize)

#+ lvls_lnet

# `lnet` ----------------

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

# llabs ----------------

library(car)
llabs <- c("'l1' = 'Individual'; 'l2' = 'Relationship'; 'l3' = 'Community'; 'l4' = 'Societal'")
lnet$from <- car::recode(lnet$from, llabs)
lnet$to <- car::recode(lnet$to, llabs)

#+ lfrq

# lfrq ----------------

lfrq1 <- lnet[, 1]
lfrq2 <- lnet[, 2]
lfrq3 <- c(lfrq1, lfrq2)
lfrq <- Rtdf(lfrq3, names = c("lvl", "Freq"))

# `lnetg` ----------------

library(igraph)
lnetg <- graph_from_data_frame(lnet, directed = FALSE, vertices = lfrq)
V(lnetg)$size <- V(lnetg)$Freq*1.5
lnetcol <- mpal(lfrq, a = 0.8)
V(lnetg)$color <- lnetcol
E(lnetg)$width <- 0.25
#'

#+ net_ecolvls_analysis
# PLOT - ecolvls ----------------

par(mar = rep(0, 4))
ll3 <- layout.gem(lnetg)
ll3n <- norm_coords(ll3)
plot(lnetg, rescale = T, layout = ll3n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA)
#'
#' \newpage
#'
#+ lnetft

# `lnetft` ----------------


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
ldeg <- log(degree(lnetg))
larcs <- lnetft %>% matrix()
larcs <- ifelse(larcs == 0, NA, larcs) %>% na.omit()

#+ arc_lvls, fig.height=3.75, fig.fullwidth=TRUE

# PLOT - `lnet - arcplot()` ----------------
par(mar = c(5, 0, 0, 0))
arcplot(ledges, col.arcs = hsv(0, 0, 0.1, 0.075), pch.nodes = 21, bg.nodes = adjustcolor(lnetcol, alpha.f = 0.25), cex.nodes = ldeg, col.nodes = pal_my[19], lwd.nodes = 1, lwd.arcs = larcs, line = 1.25, cex.labels = 0.75, font = 1, col.labels = pal_my[20])

#'
#' \newpage
#'
#' # Ecological Systems
#'
#+ sysnet

# `snet` ----------------

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


# `sfrq` ----------------

sfrq1 <- snet[, 1]
sfrq2 <- snet[, 2]
sfrq3 <- c(sfrq1, sfrq2)
sfrq <- Rtdf(sfrq3, names = c("lvl", "Freq"))


# GRAPH DATA - ECO SYS ----------------


library(igraph) ## not necessary - but included as reminder and in case I move/copy the code later ##

snetg <- graph_from_data_frame(snet, directed = FALSE, vertices = sfrq)
V(snetg)$size <- V(snetg)$Freq*1.5
# V(snetg)$label.size <- (V(snetg)$Freq*0.85
snetcol <- mpal(sfrq, a = 0.8)
V(snetg)$color <- snetcol
E(snetg)$width <- 0.25
#'
#'
#+ net_snetgfr, figPath=TRUE
# PLOT - `snetg` ----------------

par(mar = rep(0, 4))
ls1 <- layout.fruchterman.reingold(snetg)
ls1n <- norm_coords(ls1)
plot(snetg, rescale = T, layout = ls1n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA)
#'
#' \newpage
#'
#+ net_sysLvls, fig.fullwidth=TRUE, figPath=TRUE
# par(mar = c(0, 0, 0, 0), mfrow = c(1, 2))
# plot(lnetg, rescale = T, layout = ll3n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA, margin = c(0, -0.15, -0.15, 0.05)); box(which = "figure")
# plot(snetg, rescale = T, layout = ls1n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA, margin = c(0, 0, 0, -0.5)); box(which = "figure")
# par(mpar, mfrow = c(1, 1))


#'
#'
#' \newpage
#'
#+ snetft

# `snetft` ----------------

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
sarcs <- snetft %>% matrix()
sarcs <- ifelse(sarcs == 0, NA, sarcs) %>% na.omit()

#+ arc_sys, fig.height=3.5, fig.fullwidth=TRUE

# PLOT - `snetft-arcplot` ----------------

par(mar = c(5, 0, 0, 0))
arcplot(sedges, col.arcs = hsv(0, 0, 0.1, 0.06), pch.nodes = 21, bg.nodes = adjustcolor(snetcol, alpha.f = 0.5), cex.nodes = rev(sdeg)*0.25, col.nodes = snetcol, lwd.nodes = 0.75, lwd.arcs = sarcs, line = 1.25, cex.labels = 0.5, font = 1, col.labels = pal_my[20])
#'
#' \newpage
#'
#' ## \textsc{Network Analysis: Substantive Research Topics}
#+ tlnet
cbt <- cb[cb$cat == "TOPIC",
          c("bibkey", "scat", "cat", "code", "clab")] %>% droplevels()
cbt <- cbt[!duplicated(cbt), ]
tlabs <- paste0(seq(1:length(levels(cbt$clab))), " = ", levels(cbt$clab))

lt <- mplvls[, 1:4]
lt$id <- rownames(lt)

lcbt <- merge(lt, cbt, by.x = "id", by.y = "bibkey", all = FALSE)
lcbt.t <- lcbt[, c("code", "l1", "l2", "l3", "l4")] %>% dplyr::rename("id" = code)
lcbt.t.nd <- lcbt.t[!duplicated(lcbt.t), ]



tlnet1 <- lcbt.t[, 1:2] %>% Rtdf(names = c(names(lcbt.t[,1:2]), "Freq"))
tlnet1$l1 <- as.numeric(tlnet1$l1)
tlnet1$l1 <- ifelse(tlnet1$l1 == 1, NA, "l1")
tlnet1$Freq <- ifelse(tlnet1$Freq == 0, NA, tlnet1$Freq)
tlnet1 <- na.omit(tlnet1)
# tlnet1 <- tlnet1[!duplicated(tlnet1$id), ]
names(tlnet1) <- c("from", "to", "Freq")
tlnet2 <- lcbt.t[, c(1, 3)] %>% Rtdf(names = c(names(lcbt.t[, c(1, 3)]), "Freq"))
tlnet2$l2 <- as.numeric(tlnet2$l2)
tlnet2$l2 <- ifelse(tlnet2$l2 == 1, NA, "l2")
tlnet2$Freq <- ifelse(tlnet2$Freq == 0, NA, tlnet2$Freq)
tlnet2 <- na.omit(tlnet2)
names(tlnet2) <- c("from", "to", "Freq")

tlnet3 <- lcbt.t[, c(1, 4)] %>% Rtdf(names = c(names(lcbt.t[,c(1, 4)]), "Freq"))
tlnet3$l3 <- as.numeric(tlnet3$l3)
tlnet3$l3 <- ifelse(tlnet3$l3 == 1, NA, "l3")
tlnet3$Freq <- ifelse(tlnet3$Freq == 0, NA, tlnet3$Freq)
tlnet3 <- na.omit(tlnet3)
names(tlnet3) <- c("from", "to", "Freq")

tlnet4 <- lcbt.t[, c(1, 5)] %>% Rtdf(names = c(names(lcbt.t[,c(1, 5)]), "Freq"))
tlnet4$l4 <- as.numeric(tlnet4$l4)
tlnet4$l4 <- ifelse(tlnet4$l4 == 1, NA, "l4")
tlnet4$Freq <- ifelse(tlnet4$Freq == 0, NA, tlnet4$Freq)
tlnet4 <- na.omit(tlnet4)
names(tlnet4) <- c("from", "to", "Freq")

tlnet0 <- rbind(tlnet1, tlnet2, tlnet3, tlnet4)
tlnet0$clab <- recode(tlnet0$from, rec.code2clab) ## "rec.code2clab" is from "MAPrqda.R"
tlnet <- tlnet0[!duplicated(tlnet0), c("from", "to", "clab", "Freq")]

#+ tlnet_llabs
library(car)
llabs <- c("'l1' = '.Individual'; 'l2' = '.Relationship'; 'l3' = '.Community'; 'l4' = '.Societal'")
tlnet$to <- car::recode(tlnet$to, llabs)

#+ tlfrq

# tlfrq ----------------

tlfrq1 <- tlnet[, 1] %>% as.character()
tlfrq2 <- tlnet[, 2]
tlfrq3 <- c(tlfrq1, tlfrq2)
tlfrq <- Rtdf(tlfrq3, names = c("lvl", "Freq"))

tv1 <- tlnet[, 3] %>% as.character()
tv2 <- tlnet[, 2]
tv3 <- c(tv1, tv2)
tv <- Rtdf(tv3, names = c("id", "Freq"))
tv[, 1] <- as.character(tv[, 1])
# `tlnetg` ----------------

library(igraph)
tlnetg <- graph_from_data_frame(tlnet[, 1:2], directed = FALSE, vertices = tlfrq)
V(tlnetg)$size <- V(tlnetg)$Freq*3
tlnetcol <- mpal(tlfrq, a = 0.8)[-1:-4] %>% adjustcolor(alpha.f = 0.5)
tvclrs <- c(adjustcolor(vclrs[1:4], alpha.f = 0.65), tlnetcol)
V(tlnetg)$color <- tvclrs
# E(tlnetg)$width <- 0.25
E(tlnetg)$frq <- tlnet$Freq
E(tlnetg)$width <- log(tlnet$Freq) + 1
V(tlnetg)$name[-1:-4] <- seq(1:length(tv[-1:-4, 1]))
V(tlnetg)$name[1:4] <- gsub("\\.", "", V(tlnetg)$name[1:4])
tindex.g <- V(tlnetg)$name %>% length()
tlblsize <- c(log(V(tlnetg)$size[1:4])*0.125, log(V(tlnetg)$size[5:tindex.g])*0.325)
#'
#'
#+ tlnetg_cap, echo=FALSE
tlabs <- gsub("&", "\\\\&", tlabs)
tlabs <- paste(tlabs, collapse = ", ")

tlnetg_cap <- paste0("Network Diagram Showing Relations among Substantive Research Topics (numbered graph nodes) Covered \\& Ecological Levels of Analysis (named graph nodes) Involved among the Reviewed Literature: \\textit{", tlabs, "}")
# PLOTS - `tlnetg` (layout-0 & layout-2) ----------------

#'
#+ net_topics, out.height='4in', fig.fullwidth=TRUE, fig.cap=tlnetg_cap, figPath=TRUE

par(mar = rep(0, 4))
ltl <- layout_with_fr(tlnetg) %>% norm_coords()
plot(tlnetg, rescale = T, layout = ltl, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = c(vclrs[1:4], rep(NA, length(tlnetcol))), vertex.label.cex = tlblsize)

l1tpsum <- tlnet[tlnet$to == "l1", "Freq"] %>% sum()
l2tpsum <- tlnet[tlnet$to == "l2", "Freq"] %>% sum()
l3tpsum <- tlnet[tlnet$to == "l3", "Freq"] %>% sum()
l4tpsum <- tlnet[tlnet$to == "l4", "Freq"] %>% sum()

l1tpWgtMu <- l1tpsum / length(unique(tlnet$from))
l2tpWgtMu <- l2tpsum / length(unique(tlnet$from))
l3tpWgtMu <- l3tpsum / length(unique(tlnet$from))
l4tpWgtMu <- l4tpsum / length(unique(tlnet$from))

l1tpRawMu <- tlnet[tlnet$to == "l1", "Freq"] %>% mean()
l2tpRawMu <- tlnet[tlnet$to == "l2", "Freq"] %>% mean()
l3tpRawMu <- tlnet[tlnet$to == "l3", "Freq"] %>% mean()
l4tpRawMu <- tlnet[tlnet$to == "l4", "Freq"] %>% mean()

tpMuAll <- mean(tlnet$Freq)
# l1tpRawMu/tpMuAll
# l1tpWgtMu/tpMuAll
# l2tpRawMu/tpMuAll
# l2tpWgtMu/tpMuAll
#'
#'
#' \newpage
#'
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
#' ## Network Analysis: Data Analytic Approaches by Ecological Levels
#'
cba <- cb[cb$cat == "A-QT" | cb$cat == "A-QL",
          c("bibkey", "scat", "cat", "code", "clab")] %>% droplevels()
cba <- cba[!duplicated(cba), ]
# x <- cba[, c("bibkey", "code")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

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
alnet0$clab <- recode(alnet0$from, rec.code2clab) ## "rec.code2clab" is from "MAPrqda.R"
alnet <- alnet0[!duplicated(alnet0), c("from", "to", "clab", "Freq")]

#+ alnet_llabs
library(car)
llabs <- c("'l1' = '.Individual'; 'l2' = '.Relationship'; 'l3' = '.Community'; 'l4' = '.Societal'")
alnet$to <- car::recode(alnet$to, llabs)

#+ alfrq

# alfrq ----------------

alfrq1 <- alnet[, 1] %>% as.character()
alfrq2 <- alnet[, 2]
alfrq3 <- c(alfrq1, alfrq2)
alfrq <- Rtdf(alfrq3, names = c("lvl", "Freq"))

av1 <- alnet[, 3] %>% as.character()
av2 <- alnet[, 2]
av3 <- c(av1, av2)
av <- Rtdf(av3, names = c("id", "Freq"))
av[, 1] <- as.character(av[, 1])
# `alnetg` ----------------

library(igraph)
alnetg <- graph_from_data_frame(alnet[, 1:2], directed = FALSE, vertices = alfrq)
V(alnetg)$size <- V(alnetg)$Freq*1.5
alnetcol <- mpal(alfrq, a = 0.8)[-1:-4] %>% adjustcolor(alpha.f = 0.5)
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
# alabs <- gsub("\\n", "", alabs)
alabs <- gsub("&", "\\\\&", alabs)
# alabs1 <- paste0(alabs[1:length(alabs)-1], sep = ", ")
# deparse(alabs1)
alabs <- paste(alabs, collapse = ", ")

alnetg_cap <- paste0("Network Diagram Showing Relations among Analytic Approaches (numbered graph nodes) used and Ecological Levels of Analysis (named graph nodes) Invovled among the Reviewed Literature: \\textit{", alabs, "}")
# PLOTS - `alnetg` (layout-0 & layout-2) ----------------

#'
#+ arc_alnetg, out.height='4in', fig.cap=alnetg_cap

par(mar = rep(0, 4))
lal3 <- layout_with_fr(alnetg) %>% norm_coords()
plot(alnetg, rescale = T, layout = lal3, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = c(vclrs[1:4], rep(NA, length(alnetcol))), vertex.label.cex = alblsize)
#'
#'
#' \newpage
#'
#+ alnetpr
alnet.pr <- ftable(alnet[, 2:3], row.vars = 2) %>% as.matrix() #(nrow = nrow(alfrq), byrow = FALSE)
dimnames(alnet.pr) <- list(levels(factor(alnet$clab)), gsub("\\.", "", levels(factor(alnet$to))))
alnet.pr <- ifelse(alnet.pr >= 1, "$\\checkmark$", "$\\cdot$")
# rownames(alnet.pr) <- gsub("\\n", "", rownames(alnet.pr))
rownames(alnet.pr) <- gsub("&", "\\\\&", rownames(alnet.pr))
#'
kable(alnet.pr, caption = "Analytic approaches used across ecological levels of analysis", escape = F)
#'
#+ alnet_Nanalyses
analysisFrq <- tally(group_by(cba, bibkey))

#'
#' \newpage
#'
#+ aledges, echo=FALSE
# knitr::include_graphics("graphics/inputs/arc_analysEs.pdf")
# source("alnetg_arc.R")

elvl <- c("(L1) Individual", "(L2) Relationship", "(L3) Community", "(L4) Societal")
eLVL <- c("L1", "L2", "L3", "L4")

alrec <- paste0("'", alfrq$lvl[1:4], "' = '", elvl, "'", collapse = "; ") ## "alfrq" is from "bibs.R" ##
alrec1 <- paste0("'", elvl, "' = '", eLVL, "'", collapse = "; ")

aln <- alnet[, 1:2] ## "alnet" is from "bibs.R" ##
aln[, 2] <- car::recode(aln[, 2], alrec)
aln[, 1] <- car::recode(aln[, 1], rec.code2cid) ## "rec.code2cid" is from "bibs.R ##

al <- within(alfrq, {
    lvl <- car::recode(lvl, alrec)
    lvl <- car::recode(lvl, rec.code2cid)
})

# aledges ----------------------------

aledges0 <- graph_from_data_frame(aln, directed = FALSE, vertices = al)
aledges <- get.edgelist(aledges0)

alnetcol <- mpal(alfrq, p = ppal, a = 0.5)[-1:-4]
avbrdrs <- c(vclrs[1:4], alnetcol) ## "vclrs" is from "bibs.R" ##
avclrs <-
    c(adjustcolor(vclrs[1:4], alpha.f = 0.35),
      adjustcolor(alnetcol, alpha.f = 0.25))

adeg <- degree(aledges0)*.25
alstrength <- strength(aledges0)


allabs0 <- car::recode(al[, 1], rec.cid2clab)
allabs <- paste0(al[, 1], " = ", allabs0)
allabs1 <- paste0("**", al[-1:-4, 1], "**", " = _", allabs0[-1:-4], "_", collapse = ", ")

allabs.p <- gsub(" & ", " \\\\& ", allabs)
#'
#'
#' \newpage
#'
#+ arc_aledges2, fig.show='asis', fig.cap="Network Diagram Showing Relations among Analytic Approaches (numbered graph nodes) used and Ecological Levels of Analysis (named graph nodes) Invovled among the Reviewed Literature", fig.height=8, fig.fullwidth=TRUE, figPath=TRUE
# PLOT - Analytic Approaches (`alnetg_arc`) - arc ------------------------

allabs_pl0 <- gsub("_|\\*{2}|`(.*?)`|_|\\*{2}", "\\1", allabs1, perl = TRUE)
allabs_pl <- gsub(", ", "\n", allabs_pl0)
# allabs_pl1 <- strsplit(allabs_pl0, ", ")
# zdiv <- as.integer(length(allabs_pl[[1]])/2)
# z1 <- 1:zdiv
# z2 <- seq((length(z1) + 1), (length(z1) + 1 + zdiv))
# z3 <- seq((length(z1) + length(z2) + 1), (length(z1) + length(z2) + 1 + zdiv))
# z4 <- seq((length(z1) + length(z2) + length(z3) + 1), length(allabs_pl1[[1]]))

# allabs_pl21 <- paste0(allabs_pl1[[1]][z1], collapse = ", ")
# allabs_pl21 <- gsub(", ", "\n", allabs_pl21)
# allabs_pl22 <- paste0(allabs_pl1[[1]][z2], collapse = ", ")
# allabs_pl22 <- gsub(", ", "\n", allabs_pl22)
# allabs_pl23 <- paste0(allabs_pl1[[1]][z3], collapse = ", ")
# allabs_pl24 <- paste0(allabs_pl1[[1]][z4], collapse = ", ")

# z0 <- sapply(allabs_pl1, nchar)
# z1 <- c(z0, z0[length(z0)])
# z2 <- c(1, rep(NA, length(z1)))
# z3 <- ifelse(is.na(z2), z0[1:length(z0)], z2)
par(mar = c(5, 0, 0, 11.5))
arcplot(
    aledges,
    vertices = al[, 1],
    col.arcs = hsv(0, 0, 0.1, 0.15),
    pch.nodes = 21,
    bg.nodes = avclrs,
    col.nodes = avbrdrs,
    lwd.nodes = 0,
    cex.nodes = log(adeg)+1*1.25,
    lwd.arcs = adeg, #adeg,
    line = 0,
    cex.labels = 0.85,
    font = 1,
    col.labels = pal_my[20],
    horizontal = T,
    sorted = FALSE,
    # ordering = allabs,
    family = "serif"
)
mtext(text = allabs_pl, side = 4, cex = 0.5, adj = 0, padj = 0.5, las = 2, outer = TRUE, line = -11.25)

# title(sub = allabs_pl, adj =  0, cex.sub = 0.5, line = -3, outer = TRUE)
# title(sub = allabs_pl, adj =  0, cex.sub = 0.5, line = -14)
#'
#' `r allabs1`
#'
#+ echo=FALSE
par(op)
knitr::opts_chunk$set(fig.path = "graphics/bibkeys/rplot-",
                      fig.show = 'asis')#, echo = TRUE)
# options(warn = -1)
#'
#' \Frule
#'
#' \newpage
#'
#'
#+ tl_map, fig.fullwidth=TRUE, fig.width=7, out.width='\\linewidth', fig.align='center', figPath=TRUE

# MAPtl ----------------
MAP <- MAP[, c("bibkey", "year", "journal", "caseid", "scat", "jrnl", "cpv", "j.loc", "j.year", "SJR", "Hindex", "title")]

MAPtl <- within(MAP, {
    ## - making a copy so i don't mess up anything already
    ##   written below that may depend on the original
    ##   version of "MAP" ##
    # bibkey2 <- as.integer(factor(bibkey))
    bibkey2 <- ifelse(bibkey == "boal2014barriers", "boala2014barriers", bibkey)
    bibkey2 <- ifelse(bibkey2 == "boal2014impact", "boalb2014impact", bibkey2)
    bibkey2 <- gsub("(\\w+)(\\d{4})\\w+", "\\1 (\\2)", bibkey2)
    bibkey2 <- ifelse(bibkey == "boala (2014)", "boal (2014a)", bibkey2)
    bibkey2 <- ifelse(bibkey == "boalb (2014)", "boal (2014b)", bibkey2)
    bibkey2 <- sapply(bibkey2, RtCap, USE.NAMES = FALSE)
    yrv <- ifelse(cpv == "V", year, 0)
    yrcp <- ifelse(cpv == "CP", year, 0)
    cpv <- factor(cpv, labels = c("Community-Psychology", "Violence"))
})

MAPtl <- MAPtl[order(MAPtl$yrv), , drop = FALSE] %>% within({
    posv <- sequence(rle(sort(yrv))$lengths)
    posv <- ifelse(yrv == 0, 0, posv)
    posv <- log(posv + 0.5) * -0.5
})

MAPtl <- MAPtl[order(MAPtl$yrcp), , drop = FALSE] %>% within({
    poscp <- sequence(rle(sort(yrcp))$lengths)
    pos <- ifelse(yrcp == 0, posv, log(poscp - 0.5) * -0.5)
    # pos <- jitter(pos, amount = 0)
    ## could've achieved the same in the previous line,
    ## but wanted to preserve the separate pos* columns just in case ##
})
## !!!! THANKS TO THIS SO ANSWER FOR THE "sequence(rle())" solution:
##      http://stackoverflow.com/a/19998876/5944560
##      (i spent HOURS trying to figure out how to do this,
##      only to find that the geniuses behind R
##      [specifically the {utils} pkg] had already developed
##      an effecient, vectorized, solution) ##

# grays_nord <- colorRampPalette(pal_nord$polar[c(8, 1)]) ## add to pkg::Riley (in "Rpals.R")##
vawa <- 1994 ## year original VAWA was passed ##
vawaclr <- grays_nord(12)[7]
# yrcnt <- Rtdf(MAPtl$year, names = c("year", "yrcnt"))#[, 1, drop == FALSE]

# as.integer(MAPtl$year) %>% min() -> yrmin
# as.integer(MAPtl$year) %>% max()+1 -> yrmax

# GGPLOT - tl ----------------

gg.tl <- ggplot(MAPtl, aes(x = year, y = 0, colour = cpv)) +
    thm_Rtft(yticks = FALSE, ytext = FALSE, ytitle = FALSE, ltitle = TRUE,
             ptitle = TRUE, xtext = FALSE, xticks = FALSE) +
    theme(legend.text = element_text(size = rel(0.75), face = "italic"),
          legend.title = element_text(size = rel(0.95), face = "bold.italic"),
          legend.background = element_rect(fill = "transparent", size = 0.1, colour = pal_my[19]),
          legend.key.height = unit(0.5, "cm"),
          # legend.key = element_rect(size = 1),
          legend.justification = c(1, 0.67),
          legend.box.spacing = unit(0.1, "cm")) +
    labs(colour = "Journal Category", title = "Timeline of Reviewed Research\n") +
    scale_colour_manual(values = pcpv) + #, guide = FALSE) +
    geom_hline(yintercept = 0, size = 0.25, color = pal_nord$polar[7], alpha = 0.5) +
    geom_segment(aes(y = 0, yend = pos, x = year, xend = year),
                 colour = pal_my[19], alpha = 0.55,
                 na.rm = TRUE, size = 0.2, linetype = 3) +
    geom_text(aes(y = pos, x = year, label = bibkey2), hjust = 0.5, vjust = 0,
              angle = 25, size = 1.9, fontface = "bold", check_overlap = FALSE) +
    # geom_vline(xintercept = vawa, size = 0.45, color = pal_nord$polar[1], linetype = 3) +
    geom_text(aes(y = 0, x = vawa, label = "1994 Violence Against Women Act"),
              alpha = 0.5, angle = 90, colour = adjustcolor(pal_my[19], alpha.f = 0.5), size = 2.75,
              nudge_x = -0.25,
              family = "serif", fontface = "italic") +
    geom_text(aes(y = 0, x = year, label = year), check_overlap = TRUE,
              vjust = 0.5, hjust = 0.5, angle = 0, colour = pal_my[20],
              size = 2, fontface = "bold") +
    xlim(min(MAPtl$year) - 0.25, max(MAPtl$year)) +
    ylim(min(MAPtl$pos) - 0.15, max(MAPtl$pos) + 0.15)
gg.tl
#'
#' \newpage
#'
#' # \textsc{IPV Intervention \& Prevention Research}
#'
#' \Frule
#'
# reclab(cb$scat) ----------------
levels(cb$scat) <- c(1, 2)
cb$clab <- factor(cb$clab)
#'
# s3cb ----------------
s3cb <- cb[cb$scat == 1, ] %>% droplevels
s3cb.keys <- paste0("@", levels(s3cb$bibkey))
#'
#'
#+ tl_inv, fig.fullwidth=TRUE, fig.height=2.75, out.width='\\linewidth', figPath=TRUE
inv <- MAPtl[MAPtl$scat == "S3", ]

tl.inv <- inv[order(inv$year), c("bibkey", "year", "cpv", "journal", "title"), drop = FALSE] %>% droplevels()
tl.inv$bibkey <- paste0("@", tl.inv$bibkey)
tl.inv$journal <- paste0("_", tl.inv$journal, "_")
tl.inv <- dplyr::rename(tl.inv, "Study" = bibkey, "Journal" = journal, "Year Published" = year)
rownames(tl.inv) <- NULL

inv <- inv[order(inv$yrv), , drop = FALSE] %>% within({
    posv <- sequence(rle(sort(yrv))$lengths)
    posv <- ifelse(yrv == 0, 0, posv)
    posv <- log(posv + 0.75) * -1
})

inv <- inv[order(inv$yrcp), , drop = FALSE] %>% within({
    poscp <- sequence(rle(sort(yrcp))$lengths)
    pos <- ifelse(yrcp == 0, posv, log(poscp - 0.5) * -1)
})
# GGPLOT - invtl ----------------

gg.invtl <- ggplot(inv, aes(x = year, y = 0, colour = cpv)) +
    thm_Rtft(yticks = FALSE, ytext = FALSE, ytitle = FALSE, ltitle = TRUE,
             ptitle = TRUE, xtext = FALSE, xticks = FALSE) +
    theme(legend.text = element_text(size = rel(0.55)),
          legend.title = element_text(size = rel(0.65), face = "bold"),
          legend.justification = c(1, 0.8),
          legend.box.spacing = unit(0, "cm")) +
    # plot.margin = unit(c(1, rep(0.15, 3)), "cm")) +
    ylim(min(inv$pos) - 0.5, max(inv$pos) + 0.5) +
    labs(colour = "Journal Category", title = "IPV-Interventions Research Timeline") +
    scale_colour_manual(values = pcpv) + #, guide = FALSE) +
    geom_hline(yintercept = 0, size = 0.25, color = pal_nord$polar[7], alpha = 0.5) +
    geom_segment(aes(y = 0, yend = pos, x = year, xend = year),
                 colour = pal_my[19], alpha = 0.55,
                 na.rm = TRUE, size = 0.2, linetype = 3) +
    geom_text(aes(y = pos, x = year, label = bibkey2), hjust = 0.5, vjust = 1,
              angle = 25, size = 2.5, fontface = "bold") + #, nudge_y = -0.05) +
    # geom_vline(xintercept = vawa, size = 0.45, color = pal_nord$polar[1], linetype = 3) +
    geom_text(aes(y = 0, x = vawa, label = "1994 Violence Against Women Act"),
              alpha = 0.5, angle = 90, colour = vawaclr, size = 2.5,
              nudge_x = -0.25, family = "serif", fontface = "italic") +
    geom_text(aes(y = 0, x = year, label = year), check_overlap = TRUE,
              vjust = 0.5, hjust = 0.5, angle = 0, colour = pal_my[20],
              size = 2.5, family = "serif", fontface = "bold") +
    xlim(min(inv$year) - 0.25, max(inv$year)) +
    ylim(min(inv$pos) - 0.15, max(inv$pos) + 0.15)
gg.invtl

tl.inv[, c(1, 4, 2)] %>% kable(caption = "IPV Interventions Research Timeline")

#'
#' \newpage
#'

#' ## Research Topics
#'
#+ topics_s3
## s3cb - TOPICS ================

# levels(droplevels(cb[cb$cat == "TOPIC", "clab"])) %>% as.list() %>% pander()
# l1tops <-levels(droplevels(cb[cb$cat == "TOPIC", "clab"]))[c(12, 16, )]

s3top <- s3cb[s3cb$cat == "TOPIC", ] %>% droplevels()
s3top <- s3top[!duplicated(s3top), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s3top$clab, s3top$jrnl)
Rtdf(s3top$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Primary Topics Distribution (IPV Interventions Research)", align = c("l", "r"))

lvls3.tp <- paste0(seq(1:length(unique(s3top$clab))), " = ", levels(s3top$clab))
levels(s3top$clab) <- seq(1:length(unique(s3top$clab)))
ks3tp <- ftable(s3top$bibkey, s3top$clab) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3tp <- ifelse(ks3tp >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3tp) <- paste0("@", rownames(ks3tp))
#'
#' \newpage
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(ks3tp[, 1:9], caption = "Primary Topics by Study (IPV Interventions Research [1/2])")
pander(lvls3.tp[1:9])
#'
#' \newpage
#'
#+ echo=FALSE
kable(ks3tp[, 10:ncol(ks3tp)], caption = "Primary Topics by Study (IPV Interventions Research [2/2])")
pander(lvls3.tp[10:length(lvls3.tp)])
#'
#' \newpage
#' ## Target Populations/Sampling Frames
#'
#+ pop_s3
## s3cb - POPULATIONS ================
s3pop <- s3cb[s3cb$cat == "POPULATION", ] %>% droplevels()
s3pop <- s3pop[!duplicated(s3pop), ]
# x <- s3pop[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s3pop$clab, s3pop$jrnl)
Rtdf(s3pop$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Populations Included (IPV Interventions Research)", align = c("l", "r"))

lvla3.pop <- paste0(seq(1:length(unique(s3pop$clab))), " = ", levels(s3pop$clab))
levels(s3pop$clab) <- seq(1:length(unique(s3pop$clab)))
ks3pop <- ftable(s3pop$bibkey, s3pop$clab) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3pop <- ifelse(ks3pop >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3pop) <- paste0("@", rownames(ks3pop))
kable(ks3pop, caption = "Populations Included by Study (IPV Interventions Research)")
pander(lvla3.pop)
#'
#'
#' \newpage
#'
#' ## Sampling Settings
#'
#'
#+ setLvls_s3
s3set <- s3cb[s3cb$cat == "M-SETTINGS", ] %>% droplevels()
s3set <- s3set[!duplicated(s3set), ]
# x <- s3set[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4ql$clab, s4ql$jrnl)
Rtdf(s3set$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Sampling Settings (IPV Interventions Research)", align = c("l", "r"))

lvla3.set <- paste0(seq(1:length(unique(s3set$clab))), " = ", levels(s3set$clab))
levels(s3set$clab) <- seq(1:length(unique(s3set$clab)))
ks3set <- ftable(s3set$bibkey, s3set$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks3set <- ifelse(ks3set >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3set) <- paste0("@", rownames(ks3set))
kable(ks3set[, 1:10], caption = "Sampling Settings by Study (IPV Interventions Research [1/2])")
pander(lvla3.set[1:9])
#'
#' \newpage
#'
#+ echo=FALSE
kable(ks3set[, 11:ncol(ks3set)], caption = "Sampling Settings by Study (IPV Interventions Research [2/2])")
pander(lvla3.set[10:length(lvla3.set)])
#'
#'
#' \newpage
#'
#' ## Sampling Methods
#'
#'
#+ smthds_s3
s3smthds <- s3cb[s3cb$cat == "M-SAMPLING", ] %>% droplevels()
s3smthds <- s3smthds[!duplicated(s3smthds), ]
# x <- s3smthds[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4ql$clab, s4ql$jrnl)
Rtdf(s3smthds$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Ecological Levels of Analysis (IPV Interventions Research)", align = c("l", "r"))

lvla3.smthds <- paste0(seq(1:length(unique(s3smthds$clab))), " = ", levels(s3smthds$clab))
levels(s3smthds$clab) <- seq(1:length(unique(s3smthds$clab)))
ks3smthds <- ftable(s3smthds$bibkey, s3smthds$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks3smthds <- ifelse(ks3smthds >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3smthds) <- paste0("@", rownames(ks3smthds))
kable(ks3smthds, caption = "Sampling Methods by Study (IPV Interventions Research)")
pander(lvla3.smthds)
#'
#'
#' \newpage
#'
#' # Research Designs by Study
#'
#+ designs_s3
## s3cb - DESIGNS ================
s3d <- s3cb[s3cb$cat == "DESIGN", ] %>% droplevels()
s3d <- s3d[!duplicated(s3d), ]
# x <- s3d[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s3d$clab, s3d$jrnl)
Rtdf(s3d$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Research Design (IPV Interventions Research)", align = c("l", "r"))

lvla3.mo <- paste0(seq(1:length(unique(s3d$clab))), " = ", levels(s3d$clab))
levels(s3d$clab) <- seq(1:length(unique(s3d$clab)))
ks3d <- ftable(s3d$bibkey, s3d$clab) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3d <- ifelse(ks3d >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3d) <- paste0("@", rownames(ks3d))
kable(ks3d, caption = "Research Design by Study (IPV Interventions Research)")
pander(lvla3.mo)
#'
#' \newpage
#'
#' ## Overarching Methodology
#'
#+ mthds_s3
## s3cb - METHODS ================
s3mo <- s3cb[s3cb$cat == "METHODS", ] %>% droplevels()
s3mo <- s3mo[!duplicated(s3mo), ]
# x <- s3mo[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s3mo$clab, s3mo$jrnl)
Rtdf(s3mo$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Overarching Methodology (IPV Interventions Research)", align = c("l", "r"))

lvla3.mo <- paste0(seq(1:length(unique(s3mo$clab))), " = ", levels(s3mo$clab))
levels(s3mo$clab) <- seq(1:length(unique(s3mo$clab)))
ks3mo <- ftable(s3mo$bibkey, s3mo$clab) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3mo <- ifelse(ks3mo >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3mo) <- paste0("@", rownames(ks3mo))
kable(ks3mo, caption = "Methodology by Study (IPV Interventions Research)")
pander(lvla3.mo)
#'
#' \newpage
#'
#' ## Qua**L**itative Methods
#'
#+ QL_s3
## s3cb - QUAL ================

s3ql <- s3cb[s3cb$cat == "M-QL", ] %>% droplevels()
s3ql <- s3ql[!duplicated(s3ql), ]
# x <- s3ql[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s3ql$clab, s3ql$jrnl)
Rtdf(s3ql$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Qua**L**itative Methods (IPV Interventions Research)", align = c("l", "r"))

lvla3.ql <- paste0(seq(1:length(unique(s3ql$clab))), " = ", levels(s3ql$clab))
levels(s3ql$clab) <- seq(1:length(unique(s3ql$clab)))
ks3ql <- ftable(s3ql$bibkey, s3ql$clab) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3ql <- ifelse(ks3ql >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3ql) <- paste0("@", rownames(ks3ql))
kable(ks3ql, caption = "Qua**L**itative Methods by Study (IPV Interventions Research)")
pander(lvla3.ql)
#'
#' ## Qua**L**itative Analytic Appraoches (IPV Interventions)
#'
#+ qlAnalytics_s3
## s3cb - QUAL ================
s3aql <- s3cb[s3cb$cat == "A-QL", ] %>% droplevels()
# s3aql <- s3ql[!duplicated(s3aql), ]
# x <- s3aql[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s3aql$clab, s3aql$jrnl)
Rtdf(s3aql$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Qua**L**itative Methods (IPV Interventions Research)", align = c("l", "r"))

lvla3.ql <- paste0(seq(1:length(unique(s3aql$clab))), " = ", levels(s3aql$code))
levels(s3aql$clab) <- seq(1:length(unique(s3aql$clab)))
ks3aql <- ftable(s3aql$bibkey, s3aql$clab) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3aql <- ifelse(ks3aql >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3aql) <- paste0("@", rownames(ks3aql))
kable(ks3aql, caption = "Qua**L**itative Analytic Approaches by Study (IPV Interventions Research)")
pander(lvla3.ql)
#'
#' \newpage
#'
#' ## Qua**NT**itative Methods
#'
#+ qtMethods_s3
## s3cb - QUANT ================
s3qt <- s3cb[s3cb$cat == "M-QT", ] %>% droplevels()
s3qt <- s3ql[!duplicated(s3qt), ]
# x <- s3qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s3qt$clab, s3qt$jrnl)
Rtdf(s3qt$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Qua**NT**itative Methods (IPV Interventions Research)", align = c("l", "r"))

lvla3.qt <- paste0(seq(1:length(unique(s3qt$clab))), " = ", levels(s3qt$clab))
levels(s3qt$clab) <- seq(1:length(unique(s3qt$clab)))
ks3qt <- ftable(s3qt$bibkey, s3qt$clab) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3qt <- ifelse(ks3qt >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3qt) <- paste0("@", rownames(ks3qt))
kable(ks3qt, caption = "Qua**NT**itative Methods by Study (IPV Interventions Research)")
pander(lvla3.qt)
#'
#' ## QuaNTtitative Analytic Approaches
#'
#+ AQT_s3
## s3cb - QUANT - ANALYSIS ================
s3aqt <- s3cb[s3cb$cat == "A-QT", ] %>% droplevels()
# s3aqt <- s3ql[!duplicated(s3aqt), ]
# x <- s3aqt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s3aqt$clab, s3aqt$jrnl)
Rtdf(s3aqt$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Qua**NT**itative Methods (IPV Interventions Research)", align = c("l", "r"))

lvls3.aqt <- paste0(seq(1:length(unique(s3aqt$clab))), " = ", as.character(levels(s3aqt$clab)))
levels(s3aqt$clab) <- seq(1:length(unique(s3aqt$clab)))
ks3aqt <- ftable(s3aqt$bibkey, s3aqt$clab) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3aqt <- ifelse(ks3aqt >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3aqt) <- paste0("@", rownames(ks3aqt))
kable(ks3aqt, caption = "Qua**NT**itative Methods by Study (IPV Interventions Research)")
pander(lvls3.aqt)
#'
#' \newpage
#' ## Mixed-Methods
#'
#+ mmr_s3
## s3cb - MIXED-MTHDS ================
s3mm <- s3cb[s3cb$cat == "M-MM", ] %>% droplevels()
s3mm <- s3mm[!duplicated(s3mm), ]
# x <- s3mm[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s3mm$clab, s3mm$jrnl)
Rtdf(s3mm$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Mixed-Methods (IPV Interventions Research)", align = c("l", "r"))

lvls3.mm <- paste0(seq(1:length(unique(s3mm$clab))), " = ", levels(s3mm$clab))
levels(s3mm$clab) <- seq(1:length(unique(s3mm$clab)))
ks3mm <- ftable(s3mm$bibkey, s3mm$clab) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3mm <- ifelse(ks3mm >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3mm) <- paste0("@", rownames(ks3mm))
kable(ks3mm, caption = "Mixed-Methods by Study (IPV Interventions Research)")
pander(lvls3.mm)
#'
#'
#' \newpage
#'
#' ## Ecological Levels of Analysis
#'
#'
#+ ecoLvls_s3
s3eco <- s3cb[s3cb$cat == "ECO", ] %>% droplevels()
s3eco <- s3eco[!duplicated(s3eco), ]
# x <- s3eco[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4ql$clab, s4ql$jrnl)
Rtdf(s3eco$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Ecological Levels of Analysis (IPV Interventions Research)", align = c("l", "r"))

lvls3.eco <- paste0(seq(1:length(unique(s3eco$clab))), " = ", levels(s3eco$clab))
levels(s3eco$clab) <- seq(1:length(unique(s3eco$clab)))
ks3eco <- ftable(s3eco$bibkey, s3eco$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks3eco <- ifelse(ks3eco >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3eco) <- paste0("@", rownames(ks3eco))
kable(ks3eco, caption = "Levels of Analysis by Study (IPV Interventions Research)")
pander(lvls3.eco)
#'
#' \newpage
#'
#' # \textsc{SMW-Inclusive IPV Research}
#'
#' \Frule
#'
#+ s4cb
# s4cb ----------------
s4cb <- cb[cb$scat == 2, ] %>% droplevels
s4cb.keys <- paste0("@", levels(s4cb$bibkey))
# s4cb.keys %>% as.list() %>% pander
#'
#+ tl_smw, fig.fullwidth=TRUE, fig.height=2, out.width='\\linewidth', fig.show='asis', figPath=TRUE
smw <- MAPtl[MAPtl$scat == "S4", ]

tl.smw <- smw[order(smw$year), c("bibkey", "year", "cpv", "journal", "title")] %>% droplevels()

tl.smw$bibkey <- paste0("@", tl.smw$bibkey)
tl.smw$journal <- paste0("_", tl.smw$journal, "_")
tl.smw <- dplyr::rename(tl.smw, "Study" = bibkey, "Journal" = journal, "Year Published" = year)
rownames(tl.smw) <- NULL

# psmw <- pal_sci[1:length(unique(smw$journal))]
# smw$pos <- rep_len(c(1, -1), length(smw$pos))


smw <- smw[order(smw$yrv), , drop = FALSE] %>% within({
    posv <- sequence(rle(sort(yrv))$lengths)
    posv <- ifelse(yrv == 0, 0, posv)
    posv <- log(posv + 0.5) * -1
})

smw <- smw[order(smw$yrcp), , drop = FALSE] %>% within({
    poscp <- sequence(rle(sort(yrcp))$lengths)
    pos <- ifelse(yrcp == 0, posv, log(poscp - 0.5) * -0.5)
})
# GGPLOT - smwtl ----------------

gg.smwtl <- ggplot(smw, aes(x = year, y = 0, colour = cpv)) +
    thm_Rtft(yticks = FALSE, ytext = FALSE, ytitle = FALSE, ltitle = TRUE,
             ptitle = TRUE, xtext = FALSE, xticks = FALSE) +
    theme(legend.text = element_text(size = rel(0.55)),
          legend.title = element_text(size = rel(0.65), face = "bold"),
          legend.justification = c(1, 0.635),
          legend.box.spacing = unit(0, "cm")) +
    # plot.margin = unit(c(1, rep(0.15, 3)), "cm")) +
    ylim(min(smw$pos) - 0.5, max(smw$pos) + 0.5) +
    labs(colour = "Journal Category", title = "SMW-Inclusive IPV Research Timeline\n") +
    scale_colour_manual(values = pcpv) + #, guide = FALSE) +
    geom_hline(yintercept = 0, size = 0.25, color = pal_nord$polar[7], alpha = 0.5) +
    geom_segment(aes(y = 0, yend = pos, x = year, xend = year),
                 colour = pal_my[19], alpha = 0.55,
                 na.rm = TRUE, size = 0.2, linetype = 3) +
    geom_text(aes(y = pos, x = year, label = bibkey2), hjust = 0.5, vjust = 0,
              angle = 45, size = 2.5, fontface = "bold") + #, nudge_y = -0.05) +
    geom_text(aes(y = 0, x = year, label = year), check_overlap = TRUE,
              vjust = 0.5, hjust = 0.5, angle = 0, colour = pal_my[20],
              size = 2.5, family = "serif", fontface = "bold") +
    xlim(min(smw$year) - 0.25, max(smw$year))
gg.smwtl

tl.smw[, c(1, 4)] %>% kable(caption = "SMW-Inclusive Research Timeline")


#'
#' \newpage
#'
#' ## Research Topics
#'
#+ topics_s4
## s4cb - TOPICS ================
s4top <- s4cb[s4cb$cat == "TOPIC", ] %>% droplevels()
s4top <- s4top[!duplicated(s4top), ]
# x <- s4top[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4top$clab, s4top$jrnl)
Rtdf(s4top$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Primary Topics (SMW-Inclusive Research)",
          align = c("l", "r"))

lvls4.tp <- paste0(seq(1:length(unique(s4top$clab))), " = ", levels(s4top$clab))
levels(s4top$clab) <- seq(1:length(unique(s4top$clab)))
ks4tp <- ftable(s4top$bibkey, s4top$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4tp <- ifelse(ks4tp >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4tp) <- paste0("@", rownames(ks4tp))

#'
#+ echo=FALSE
kable(ks4tp, caption = "Primary Topics by Study (SMW-Inclusive Research)")
pander(lvls4.tp)
#'
#' \newpage
#' ## Target Populations/Sampling Frames
#'
#+ pop_s4
## s4cb - POPULATIONS ================
s4pop <- s4cb[s4cb$cat == "POPULATION", ] %>% droplevels()
s4pop <- s4pop[!duplicated(s4pop), ]
# x <- s4pop[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4pop$clab, s4pop$jrnl)
Rtdf(s4pop$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Populations Included (SMW-Inclusive Research)",
          align = c("l", "r"))

lvla4.pop <- paste0(seq(1:length(unique(s4pop$clab))), " = ", levels(s4pop$clab))
levels(s4pop$clab) <- seq(1:length(unique(s4pop$clab)))
ks4pop <- ftable(s4pop$bibkey, s4pop$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4pop <- ifelse(ks4pop >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4pop) <- paste0("@", rownames(ks4pop))

#'
#+ echo=FALSE
kable(ks4pop, caption = "Populations Included by Study (SMW-Inclusive Research)")
pander(lvla4.pop)
#'
#'
#' \newpage
#'
#' ## Sampling Settings
#'
#'
#+ setLvls_s4
s4set <- s4cb[s4cb$cat == "M-SETTINGS", ] %>% droplevels()
s4set <- s4set[!duplicated(s4set), ]
# x <- s4set[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4ql$clab, s4ql$jrnl)
Rtdf(s4set$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Sampling Settings (SMW-Inclusive Research)",
          align = c("l", "r"))

lvla4.set <- paste0(seq(1:length(unique(s4set$clab))), " = ", levels(s4set$clab))
levels(s4set$clab) <- seq(1:length(unique(s4set$clab)))
ks4set <- ftable(s4set$bibkey, s4set$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4set <- ifelse(ks4set >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4set) <- paste0("@", rownames(ks4set))

#'
#+ echo=FALSE
kable(ks4set, caption = "Sampling Settings by Study (SMW-Inclusive IPV Research)")
pander(lvla4.set)
#'
#'
#' \newpage
#'
#' ## Sampling Methods
#'
#'
#+ smthdsLvls_s4
s4smthds <- s4cb[s4cb$cat == "M-SAMPLING", ] %>% droplevels()
s4smthds <- s4smthds[!duplicated(s4smthds), ]
# x <- s4smthds[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4ql$clab, s4ql$jrnl)
Rtdf(s4smthds$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Sampling Methods (SMW-Inclusive Research)",
          align = c("l", "r"))

lvla4.smthds <- paste0(seq(1:length(unique(s4smthds$clab))), " = ", levels(s4smthds$clab))
levels(s4smthds$clab) <- seq(1:length(unique(s4smthds$clab)))
ks4smthds <- ftable(s4smthds$bibkey, s4smthds$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4smthds <- ifelse(ks4smthds >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4smthds) <- paste0("@", rownames(ks4smthds))

#'
#+ echo=FALSE
kable(ks4smthds, caption = "Sampling Methods by Study (SMW-Inclusve Research)")
pander(lvla4.smthds)
#'
#' \newpage
#'
#' # Research Designs by Study (SMW-Inclusvive Research)
#'
#+ designs_s4
## s4cb - DESIGNS ================
s4d <- s4cb[s4cb$cat == "DESIGN", ] %>% droplevels()
s4d <- s4d[!duplicated(s4d), ]
# x <- s4d[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4d$clab, s4d$jrnl)
Rtdf(s4d$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Research Design (SMW-Inclusive Research)", align = c("l", "r"))

lvla3.mo <- paste0(seq(1:length(unique(s4d$clab))), " = ", levels(s4d$clab))
levels(s4d$clab) <- seq(1:length(unique(s4d$clab)))
ks4d <- ftable(s4d$bibkey, s4d$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4d <- ifelse(ks4d >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4d) <- paste0("@", rownames(ks4d))
kable(ks4d, caption = "Research Design by Study (SMW-Inclusive Research)")
pander(lvla3.mo)
#'
#' \newpage
#'
#' ## Overarching Methodology
#'
#+ mthds_s4
## s4cb - METHODS ================
s4mo <- s4cb[s4cb$cat == "METHODS", ] %>% droplevels()
s4mo <- s4mo[!duplicated(s4mo), ]
# x <- s4mo[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4mo$clab, s4mo$jrnl)

Rtdf(s4mo$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Overarching Methodology (SMW-Inclusive Research)",
          align = c("l", "r"))
lvla4.mo <- paste0(seq(1:length(unique(s4mo$clab))), " = ", levels(s4mo$clab))
levels(s4mo$clab) <- seq(1:length(unique(s4mo$clab)))
ks4mo <- ftable(s4mo$bibkey, s4mo$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4mo <- ifelse(ks4mo >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4mo) <- paste0("@", rownames(ks4mo))
#'
#+ echo=FALSE
kable(ks4mo, caption = "Methodology by Study (SMW-Inclusive Research)")
pander(lvla4.mo)
#'
#' \newpage
#' <!-- ## qual Methods -->
#'
#+ QL_s4
## s4cb - QUAL ================
s4ql <- s4cb[s4cb$cat == "M-QL", ] %>% droplevels()
s4ql <- s4ql[!duplicated(s4ql), ]
# x <- s4ql[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4ql$clab, s4ql$jrnl)
Rtdf(s4ql$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Qua**L**itative Methods (SMW-Inclusive Research)", align = c("l", "r"))

lvla4.ql <- paste0(seq(1:length(unique(s4ql$clab))), " = ", levels(s4ql$clab))
levels(s4ql$clab) <- seq(1:length(unique(s4ql$clab)))
ks4ql <- ftable(s4ql$bibkey, s4ql$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4ql <- ifelse(ks4ql >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4ql) <- paste0("@", rownames(ks4ql))
#'
#+ echo=FALSE
kable(ks4ql, caption = "Qua**L**itative Methods by Study (SMW-Inclusive Research)")
pander(lvla4.ql)

#'
#' ## Qua**NT**itative Analytic Appraoches
#'
#+ AQL_s4
## s4cb - QUAL ================
s4aqt <- s4cb[s4cb$cat == "A-QT", ] %>% droplevels()
# s4aqt <- s4ql[!duplicated(s4aqt), ]
# x <- s4aqt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4aqt$clab, s4aqt$jrnl)
Rtdf(s4aqt$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Qua**NT**itative Methods (IPV Interventions Research)", align = c("l", "r"))

lvla4.qt <- paste0(seq(1:length(unique(s4aqt$clab))), " = ", levels(s4aqt$clab))
levels(s4aqt$clab) <- seq(1:length(unique(s4aqt$clab)))
ks4aqt <- ftable(s4aqt$bibkey, s4aqt$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4aqt <- ifelse(ks4aqt >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4aqt) <- paste0("@", rownames(ks4aqt))
#'
#+ echo=FALSE
kable(ks4aqt, caption = "Qua**NT**itative Analytic Approaches by Study (IPV Interventions Research)")
pander(lvla4.qt)
#'
#'  \newpage
#' ## QuaNTitative Methods
#'
#+ QT_s4
## s4cb - QUANT ================
s4qt <- s4cb[s4cb$cat == "M-QT", ] %>% droplevels()
s4qt <- s4qt[!duplicated(s4qt), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4qt$clab, s4qt$jrnl)
Rtdf(s4qt$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Qua**NT**itative Methods (SMW-Inclusive Research)", align = c("l", "r"))

lvla4.qt <- paste0(seq(1:length(unique(s4qt$clab))), " = ", levels(s4qt$clab))
levels(s4qt$clab) <- seq(1:length(unique(s4qt$clab)))
ks4qt <- ftable(s4qt$bibkey, s4qt$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4qt <- ifelse(ks4qt >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4qt) <- paste0("@", rownames(ks4qt))
kable(ks4qt, caption = "Qua**NT**itative Methods by Study (SMW-Inclusive Research)")
pander(lvla4.qt)
#'
#' ## Quantitative Analytic Appraoches
#'
#+ AQT_s4
## s4cb - QUANT ================
s4aqt <- s4cb[s4cb$cat == "A-QT", ] %>% droplevels()
# s4aqt <- s4ql[!duplicated(s4aqt), ]
# x <- s4aqt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4aqt$clab, s4aqt$jrnl)
Rtdf(s4aqt$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Qua**NT**itative Methods (IPV Interventions Research)", align = c("l", "r"))

lvla4.qt <- paste0(seq(1:length(unique(s4aqt$clab))), " = ", levels(s4aqt$code))
levels(s4aqt$clab) <- seq(1:length(unique(s4aqt$clab)))
ks4aqt <- ftable(s4aqt$bibkey, s4aqt$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4aqt <- ifelse(ks4aqt >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4aqt) <- paste0("@", rownames(ks4aqt))
kable(ks4aqt, caption = "Qua**NT**itative Analytic Approaches by Study (IPV Interventions Research)")
pander(lvla4.qt)
#'
#' \newpage
#' ## Mixed-Methods
#'
#+ mmr_s4
## s4cb - MIXED-MTHDS ================
s4mm <- s4cb[s4cb$cat == "M-MM", ] %>% droplevels()
s4mm <- s4mm[!duplicated(s4mm), ]
# s4ql <- s4ql[!duplicated(s4ql), ]
# x <- s4ql[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4mm$clab, s4mm$jrnl)
Rtdf(s4mm$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Mixed-Methods (SMW-Inclusive Research)", align = c("l", "r"))

lvla4.mm <- paste0(seq(1:length(unique(s4mm$clab))), " = ", levels(s4mm$clab))
levels(s4mm$clab) <- seq(1:length(unique(s4mm$clab)))
ks4mm <- ftable(s4mm$bibkey, s4mm$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4mm <- ifelse(ks4mm >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4mm) <- paste0("@", rownames(ks4mm))
kable(ks4mm, caption = "Mixed-Methods by Study (SMW-Inclusive Research)")
pander(lvla4.mm)
#'
#'
#' \newpage
#'
#' ## Ecological Levels of Analysis
#'
#'
#+ ecoLvls_s4
s4eco <- s4cb[s4cb$cat == "ECO", ] %>% droplevels()
s4eco <- s4eco[!duplicated(s4eco), ]
# x <- s4eco[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4ql$clab, s4ql$jrnl)
Rtdf(s4eco$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Ecological Levels of Analysis (SMW-Inclusive Research)", align = c("l", "r"))

lvla4.eco <- paste0(seq(1:length(unique(s4eco$clab))), " = ", levels(s4eco$clab))
levels(s4eco$clab) <- seq(1:length(unique(s4eco$clab)))
ks4eco <- ftable(s4eco$bibkey, s4eco$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4eco <- ifelse(ks4eco >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4eco) <- paste0("@", rownames(ks4eco))
kable(ks4eco, caption = "Levels of Analysis by Study (SMW-Inclusive Research)")
pander(lvla4.eco)

par(pmar)
#'
#' \newpage\onehalfspacing
#'
#' # References`r Rcite_r(file = "../auxREFs.bib", footnote = TRUE)`
#'
#' \refs
#'
