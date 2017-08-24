#' ---
#' title: "Appendix C: Results from Qualitative Comparative Analyses"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
# SETUP ----------------
source("../SETUP.R") ## SEE EccRiley.github.io/Rrscs/SETUP.R ##
options(width = 60)
knitr::opts_chunk$set(
    tidy = TRUE,
    echo = TRUE,
    fig.keep = 'high',
    fig.show = 'asis',
    results = 'asis',
    tidy.opts = list(comment = FALSE, blank = FALSE),
    # echoRule = NULL,
    # echoRuleb = NULL,
    # fig.height = 5,
    fig.path = "graphics/bibs/rplot-",
    fignos = TRUE,
    dev = c('png', 'svg'),
    fig.retina = 6,
    Rplot = TRUE,
    Rplot_whbg = NULL,
    hideCap = FALSE,
    figPath = FALSE,
    fpath = NULL)

op <- par(no.readonly = TRUE) ## current values for settable plotting params (see "?par") ##
pmar <- par(mar = c(5, 4, 4, 2))
sci <- colorRampPalette(pal_sci[c(1, 2, 4, 5, 8)])
# panderOptions("table.emphasize.rownames", "FALSE")
# rpm()
knitr::opts_template$set(invisible = list(echo = FALSE, results = 'hide', message = FALSE, warning = FALSE, cache = FALSE, fig.keep = 'none', fig.show = 'none'))
hideLabs <- function(x, labs) {
    if (knitr::opts_chunk$get("hideCap") == FALSE) {
        x + labs
    } else {
        x
    }
}
## FOR DOCX/HTML OUTPUT ##
knit_hooks$set(plot = function(x, options) {
    if (!is.null(options$fig.lab)) {
    paste('![', options$fig.cap, '](',
          opts_knit$get('base.url'), paste(x, collapse = '.'),
          '){#fig:', options$fig.lab, "}",
          sep = '')
        }
})
#'
#' -----
#'
#' # Systematic Database Search
#'
#' Six separate literature searches were conducted using the [_PsycINFO_](http://www.apa.org/pubs/databases/psycinfo/) and [_Web of Science_](http://wokinfo.com) online citation indexing databases via the [Portland State University library website](library.pdx.edu)^[Note that (1) "intimate partner violence" included "domestic violence" and "partner abuse", (2) "same-sex" included "same-gender", and (3) the results ranges provided after each search description listed reflect the minimum and maximum number of results returned across the two databases searched.]:
#'
#'
#' 1. _**IPV - General**_
#' 2. --- _IPV Interventions_
#' 3. --- _IPV Intervention Evaluations_
#' 4. _**Female Same-Sex/Same-Gender IPV (FSSIPV) - General**_
#' 5. --- _FSSIPV Interventions_
#' 6. --- _FSSIPV Intervention Evaluations_
#'
#+ src_dbsrch, opts.label='invisible'
# dbsrch ----------------
source("dbsrch.R")
#'
#+ dbsrch, results='asis',echo=FALSE
# pander(dbsrch, justify = c("right", "left", "centre"), caption = "Descriptions of database searches conducted with corresponding ranges of the number of results returned")
library(kableExtra)
kable(dbsrch, caption = "Descriptions of database searches conducted with corresponding ranges of the number of results returned", justify = c("r", "l", "l")) %>%
    add_footnote(c("Note: For each database search, multiple search terms were included for the subject/keywords parameters to represent intimate partner violence", "'PI' = PsycINFO; 'WoS' = Web of Science", threeparttable = TRUE))
#'
#' # Community Psychology Publications & Closely-Related Violence-Specific Publications
#'
#+ src_journals, opts.label='invisible'
# JOURNALS ----------------
source("journals.R")
#'
#' **Community-psychology journals.** included in database searches:
#'
#+ journals, results='asis', echo=FALSE
j.cpp %>% as.list() %>% pander()
#'
#' **Violence-specific journals.** selected for inclusion in database searches.
#'
#+ journals2, results='asis', echo=FALSE
j.vp %>% as.list() %>% pander()
#'
#' # Results of Systematic Database Searches:
#'
#+ bibdf
### FUN - 'Rbibkeys()' ####
Rbibkeys <- function(bib) {
    keys <- bib[grep("@.*?\\{.*?,", bib, perl = TRUE)]
    keys <- gsub("@\\w+\\{(.*?)", "\\1", keys, perl = TRUE)
    keys <- keys[!grepl("%.*?,", keys, perl = TRUE)]
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
panderOptions("p.copula", ", & ")
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
#'
#'
#' # Systematically-Reviewed Literature
#'
#'
#'
#' **The resulting selection of empirical literature.**, representing a community-psychology-focused subset of the U.S.-based IPV-related literature, was reviewed using a `primarily deductive` _qualitative comparative analytic approach_ [_QCA_; @leech2007array; @onwuegbuzie2017framework]. This approach was conducted as part of an initial data reduction and organization process in which the reviewed literature was categorized according to the commonalities in overarching research topics, target populations, sampling frames, sampling and data collection methodologies, and data analytic approaches. In addition, the QCA approach served as a systematic method for examining the similarities, differences, and anomalies within the groups identified in the initial data reduction and organization process [@onwuegbuzie2017framework; @onwuegbuzie2009qualitative]. The qualitative comparative analysis of the reviewed literature was aided by the _`RQDA`_ package created for use with the _`R` Statistical Programming Language and Environment_ [@R-RQDA; @R-base].
#'
#+ cb
# CTBL/CB ----------------
rec.key2scat <- paste0("\"", MAP$bibkey, "\" = \"", MAP$scat, "\"", collapse = "; ")
cb <- merge(MAP, ctbl, by = c("caseid", "scat"))
cb <- within(cb, {
    # journal <- droplevels(journal)
    jrnl <- sapply(as.character(journal), Rabbr)
    code <- gsub("FG-\\w+", "FG", code)
    # code <- gsub("EXP-\w+", "EXP", code)
    # code <- gsub("LT-\w+", "LT", code)
    code <- gsub("SVY-QL-MM", "SVY-QL", code)
    code <- gsub("SVY-QT-MM", "SVY-QT", code)
    # code <- gsub("XS-\w+", "XS", code)
    code <- gsub("IVW-\\w+", "IVW", code)
    code <- gsub("SMIN-\\w+", NA, code)
    code <- gsub("HET", NA, code)
    scat <- factor(scat, labels = c("IPV Interventions", "SMW-Specific Research"))
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
#'
#'
#' # Codebook
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
#'
#'
#+ echo=FALSE
kable(cdbk.ft, col.names = c("**Information Category**", "Codes"), caption = "Codebook Constructed from the Discrete Summative Data Compiled Across the Formally Reviewed Literature", align = c("r", "l"))
#'
#'
#'
#' # General Research Categories
#'
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
    scat <- ifelse(scat == "S3", "IPV Interventions", "SMW-Specific Research")
})
t.scat <- Rtdf(ct.scat$scat, names = c("Category", "N"))
## "ct.scat" created in "MAPrqda.R" ##
t.scat %>% kable(caption = "Count of Articles in Each Research Category")
scat.t <- table(ct.scat$scat)
scat.bn <- Rbinom(scat.t)
scat.bn %>% kable(caption = "Binomial Test of the Difference in Search Category Proportions", col.names = c("Alternative", "Null Value ($\\pi_{0}$)", "Parameter", "Estimate", "$\\chi^{2}$", "$p$-value", "CI"), escape = FALSE)
N.MAP <- nrow(MAP)
options(Rperc_n = N.MAP)
t.scat$prop = t.scat$N / N.MAP
t.scat <- t.scat[order(t.scat$prop), ]
t.scat$ymax <- cumsum(t.scat$prop)
t.scat$ymin <- c(0, head(t.scat$ymax, n = -1))
t.scat$perc <- paste0((round(t.scat$prop, digits = 1)*100), "%")
# t.scat$labs <- paste("n = ", t.scat$N, " (", t.scat$perc, ")", sep = "")
#'
#+ donut_scat, fig.cap="Proportions of reviewed articles in each of the two overarching research categories: IPV interventions research, and SMW-Specific IPV research", fig.lab="donut_scat", fig.fullwidth=TRUE
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
    annotate("text", x = 0, y = 0,
             label = "Category Proportions",
             family = "serif", size = 5,
             fontface = "italic") +
    geom_text(aes(x = 3.5, y = c(0.1, 0.60),
                  label = t.scat$perc, #angle = c(-35, -35),
                  fontface = "bold"),
              colour = pal_my[2], size = 5.5,
              family = "serif", vjust = 0.3) +
    # labs(title = "", fill = "Research Category") +
    thm_Rtft(ytitle = FALSE, xtitle = FALSE) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = rel(1)),
          legend.key.width = unit(0.5, "cm"),
          legend.key.height = unit(0.5, "cm"))
scat.p
#'
#'
#'
#'
#' # Publication Titles
#'
#'
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
# cpv.bn1 %>% pander(caption = "Binomial Test of $N_{articles}$ per Journal Category (Violence vs. Community Psychology ($\pi_{0} = 0.5$).")
# cpv.bn2 %>% pander(caption = paste0("Binomial Test of $N_{articles}$ per Journal Category (Violence vs. Community Psychology [$\pi_{0} = ", round(pr.jv, 3), "$ (based on proportion of $N_{journals}$ per journal category included in database searches; $n_{journals_{V}} = ", length(j.v), "$; $n_{journals_{CP}} = ", length(j.cp), "$)]."))
#'
#'
#'
#' # Research Category by Journal & Journal Category
#'
#+ dot_scatXjournal
### PLOT - scat-x-journal - dotchart ####
ftm.j <- Rna(ft.jrnl)
sum.j <- apply(ftm.j, 1, sum)
ftm.j <- ifelse(ftm.j == 0, NA, ftm.j)
ftm.jp <- cbind(ft.jrnl, "**Total**" = paste0("**", sum.j, "**"))
ftm.jp %>% kable(align = rep("r", ncol(ftm.jp)),
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
                            "SMW-Specific Research")
pj <- mpal(seq_along(unique(MAP$jrnl)), p = ppal)
library(ggparallel)
pscat <- c("#a6afbb", pal_my[17])
pscat.a <- adjustcolor(pscat, alpha.f = 0.5)
al.j <- c(rep(90, length(unique(MAP.jrnl$Journal))),
          rep(0, length(unique(MAP.jrnl$Category))))
t.jrnl$log <- log(t.jrnl[, 2]) + 3
lj <- c(t.jrnl[, 3], rep(3.50, length(unique(MAP.jrnl$Category))))
parset.jrnl <- ggparset2(list("Journal", "Category"),
                         data = MAP.jrnl,
                         method = "adj.angle", label = TRUE,
                         label.size = lj, text.angle = al.j, order = c(0, 0)) +
    scale_fill_manual(values = c(pscat.a, adjustcolor(pj, alpha.f = 0.55)),
                      guide = FALSE) +
    scale_colour_manual(values = c(pscat, pj), guide = FALSE) +
    coord_flip() +
    thm_Rtft(ticks = FALSE, ytext = TRUE, xtext = FALSE) #+ scale_y_discrete(labels = c("Category" = "Category", "J" = "Journal"))
hideLabs(x = parset.jrnl, labs = labs(subtitle = "Journals"))
MAP.jrnl <- MAP[, c("scat", "journal", "cpv", "jrnl")]
pcpv <- pp[c(11, 30)]
clr.cpv1 <- levels(factor(MAP.jrnl$journal))
clr.cpv <- ifelse(clr.cpv1 %in% j.cpp, pcpv[1], pcpv[2])
MAP.jrnl$cpv <- ifelse(MAP.jrnl$cpv == "CP", "Community Psychology",
                       "Violence-Specific")
MAP.jrnl$scat <- ifelse(MAP.jrnl$scat == "S3", "IPV Interventions",
                        "SMW-Specific Research")
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
hideLabs(x = parset.jrnl2,
         labs = labs(subtitle = "Journals & Research Disciplines by Research Category"))
#'
#'
#' # Publication Years
#'
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
legend(x = 1990, y = 2.75, legend = c("SMW-Specific Research", "IPV Interventions Research (general)"), fill = c(pal_sci[8], pal_my[16]), density = 50, angle = c(45, -45), border = NA, bty = 'n', cex = 0.8, text.font = 3, trace = F)
#'
#'
#+ tl_map, fig.fullwidth=TRUE, fig.width=7, out.width='\\linewidth', fig.align='center', figPath=TRUE
# PLOT - MAPtl ----------------
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
#'
#'
#' # Publication Dates & Journals
#'
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
### recode cb$scat & cb$sclab ####
cb$scat2 <- factor(cb$scat, labels = c(1, 2))
cb$clab <- factor(cb$clab)
### s3cb ####
s3cb <- cb[cb$scat2 == 1, ] %>% droplevels
s3cb <- s3cb[!duplicated(s3cb), ]
s3cb.keys <- paste0("@", levels(factor(s3cb$bibkey)))
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
#'
#'
### s4cb ####
s4cb <- cb[cb$scat2 == 2, ] %>% droplevels
s4cb <- s4cb[!duplicated(s4cb), ]
s4cb.keys <- paste0("@", levels(factor(s4cb$bibkey)))
#'
#'
#+ tl_smw, fig.fullwidth=TRUE, fig.height=2.75, out.width='\\linewidth', figPath=TRUE
smw <- MAPtl[MAPtl$scat == "S4", ]
tl.smw <- smw[order(smw$year), c("bibkey", "year", "cpv", "journal", "title"), drop = FALSE] %>% droplevels()
tl.smw$bibkey <- paste0("@", tl.smw$bibkey)
tl.smw$journal <- paste0("_", tl.smw$journal, "_")
tl.smw <- dplyr::rename(tl.smw, "Study" = bibkey, "Journal" = journal, "Year Published" = year)
rownames(tl.smw) <- NULL
smw <- smw[order(smw$yrv), , drop = FALSE] %>% within({
    posv <- sequence(rle(sort(yrv))$lengths)
    posv <- ifelse(yrv == 0, 0, posv)
    posv <- log(posv + 0.75) * -1
})
smw <- smw[order(smw$yrcp), , drop = FALSE] %>% within({
    poscp <- sequence(rle(sort(yrcp))$lengths)
    pos <- ifelse(yrcp == 0, posv, log(poscp - 0.5) * -1)
})
# GGPLOT - smwtl ----------------
gg.smwtl <- ggplot(smw, aes(x = year, y = 0, colour = cpv)) +
    thm_Rtft(yticks = FALSE, ytext = FALSE, ytitle = FALSE, ltitle = TRUE,
             ptitle = TRUE, xtext = FALSE, xticks = FALSE) +
    theme(legend.text = element_text(size = rel(0.55)),
          legend.title = element_text(size = rel(0.65), face = "bold"),
          legend.justification = c(1, 0.8),
          legend.box.spacing = unit(0, "cm")) +
    # plot.margin = unit(c(1, rep(0.15, 3)), "cm")) +
    ylim(min(smw$pos) - 0.5, max(smw$pos) + 0.5) +
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
    xlim(min(smw$year) - 0.25, max(smw$year)) +
    ylim(min(smw$pos) - 0.15, max(smw$pos) + 0.15)
gg.smwtl
tl.smw[, c(1, 4, 2)] %>% kable(caption = "SMW-Specific IPV Research Timeline")
#'
#'
#'
#' # Primary Topics
#'
#+ topics
## topics ================
codes.tp <- cb[cb$cat == "TOPIC", "clab"] %>% droplevels()
ctp.dnn <- c("Topic", "$N_{Articles}$")
scats.tp <- cb[cb$cat == "TOPIC", "scat"] %>% droplevels()
stp.dnn <- c("IPV Interventions", "SMW-Specific Research")
topics <- Rftm(codes.tp, scats.tp, dnn = list(ctp.dnn, stp.dnn))
t.tp <- topics[[1]]
ftm.tp <- topics[[2]]
ftm.tp2 <- Rna(ftm.tp)
sum.tp <- apply(ftm.tp2, 1, sum)
ftm.tpp <- cbind(ftm.tp, "**Total**" = paste0("**", sum.tp, "**"))
ftm.tpp %>% kable(align = rep("r", ncol(ftm.tpp)),
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
#'
#'
topcb <- cb[cb$cat == "TOPIC", ] %>% droplevels()
topcb <- topcb[!duplicated(topcb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
topcb.tly <- tally(group_by(topcb, clab))
lvl.top <- paste0(seq(1:nrow(topcb.tly)), " = ", levels(topcb$clab))
topcb$clab2 <- factor(topcb$clab, labels = seq(1:nrow(topcb.tly)))
# levels(topcb$clab) <- seq(1:length(unique(topcb$clab)))
ktop <- ftable(topcb$bibkey, topcb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ktop <- ifelse(ktop >= 1, "&#10003;", "&middot;")
ktopscat <- car::recode(rownames(ktop), rec.key2scat)
ktopscat <- ifelse(ktopscat == "S4", paste0("@", rownames(ktop), "$^*$"), paste0("@", rownames(ktop)))
rownames(ktop) <- ktopscat
# rownames(ktop) <- paste0("@", rownames(ktop))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(ktop[, 1:11], caption = "Primary Topics by Study (1/2)", col.names = c()) %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.top[1:11])
#'
#'
#'
#+ echo=FALSE
kable(ktop[, 12:ncol(ktop)],
      caption = "Primary Topics by Study (2/2)") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.top[12:length(lvl.top)])
#'
#'
#'
#' # Research Designs
#'
#+ designs
## designs ================
ct.d <- cb[cb$cat == "DESIGN", ] %>% droplevels()
ct.d <- ct.d[!duplicated(ct.d), ]
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
ftm.d <- cbind(ftm.d, "**Total**" = paste0("**", sum.d, "**"))
ftm.d %>% kable(align = rep("r", ncol(ftm.d)),
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
#'
#'
descb <- cb[cb$cat == "DESIGN", ] %>% droplevels()
descb <- descb[!duplicated(descb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
descb.tly <- tally(group_by(descb, clab))
lvl.des <- paste0(seq(1:nrow(descb.tly)), " = ", levels(descb$clab))
descb$clab2 <- factor(descb$clab, labels = seq(1:nrow(descb.tly)))
# levels(descb$clab) <- seq(1:length(unique(descb$clab)))
kdes <- ftable(descb$bibkey, descb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
kdes <- ifelse(kdes >= 1, "&#10003;", "&middot;")
kdesscat <- car::recode(rownames(kdes), rec.key2scat)
kdesscat <- ifelse(kdesscat == "S4", paste0("@", rownames(kdes), "$^*$"), paste0("@", rownames(kdes)))
rownames(kdes) <- kdesscat
# rownames(kdes) <- paste0("@", rownames(kdes))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(kdes, caption = "Primary Research Designs by Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.des)
#'
#'
#'
#' **Experimental Research Designs.**
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
ftm.expp <- cbind(ftm.exp, "**Total**" = paste0("**", sum.exp, "**"))
# rownames(ftm.expp) <- gsub("\n", "", rownames(ftm.expp))
ftm.expp %>% kable(align = rep("r", ncol(ftm.expp)),
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
#'
#'
expcb <- cb[cb$cat == "DESIGN-EXP", ] %>% droplevels()
expcb <- expcb[!duplicated(expcb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
expcb.tly <- tally(group_by(expcb, clab))
lvl.exp <- paste0(seq(1:nrow(expcb.tly)), " = ", levels(expcb$clab))
expcb$clab2 <- factor(expcb$clab, labels = seq(1:nrow(expcb.tly)))
# levels(expcb$clab) <- seq(1:length(unique(expcb$clab)))
kexp <- ftable(expcb$bibkey, expcb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
kexp <- ifelse(kexp >= 1, "&#10003;", "&middot;")
kexpscat <- car::recode(rownames(kexp), rec.key2scat)
kexpscat <- ifelse(kexpscat == "S4", paste0("@", rownames(kexp), "$^*$"), paste0("@", rownames(kexp)))
rownames(kexp) <- kexpscat
# rownames(kexp) <- paste0("@", rownames(kexp))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(kexp, caption = "Experimental Designs by Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.exp)
#'
#'
#'
#' # Data Collection Methodologies
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
ftm.mo <- cbind(ftm.mo, "**Total**" = paste0("**", sum.mo, "**"))
ftm.mo %>% kable(align = rep("r", ncol(ftm.mo)),
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
#'
#'
mocb <- cb[cb$cat == "METHODS", ] %>% droplevels()
mocb <- mocb[!duplicated(mocb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
mocb.tly <- tally(group_by(mocb, clab))
lvl.mo <- paste0(seq(1:nrow(mocb.tly)), " = ", levels(mocb$clab))
mocb$clab2 <- factor(mocb$clab, labels = seq(1:nrow(mocb.tly)))
# levels(mocb$clab) <- seq(1:length(unique(mocb$clab)))
kmo <- ftable(mocb$bibkey, mocb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
kmo <- ifelse(kmo >= 1, "&#10003;", "&middot;")
kmoscat <- car::recode(rownames(kmo), rec.key2scat)
kmoscat <- ifelse(kmoscat == "S4", paste0("@", rownames(kmo), "$^*$"), paste0("@", rownames(kmo)))
rownames(kmo) <- kmoscat
# rownames(kmo) <- paste0("@", rownames(kmo))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(kmo, caption = "Methodology by Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.mo)
#'
#'
#'
#'
#' **QuaLitative Research _Designs_.**
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
ftm.dql <- cbind(ftm.dql, "**Total**" = paste0("**", sum.dql, "**"))
ftm.dql %>% kable(align = rep("r", ncol(ftm.dql)),
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
#'
#'
dqlcb <- cb[cb$cat == "D-QL", ] %>% droplevels()
dqlcb <- dqlcb[!duplicated(dqlcb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
dqlcb.tly <- tally(group_by(dqlcb, clab))
lvl.dql <- paste0(seq(1:nrow(dqlcb.tly)), " = ", levels(dqlcb$clab))
dqlcb$clab2 <- factor(dqlcb$clab, labels = seq(1:nrow(dqlcb.tly)))
# levels(dqlcb$clab) <- seq(1:length(unique(dqlcb$clab)))
kdql <- ftable(dqlcb$bibkey, dqlcb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
kdql <- ifelse(kdql >= 1, "&#10003;", "&middot;")
kdqlscat <- car::recode(rownames(kdql), rec.key2scat)
kdqlscat <- ifelse(kdqlscat == "S4", paste0("@", rownames(kdql), "$^*$"), paste0("@", rownames(kdql)))
rownames(kdql) <- kdqlscat
# rownames(kdql) <- paste0("@", rownames(kdql))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(kdql, caption = "Qualitative Designs by Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.dql)
#'
#'
#' **QuaLitative _Methods_.**
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
ftm.ql <- cbind(ftm.ql, "**Total**" = paste0("**", sum.ql, "**"))
ftm.ql %>% kable(align = rep("r", ncol(ftm.ql)),
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
#'
#'
mqlcb <- cb[cb$cat == "M-QL", ] %>% droplevels()
mqlcb <- mqlcb[!duplicated(mqlcb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
mqlcb.tly <- tally(group_by(mqlcb, clab))
lvl.mql <- paste0(seq(1:nrow(mqlcb.tly)), " = ", levels(mqlcb$clab))
mqlcb$clab2 <- factor(mqlcb$clab, labels = seq(1:nrow(mqlcb.tly)))
# levels(mqlcb$clab) <- seq(1:length(unique(mqlcb$clab)))
kmql <- ftable(mqlcb$bibkey, mqlcb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
kmql <- ifelse(kmql >= 1, "&#10003;", "&middot;")
kmqlscat <- car::recode(rownames(kmql), rec.key2scat)
kmqlscat <- ifelse(kmqlscat == "S4", paste0("@", rownames(kmql), "$^*$"), paste0("@", rownames(kmql)))
rownames(kmql) <- kmqlscat
# rownames(kmql) <- paste0("@", rownames(kmql))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(kmql, caption = "Qua**L**itative Methods by Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.mql)
#'
#'
#'
#' **QuaLitative Data Analytic Approaches.**
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
                                   "SMW-Specific Research"))
sum.aql <- apply(ftm.aql, 1, sum)
ftm.aql <- ifelse(ftm.aql == 0, NA, ftm.aql)
ftm.aqlp <- cbind(ftm.aql, "**Total**" = paste0("**", sum.aql, "**"))
# rownames(ftm.aqlp) <- gsub("\n", "", rownames(ftm.aqlp))
ftm.aqlp %>% kable(align = rep("r", ncol(ftm.aqlp)),
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
#'
aqlcb <- cb[cb$cat == "A-QL", ] %>% droplevels()
aqlcb <- aqlcb[!duplicated(aqlcb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
aqlcb.tly <- tally(group_by(aqlcb, clab))
lvl.aql <- paste0(seq(1:nrow(aqlcb.tly)), " = ", levels(aqlcb$clab))
aqlcb$clab2 <- factor(aqlcb$clab, labels = seq(1:nrow(aqlcb.tly)))
# levels(aqlcb$clab) <- seq(1:length(unique(aqlcb$clab)))
kaql <- ftable(aqlcb$bibkey, aqlcb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
kaql <- ifelse(kaql >= 1, "&#10003;", "&middot;")
kaqlscat <- car::recode(rownames(kaql), rec.key2scat)
kaqlscat <- ifelse(kaqlscat == "S4", paste0("@", rownames(kaql), "$^*$"), paste0("@", rownames(kaql)))
rownames(kaql) <- kaqlscat
# rownames(kaql) <- paste0("@", rownames(kaql))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(kaql, caption = "Qua**L**itative Analytic Approaches by Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.aql)
#'
#'
#'
#'
#' **QuaNTitative Research _Designs_.**
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
ftm.dqt <- cbind(ftm.dqt, "**Total**" = paste0("**", sum.dqt, "**"))
ftm.dqt %>% kable(align = rep("r", ncol(ftm.dqt)),
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
#'
dqtcb <- cb[cb$cat == "D-QT", ] %>% droplevels()
dqtcb <- dqtcb[!duplicated(dqtcb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
dqtcb.tly <- tally(group_by(dqtcb, clab))
lvl.dqt <- paste0(seq(1:nrow(dqtcb.tly)), " = ", levels(dqtcb$clab))
dqtcb$clab2 <- factor(dqtcb$clab, labels = seq(1:nrow(dqtcb.tly)))
# levels(dqtcb$clab) <- seq(1:length(unique(dqtcb$clab)))
kdqt <- ftable(dqtcb$bibkey, dqtcb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
kdqt <- ifelse(kdqt >= 1, "&#10003;", "&middot;")
kdqtscat <- car::recode(rownames(kdqt), rec.key2scat)
kdqtscat <- ifelse(kdqtscat == "S4", paste0("@", rownames(kdqt), "$^*$"), paste0("@", rownames(kdqt)))
rownames(kdqt) <- kdqtscat
# rownames(kdqt) <- paste0("@", rownames(kdqt))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(kdqt, caption = "Qua**NT**itative Designs by Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.dqt)
#'
#'
#'
#' **QuaNTitative _Methods_.**
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
ftm.qt <- cbind(ftm.qt, "**Total**" = paste0("**", sum.qt, "**"))
ftm.qt %>% kable(align = rep("r", ncol(ftm.qt)),
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
#'
#'
mqtcb <- cb[cb$cat == "M-QT", ] %>% droplevels()
mqtcb <- mqtcb[!duplicated(mqtcb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
mqtcb.tly <- tally(group_by(mqtcb, clab))
lvl.mqt <- paste0(seq(1:nrow(mqtcb.tly)), " = ", levels(mqtcb$clab))
mqtcb$clab2 <- factor(mqtcb$clab, labels = seq(1:nrow(mqtcb.tly)))
# levels(mqtcb$clab) <- seq(1:length(unique(mqtcb$clab)))
kmqt <- ftable(mqtcb$bibkey, mqtcb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
kmqt <- ifelse(kmqt >= 1, "&#10003;", "&middot;")
kmqtscat <- car::recode(rownames(kmqt), rec.key2scat)
kmqtscat <- ifelse(kmqtscat == "S4", paste0("@", rownames(kmqt), "$^*$"), paste0("@", rownames(kmqt)))
rownames(kmqt) <- kmqtscat
# rownames(kmqt) <- paste0("@", rownames(kmqt))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(kmqt, caption = "Qua**NT**itative Methods by Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.mqt)
#'
#'
#'
#' **QuaNTitative Data Analytic Approaches.**
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
                                   "SMW-Specific Research"))
sum.aqt <- apply(ftm.aqt, 1, sum)
ftm.aqt <- ifelse(ftm.aqt == 0, NA, ftm.aqt)
ftm.aqtp <- cbind(ftm.aqt, "**Total**" = paste0("**", sum.aqt, "**"))
ftm.aqtp %>% kable(align = rep("r", ncol(ftm.aqtp)),
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
#'
aqtcb <- cb[cb$cat == "A-QT", ] %>% droplevels()
aqtcb <- aqtcb[!duplicated(aqtcb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
aqtcb.tly <- tally(group_by(aqtcb, clab))
lvl.aqt <- paste0(seq(1:nrow(aqtcb.tly)), " = ", levels(aqtcb$clab))
aqtcb$clab2 <- factor(aqtcb$clab, labels = seq(1:nrow(aqtcb.tly)))
# levels(aqtcb$clab) <- seq(1:length(unique(aqtcb$clab)))
kaqt <- ftable(aqtcb$bibkey, aqtcb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
kaqt <- ifelse(kaqt >= 1, "&#10003;", "&middot;")
kaqtscat <- car::recode(rownames(kaqt), rec.key2scat)
kaqtscat <- ifelse(kaqtscat == "S4", paste0("@", rownames(kaqt), "$^*$"), paste0("@", rownames(kaqt)))
rownames(kaqt) <- kaqtscat
# rownames(kaqt) <- paste0("@", rownames(kaqt))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(kaqt, caption = "Qua**NT**itative Analytic Approaches by Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.aqt)
#'
#'
#'
#' **Archival/Secondary Data Sources.**
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
ftm.rcrd <- cbind(ftm.rcrd, "**Total**" = paste0("**", sum.rcrd, "**"))
ftm.rcrd %>% kable(align = rep("r", ncol(ftm.rcrd)),
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
#'
#'
rcrdcb <- cb[cb$cat == "M-RCRD", ] %>% droplevels()
rcrdcb <- rcrdcb[!duplicated(rcrdcb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
rcrdcb.tly <- tally(group_by(rcrdcb, clab))
lvl.rcrd <- paste0(seq(1:nrow(rcrdcb.tly)), " = ", levels(rcrdcb$clab))
rcrdcb$clab2 <- factor(rcrdcb$clab, labels = seq(1:nrow(rcrdcb.tly)))
# levels(rcrdcb$clab) <- seq(1:length(unique(rcrdcb$clab)))
krcrd <- ftable(rcrdcb$bibkey, rcrdcb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
krcrd <- ifelse(krcrd >= 1, "&#10003;", "&middot;")
krcrdscat <- car::recode(rownames(krcrd), rec.key2scat)
krcrdscat <- ifelse(krcrdscat == "S4", paste0("@", rownames(krcrd), "$^*$"), paste0("@", rownames(krcrd)))
rownames(krcrd) <- krcrdscat
# rownames(krcrd) <- paste0("@", rownames(krcrd))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(krcrd, caption = "Archival/Secondary Data Sources by Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.rcrd)
#'
#'
#'
#' **Mixed-Methodological _Designs_.**
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
                                   "SMW-Specific Research"))
# t.dmm
sum.dmm <- apply(ftm.dmm, 1, sum)
ftm.dmm <- ifelse(ftm.dmm == 0, NA, ftm.dmm)
ftm.dmm <- cbind(ftm.dmm, "**Total**" = paste0("**", sum.dmm, "**"))
ftm.dmm %>% kable(align = rep("r", ncol(ftm.dmm)),
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
#'
#'
dmmcb <- cb[cb$cat == "D-MM", ] %>% droplevels()
dmmcb <- dmmcb[!duplicated(dmmcb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
dmmcb.tly <- tally(group_by(dmmcb, clab))
lvl.dmm <- paste0(seq(1:nrow(dmmcb.tly)), " = ", levels(dmmcb$clab))
dmmcb$clab2 <- factor(dmmcb$clab, labels = seq(1:nrow(dmmcb.tly)))
# levels(dmmcb$clab) <- seq(1:length(unique(dmmcb$clab)))
kdmm <- ftable(dmmcb$bibkey, dmmcb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
kdmm <- ifelse(kdmm >= 1, "&#10003;", "&middot;")
kdmmscat <- car::recode(rownames(kdmm), rec.key2scat)
kdmmscat <- ifelse(kdmmscat == "S4", paste0("@", rownames(kdmm), "$^*$"), paste0("@", rownames(kdmm)))
rownames(kdmm) <- kdmmscat
# rownames(kdmm) <- paste0("@", rownames(kdmm))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(kdmm, caption = "Mixed-Methods Designs by Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.dmm)
#'
#'
#' **Mixed (QuaLitative & QuaNTitative) _Methods_.**
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
                                  "SMW-Specific Research"))
# t.mm
sum.mm <- apply(ftm.mm, 1, sum)
ftm.mm <- ifelse(ftm.mm == 0, NA, ftm.mm)
ftm.mm <- cbind(ftm.mm, "**Total**" = paste0("**", sum.mm, "**"))
ftm.mm %>% kable(align = rep("r", ncol(ftm.mm)),
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
#'
#'
mmmcb <- cb[cb$cat == "M-MM", ] %>% droplevels()
mmmcb <- mmmcb[!duplicated(mmmcb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
mmmcb.tly <- tally(group_by(mmmcb, clab))
lvl.mmm <- paste0(seq(1:nrow(mmmcb.tly)), " = ", levels(mmmcb$clab))
mmmcb$clab2 <- factor(mmmcb$clab, labels = seq(1:nrow(mmmcb.tly)))
# levels(mmmcb$clab) <- seq(1:length(unique(mmmcb$clab)))
kmmm <- ftable(mmmcb$bibkey, mmmcb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
kmmm <- ifelse(kmmm >= 1, "&#10003;", "&middot;")
kmmmscat <- car::recode(rownames(kmmm), rec.key2scat)
kmmmscat <- ifelse(kmmmscat == "S4", paste0("@", rownames(kmmm), "$^*$"), paste0("@", rownames(kmmm)))
rownames(kmmm) <- kmmmscat
# rownames(kmmm) <- paste0("@", rownames(kmmm))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(kmmm, caption = "Mixed-Methods by Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.mmm)
#'
#'
#'
#' # Target Populations & Sampling Frames
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
        scat = c("IPV Interventions", "SMW-Specific Research")
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
#'
#'
popcb <- cb[cb$cat == "POPULATION", ] %>% droplevels()
popcb <- popcb[!duplicated(popcb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
popcb.tly <- tally(group_by(popcb, clab))
lvl.pop <- paste0(seq(1:nrow(popcb.tly)), " = ", levels(popcb$clab))
popcb$clab2 <- factor(popcb$clab, labels = seq(1:nrow(popcb.tly)))
# levels(popcb$clab) <- seq(1:length(unique(popcb$clab)))
kpop <- ftable(popcb$bibkey, popcb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
kpop <- ifelse(kpop >= 1, "&#10003;", "&middot;")
kpopscat <- car::recode(rownames(kpop), rec.key2scat)
kpopscat <- ifelse(kpopscat == "S4", paste0("@", rownames(kpop), "$^*$"), paste0("@", rownames(kpop)))
rownames(kpop) <- kpopscat
# rownames(kpop) <- paste0("@", rownames(kpop))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(kpop[, 1:9], caption = "Sampling Frame by Study (1/2)") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.pop[1:9])
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(kpop[, 9:length(lvl.pop)], caption = "Sampling Frame by Study (2/2)") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.pop[9:length(lvl.pop)])
#'
#'
#'
#' # Sampling Settings
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
        scat = c("IPV Interventions", "SMW-Specific Research")
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
#'
#'
ssetcb <- cb[cb$cat == "M-SETTINGS", ] %>% droplevels()
ssetcb <- ssetcb[!duplicated(ssetcb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
ssetcb.tly <- tally(group_by(ssetcb, clab))
lvl.sset <- paste0(seq(1:nrow(ssetcb.tly)), " = ", levels(ssetcb$clab))
ssetcb$clab2 <- factor(ssetcb$clab, labels = seq(1:nrow(ssetcb.tly)))
# levels(ssetcb$clab) <- seq(1:length(unique(ssetcb$clab)))
ksset <- ftable(ssetcb$bibkey, ssetcb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ksset <- ifelse(ksset >= 1, "&#10003;", "&middot;")
kssetscat <- car::recode(rownames(ksset), rec.key2scat)
kssetscat <- ifelse(kssetscat == "S4", paste0("@", rownames(ksset), "$^*$"), paste0("@", rownames(ksset)))
rownames(ksset) <- kssetscat
# rownames(ksset) <- paste0("@", rownames(ksset))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(ksset[,1:12], caption = "Sampling Settings by Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.sset[1:12])
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(ksset[,13:length(lvl.sset)], caption = "Sampling Settings by Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.sset[13:length(lvl.sset)])
#'
#'
#'
#' # Sampling Methods
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
        scat = c("IPV Interventions", "SMW-Specific Research")
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
#'
#'
smthdcb <- cb[cb$cat == "M-SAMPLING", ] %>% droplevels()
smthdcb <- smthdcb[!duplicated(smthdcb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
smthdcb.tly <- tally(group_by(smthdcb, clab))
lvl.smthd <- paste0(seq(1:nrow(smthdcb.tly)), " = ", levels(smthdcb$clab))
smthdcb$clab2 <- factor(smthdcb$clab, labels = seq(1:nrow(smthdcb.tly)))
# levels(smthdcb$clab) <- seq(1:length(unique(smthdcb$clab)))
ksmthd <- ftable(smthdcb$bibkey, smthdcb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ksmthd <- ifelse(ksmthd >= 1, "&#10003;", "&middot;")
ksmthdscat <- car::recode(rownames(ksmthd), rec.key2scat)
ksmthdscat <- ifelse(ksmthdscat == "S4", paste0("@", rownames(ksmthd), "$^*$"), paste0("@", rownames(ksmthd)))
rownames(ksmthd) <- ksmthdscat
# rownames(ksmthd) <- paste0("@", rownames(ksmthd))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(ksmthd, caption = "Sampling Methods by Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.smthd)
#'
#'
#'
#'
#' # Levels of Analysis Invovled in The Included Literature
#'
#'
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
                                   "SMW-Specific Research"))
# t.eco
sum.eco <- apply(ftm.eco, 1, sum)
ftm.eco <- ifelse(ftm.eco == 0, NA, ftm.eco)
ftm.eco <- cbind(ftm.eco, "**Total**" = paste0("**", sum.eco, "**"))
ftm.eco %>% kable(align = rep("r", ncol(ftm.eco)),
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
#'
#'
ecocb <- cb[cb$cat == "ECO", ] %>% droplevels()
ecocb <- ecocb[!duplicated(ecocb), ]
# x <- s4qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()
ecocb.tly <- tally(group_by(ecocb, clab))
lvl.eco <- paste0(seq(1:nrow(ecocb.tly)), " = ", levels(ecocb$clab))
ecocb$clab2 <- factor(ecocb$clab, labels = seq(1:nrow(ecocb.tly)))
# levels(ecocb$clab) <- seq(1:length(unique(ecocb$clab)))
keco <- ftable(ecocb$bibkey, ecocb$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
keco <- ifelse(keco >= 1, "&#10003;", "&middot;")
kecoscat <- car::recode(rownames(keco), rec.key2scat)
kecoscat <- ifelse(kecoscat == "S4", paste0("@", rownames(keco), "$^*$"), paste0("@", rownames(keco)))
rownames(keco) <- kecoscat
# rownames(keco) <- paste0("@", rownames(keco))
#'
#'
#'
#+ echo=FALSE
# panderOptions("table.split.table", 120)
kable(keco, caption = "Ecological Levels of Analysis Involved in Each Study") %>%
    add_footnote("SMW-Specific", notation = "symbol")
pander(lvl.eco)
#'
#'
#'
#' # Ecological **Network** Analysis
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
#'
#+ clrs_labs
# COLORS & LABELS ----------------
vclrs <- rev(grad(1:10, p = nord_polar))[4:7]
vtclrs <- rev(grad(1:10, p = nord_polar))[6:9]
catpal85 <- adjustcolor(catpal, alpha.f = 0.85)
lcvclrs <- c("1 = vclrs[1]; 2 = vclrs[2]; 3 = vclrs[3]; 4 = vclrs[4]; 'S3' = catpal[1]; 'S4' = catpal[2]")
ltclrs <- c("1 = vtclrs[1]; 2 = vtclrs[2]; 3 = vtclrs[3]; 4 = vtclrs[4]")
llabs1 <- c("1 = 'Individual'; 2 = 'Relationship'; 3 = 'Community'; 4 = 'Societal'")
sclabs <- c("'S3' = 'IPV Interventions Research'; 'S4' = 'SMW-Specific IPV Research'")
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
kindex <- nrow(llongv01) - 4
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
llongg <- graph_from_data_frame(llong, directed = FALSE, vertices = llongv)
lvnames0 <- vertex_attr(llongg, "name")
lvnames1 <- gsub("(\\w+)(\\d{4})\\w+", "\\1 (\\2)", lvnames0)
lvnames <- sapply(lvnames1, RtCap, USE.NAMES = FALSE)
V(llongg)$name <- lvnames
V(llongg)$size <- V(llongg)$Freq+1
V(llongg)$color <- adjustcolor(V(llongg)$vclr, alpha.f = 0.5)
V(llongg)$frame.color <- adjustcolor(V(llongg)$vclr, alpha.f = 0.85)
E(llongg)$width <- 0.35
kindex.g <- V(llongg)$name %>% length() - 4
lblsize <- c(log(V(llongg)$size[1:kindex.g])*0.5, log(V(llongg)$size[-1:-kindex.g])*0.2)
#'
#+ net_lvls_bibkeys, fig.fullwidth=TRUE, figPath=TRUE
# PLOT - `llongg` ----------------
par(mar = rep(0, 4))
lfr <- layout_with_fr(llongg) %>% norm_coords()
plot(llongg, rescale = T, layout = lfr, vertex.label.color = V(llongg)$cvclr, vertex.label.cex = lblsize)
#'
#'
#'
#' # Ecological Network Adjacency Matrix^[See Also: @lincoln2014adjacency]
#'
#+ net_lvls_keys_bibkeys_mat, dev="png", fig.retina=6, fig.fullwidth=TRUE, fig.cap=sprintf("Ecological Network: Levels of Analysis x Case. Group memberships derived from %s [@brandes2008on]", c("communality clustering algorithm which maximizes modularity across all possible partitions of the graph data in order to calculate the optimal cluster structure for the data", "eigenvalues computed for each case on each level of analysis presented in the matrix")), figPath=TRUE
llg.d <- graph_from_data_frame(llong, directed = TRUE, vertices = llongv[, c("x", "Freq")])
llongg_comm <- cluster_louvain(llongg)
llg.d_comm <- cluster_optimal((llg.d))
V(llg.d)$name <- lvnames
V(llg.d)$comm <- membership(llg.d_comm) ## node groupings according to 'optimal community structure' of the graph data ##
## ^STALLS ON *UN*DIRECTED GRAPH^ ##
V(llg.d)$deg <- degree(llg.d) ## vertex degrees (i.e., N_{adjacentEdges} per node) ##
V(llg.d)$close <- centr_clo(llg.d)$res ## node-level closeness scores ##
V(llg.d)$btw <- centr_betw(llg.d)$res ## node-level betweenness scores ##
V(llg.d)$eig <- centr_eigen(llg.d)$vector ## node-level eigenvector scores ##
V(llg.d)$eigVal <- centr_eigen(llg.d)$value ## eigenvalue corresponding to V(llg.d)$eig (above) ##
llg.nde <- get.data.frame(llg.d, what = "vertices")
llg.edg <- get.data.frame(llg.d, what = "edges") %>%
    inner_join(llg.nde %>% select(name, comm), by = c("from" = "name")) %>%
    inner_join(llg.nde %>% select(name, comm), by = c("to" = "name")) %>%
    mutate(group = ifelse(comm.x == comm.y, comm.x, NA) %>% factor())
llg.nde.a <- sort(llg.nde$name)
pdat0 <- llg.edg %>% mutate( ## base plot data, arranged alphabettically by 'llg.nde.a' values (i.e., llg.nde$name), which is not ideal, but good to have a base dataframe just in case ##
    to = factor(to, levels = llg.nde.a),
    from = factor(from, levels = llg.nde.a))
llg.comm <- (llg.nde %>% arrange(comm))$name
pdat.comm <- llg.edg %>% mutate( ## plot data arranged by node communities ##
    to = factor(to, levels = llg.comm),
    from = factor(from, levels = llg.comm))
llg.mat1 <- ggplot(pdat.comm, aes(x = to, y = from, fill = group)) +
    geom_raster() +
    thm_Rtft(ytitle = FALSE, xtitle = FALSE, ptitle = T, pcaption = TRUE) + ## I ADDED THIS USING A THEME FROM MY "Riley" PKG ##
    scale_fill_manual(values = mpal(seq_along(unique(na.omit(pdat.comm$group)))), na.value = pal_my[2], labels = c("Group-1", "Group-2", "Group-3", "Multi-Group")) +
    # Because we need the x and y axis to display every node,
    # not just the nodes that have connections to each other,
    # make sure that ggplot does not drop unused factor levels -- @lincoln2014adjacency
    scale_x_discrete(drop = TRUE) + ## except in my case, i want to drop the unused levels bc the data used to generate the network graph data is specifically setup so that the only unique values in the "to" column are the ecological levels of analysis involved in each study, and vise-versa in the "from column, so a matrix plot showing all levels on both axes is inappropriate for these data. ##
    scale_y_discrete(drop = TRUE) +
    theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        # Force the plot into a square aspect ratio
        aspect.ratio = 1)#,
# Hide the legend (optional)
# legend.position = "none")
hideLabs(x = llg.mat1, labs = labs(title = "Ecological Network: Levels of Analysis x Case"))#, caption = "Groups Represent Caclulated Cluster Memberships using  "))
llg.eig <- (llg.nde %>% arrange(eig))$name
pdat.eig <- llg.edg %>% mutate( ## plot data arranged by node eigenvalues ##
    to = factor(to, levels = llg.eig),
    from = factor(from, levels = llg.eig))
llg.mat2 <- ggplot(pdat.eig, aes(x = from, y = to, fill = group)) +
    geom_raster() +
    thm_Rtft(ytitle = FALSE, xtitle = FALSE, ptitle = T, pcaption = TRUE) +
    scale_fill_manual(values = mpal(seq_along(unique(na.omit(pdat.eig$group)))), na.value = pal_my[2], labels = c("Group-1", "Group-2", "Group-3", "Multi-Group")) +
    scale_x_discrete(drop = TRUE) +
    scale_y_discrete(drop = TRUE) +
    theme(
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = rel(.85)),
        axis.text.x = element_text(angle = 270, hjust = 0, size = rel(.85)),
        aspect.ratio = 1,
        plot.caption = element_text(size = rel(0.75)))
hideLabs(x = llg.mat2, labs = labs(subtitle = "Ecological Netowrk: Levels of Analysis", caption = "Group memberships derived from clustering algorithm which maximizes modularity\nacross all possible partitions of the graph data in order to calculate the\noptimal cluster structure for the data (Brandes et al., 2008)"))
#'
#' **The above adjacency matrix plot.** provides several pieces of information.
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
# library(igraph) ## not necessary - but included as reminder and in case I move/copy the code later ##
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
#'
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
#+ arc_lvls_TEST, fig.height=3.75, fig.fullwidth=TRUE
# PLOT - `lnet - arcplot()` ----------------
par(mar = c(5, 0, 0, 0))
arcplot(ledges, col.arcs = hsv(0, 0, 0.1, 0.075), pch.nodes = 21, bg.nodes = adjustcolor(lnetcol, alpha.f = 0.25), cex.nodes = ldeg, col.nodes = pal_my[19], lwd.nodes = 1, lwd.arcs = larcs, line = 1.25, cex.labels = 0.75, font = 1, col.labels = pal_my[20])
#'
#'
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
# library(igraph) ## not necessary - but included as reminder and in case I move/copy the code later ##
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
#'
#'
#+ net_sysLvls, fig.fullwidth=TRUE, figPath=TRUE
# par(mar = c(0, 0, 0, 0), mfrow = c(1, 2))
# plot(lnetg, rescale = T, layout = ll3n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA, margin = c(0, -0.15, -0.15, 0.05)); box(which = "figure")
# plot(snetg, rescale = T, layout = ls1n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA, margin = c(0, 0, 0, -0.5)); box(which = "figure")
# par(mpar, mfrow = c(1, 1))
#'
#'
#'
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
#'
#'
#' ## Network Analysis: Substantive Research Topics
#'
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
# library(igraph) ## not necessary - but included as reminder and in case I move/copy the code later ##
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
tlabs <- gsub("&", "&", tlabs)
tlabs <- paste(tlabs, collapse = ", ")
tlnetg_cap <- paste0("Network Diagram Showing Relations among Substantive Research Topics (numbered graph nodes) Covered & Ecological Levels of Analysis (named graph nodes) Involved among the Reviewed Literature: _", tlabs, "_")
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
#'
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
#'
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
# library(igraph) ## not necessary - but included as reminder and in case I move/copy the code later ##
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
# alabs <- gsub("\n", "", alabs)
alabs <- gsub("&", "&", alabs)
# alabs1 <- paste0(alabs[1:length(alabs)-1], sep = ", ")
# deparse(alabs1)
alabs <- paste(alabs, collapse = ", ")
alnetg_cap <- paste0("Network Diagram Showing Relations among Analytic Approaches (numbered graph nodes) used and Ecological Levels of Analysis (named graph nodes) Invovled among the Reviewed Literature: _", alabs, "_")
# PLOTS - `alnetg` (layout-0 & layout-2) ----------------
#'
#+ arc_alnetg, out.height='4in', fig.cap=alnetg_cap
par(mar = rep(0, 4))
lal3 <- layout_with_fr(alnetg) %>% norm_coords()
plot(alnetg, rescale = T, layout = lal3, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = c(vclrs[1:4], rep(NA, length(alnetcol))), vertex.label.cex = alblsize)
#'
#'
#'
#'
#+ alnetpr
alnet.pr <- ftable(alnet[, 2:3], row.vars = 2) %>% as.matrix() #(nrow = nrow(alfrq), byrow = FALSE)
dimnames(alnet.pr) <- list(levels(factor(alnet$clab)), gsub("\\.", "", levels(factor(alnet$to))))
alnet.pr <- ifelse(alnet.pr >= 1, "&#10003;", "&middot;")
# rownames(alnet.pr) <- gsub("\n", "", rownames(alnet.pr))
rownames(alnet.pr) <- gsub("&", "&", rownames(alnet.pr))
#'
kable(alnet.pr, caption = "Analytic approaches used across ecological levels of analysis", escape = F)
#'
#+ alnet_Nanalyses
analysisFrq <- tally(group_by(cba, bibkey))
#'
#'
#'
#+ aledges, echo=FALSE
# knitr::include_graphics("graphics/inputs/arc_analysEs.png")
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
allabs.p <- gsub(" & ", " & ", allabs)
#'
#'
#'
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
#'
#'
#' # Cluster Analysis: Topics Covered in Each Study
#'
#+ echo=FALSE
knitr::opts_chunk$set(fig.path = "graphics/cluster/rplot-")
#'
#+ hclust_topics1, fig.fullwidth=TRUE, figPath=TRUE
top <- cb[cb$cat == "TOPIC", ] %>% droplevels()
top <- within(top, {
    cid <- ifelse(top$cid %in% tpFilter, cid, NA)
})
top <- within(top, {
    bibkey2 <- ifelse(bibkey == "boal2014barriers", "boala2014barriers", bibkey)
    bibkey2 <- ifelse(bibkey2 == "boal2014impact", "boalb2014impact", bibkey2)
    bibkey2 <- gsub("(\\w+)(\\d{4})\\w+", "\\1 (\\2)", bibkey2)
    bibkey2 <- ifelse(bibkey2 == "boala (2014)", "boal (2014a)", bibkey2)
    bibkey2 <- ifelse(bibkey2 == "boalb (2014)", "boal (2014b)", bibkey2)
    bibkey2 <- sapply(bibkey2, RtCap, USE.NAMES = FALSE)
    bibkey3 <- paste0("@", bibkey)
    clab <- gsub(" \\(General\\) ", "", clab)
    clab <- gsub(" \\(General\\)", "", clab)
    clab <- gsub("IPV Victimization", "IPV - V", clab)
    clab <- gsub("IPV Perpetration", "IPV - P", clab)
    clab <- gsub("IPV Perpetrator", "IPV - P", clab)
    clab <- gsub("Victims'/Survivors'", "IPV - V", clab)
    clab <- gsub("IPV Intervention", "Intervention", clab)
    clab <- gsub("Intervention Program", "Intervention", clab)
    clab <- gsub("Intervention/Prevention", "Intervention", clab)
    clab <- gsub("Evaluation", "Eval.", clab)
    clab <- gsub("Interventions", "Intervention", clab)
    clab <- gsub("Coordinated Community Response to IPV", "CCR", clab)
    clab <- gsub("Key Stakeholders", "Stakeholders", clab)
})
rec.clab2cid2 <- paste0("\"", top$clab, "\" = \"", top$cid, "\"", collapse = "; ")
top <- na.omit(top)
# top$bibkey.pr <- paste0("@", top$bibkey)
# rec.key2pr <- paste0("\"", top$bibkey, "\" = \"", top$bibkey.pr, "\"", collapse = "; ")
Rtdf(top$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(align = c("l", "r"))
topmat <- ftable(top$bibkey2, top$cid) %>% as.matrix()
topmatpr <- ftable(top$bibkey3, top$clab) %>% as.matrix() ## "ks4" == "bibkeys - s4" ##
# topmatt <- t(topmat)
topmatt <- ftable(top$bibkey2, top$clab) %>% as.matrix() %>% t() ## "ks4" == "bibkeys - s4" ##
topmattpr <- ftable(top$bibkey3, top$clab) %>% as.matrix() %>% t() ## "ks4" == "bibkeys - s4" ##
kable(Rmsmm(as.data.frame(topmatpr)))
kable(Rmsmm(as.data.frame(topmattpr)))
topdist <- dist(topmat)
Rmsmm(topdist)
topclust <- hclust(topdist, method = "ward.D2")
# names(topclust)
# topclust$merge
# topclust$height
# topclust$order
# topclust$labels <- paste0("@", topclust$labels)
palette(pal_my)
par(mar = c(0,2,0.7,0))
plot(topclust, sub = ' ', xlab = ' ', main = "Bibkeys Clustered by Topics",
     frame.plot = F, col = 20, col.main = 18, col.axis = 18,
     cex = 0.8, cex.main = 1, cex.axis = 0.65, lwd = 1.5,
     font.main = 3)
abline(a = 3.5, b = 0, lty = 3, lwd = 1.5,
       col = adjustcolor(pal_my[19], alpha.f = 0.75))
rect.hclust(topclust, h = 3.5, border = "#3B0C67") -> rhcl
par(mpar)
topGrps <- cutree(topclust, 3)
barplot(table(topGrps), col = pal_sci[1], border = pal_sci[1], main = "Cluster Member Counts - Topics", xlab = ' ', cex = 1.25, cex.main = 1.15, cex.lab = 1, cex.axis = 0.85)
as.data.frame(table(topGrps)) %>%
    dplyr::rename(Group = topGrps, Nmembers = Freq) %>% ## 'rename()' {dplyr} args: [output]=[input]
    kable(caption = "4-Group Solution Membership Counts (Topics)")
topmembers <- sapply(unique(topGrps),function(g) paste(rownames(topmat)[topGrps == g]))
names(topmembers) <- paste0("Group.", seq_along(topmembers))
topmembers <- t(t(topmembers))
names(topmembers) <- "Group Members"
pander(topmembers, caption = "Group Memberships Resulting from 4-Group Hierarchical Cluster Solution (Topics)")
#'
#+ kclust_topics1
library(cluster)
topkfit <- kmeans(topmat, 3)
p <- pal_sci[c(1, 2, 3, 3)]
clusplot(topmat, topkfit$cluster, main = '4-Cluster K-Means Solution (Topics)',
         color = T, shade = T, plotchar = F, cex.txt = 0.5,
         col.clus = p, pch = 21, bg = adjustcolor(pal_my[19], alpha.f = .25),
         labels = 4, lines = 0, col.p = pal_my[19], font.main = 3, verbose = T, span = T)
pander(t(topkfit$centers), caption = "Per Variable Cluster Means ('centers') for 4-Cluster K-Means Solution (Topics)")
#'
#' -----
#'
#' # Which topics cluster with other topics?
#'
#'
#+ hclust_topics2, fig.fullwidth=TRUE, figPath=TRUE
top2dist <- dist(topmatt)
Rmsmm(top2dist)
top2clust <- hclust(top2dist, method = "ward.D2")
# names(top2clust)
# top2clust$merge
# top2clust$height
# top2clust$order
# top2clust$labels
palette(pal_my)
par(mar = c(0,2,0.7,0))
plot(top2clust, sub = ' ', xlab = ' ', main = "Topics Clustered by Bibkeys",
     frame.plot = F, col = 20, col.main = 18, col.axis = 18,
     cex = 0.65, cex.main = 1, cex.axis = 0.65, lwd = 1.5,
     font.main = 3)
abline(a = 2.8, b = 0, lty = 3, lwd = 1.5,
       col = adjustcolor(pal_sci[4], alpha.f = 0.75))
rect.hclust(top2clust, h = 2.8,
            border = adjustcolor(pal_sci[4], alpha.f = 0.75)) -> rhcl.h28
par(op)
top2clusts <- list()
for (i in 1:length(rhcl.h28)) {
    top2clusts[[i]] <- car::recode(names(rhcl.h28[[i]]), rec.clab2cid2)
}
top2 <- within(top, {
    tp_c1 <- ifelse(cid %in% top2clusts[[1]], 1, 0)
    tp_c2 <- ifelse(cid %in% top2clusts[[2]], 1, 0)
    tp_c3 <- ifelse(cid %in% top2clusts[[3]], 1, 0)
    tp_c4 <- ifelse(cid %in% top2clusts[[4]], 1, 0)
    tp_c5 <- ifelse(cid %in% top2clusts[[5]], 1, 0)
    tp_clust <- ifelse(cid %in% top2clusts[[1]], "C-1", NA)
    tp_clust <- ifelse(cid %in% top2clusts[[2]], "C-2", tp_clust)
    tp_clust <- ifelse(cid %in% top2clusts[[3]], "C-3", tp_clust)
    tp_clust <- ifelse(cid %in% top2clusts[[4]], "C-4", tp_clust)
    tp_clust <- ifelse(cid %in% top2clusts[[5]], "C-5", tp_clust)
})
top2bib <- top2[, c("bibkey2", "code", paste0("tp_c", 1:5), "tp_clust")]
# top2bib <- top2bib[!duplicated(top2bib), ]
topmatbib <- ftable(top2bib$bibkey2, top2bib$tp_clust) %>% as.matrix()
barcol <- adjustcolor(pal_sci[c(2, 4, 5, 8)], alpha.f = 0.65)
top2Grps <- cutree(top2clust, 5)
barplot(table(top2Grps), col = barcol, border = pal_sci[c(2, 4, 5, 8)], main = "Cluster Member Counts - Topics", xlab = ' ', cex = 1.25, cex.main = 1.15, cex.lab = 1, cex.axis = 0.85)
as.data.frame(table(top2Grps)) %>%
    dplyr::rename(Group = top2Grps, Nmembers = Freq) %>% ## 'rename()' {dplyr} args: [output]=[input]
    kable(caption = "3-Group Solution Membership Counts (Topics)")
top2members <- sapply(unique(top2Grps),function(g) paste(car::recode(rownames(topmatt), rec.cid2clab)[top2Grps == g]))
names(top2members) <- paste0("Group.", seq_along(top2members))
top2members <- t(t(top2members))
names(top2members) <- "Group Members"
pander(top2members, caption = "Group Memberships Resulting from 4-Group Hierarchical Cluster Solution (Topics)")
#'
#'
#'
#+ hclust_bibkeys
topmatbib.di <- Rdich(topmatbib)
topbibdist.di <- dist(topmatbib.di)
topbibclust.di <- hclust(topbibdist.di, method = "ward.D2")
topbibdist <- dist(topmatbib)
topbibclust <- hclust(topbibdist, method = "ward.D2")
palette(pal_my)
par(mar = c(0,2,0.7,0))
plot(topbibclust, sub = ' ', xlab = ' ', main = "Bibkey Clusters based on Topics Clusters",
     frame.plot = F, col = 20, col.main = 18, col.axis = 18,
     cex = 0.65, cex.main = 1, cex.axis = 0.65, lwd = 1.5,
     font.main = 3)
abline(a = 4, b = 0, lty = 3, lwd = 1.5,
       col = pal_sci[8])
# rect.hclust(top2clust, h = 2.8,
#             border = adjustcolor(pal_my[18], alpha.f = 0.75))
rect.hclust(hclust(dist(topmatbib), method = "ward.D2"),
            h = 4, border = pal_sci[8]) -> rhcl_bib
topmatbib2 <- ftable(top2$bibkey3, top2bib$tp_clust) %>% as.matrix()
kable(topmatbib2, caption = "Bibkey Clusters based on Topics Clusters", format.args = list(zero.print = ".") )
c1 <- names(rhcl.h28[[1]]) %>% as.list
c2 <- names(rhcl.h28[[2]]) %>% as.list
c3 <- names(rhcl.h28[[3]]) %>% as.list
c4 <- names(rhcl.h28[[4]]) %>% as.list
c5 <- names(rhcl.h28[[5]]) %>% as.list
cat(tufte::newthought("'C-1 Topics:"))
pander(c1)
cat(tufte::newthought("'C-2 Topics:"))
pander(c2)
cat(tufte::newthought("'C-3 Topics:"))
pander(c3)
cat(tufte::newthought("'C-4 Topics:"))
pander(c4)
cat(tufte::newthought("'C-5 Topics:"))
pander(c5)
topmatcodes <- ftable(top2$clab, top2$tp_clust) %>% as.matrix()
topmatcodespr <- ifelse(topmatcodes == 0, "&middot;", "&#10003;")
kable(topmatcodespr, caption = "Topic Clusters' Membership")
#'
#'
#'
#+ hclust_topics3, fig.fullwidth=TRUE, fig.cap="3-Group K-Means Clustering Solution for Primary Topics Covered Across the Included Literature", figPath=TRUE
library(cluster)
palette(pal_my)
par(mar = c(0,2,0.7,0))
plot(topbibclust.di, sub = ' ', xlab = ' ', main = "Bibkey Clusters based on Topics Clusters",
     frame.plot = F, col = 20, col.main = 18, col.axis = 18,
     cex = 0.65, cex.main = 1, cex.axis = 0.65, lwd = 1.5,
     font.main = 3)
abline(a = 3, b = 0, lty = 3, lwd = 1.5,
       col = pal_sci[8])
rect.hclust(hclust(dist(topmatbib), method = "ward.D2"),
            h = 3, border = pal_sci[8]) -> rhcl_bib
#'
#'
#+ kclust_topics2
par(pmar)
km.topbib.di <- kmeans(topmatbib.di, 3)
# tb.di.fanmemb0 <- fan.topbib.di$membership
# fan.mu <- apply(tb.di.fanmemb0, 2, mean)
# tb.di.fanmemb1 <- ifelse(tb.di.fanmemb0 < fan.mu, 0, 1)
clusplot(topmatbib.di, km.topbib.di$cluster, main = ' ', color = T, shade = T, plotchar = F, cex.txt = 0.5, col.clus = p, pch = 21, bg = adjustcolor(pal_my[19], alpha.f = .25), labels = 0, lines = 0, col.p = pal_my[19], font.main = 3, verbose = T, span = T)
# pander(, caption = "Per Variable Cluster Means ('centers') for 2-Cluster K-Means Solution (Topics)")
# topmatbib3 <- ftable(top2$bibkey2, top2bib$tp_clust) %>% as.matrix()
# tmb_c1 <- fan.topbib.di$clustering[fan.topbib.di$clustering == 1] %>% names()
# tmb_c2 <- fan.topbib.di$clustering[fan.topbib.di$clustering == 2] %>% names()
# tmb_c3 <- fan.topbib.di$clustering[fan.topbib.di$clustering == 3] %>% names()
tmb_c1 <- km.topbib.di$cluster[km.topbib.di$cluster == 1] %>% names()
tmb_c2 <- km.topbib.di$cluster[km.topbib.di$cluster == 2] %>% names()
tmb_c3 <- km.topbib.di$cluster[km.topbib.di$cluster == 3] %>% names()
rec.key2key <- paste0("\"", top2$bibkey2, "\" = \"", top2$bibkey, "\"", collapse = "; ")
# fans <- data.frame(bibkey = recode(names(fan.topbib.di$clustering), rec.key2key), fan_memb = fan.topbib.di$clustering)
# rownames(fans) <- NULL
# top3 <- merge(top2, fans, by = "bibkey")
km <- data.frame(bibkey = recode(names(km.topbib.di$cluster), rec.key2key), km_memb = km.topbib.di$cluster)
rownames(km) <- NULL
top3 <- merge(top2, km, by = "bibkey")
top3 <- within(top3, {
    km1 <- ifelse(km_memb == 1, 1, 0)
    km2 <- ifelse(km_memb == 2, 1, 0)
    km3 <- ifelse(km_memb == 3, 1, 0)
})
kmbib <- top3[, c("bibkey", "scat", "km1", "km2", "km3")]
kmbib <- kmbib[!duplicated(kmbib), ]
kmtop <- top3[, c("code", "clab", "km1", "km2", "km3")]
kmtop <- kmtop[!duplicated(kmtop), ]
levels(top3$scat) <- c("IPV Interventions", "SMW-Specific")
ftable(top3$km_memb, top3$scat)
ftable(top3$bibkey, top3$km_memb)
ftable(top3$clab, top3$km_memb)
# ftable(top3$fan_memb, top3$scat)
# ftable(top3$bibkey, top3$fan_memb)
# ftable(top3$clab, top3$fan_memb)
#'
#' -----
#'
#' # R-Packages
#'
#' `r Rcite_r(file = "../auxDocs/REFs.bib", footnote = FALSE, Rnote = "This project was conducted using", pkgnote = ", SQL, and the following _**R**-packages_:")`
#'
#' -----
#'
#' # References
#'
#'
