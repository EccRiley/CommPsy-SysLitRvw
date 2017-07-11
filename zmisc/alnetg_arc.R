#+ setup, echo=1, results='hide', message=FALSE, warning=FALSE, cache=FALSE, fig.keep='none', fig.show='none'
source("bibs.R")
knitr::opts_chunk$set(
    echo = TRUE,
    tidy = FALSE,
    fig.height = 7,
    fig.fullwidth = TRUE,
    out.width = '1.05\\linewidth'
)
# options(width = 80)
#'

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


# aledges ----------------------------------------------------------------

aledges0 <- graph_from_data_frame(aln, directed = FALSE, vertices = al)
aledges <- get.edgelist(aledges0)

alnetcol <- mpal(alfrq, p = sci, a = 0.5)[-1:-4]
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
# alcap0 <- paste0("\\textit{", allabs.p[-1:-4], "}", collapse = ", ")
# alcap0 <- paste0("\\textbf{", allabs.p[-1:-4], "}", collapse = ", ")

# ARCPLOT - Analytic Approaches (`alnetg_arc`) -------------------------------------------------
#'
#'
#+ alnetg_arc, fig.show='asis', fig.cap="Network Diagram Showing Relations among Analytic Approaches (numbered graph nodes) used and Ecological Levels of Analysis (named graph nodes) Invovled among the Reviewed Literature"
arcplot(
    aledges,
    vertices = al[, 1],
    col.arcs = hsv(0, 0, 0.1, 0.15),
    pch.nodes = 21,
    bg.nodes = avclrs,
    col.nodes = avbrdrs,
    lwd.nodes = 0,
    cex.nodes = log(adeg)+1*1.25,
    lwd.arcs = alstrength-3, #adeg,
    line = 0,
    cex.labels = 0.85,
    font = 1,
    col.labels = pal_my[20],
    horizontal = T,
    sorted = FALSE,
    # ordering = allabs,
    family = "serif"
)
#':
#'
#' `r allabs1`
#'
#' \newpage
#'
#'
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
#'
#' \newpage
#'
#' # The data "under the hood" of that arc diagram
#'
#' `r tufte::newthought("Potentially useful for later/someday")` (primarily in terms of `R-programming`, not so much with regards to the actual current analysis)
#'
#+ alarc_df, fig.keep=c(2, 4, 8), fig.show=c(2, 4, 8), fig.height=7

#
# xy.alarc <- xynodes(length(allabs), order(allabs), allabs)
# locs.alarc <-
#     arc_radius_locs(aledges, al[, 1], xy.alarc) %>% data.frame()
# locs.alarc$edgeID <- rownames(locs.alarc) %>% as.integer()
#
# alarc.df1 <- aledges %>% data.frame()
# names(alarc.df1) <- c("ecoLvls", "analysis")
# alarc.df1$edgeID <- rownames(alarc.df1) %>% as.integer()
#
# alarc.df <- merge(alarc.df1, locs.alarc)
# avclrs2 <- c(vclrs, mpal(alfrq, p = sci)[-1:-4])
# cor(alarc.df[, "locs"], alarc.df[, "radios"])
# # plot(alarc.df[, "locs"], alarc.df[, "radios"], type='l', col = avclrs2)
#
# adeg2 <- ifelse(adeg > 1, log1p(adeg)/1.75, log1p(adeg))
#
# plot(alarc.df[, "radios"], alarc.df[, "locs"], type = 'n', ##p1
#      xaxt = 'n', yaxt = 'n')
#
# points(alarc.df[, "radios"], alarc.df[, "locs"], type = 'p', ## p2 - KEEP
#        pch = 19, col = adjustcolor(avclrs2, alpha.f = 0.75))
#
# plot(alarc.df[, "radios"], alarc.df[, "locs"], type = 'n', ## p3
#      xaxt = 'n', yaxt = 'n')
#
# lines(alarc.df[, "radios"], alarc.df[, "locs"], ## p4 - KEEP
#       col = pal_my[19])
#
# plot(alarc.df[, "radios"], alarc.df[, "locs"], type = 'n', ## p5
#      xaxt = 'n', yaxt = 'n')
#
# points(alarc.df[, "radios"], alarc.df[, "locs"], type = 'p', ## p6
#        pch = 19, col = adjustcolor(avclrs2, alpha.f = 0.75))
#
# lines(alarc.df[, "radios"], alarc.df[, "locs"], type = 'h', ## p7
#       lwd = 3, col = adjustcolor(avclrs2, alpha.f = 0.27))
#
# text(alarc.df[, "radios"], alarc.df[, "locs"], al[, 1], ## p8 - KEEP
#      cex = adeg2) #, srt = 41)

# text(jitter(alarc.df[, "radios"]), alarc.df[, "locs"] + c(0.005, -0.007),
     # allabs2, ## p8 - KEEP
     # cex = adeg2) #, srt = 41)
