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
alrec <- c("
            '.Individual' = '(L1) Individual';
            '.Relationship' = '(L2) Relationship';
            '.Community' = '(L3) Community';
            '.Societal' = '(L4) Societal'
           ")

# alrec2 <- c("'(L1) Individual' = expression(bold((L1))); '(L2) Relationship' = expression(bold((L2))); '(L3) Community' = expression(bold((L3))); '(L4) Societal' = expression(bold((L4)))")

aln1 <- car::recode(alnet[, 1], alrec) %>% as.character()

aln <- cbind(aln1, alnet[, 2])
aln[, 2] <- car::recode(aln[, 2], alrec)

al <- within(alfrq, {
    lvl <- car::recode(lvl, alrec)
})

aledges0 <- graph_from_data_frame(aln, directed = FALSE, vertices = al)
aledges <- get.edgelist(aledges0)

allabs <- sort(c(unique(aledges[, 1]), unique(aledges[, 2])))
# allabs2 <- car::recode(allabs, alrec2)

allabs2 <- ifelse(grepl("\\(L\\d\\) \\w+", allabs),
                  gsub("\\((L\\d)\\) \\w+",
                       "\\1", allabs), allabs)
# allabs2[1:4] <- sapply(allabs2, 1:4, noquote)

avals <- V(aledges0)$Freq

alnetcol <- mpal(alfrq, p = sci, a = 0.5)[-1:-4]
avbrdrs <- c(vclrs[1:4], alnetcol)
avclrs <-
    c(adjustcolor(vclrs[1:4], alpha.f = 0.35),
      adjustcolor(alnetcol, alpha.f = 0.25))

adeg <- degree(aledges0)*.25
aarcs <- .25 * (avals %>% matrix())
aarcs <- ifelse(aarcs == 0, NA, aarcs) %>% na.omit()

# ARCPLOT - Analytic Approaches (`alnetg_arc`) -------------------------------------------------
#'
#' \newpage
#'
#+ alnetg_arc
arcplot(
    aledges,
    vertices = allabs,
    col.arcs = hsv(0, 0, 0.1, 0.15),
    pch.nodes = 21,
    bg.nodes = avclrs,
    col.nodes = avbrdrs,
    lwd.nodes = 0,
    cex.nodes = log(adeg)+1*1.25,
    lwd.arcs = adeg,
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
#' `r alnetg_cap`
#'
#' \newpage
#'
#' # The data "under the hood" of that arc diagram
#'
#' `r tufte::newthought("Potentially useful for later/someday")` (primarily in terms of `R-programming`, not so much with regards to the actual current analysis)
#'
#+ alarc_df, fig.keep=c(2, 4, 8), fig.show=c(2, 4, 8), fig.height=7


xy.alarc <- xynodes(length(allabs), order(allabs), allabs)
locs.alarc <-
    arc_radius_locs(aledges, allabs, xy.alarc) %>% data.frame()
locs.alarc$edgeID <- rownames(locs.alarc) %>% as.integer()

alarc.df1 <- aledges %>% data.frame()
names(alarc.df1) <- c("ecoLvls", "analysis")
alarc.df1$edgeID <- rownames(alarc.df1) %>% as.integer()

alarc.df <- merge(alarc.df1, locs.alarc)
avclrs2 <- c(vclrs, mpal(alfrq, p = sci)[-1:-4])
cor(alarc.df[, "locs"], alarc.df[, "radios"])
# plot(alarc.df[, "locs"], alarc.df[, "radios"], type='l', col = avclrs2)

adeg2 <- ifelse(adeg > 1, log1p(adeg)/1.75, log1p(adeg))

plot(alarc.df[, "radios"], alarc.df[, "locs"], type = 'n', ##p1
     xaxt = 'n', yaxt = 'n')

points(alarc.df[, "radios"], alarc.df[, "locs"], type = 'p', ## p2 - KEEP
       pch = 19, col = adjustcolor(avclrs2, alpha.f = 0.75))

plot(alarc.df[, "radios"], alarc.df[, "locs"], type = 'n', ## p3
     xaxt = 'n', yaxt = 'n')

lines(alarc.df[, "radios"], alarc.df[, "locs"], ## p4 - KEEP
      col = pal_my[19])

plot(alarc.df[, "radios"], alarc.df[, "locs"], type = 'n', ## p5
     xaxt = 'n', yaxt = 'n')

points(alarc.df[, "radios"], alarc.df[, "locs"], type = 'p', ## p6
       pch = 19, col = adjustcolor(avclrs2, alpha.f = 0.75))

lines(alarc.df[, "radios"], alarc.df[, "locs"], type = 'h', ## p7
      lwd = 3, col = adjustcolor(avclrs2, alpha.f = 0.27))

text(alarc.df[, "radios"], alarc.df[, "locs"], allabs2, ## p8 - KEEP
     cex = adeg2) #, srt = 41)

# text(jitter(alarc.df[, "radios"]), alarc.df[, "locs"] + c(0.005, -0.007),
     # allabs2, ## p8 - KEEP
     # cex = adeg2) #, srt = 41)
