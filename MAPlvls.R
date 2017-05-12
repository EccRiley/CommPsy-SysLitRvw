#' ---
#' title: "MAP - Levels of Analysis"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', fig.keep='none', fig.show='none', message=FALSE, warning=FALSE, cache=FALSE
# SETUP --------------------------------------------------------------

source("../SETUP.R")
knitr::opts_chunk$set(
    tidy = TRUE,
    echo = TRUE,
    fig.keep = 'high',
    fig.show = 'asis',
    results = 'asis',
    tidy.opts = list(comment = FALSE),
    echoRule = NULL,
    echoRuleb = NULL,
    fig.width = 7,
    fig.height = 7,
    out.width = '0.75\\linewidth',
    fig.path = "graphics/EcoLvls/rplot-") #,
    # dev = 'png', fig.retina = 6)
knitr::opts_template$set(invisible = list(echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE, fig.keep='none', fig.show='none'))
#'
#+ src_bibs, opts.label='invisible'
# source("bibs.R", echo = FALSE, print.eval = FALSE, verbose = FALSE)
#'
#+ ecoLvlsData
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
#+ lvls_llong

# llong -------------------------------------------------------------------
llabs1 <- c("1 = 'Individual'; 2 = 'Relationship'; 3 = 'Community'; 4 = 'Societal'")

l <- mplvls[, 1:4]
l$id <- rownames(l)
llong <- reshape(l, varying = 1:4, direction = 'long', sep = "")
llong <- dplyr::rename(llong, lvl = time, ynlvl = l)
rownames(llong) <- NULL
llong$ynlvl <- ifelse(llong$ynlvl == 0, NA, llong$ynlvl)
llong <- na.omit(llong)[, -3]
# vclrs <- mpal(1:length(unique(llong[, 2])))
vclrs <- rev(grad(1:10, p = nord_polar))[5:8]
lvclrs <- c("1 = vclrs[1]; 2 = vclrs[2]; 3 = vclrs[3]; 4 = vclrs[4]")
llong$vclr <- car::recode(llong$lvl, lvclrs)
llong1 <- within(llong, {
    lvl <- car::recode(lvl, llabs1)
    id <- gsub("(\\w+)\\d{4}\\w+", "\\1", id)
    id <- sapply(id, RtCap)
})

llongv1 <- Rtdf(llong1[, 1], names = c("x", "Freq"))
# llongv1$vclr <- ifelse(llongv1$lvl == "Individual", vclrs[1], llongv1$lvl)
# llongv1$vclr <- ifelse(llongv1$lvl == "Relationship", vclrs[2], llongv1$vclr)
# llongv1$vclr <- ifelse(llongv1$lvl == "Community", vclrs[3], llongv1$vclr)
# llongv1$vclr <- ifelse(llongv1$lvl == "Societal", vclrs[4], llongv1$vclr)
# llongv1$Freq <- ifelse(llongv1$Freq == 0, NA, llongv1$Freq)
# llongv <- na.omit(llongv1)
# llongv1$lvl <- car::recode(llongv1$lvl, llabs1)
llongv2 <- Rtdf(llong1[, 2], names = c("x", "Freq"))
# llongv2$key <- gsub("(\\w+)\\d{4}\\w+", "\\1", llongv2$key)
# llongv2$key <- sapply(llongv2$key, RtCap)
llongv <- rbind(llongv1, llongv2)
# llongv <- merge(llongv3, llong1, by.y = "id", by.x = "x", all = T)

llongv$vclr <- ifelse(llongv$x == "Individual", vclrs[1], NA)
llongv$vclr <- ifelse(llongv$x == "Relationship", vclrs[2], llongv$vclr)
llongv$vclr <- ifelse(llongv$x == "Community", vclrs[3], llongv$vclr)
llongv$vclr <- ifelse(llongv$x == "Societal", vclrs[4], llongv$vclr)

llongv$border <- ifelse(llongv$x == "Individual", vclrs[1], NA)
llongv$border <- ifelse(llongv$x == "Relationship", vclrs[2], llongv$vclr)
llongv$border <- ifelse(llongv$x == "Community", vclrs[3], llongv$vclr)
llongv$border <- ifelse(llongv$x == "Societal", vclrs[4], llongv$vclr)
# llongv <- llongv[, c("id", "Freq", "vclr")]

#+ keysnet, fig.fullwidth=TRUE
library(igraph)
llongg <- graph_from_data_frame(llong1, directed = FALSE, vertices = llongv)
V(llongg)$size <- V(llongg)$Freq
V(llongg)$color <- adjustcolor(V(llongg)$vclr, alpha.f = 0.5)
V(llongg)$frame.color <- V(llongg)$border
    #mpal(1:length(unique(llongv3[, 1])))

# lv <- cbind(names(V(llongg)), V(llongg)$Freq) %>% data.frame()
# rep(V(llongg)$Freq, V(llongg)$Freq)
# lv$Freq <- as.integer(lv$Freq)
E(llongg)$width <- 0.35

lfr <- layout_with_fr(llongg)
ln <- norm_coords(lfr)#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
plot(llongg, rescale = T, layout = ln, edge.arrow.size = 0.5, vertex.label.color = "#1a1e22", vertex.label.cex = log(V(llongg)$size)*0.35)

#+ keysnet, fig.fullwidth=TRUE
library(igraph)
lsys <- c("'Individual' = 'Micro'; 'Relationship' = 'Micro-Meso'; 'Community' = 'Meso-Exo'; 'Societal' = 'Exo-Macro'")
slong <- within(llong1, {
    lvl <- car::recode(lvl, lsys)
})

slongv <- within(llongv, {
    x <- car::recode(x, lsys)
})

slongg <- graph_from_data_frame(slong, directed = FALSE, vertices = slongv)

V(slongg)$size <- V(slongg)$Freq
V(slongg)$color <- adjustcolor(V(slongg)$vclr, alpha.f = 0.5)
V(slongg)$frame.color <- V(slongg)$border
#mpal(1:length(unique(llongv3[, 1])))

# lv <- cbind(names(V(llongg)), V(llongg)$Freq) %>% data.frame()
# rep(V(llongg)$Freq, V(llongg)$Freq)
# lv$Freq <- as.integer(lv$Freq)
E(slongg)$width <- 0.35

lsfr <- layout_with_fr(slongg)
lsn <- norm_coords(lsfr)#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
plot(slongg, rescale = T, layout = lsn, edge.arrow.size = 0.5, vertex.label.color = "#1a1e22", vertex.label.cex = log(V(slongg)$size)*0.35)

#+ lvls_lnet

# lnet --------------------------------------------------------------------

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

# lgraph ------------------------------------------------------------------

library(igraph)
lnetg <- graph_from_data_frame(lnet, directed = FALSE, vertices = lfrq)
V(lnetg)$size <- V(lnetg)$Freq*1.5
lnetcol <- mpal(lfrq, p = nord_aurora, a = 0.8)
V(lnetg)$color <- lnetcol
E(lnetg)$width <- 0.25
#'
#+ lnetg_random
plot(lnetg, rescale = T, edge.arrow.size = 0.35, vertex.label.color = "#1a1e22", vertex.frame.color = adjustcolor(pal_my[17], alpha.f = 0.25))
#'
#+ lnetg_fr
ll1 <- layout_with_fr(lnetg)
ll1n <- norm_coords(ll1, ymin = -2, ymax = 2, xmin = -2, xmax = 2)
plot(lnetg, rescale = T, layout = ll1n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA)
#'
#+ lnetg_circle
ll2 <- layout.circle(lnetg)
ll2n <- norm_coords(ll2)#, ymin = -2, ymax = 2, xmin = -2, xmax = 2)
plot(lnetg, rescale = T, layout = ll2n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA)
#'
#+ lnetg_gem
ll3 <- layout.gem(lnetg)
ll3n <- norm_coords(ll2)#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
plot(lnetg, rescale = T, layout = ll3n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA)
#'
#+ lnetg_rt
ll4 <- layout.reingold.tilford(lnetg)
ll4n <- norm_coords(ll4)#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
plot(lnetg, rescale = T, layout = ll4n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA)
#'
#' \newpage
#'
lnetft <- ftable(lnet)# %>% matrix(nrow = nrow(lfrq), byrow = FALSE)
# dimnames(lnetft) <- list(levels(factor(lnet$to)), levels(factor(lnet$from)))
llvls <- c("Individual", "Relationship", "Community", "Societal")

lnetp <- within(lnet, {
    from <- factor(from, ordered = FALSE)
    to <- factor(to, ordered = FALSE)
    levels(from) <- c(levels(from), llvls[!llvls %in% levels(from)])
    levels(to) <- c(levels(to), llvls[!llvls %in% levels(to)])
}) ## NOTE: in both of the factor() calls above,
## the second arg is not necessarily essential,
## since the default for the 'ordered' arg in 'factor()'
## is to check whether the variable being factored is ordered
## (i.e., 'is.ordered(x)' - see '?factor') ...
## ... but, like most of my coding habits/conventions,
## i tend to specify it explicitly for programmatic
## and reproducibility/reusability purposes ##

lnetftp <- ftable(lnetp) %>% matrix(nrow = nrow(lfrq), byrow = FALSE)
dimnames(lnetftp) <- list("Level-1" = levels(lnetp$to), "Level-2" = levels(lnetp$from))
lnetftp <- ifelse(lnetftp == 0, NA, lnetftp)
pander(lnetftp, justify = c("left", "centre", "centre", "center", "center"))

sapply(mplvls[, c(1, 2)], table) %>% chisq.test
sapply(mplvls[, c(1, 3)], table) %>% chisq.test
sapply(mplvls[, c(1, 4)], table) %>% chisq.test
sapply(mplvls[, c(2, 3)], table) %>% chisq.test
sapply(mplvls[, c(2, 4)], table) %>% chisq.test
sapply(mplvls[, c(3, 4)], table) %>% chisq.test

library(arcdiagram)

ledges <- cbind(lnet$from, lnet$to)
lvals <- V(lnetg)$Freq
ldeg <- degree(lnetg)
larcs <- .35*(lnetft %>% matrix())
larcs <- ifelse(larcs == 0, NA, larcs) %>% na.omit()

#+ arc_lvls, fig.height=3.75, fig.fullwidth=TRUE
arcplot(ledges, col.arcs = hsv(0, 0, 0.1, 0.075), pch.nodes = 21, bg.nodes = adjustcolor(lnetcol, alpha.f = 0.75), cex.nodes = log(ldeg[c("Individual", "Relationship", "Community", "Societal")])*0.37, col.nodes = lnetcol, lwd.nodes = 0.75, lwd.arcs = larcs, line = 1.25, cex.labels = 0.5, font = 1, col.labels = pal_my[20])

#'
#' \newpage
#'
#' # Ecological Systems
#'
#+ sysnet

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

sfrq1 <- snet[, 1]
sfrq2 <- snet[, 2]
sfrq3 <- c(sfrq1, sfrq2)
sfrq <- Rtdf(sfrq3, names = c("lvl", "Freq"))

library(igraph)
snetg <- graph_from_data_frame(snet, directed = FALSE, vertices = sfrq)
V(snetg)$size <- V(snetg)$Freq*1.5
# V(snetg)$label.size <- (V(snetg)$Freq*0.85
snetcol <- mpal(sfrq, p = nord_aurora, a = 0.8)
V(snetg)$color <- snetcol
E(snetg)$width <- 0.25
#'
#+ snetg_random
plot(snetg, rescale = T, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA)
#'
#+ snetg_fr
ls1 <- layout.fruchterman.reingold(snetg)
ls1n <- norm_coords(ls1)#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
plot(snetg, rescale = T, layout = ls1n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA)
#'
#+ snetg_circle
ls2 <- layout.circle(snetg)
ls2n <- norm_coords(ls2)#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
plot(snetg, rescale = T, layout = ls2n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA)
#'
#+ snetg_gem
ls3 <- layout.gem(snetg)
ls3n <- norm_coords(ls3)#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
plot(snetg, rescale = T, layout = ls3n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA)
#'
#+ snetg_rt
ls4 <- layout.reingold.tilford(snetg)
ls4n <- norm_coords(ls4)#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
plot(snetg, rescale = T, layout = ls4n, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = NA)
#'
#' \newpage
#'
#+ snetft
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
pander(snetftp, justify = c("left", "centre", "centre"))

sapply(mplvls[, c(5, 6)], table) %>% chisq.test
sapply(mplvls[, c(5, 7)], table) %>% chisq.test
sapply(mplvls[, c(6, 7)], table) %>% chisq.test

sedges <- cbind(snet$from, snet$to)
svals <- V(snetg)$Freq
sdeg <- degree(snetg)
sarcs <- .15*(snetft %>% matrix())
sarcs <- ifelse(sarcs == 0, NA, sarcs) %>% na.omit()

#+ arc_sys, fig.height=3.5, fig.fullwidth=TRUE
arcplot(sedges, col.arcs = hsv(0, 0, 0.1, 0.06), pch.nodes = 21, bg.nodes = adjustcolor(snetcol, alpha.f = 0.5), cex.nodes = log(sdeg[c("Micro", "Meso-Exo", "Exo-Macro")])*0.3, col.nodes = snetcol, lwd.nodes = 0.75, lwd.arcs = sarcs, line = 1.25, cex.labels = 0.5, font = 1, col.labels = pal_my[20])
#
# MAPeco <- merge(MAP, mplvls1, by.x = "bibkey", by.y = "key", all = TRUE)[, c("bibkey", "jrnl", "year", "journal", "scat", "prop", "j.loc", "j.year", "SJR", "Hindex", "cpv", "l1", "l2", "l3", "l4", "exo_macro", "meso_exo", "micro", "nlvls", "nsys")]
