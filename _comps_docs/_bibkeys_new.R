#'
#' # Case-by-Case Timeline of Included Literature
#'
#+ echo=FALSE
par(mpar)
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
#' \newpage
#'
#' # \textsc{IPV Intervention \& Prevention Research}
#'
#' \Frule
#'
# reclab(cb$scat) ----------------
cb$scat2 <- factor(cb$scat, labels = c(1, 2))
cb$clab <- factor(cb$clab)
#'
# s3cb ----------------
s3cb <- cb[cb$scat2 == 1, ] %>% droplevels
# s3cb <- s3cb[!duplicated(s3cb), ]
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

Rtdf(s3top$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Primary Topics Distribution (IPV Interventions Research)", align = c("l", "r"))

s3top.tly <- tally(group_by(s3top, clab))
lvls3.tp <- paste0(seq(1:nrow(s3top.tly)), " = ", levels(s3top$clab))

s3top$clab2 <- factor(s3top$clab, labels = seq(1:nrow(s3top.tly)))
# levels(s3top$clab) <- seq(1:length(unique(s3top$clab)))
ks3tp <- ftable(s3top$bibkey, s3top$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
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

s3pop.tly <- tally(group_by(s3pop, clab))
lvls3.pop <- paste0(seq(1:nrow(s3pop.tly)), " = ", levels(s3pop$clab))

s3pop$clab2 <- factor(s3pop$clab, labels = seq(1:nrow(s3pop.tly)))

# lvla3.pop <- paste0(seq(1:length(unique(s3pop$clab))), " = ", levels(s3pop$clab))
# levels(s3pop$clab) <- seq(1:length(unique(s3pop$clab)))
ks3pop <- ftable(s3pop$bibkey, s3pop$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3pop <- ifelse(ks3pop >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3pop) <- paste0("@", rownames(ks3pop))
kable(ks3pop, caption = "Populations Included by Study (IPV Interventions Research)")
pander(lvls3.pop)
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

Rtdf(s3set$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Sampling Settings (IPV Interventions Research)", align = c("l", "r"))

s3set.tly <- tally(group_by(s3set, clab))
lvls3.set <- paste0(seq(1:nrow(s3set.tly)), " = ", levels(s3set$clab))

s3set$clab2 <- factor(s3set$clab, labels = seq(1:nrow(s3set.tly)))

# lvla3.set <- paste0(seq(1:length(unique(s3set$clab))), " = ", levels(s3set$clab))
# levels(s3set$clab) <- seq(1:length(unique(s3set$clab)))
ks3set <- ftable(s3set$bibkey, s3set$clab2) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks3set <- ifelse(ks3set >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3set) <- paste0("@", rownames(ks3set))
kable(ks3set[, 1:10], caption = "Sampling Settings by Study (IPV Interventions Research [1/2])")
pander(lvls3.set[1:9])
#'
#' \newpage
#'
#+ echo=FALSE
kable(ks3set[, 11:ncol(ks3set)], caption = "Sampling Settings by Study (IPV Interventions Research [2/2])")
pander(lvls3.set[10:length(lvls3.set)])
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

s3smthds.tly <- tally(group_by(s3smthds, clab))
lvls3.smthds <- paste0(seq(1:nrow(s3smthds.tly)), " = ", levels(s3smthds$clab))

s3smthds$clab2 <- factor(s3smthds$clab, labels = seq(1:nrow(s3smthds.tly)))
# lvla3.smthds <- paste0(seq(1:length(unique(s3smthds$clab))), " = ", levels(s3smthds$clab))
# levels(s3smthds$clab) <- seq(1:length(unique(s3smthds$clab)))
ks3smthds <- ftable(s3smthds$bibkey, s3smthds$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks3smthds <- ifelse(ks3smthds >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3smthds) <- paste0("@", rownames(ks3smthds))
kable(ks3smthds, caption = "Sampling Methods by Study (IPV Interventions Research)")
pander(lvls3.smthds)
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


s3d.tly <- tally(group_by(s3d, clab))
lvls3.d <- paste0(seq(1:nrow(s3d.tly)), " = ", levels(s3d$clab))

s3d$clab2 <- factor(s3d$clab, labels = seq(1:nrow(s3d.tly)))

ks3d <- ftable(s3d$bibkey, s3d$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3d <- ifelse(ks3d >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3d) <- paste0("@", rownames(ks3d))
kable(ks3d, caption = "Research Design by Study (IPV Interventions Research)")
pander(lvls3.d)
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

s3mo.tly <- tally(group_by(s3mo, clab))
lvls3.mo <- paste0(seq(1:nrow(s3mo.tly)), " = ", levels(s3mo$clab))

s3mo$clab2 <- factor(s3mo$clab, labels = seq(1:nrow(s3mo.tly)))
lvla3.mo <- paste0(seq(1:length(unique(s3mo$clab2))), " = ", levels(s3mo$clab))
levels(s3mo$clab) <- seq(1:length(unique(s3mo$clab)))
ks3mo <- ftable(s3mo$bibkey, s3mo$clab) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3mo <- ifelse(ks3mo >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3mo) <- paste0("@", rownames(ks3mo))
kable(ks3mo, caption = "Methodology by Study (IPV Interventions Research)")
pander(lvls3.mo)
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

s3ql.tly <- tally(group_by(s3ql, clab))
lvls3.ql <- paste0(seq(1:nrow(s3ql.tly)), " = ", levels(s3ql$clab))

s3ql$clab2 <- factor(s3ql$clab, labels = seq(1:nrow(s3ql.tly)))
# lvla3.ql <- paste0(seq(1:length(unique(s3ql$clab))), " = ", levels(s3ql$clab))
# levels(s3ql$clab) <- seq(1:length(unique(s3ql$clab)))
ks3ql <- ftable(s3ql$bibkey, s3ql$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3ql <- ifelse(ks3ql >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3ql) <- paste0("@", rownames(ks3ql))
kable(ks3ql, caption = "Qua**L**itative Methods by Study (IPV Interventions Research)")
pander(lvls3.ql)
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

s3aql.tly <- tally(group_by(s3aql, clab))
lvls3.aql <- paste0(seq(1:nrow(s3aql.tly)), " = ", levels(s3aql$clab))

s3aql$clab2 <- factor(s3aql$clab, labels = seq(1:nrow(s3aql.tly)))
# lvls3.ql <- paste0(seq(1:length(unique(s3aql$clab))), " = ", levels(s3aql$clab))
# levels(s3aql$clab) <- seq(1:length(unique(s3aql$clab)))
ks3aql <- ftable(s3aql$bibkey, s3aql$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3aql <- ifelse(ks3aql >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3aql) <- paste0("@", rownames(ks3aql))
kable(ks3aql, caption = "Qua**L**itative Analytic Approaches by Study (IPV Interventions Research)")
pander(lvls3.ql)
#'
#' \newpage
#'
#' ## Qua**NT**itative Methods
#'
#+ qtMethods_s3
## s3cb - QUANT ================
s3qt <- s3cb[s3cb$cat == "M-QT", ] %>% droplevels()
s3qt <- s3qt[!duplicated(s3qt), ]
# x <- s3qt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s3qt$clab, s3qt$jrnl)
Rtdf(s3qt$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Qua**NT**itative Methods (IPV Interventions Research)", align = c("l", "r"))

s3qt.tly <- tally(group_by(s3qt, clab))
lvls3.qt <- paste0(seq(1:nrow(s3qt.tly)), " = ", levels(s3qt$clab))

s3qt$clab2 <- factor(s3qt$clab, labels = seq(1:nrow(s3qt.tly)))

# lvla3.qt <- paste0(seq(1:length(unique(s3qt$clab))), " = ", levels(s3qt$clab))
# levels(s3qt$clab) <- seq(1:length(unique(s3qt$clab)))

ks3qt <- ftable(s3qt$bibkey, s3qt$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3qt <- ifelse(ks3qt >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks3qt) <- paste0("@", rownames(ks3qt))

kable(ks3qt, caption = "Qua**NT**itative Methods by Study (IPV Interventions Research)")
pander(lvls3.qt)
#'
#' ## QuaNTtitative Analytic Approaches
#'
#+ AQT_s3
## s3cb - QUANT - ANALYSIS ================
s3aqt <- s3cb[s3cb$cat == "A-QT", ] %>% droplevels()
s3aqt <- s3aqt[!duplicated(s3aqt), ]
# x <- s3aqt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s3aqt$clab, s3aqt$jrnl)
Rtdf(s3aqt$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Qua**NT**itative Methods (IPV Interventions Research)", align = c("l", "r"))

s3aqt.tly <- tally(group_by(s3aqt, clab))
lvls3.aqt <- paste0(seq(1:nrow(s3aqt.tly)), " = ", levels(s3aqt$clab))

s3aqt$clab2 <- factor(s3aqt$clab, labels = seq(1:nrow(s3aqt.tly)))
# lvls3.aqt <- paste0(seq(1:length(unique(s3aqt$clab))), " = ", as.character(levels(s3aqt$clab)))
# levels(s3aqt$clab) <- seq(1:length(unique(s3aqt$clab)))
ks3aqt <- ftable(s3aqt$bibkey, s3aqt$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
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

s3mm.tly <- tally(group_by(s3mm, clab))
lvls3.mm <- paste0(seq(1:nrow(s3mm.tly)), " = ", levels(s3mm$clab))

s3mm$clab2 <- factor(s3mm$clab, labels = seq(1:nrow(s3mm.tly)))
# lvls3.mm <- paste0(seq(1:length(unique(s3mm$clab))), " = ", levels(s3mm$clab))
# levels(s3mm$clab) <- seq(1:length(unique(s3mm$clab)))
ks3mm <- ftable(s3mm$bibkey, s3mm$clab2) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
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

s3eco.tly <- tally(group_by(s3eco, clab))
lvls3.eco <- paste0(seq(1:nrow(s3eco.tly)), " = ", levels(s3eco$clab))

s3eco$clab2 <- factor(s3eco$clab, labels = seq(1:nrow(s3eco.tly)))
# lvls3.eco <- paste0(seq(1:length(unique(s3eco$clab))), " = ", levels(s3eco$clab))
# levels(s3eco$clab) <- seq(1:length(unique(s3eco$clab)))
ks3eco <- ftable(s3eco$bibkey, s3eco$clab2) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
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
s4cb <- cb[cb$scat2 == 2, ] %>% droplevels
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

s4top.tly <- tally(group_by(s4top, clab))
lvls4.tp <- paste0(seq(1:nrow(s4top.tly)), " = ", levels(s4top$clab))

s4top$clab2 <- factor(s4top$clab, labels = seq(1:nrow(s4top.tly)))
# lvls4.tp <- paste0(seq(1:length(unique(s4top$clab))), " = ", levels(s4top$clab))
# levels(s4top$clab) <- seq(1:length(unique(s4top$clab)))
ks4tp <- ftable(s4top$bibkey, s4top$clab2) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
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
s4pop.tly <- tally(group_by(s4pop, clab))
lvls4.pop <- paste0(seq(1:nrow(s4pop.tly)), " = ", levels(s4pop$clab))

s4pop$clab2 <- factor(s4pop$clab, labels = seq(1:nrow(s4pop.tly)))
# lvla4.pop <- paste0(seq(1:length(unique(s4pop$clab))), " = ", levels(s4pop$clab))
# levels(s4pop$clab) <- seq(1:length(unique(s4pop$clab)))
ks4pop <- ftable(s4pop$bibkey, s4pop$clab2) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4pop <- ifelse(ks4pop >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4pop) <- paste0("@", rownames(ks4pop))

#'
#+ echo=FALSE
kable(ks4pop, caption = "Populations Included by Study (SMW-Inclusive Research)")
pander(lvls4.pop)
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

s4set.tly <- tally(group_by(s4set, clab))
lvls4.set <- paste0(seq(1:nrow(s4set.tly)), " = ", levels(s4set$clab))

s4set$clab2 <- factor(s4set$clab, labels = seq(1:nrow(s4set.tly)))
# lvla4.set <- paste0(seq(1:length(unique(s4set$clab))), " = ", levels(s4set$clab))
# levels(s4set$clab) <- seq(1:length(unique(s4set$clab)))
ks4set <- ftable(s4set$bibkey, s4set$clab2) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4set <- ifelse(ks4set >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4set) <- paste0("@", rownames(ks4set))

#'
#+ echo=FALSE
kable(ks4set, caption = "Sampling Settings by Study (SMW-Inclusive IPV Research)")
pander(lvls4.set)
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

s4smthds.tly <- tally(group_by(s4smthds, clab))
lvls4.smthds <- paste0(seq(1:nrow(s4smthds.tly)), " = ", levels(s4smthds$clab))

s4smthds$clab2 <- factor(s4smthds$clab, labels = seq(1:nrow(s4smthds.tly)))
# lvla4.smthds <- paste0(seq(1:length(unique(s4smthds$clab))), " = ", levels(s4smthds$clab))
# levels(s4smthds$clab) <- seq(1:length(unique(s4smthds$clab)))
ks4smthds <- ftable(s4smthds$bibkey, s4smthds$clab2) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4smthds <- ifelse(ks4smthds >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4smthds) <- paste0("@", rownames(ks4smthds))

#'
#+ echo=FALSE
kable(ks4smthds, caption = "Sampling Methods by Study (SMW-Inclusve Research)")
pander(lvls4.smthds)
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

s4d.tly <- tally(group_by(s4d, clab))
lvls4.d <- paste0(seq(1:nrow(s4d.tly)), " = ", levels(s4d$clab))

s4d$clab2 <- factor(s4d$clab, labels = seq(1:nrow(s4d.tly)))
# lvla3.mo <- paste0(seq(1:length(unique(s4d$clab))), " = ", levels(s4d$clab))
# levels(s4d$clab) <- seq(1:length(unique(s4d$clab)))
ks4d <- ftable(s4d$bibkey, s4d$clab2) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
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

s4mo.tly <- tally(group_by(s4mo, clab))
lvls4.mo <- paste0(seq(1:nrow(s4mo.tly)), " = ", levels(s4mo$clab))

s4mo$clab2 <- factor(s4mo$clab, labels = seq(1:nrow(s4mo.tly)))
# lvla4.mo <- paste0(seq(1:length(unique(s4mo$clab))), " = ", levels(s4mo$clab))
# levels(s4mo$clab) <- seq(1:length(unique(s4mo$clab)))
ks4mo <- ftable(s4mo$bibkey, s4mo$clab2) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4mo <- ifelse(ks4mo >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4mo) <- paste0("@", rownames(ks4mo))
#'
#+ echo=FALSE
kable(ks4mo, caption = "Methodology by Study (SMW-Inclusive Research)")
pander(lvls4.mo)
#'
#' \newpage
#'
#' ## QuaLitative Methods
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

s4ql.tly <- tally(group_by(s4ql, clab))
lvls4.ql <- paste0(seq(1:nrow(s4ql.tly)), " = ", levels(s4ql$clab))

s4ql$clab2 <- factor(s4ql$clab, labels = seq(1:nrow(s4ql.tly)))
# lvla4.ql <- paste0(seq(1:length(unique(s4ql$clab))), " = ", levels(s4ql$clab))
# levels(s4ql$clab) <- seq(1:length(unique(s4ql$clab)))
ks4ql <- ftable(s4ql$bibkey, s4ql$clab2) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4ql <- ifelse(ks4ql >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4ql) <- paste0("@", rownames(ks4ql))
#'
#+ echo=FALSE
kable(ks4ql, caption = "Qua**L**itative Methods by Study (SMW-Inclusive Research)")
pander(lvls4.ql)
#'
#' ## Qua**L**itative Analytic Appraoches
#'
#+ AQL_s4
## s4cb - QUAL-ANALYTICS ================

s4aql <- s4cb[s4cb$cat == "A-QL", ] %>% droplevels()
# s4aql <- s4ql[!duplicated(s4aql), ]
# x <- s4aql[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4aql$clab, s4aql$jrnl)
Rtdf(s4aql$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Qua**NT**itative Methods (SMW-Inclusive IPV Research)", align = c("l", "r"))

s4aql.tly <- tally(group_by(s4aql, clab))
lvls4.aql <- paste0(seq(1:nrow(s4aql.tly)), " = ", levels(s4aql$clab))

s4aql$clab2 <- factor(s4aql$clab, labels = seq(1:nrow(s4aql.tly)))
# lvla4.qt <- paste0(seq(1:length(unique(s4aql$clab))), " = ", levels(s4aql$code))
# levels(s4aql$clab) <- seq(1:length(unique(s4aql$clab)))
ks4aql <- ftable(s4aql$bibkey, s4aql$clab2) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4aql <- ifelse(ks4aql >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4aql) <- paste0("@", rownames(ks4aql))
kable(ks4aql, caption = "Qua**L**itative Analytic Approaches by Study (SMW-Inclusive IPV Research)")
pander(lvls4.aql)
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


s4qt.tly <- tally(group_by(s4qt, clab))
lvls4.qt <- paste0(seq(1:nrow(s4qt.tly)), " = ", levels(s4qt$clab))

s4qt$clab2 <- factor(s4qt$clab, labels = seq(1:nrow(s4qt.tly)))
# lvla4.qt <- paste0(seq(1:length(unique(s4qt$clab))), " = ", levels(s4qt$clab))
# levels(s4qt$clab) <- seq(1:length(unique(s4qt$clab)))
ks4qt <- ftable(s4qt$bibkey, s4qt$clab2) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4qt <- ifelse(ks4qt >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4qt) <- paste0("@", rownames(ks4qt))
kable(ks4qt, caption = "Qua**NT**itative Methods by Study (SMW-Inclusive Research)")
pander(lvls4.qt)
#'
#' ## Quantitative Analytic Appraoches
#'
#+ AQT_s4
## s4cb - QUANT-ANALYTICS ================
s4aqt <- s4cb[s4cb$cat == "A-QT", ] %>% droplevels()
# s4aqt <- s4ql[!duplicated(s4aqt), ]
# x <- s4aqt[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4aqt$clab, s4aqt$jrnl)
Rtdf(s4aqt$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Qua**NT**itative Methods (IPV Interventions Research)", align = c("l", "r"))

s4aqt.tly <- tally(group_by(s4aqt, clab))
lvls4.aqt <- paste0(seq(1:nrow(s4aqt.tly)), " = ", levels(s4aqt$clab))

s4aqt$clab2 <- factor(s4aqt$clab, labels = seq(1:nrow(s4aqt.tly)))
# lvla4.qt <- paste0(seq(1:length(unique(s4aqt$clab))), " = ", levels(s4aqt$code))
# levels(s4aqt$clab) <- seq(1:length(unique(s4aqt$clab)))
ks4aqt <- ftable(s4aqt$bibkey, s4aqt$clab2) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4aqt <- ifelse(ks4aqt >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4aqt) <- paste0("@", rownames(ks4aqt))
kable(ks4aqt, caption = "Qua**NT**itative Analytic Approaches by Study (IPV Interventions Research)")
pander(lvls4.qt)
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

s4mm.tly <- tally(group_by(s4mm, clab))
lvls4.mm <- paste0(seq(1:nrow(s4mm.tly)), " = ", levels(s4mm$clab))

s4mm$clab2 <- factor(s4mm$clab, labels = seq(1:nrow(s4mm.tly)))
# lvla4.mm <- paste0(seq(1:length(unique(s4mm$clab))), " = ", levels(s4mm$clab))
# levels(s4mm$clab) <- seq(1:length(unique(s4mm$clab)))
ks4mm <- ftable(s4mm$bibkey, s4mm$clab2) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4mm <- ifelse(ks4mm >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4mm) <- paste0("@", rownames(ks4mm))
kable(ks4mm, caption = "Mixed-Methods by Study (SMW-Inclusive Research)")
pander(lvls4.mm)
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

s4eco.tly <- tally(group_by(s4eco, clab))
lvls4.eco <- paste0(seq(1:nrow(s4eco.tly)), " = ", levels(s4eco$clab))

s4eco$clab2 <- factor(s4eco$clab, labels = seq(1:nrow(s4eco.tly)))
# lvla4.eco <- paste0(seq(1:length(unique(s4eco$clab))), " = ", levels(s4eco$clab))
# levels(s4eco$clab) <- seq(1:length(unique(s4eco$clab)))
ks4eco <- ftable(s4eco$bibkey, s4eco$clab2) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4eco <- ifelse(ks4eco >= 1, "$\\checkmark$", "$\\cdot$")
rownames(ks4eco) <- paste0("@", rownames(ks4eco))
kable(ks4eco, caption = "Levels of Analysis by Study (SMW-Inclusive Research)")
pander(lvls4.eco)

par(pmar)
#'
#' \newpage\onehalfspacing
#'
#' # References`r Rcite_r(file = "../auxREFs.bib", footnote = TRUE)`
#'
#' \refs
#'
