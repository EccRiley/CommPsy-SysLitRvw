#' ---
#' title: "MAP - Bibliography (Bibkeys - CP Only)"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', fig.keep='none', fig.show='none', message=FALSE, warning=FALSE, cache=FALSE
# SETUP --------------------------------------------------------------

source("bibs_cp.R", echo = FALSE, print.eval = FALSE, verbose = FALSE)
knitr::opts_chunk$set(fig.path = "graphics/bibkeys_cp/rplot-", dev = 'pdf')
# options(warn = -1)
#'
#' \Frule
#'
#'
#+ maptl, fig.fullwidth=TRUE, fig.height=3, out.width='\\linewidth'
MAPtl <- within(MAP, { ## making a copy so i don't mess up anything already written below that may depend on the original version of "MAP" ##
    scat2 <- factor(scat, labels = c("IPV Interventions Reserach", "SMW-Inclusive Research"))
    bibkey2 <- as.integer(factor(bibkey))
})

MAPtl$bibkey2 <- gsub("(\\w+)\\d{4}\\w+", "\\1", MAPtl$bibkey)
MAPtl$bibkey2 <- sapply(MAPtl$bibkey2, RtCap, USE.NAMES = FALSE)
MAPtl$pos <- sample(seq(1, nrow(MAPtl), by = 1), size = nrow(MAPtl), replace = FALSE)

pcpv <- mpal(1:2) %>% rev()
gg.tl <- ggplot(MAPtl, aes(x = year, y = 0, colour = scat2)) +
    thm_Rtft(yticks = FALSE, ytext = FALSE, ytitle = FALSE, ltitle = TRUE, ptitle = TRUE) +
    theme(legend.text = element_text(size = rel(0.65)),
          legend.title = element_text(size = rel(0.75), face = "bold"),
          plot.title = element_text(size = rel(0.8))) +
    labs(colour = "Research Category") +
    scale_colour_manual(values = pcpv) + #, guide = FALSE) +
    geom_hline(yintercept = mean(MAPtl$pos), size = 0.25, color = pal_my[19], alpha = 0.5) +
    geom_segment(aes(y = mean(MAPtl$pos), yend = pos, x = year, xend = year),
                 colour = pal_my[19], alpha = 0.45,
                 na.rm = TRUE, size = 0.15) +
    ##, position = position_dodge(width = 1)) +
    geom_text(aes(y = pos, x = year, label = bibkey2),#, position = position_jitter(),
              vjust = "outward", angle = 0, size = 2.5, fontface = "bold"); gg.tl +
    ggtitle("Timeline of Reviewed Community-Psychology (CP)-Specific Research")
#'
#' \newpage
#'
#' # \textsc{IPV Interventions Research}
#'
#' \Frule
#'
# reclab(cb$scat) --------------------------------------------------------
levels(cb$scat) <- c(1, 2)
cb$clab <- factor(cb$clab)
#'
# s3cb --------------------------------------------------------
s3cb <- cb[cb$scat == 1, ] %>% droplevels()
# s3cb.keys <- paste0("_**", levels(s3cb$bibkey), "**_ [@", levels(s3cb$bibkey), "]")
s3cb.keys <- paste0("@", levels(s3cb$bibkey))
# s3cb.keys %>% as.list() %>% pander()
#'
#'
#+ inv, fig.fullwidth=TRUE, fig.height=2, out.width='\\linewidth'
inv <- MAP[MAP$scat == "S3", ]
# inv.a <- cb[as.character(cb$bibkey) %in% as.character(inv$bibkey), ]

tl.inv <- inv[order(inv$year), c("bibkey", "year", "journal", "title")] %>% droplevels()
tl.inv$bibkey <- paste0("@", tl.inv$bibkey)
tl.inv <- dplyr::rename(tl.inv, "Study" = bibkey, "Journal" = journal, "Year Published" = year)
rownames(tl.inv) <- NULL
tl.inv[, c(1, 3, 2)] %>% kable(caption = "CP-Specific IPV Interventions Research Timeline")

bibkey.inv <- gsub("(\\w+)\\d{4}\\w+", "\\1", inv$bibkey)
bibkey.inv <- sapply(bibkey.inv, RtCap, USE.NAMES = FALSE)
inv$bibkey <- bibkey.inv
# tl.inv$pos <- runif(nrow(tl.inv), min = -1, max = 1)#*1.5
# tl.inv$pos <- ifelse(abs(tl.inv$pos) < 0.25, tl.inv$pos*10, tl.inv$pos)
# probs1 <- seq(1, nrow(inv), by = 1)
# probs2 <- mean(probs1)

inv$pos <- sample(seq(1, nrow(inv), by = 1), size = nrow(inv), replace = FALSE)

# inv$yrjt <- jitter(tl.inv$year, amount = 1.5)
# jitter(tl.inv$pos, amount = 2)

pinv <- pal_sci[1:length(unique(inv$journal))]

inv$bibkey2 <- as.integer(factor(inv$bibkey))
gg.tlinv <- ggplot(inv, aes(x = year, y = 0, colour = journal)) +
    thm_Rtft(yticks = FALSE, ytext = FALSE, ytitle = FALSE, ltitle = TRUE, ptitle = TRUE) +
    theme(legend.text = element_text(size = rel(0.65)),
          legend.title = element_text(size = rel(0.75), face = "bold"),
          plot.title = element_text(size = rel(0.8))) +
    labs(colour = "Journal Title") +
    scale_colour_manual(values = pinv) + #, guide = FALSE) +
    geom_hline(yintercept = mean(inv$pos), size = 0.25, color = pal_my[19], alpha = 0.5) +
    geom_segment(aes(y = mean(inv$pos), yend = pos, x = year, xend = year),
                 colour = pal_my[19], alpha = 0.45,
                 na.rm = TRUE, size = 0.15) +
    ##, position = position_dodge(width = 1)) +
    geom_text(aes(y = pos, x = year, label = bibkey),#, position = position_jitter(),
              vjust = "outward", angle = 0, size = 2.25, fontface = "bold"); gg.tlinv +
    ggtitle("CP-Specific IPV-Interventions Research Timeline")

#'
#' ## Research Topics
#'
## s3cb - TOPICS ========================================================

s3top <- s3cb[s3cb$cat == "TOPIC", ] %>% droplevels()

# Rtdf(s3top$clab, names = c(" ", "$N_{Articles}$")) %>%
#     pander(caption = "Primary Topics", justify = c("left", "right"))

lvlsn.tp <-paste0(seq(1:nlevels(s3top$clab)), " = ", levels(s3top$clab))
s3top$clabn <- factor(s3top$clab, labels = seq(1:nlevels(s3top$clab)))
ks3tp <- ftable(s3top$bibkey, s3top$clabn) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3tp <- ifelse(ks3tp >= 1, "\\checkmark", "$\\cdot$")
rownames(ks3tp) <- paste0("@", rownames(ks3tp))

kable(ks3tp, caption = "Research Topics")

# library(kableExtra)
# kable(ks3tp, format = "latex", booktabs = T, escape = FALSE) %>%
# kable_styling(latex_options = c("scale_down"))
pander(lvlsn.tp)
#'
#' \newpage
#'
#' ## Target Populations/Sampling Frames
#'
## s3cb - POPULATIONS ========================================================
s3pop <- s3cb[s3cb$cat == "POPULATION" & s3cb$scat == 1, ] %>% droplevels()
# ftable(s3pop$clab, s3pop$jrnl)
# Rtdf(s3pop$clab, names = c(" ", "$N_{Articles}$")) %>%
#     kable(caption = "Populations Included", align = c("l", "r"))

lvlsn.pop <-paste0(seq(1:nlevels(s3pop$clab)), " = ", levels(s3pop$clab))
s3pop$clabn <- factor(s3pop$clab, labels = seq(1:nlevels(s3pop$clab)))
ks3pop <- ftable(s3pop$bibkey, s3pop$clabn) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3pop <- ifelse(ks3pop >= 1, "\\checkmark", "$\\cdot$")
rownames(ks3pop) <- paste0("@", rownames(ks3pop))
kable(ks3pop)
pander(lvlsn.pop)
#'
#' ## Methodologies
#'
## s3cb - METHODS ========================================================
s3mo <- s3cb[s3cb$cat == "METHODS", ] %>% droplevels()
# ftable(s3mo$clab, s3mo$jrnl)
# Rtdf(s3mo$clab, names = c(" ", "$N_{Articles}$")) %>%
#     kable(caption = "Overarching Methodology", align = c("l", "r"))

lvlsn.mo <-paste0(seq(1:nlevels(s3mo$clab)), " = ", levels(s3mo$clab))
s3mo$clabn <- factor(s3mo$clab, labels = seq(1:nlevels(s3mo$clab)))
ks3mo <- ftable(s3mo$bibkey, s3mo$clabn) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3mo <- ifelse(ks3mo >= 1, "\\checkmark", "$\\cdot$")
rownames(ks3mo) <- paste0("@", rownames(ks3mo))
kable(ks3mo)
pander(lvlsn.mo)
#'
#' ## QuaNTitative Methods
#'
## s3cb - QUANT ========================================================
s3qt <- s3cb[s3cb$cat == "M-QT", ] %>% droplevels()
# ftable(s3qt$clab, s3qt$jrnl)
# Rtdf(s3qt$clab, names = c(" ", "$N_{Articles}$")) %>%
#     kable(caption = "Qua**NT**itative Methods", align = c("l", "r"))

lvlsn.qt <-paste0(seq(1:nlevels(s3qt$clab)), " = ", levels(s3qt$clab))
s3qt$clabn <- factor(s3qt$clab, labels = seq(1:nlevels(s3qt$clab)))
ks3qt <- ftable(s3qt$bibkey, s3qt$clabn) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
ks3qt <- ifelse(ks3qt >= 1, "\\checkmark", "$\\cdot$")
rownames(ks3qt) <- paste0("@", rownames(ks3qt))
kable(ks3qt)
pander(lvlsn.qt)
#'
#' \newpage
#'
## s3cb - MIXED-MTHDS (NA) ========================================================
# s3mm <- s3cb[s3cb$cat == "M-MM", ] %>% droplevels()
# # ftable(s3mm$clab, s3mm$jrnl)
# Rtdf(s3mm$clab, names = c(" ", "$N_{Articles}$")) %>%
#     kable(caption = "Mixed-Methods", align = c("l", "r"))
#
# lvlsn.mm <-paste0(seq(1:nlevels(s3mm$clab)), " = ", levels(s3mm$clab))
# s3mm$clabn <- factor(s3mm$clab, labels = seq(1:nlevels(s3mm$clab)))
# ks3mm <- ftable(s3mm$bibkey, s3mm$clabn) %>% as.matrix ## "ks3" == "bibkeys - s3" ##
# ks3mm <- ifelse(ks3mm >= 1, "\\checkmark", "$\\cdot$")
# rownames(ks3mm) <- paste0("@", rownames(ks3mm))
# kable(ks3mm)
# pander(lvlsn.mm)
#'
#' # \textsc{SMW-Inclusive IPV Research}
#'
#' \Frule
#'
#+ s4cb
# s4cb --------------------------------------------------------
s4cb <- cb[cb$scat == 2, ] %>% droplevels
s4cb.keys <- paste0("@", levels(s4cb$bibkey))
# s4cb.keys %>% as.list() %>% pander
#'
#'
#+ smw, fig.fullwidth=TRUE, fig.height=3, out.width='\\linewidth', fig.show='asis'
smw <- MAP[MAP$scat == "S4", ]
# inv.a <- cb[as.character(cb$bibkey) %in% as.character(inv$bibkey), ]

tl.smw <- smw[order(smw$year), c("bibkey", "year", "journal", "title")] %>% droplevels()
tl.smw$bibkey <- paste0("@", tl.smw$bibkey)
tl.smw <- dplyr::rename(tl.smw, "Study" = bibkey, "Journal" = journal, "Year Published" = year)
rownames(tl.smw) <- NULL
tl.smw[, c(1, 3, 2)] %>% kable(caption = "SMW-Inclusive Research Timeline")

bibkey.smw <- gsub("(\\w+)\\d{4}\\w+", "\\1", smw$bibkey)
bibkey.smw <- sapply(bibkey.smw, RtCap, USE.NAMES = FALSE)
smw$bibkey <- bibkey.smw

smw$pos <- sample(seq(1, nrow(smw), by = 1), size = nrow(smw), replace = FALSE) %>% jitter(amount = 2)
psmw <- pal_sci[1:length(unique(smw$journal))]

gg.tlsmw <- ggplot(smw, aes(x = year, y = 0, colour = journal)) +
    thm_Rtft(yticks = FALSE, ytext = FALSE, ytitle = FALSE, ltitle = TRUE, ptitle = TRUE) +
    theme(legend.text = element_text(size = rel(0.65)),
          legend.title = element_text(size = rel(0.75), face = "bold"),
          plot.title = element_text(size = rel(0.8))) +
    labs(colour = "Journal Title") +
    scale_colour_manual(values = psmw) + #, guide = FALSE) +
    geom_hline(yintercept = mean(smw$pos), size = 0.25, color = pal_my[19], alpha = 0.5) +
    geom_segment(aes(y = mean(smw$pos), yend = pos, x = year, xend = year),
                 colour = pal_my[19], alpha = 0.45,
                 na.rm = TRUE, size = 0.15) +
    geom_text(aes(y = pos, x = year, label = bibkey),#, position = position_jitter(),
              vjust = "outward", angle = 0, size = 2.5, fontface = "bold"); gg.tlsmw +
    ggtitle("CP-Specific SMW-Inclusive Research Timeline")

#'
#' \newpage
#'
#' ## Research Topics
#'
## s4cb - TOPICS ========================================================
s4top <- s4cb[s4cb$cat == "TOPIC", ] %>% droplevels()
# Rtdf(s4top$clab, names = c(" ", "$N_{Articles}$")) %>%
#     kable(caption = "Primary Topics", align = c("l", "r"))

# s4jtp <- ftable(s4top$clab, s4top$jrnl) %>% as.matrix
# dimnames(s4jtp) <- list("Topic" = dimnames(s4jtp)[[1]], "Journal" = dimnames(s4jtp)[[2]])
# ks4jtp <- ifelse(s4jtp >= 1, "\\checkmark", "$\\cdot$")
# lvlsn.jtp <- paste0(levels(factor(s4top$jrnl)), " = ", levels(factor(s4top$journal)))
# kable(ks4jtp)
# pander(lvlsn.jtp)

lvlsn.tp <- paste0(seq(1:nlevels(s4top$clab)), " = ", levels(s4top$clab))
s4top$clabn <- factor(s4top$clab, labels = seq(1:nlevels(s4top$clab)))

# s4top$clabn <- factor(s4top$clab, labels = seq(1:nlevels(s4top$clab)))
ks4tp <- ftable(s4top$bibkey, s4top$clabn) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4tp <- ifelse(ks4tp >= 1, "\\checkmark", "$\\cdot$")
rownames(ks4tp) <- paste0("@", rownames(ks4tp))
kable(ks4tp)
pander(lvlsn.tp)
#'
#' ## Target Populations/Sampling Frames
#'
## s4cb - POPULATIONS ========================================================
s4pop <- s4cb[s4cb$cat == "POPULATION", ] %>% droplevels()
# lvls.pop <-levels(s4pop$clab)
# ftable(s4pop$clab, s4pop$jrnl)
# Rtdf(s4pop$clab, names = c(" ", "$N_{Articles}$")) %>%
#     kable(caption = "Populations Included", align = c("l", "r"))
#
lvlsn.pop <- paste0(seq(1:nlevels(s4pop$clab)), " = ", levels(s4pop$clab))
s4pop$clabn <- factor(s4pop$clab, labels = seq(1:nlevels(s4pop$clab)))
# s4pop$clabn <- factor(s4pop$clab, labels = seq(1:nlevels(s4pop$clab)))
fts4pop <- ftable(s4pop$bibkey, s4pop$clabn) %>% as.matrix ## "ks4" == "bibkeys - s4" ##

fts4pop <- ifelse(fts4pop >= 1, 1, 0)

# ts4pop <- cbind(levels(s4pop$clab), apply(fts4pop, 2, sum))
# colnames(ts4pop) <- c(" ", "$N_{Articles}$")
# kable(ts4pop, caption = "Populations Included in Sampling Frame", align = c("l", "r"))

ks4pop <- Rdich(fts4pop, values = c("\\checkmark", "$\\cdot$"))
rownames(ks4pop) <- paste0("@", rownames(ks4pop))
kable(ks4pop)
pander(lvlsn.pop)
#'
#' \newpage
#'
#' ## Methodology
#'
## s4cb - METHODS ========================================================
s4mo <- s4cb[s4cb$cat == "METHODS", ] %>% droplevels()
# ftable(s4mo$clab, s4mo$jrnl)
# Rtdf(s4mo$clab, names = c(" ", "$N_{Articles}$")) %>%
#     kable(caption = "Overarching Methodology", align = c("l", "r"))

lvlsn.mo <-paste0(seq(1:nlevels(s4mo$clab)), " = ", levels(s4mo$clab))
s4mo$clabn <- factor(s4mo$clab, labels = seq(1:nlevels(s4mo$clab)))
ks4mo <- ftable(s4mo$bibkey, s4mo$clabn) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4mo <- ifelse(ks4mo >= 1, "\\checkmark", "$\\cdot$")
rownames(ks4mo) <- paste0("@", rownames(ks4mo))
kable(ks4mo)
pander(lvlsn.mo)
#'
#' ## QuaLitative Methods
#'
## s4cb - QUAL
s4ql <- s4cb[s4cb$cat == "M-QL", ] %>% droplevels()
# ftable(s4ql$clab, s4ql$jrnl)
Rtdf(s4ql$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(caption = "Qua**L**itative Methods", align = c("l", "r"))

lvlsn.ql <-paste0(seq(1:nlevels(s4ql$clab)), " = ", levels(s4ql$clab))
s4ql$clabn <- factor(s4ql$clab, labels = seq(1:nlevels(s4ql$clab)))
ks4ql <- ftable(s4ql$bibkey, s4ql$clabn) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4ql <- ifelse(ks4ql >= 1, "\\checkmark", "$\\cdot$")
rownames(ks4ql) <- paste0("@", rownames(ks4ql))
kable(ks4ql)
pander(lvlsn.ql)
#'
#' ## QuaNTitative Methods
#'
## s4cb - QUANT ========================================================
s4qt <- s4cb[s4cb$cat == "M-QT", ] %>% droplevels()
# ftable(s4qt$clab, s4qt$jrnl)
# Rtdf(s4qt$clab, names = c(" ", "$N_{Articles}$")) %>%
#     kable(caption = "Qua**NT**itative Methods", align = c("l", "r"))

lvlsn.qt <-paste0(seq(1:nlevels(s4qt$clab)), " = ", levels(s4qt$clab))
s4qt$clabn <- factor(s4qt$clab, labels = seq(1:nlevels(s4qt$clab)))
ks4qt <- ftable(s4qt$bibkey, s4qt$clabn) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4qt <- ifelse(ks4qt >= 1, "\\checkmark", "$\\cdot$")
rownames(ks4qt) <- paste0("@", rownames(ks4qt))
kable(ks4qt)
pander(lvlsn.qt)
#'
#' ## Mixed-Methods
#'
## s4cb - MIXED-MTHDS ========================================================
s4mm <- s4cb[s4cb$cat == "M-MM", ] %>% droplevels()
# ftable(s4mm$clab, s4mm$jrnl)
# Rtdf(s4mm$clab, names = c(" ", "$N_{Articles}$")) %>%
#     kable(caption = "Mixed-Methods", align = c("l", "r"))

lvlsn.mm <-paste0(seq(1:nlevels(s4mm$clab)), " = ", levels(s4mm$clab))
s4mm$clabn <- factor(s4mm$clab, labels = seq(1:nlevels(s4mm$clab)))
ks4mm <- ftable(s4mm$bibkey, s4mm$clabn) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
ks4mm <- ifelse(ks4mm >= 1, "\\checkmark", "$\\cdot$")
rownames(ks4mm) <- paste0("@", rownames(ks4mm))
kable(ks4mm)
pander(lvlsn.mm)
#'
#' \newpage\onehalfspacing
#'
#' # References
