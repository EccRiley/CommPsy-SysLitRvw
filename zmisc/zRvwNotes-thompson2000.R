#'---
#'title: "Literature Summaries"
#'author: "Riley M. Smith"
#'---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R")
knitr::opts_chunk$set(tidy = FALSE, echo = FALSE, results = 'asis')
library(kableExtra)
rpm()
#'
#' \Frule
#'
#+ key, echo=FALSE
bibkey <- "thompson2000identification"
#'
#' # @`r bibkey`
#'
#' ## Hypotheses
#'
#' > \Large{$\mathpzc{H1.}$} \large{\textit{"" (p. \todo).}}
#'
#' ## Sampling Frame (SF):\todo
#'
#' `r tufte::newthought("Study locations include:")` \todo
#'
#+ locations

# STACK - locations ----------------------------------------------------------
sf.locations <- list("State" = c(Washington),
                     "ST" = c(WA),
                     "County" = c(Snohomish),
                     "City" = c(Puget Sound)) %>% stack()

# PRINT OUTPUT - 'sf.locations' ----------------------------------------
paste0(sf.locations[sf.locations$ind == "City", 1], ", ", sf.locations[sf.locations$ind == "ST", 1]) %>% pander
#'
#+ sf
samples <- c()
S1.sf <- c("Adult care teams within primary care clinics from the Group Health Cooperative, each of which volunteered to participate")
S1.sf.incl <- c("physicians", "nurse practitioners (NPs)", "physician assistants (PAs)", "registered nurses (RNs)", "licensed practical nurses (LPNs)", "medical assistants (MAs)")
S1.sf.excl <- c("Pediatricians", "Receptionists", "Personnel not located in practice clusters", "Non-healthcare professionals")
    ## 'Pediatricians, receptionists, and personnel not located in practice clusters were excluded' (p. 254). ##

# STACKS - sf ---------------------------------------------
sf <- list("S1.sf" = S1.sf) %>% stack()

sf.incl <- list("S1.incl" = S1.sf.incl) %>% stack()


sf.excl <- list("S1.excl" = S1.sf.excl) %>% stack()

m1.sf.all <- rbind(sf, sf.incl, sf.excl)
#'
#' ## Sampling Methods
#'
#+ smthds
S1.smthds <- c("Purposive")

S1.rctMthds <- c("Stratified Randomization")
# STACK - smthds ---------------------------------------------------

m2.sampMthds <- list() %>% stack()
#'
#'
#'
#' ## Data Collection Methods (DC)
#'
#+ dc

S1.dc <- c("Randomized Control Trial (RCT)", "Longitudinal")

# STACK - dc ---------------------------------------------------------
m3.dc <- list() %>% stack()
#'
#' > "The design is naturalistic in the sense that it examines programs as they are implemented in their respective communities, and it is comparative in the sense that systems of a range of components and linkages are selected" (p. 43).
#'
#' ## Measures - Quantitative (QT)
#'
#+ qt

# quaNT measures -----------------------------------------------------
S1.qt <- c()

# STACK - m4.measures.qt ---------------------------------------------------
m4.measures.qt <- list() %>% stack()
#'
MeasuresTypes <- list() %>% stack()
#'
#' ## Measures - Qualitative (QL)
#'
#+ ql

# quaL measures ------------------------------------------------------
S1.ql <- c()

# STACK - m5.measures.ql ---------------------------------------------------
m5.measures.ql <- list() %>% stack()
#'
#' # Summary of @gondolf1999comparison's Methods
#'
#' -----
#'
#'
#+ AllMthds

# ALL METHODS VARIABLES ----------------------------------------------
mthds <- rbind(m1.sf.all, m2.sampMthds, m3.dc, m4.measures.qt, m5.measures.ql)
names(mthds) <- c("value", "var")
mthds$var <- gsub("[ ]+", "", mthds$var, perl = TRUE)
mthds <- mthds %>% separate(var, c("L", "V"))
mthds <- within(mthds, {
    V <- factor(V)
    L <- factor(L)
})

vlabs <- c("Methodology", "Qualitative Measures", "Quantitative Measures", "Sampling Methods", "Sampling Frame", "Sampling Frame Includes", "Sampling Frame Excludes")
thompson2000 <- mthds
#'
#+ mthds.v
# TBL - 'mthds.v' ---------------------------------------------
mthds.v <- group_by(mthds, V)
attr(mthds.v, "labels") <- vlabs
v.labs <- attr(mthds.v, "labels")
mthds.c <- count(mthds.v) %>% data.frame()
mthds.c[, 1] <- v.labs
names(mthds.c) <- c("Variable", "Count")
mthds.c[order(mthds.c$Variable), ]
v.labs <- attr(mthds.v, "labels")
v.grps <- attr(mthds.v, "indices")
v.grps <- lapply(v.grps, function(x){x + 1})
names(mthds.v) <- c("Value", "Sub-Sample", "Variable")

# LOOP - 'mthds.v' kable ================================================
with(mthds.v, {
    for (i in 1:length(v.grps)) {
        print(pandoc.table(as.data.frame(mthds.v[v.grps[[i]], c(2, 1)]), caption = as.character(v.labs[i])))
    }
})
#'
#'
#'
#+ mthds.l

# TBL - 'mthds.l' ---------------------------------------------
mthds.l <- group_by(mthds, L)
mthds.l$V <- factor(mthds.l$V, labels = vlabs)
l.labs <- attr(mthds.l, "labels")[, 1]
mthds.c <- count(mthds.l) %>% data.frame()
names(mthds.c) <- c("SubSample", "Count")
mthds.c[order(mthds.c$SubSample), ]
l.grps <- attr(mthds.l, "indices")
l.grps <- lapply(l.grps, function(x){x + 1})
names(mthds.l) <- c("Value", "L", "Variable")

# LOOP - 'mthds.l' kable ================================================
with(mthds.l, {
    for (i in 1:length(l.grps)) {
        print(pandoc.table(as.data.frame(mthds.l[l.grps[[i]], c(3, 1)]), caption = paste0(as.character(l.labs[i]), " Sub-Sample")))
    }
})
#'
#'
#'
#' # References
