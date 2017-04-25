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
bibkey <- "gondolf1999comparison"
#'
#' # @`r bibkey`
#'
#' ## Hypotheses
#'
#' > \Large{$\mathpzc{H1.}$} \large{\textit{"More comprehensive systems ... would have significantly lower reassault rates than less comprehensive systems" (p. 43).}}
#'
#' `r tufte::newthought("More comprehensive systems include:")`
#'
#' 1. Added sanctions imposed by post-conviction, as opposed to pre-trial referral
#' 2. Extended batterer counseling and observation implied by longer-term BI programs
#' 3. Additional alcohol treatment servcices provided
#' 4. Victim services that address compounding problems
#'
#' ## Sampling Frame (SF):
#'
#' `r tufte::newthought("Study locations include:")`
#'
#+ locations

# STACK - locations ----------------------------------------------------------
sf.locations <- list("State" = c("Pennsylvania", "Texas", "Texas", "Colorado"),
                     "ST" = c("PA", "TX", "TX", "CO"),
                     "County" = c("Allegheny", "Dallas", "Harris", "Denver"),
                     "City" = c("Pittsburg", "Dallas", "Houston", "Denver")) %>% stack()

# PRINT OUTPUT - 'sf.locations' ----------------------------------------
paste0(sf.locations[sf.locations$ind == "City", 1], ", ", sf.locations[sf.locations$ind == "ST", 1]) %>% pander
#'
#' `r tufte::newthought("Continuum of intervention systems determined by:")`
#'
#+ sfcontinuum

### PRINT OUTPUT - 'sf.continuum' ####
S1.scontinuum <- c("Court referral type", "length of intervention", "additional services provided")
pander(S1.scontinuum)
#'
#+ sf
samples <- c("S1", "S2", "S3")
S1.sf <- "Batterer intevention systems based on a well established batterer program in Pittsburg, PA; Dallas, TX; Houston, TX; and Denver, CO"
S1.sf.incl <- c("Systems in compliance with their states\' BIP standards",
                "Systems that collaborate with victim services",
                "Systems employing cognitive-behavioral approach according to evidence-based standards",
                "Systems with 40-50+ referrals per month",
                "Systems operating for 5+ years",
                "Systems that provide training and supervision to new programs")
S1.sf.excl <- c("Systems \\textit{not} specific to intimate partner violence",
                "Systems \\textit{not} in compliance with their states\' BIP standards",
                "Systems that \\textit{do not} collaborate with victim services",
                "Systems \\textit{not} employing cognitive-behavioral approach according to evidence-based standards",
                "Systems with $<40$ referrals per month",
                "Systems operating for $<5$ years",
                "Systems that \\textit{do not} provide training and supervision to new programs")

S2.sf <- "The first 20-25 male-identified IPV-perpetrators appearing for intake each month at each site (until each site\'s recruitment cap, n = 210 per site, was met)"
S2.sf.incl <- c("Court-mandated IPV-P",
                "Voluntary IPV-P",
                "Males",
                "Adults",
                "Heterosexuals")
S2.sf.excl <- c("Non-IPV-P",
                "Non-Males",
                "Non-Heterosexuals",
                "Individuals under 18 years of age")

S3.sf <- "Female-identified initial or new (at T1) partners of S2 participants"
S3.sf.incl <- c("IPV-V", "No IPV-V", "Females", "Adults", "Heterosexuals")
S3.sf.excl <- c("Past partners of S2 participants",
                "Non-Females",
                "Non-Heterosexuals",
                "Individuals under 18 years of age")

# STACKS - sf ---------------------------------------------
sf <- list("S1-continuum" = S1.scontinuum,
           "S1-SF" = S1.sf,
           "S2-SF" = S2.sf,
           "S3-SF" = S3.sf) %>% stack()

sf.incl <- list("S1-SFaincl" = S1.sf.incl,
                "S2-SFaincl" = S2.sf.incl,
                "S3-SFaincl" = S3.sf.incl) %>% stack()


sf.excl <- list("S1-SFbexcl" = S1.sf.excl,
                "S2-SFbexcl" = S2.sf.excl,
                "S3-SFbexcl" = S3.sf.excl) %>% stack()

m1.sf.all <- rbind(sf, sf.incl, sf.excl)

sf <- list("S1-SF" = S1.sf,
           "S2-SF" = S2.sf,
           "S3-SF" = S3.sf) %>% stack()

# TBL - 'sf' --------------------------------------------------
sf[, 2] <- gsub("-\\w+", "", sf[, 2], perl = TRUE)
names(sf) <- c("Definition", "S")

sf.incl[, 2] <- gsub("-\\w+", "", sf.incl[, 2], perl = TRUE)
names(sf.incl) <- c("\\textit{Included} Population", "S")

sf.excl[, 2] <- gsub("-\\w+", "", sf.excl[, 2], perl = TRUE)
names(sf.excl) <- c("\\textit{Excluded} Population", "S")
#'
### sf kable with footnote ####
kable(sf[, c(2, 1)], align = c("r", "l"), col.names = c("S[note]", names(sf)[1]), caption = paste0("'", samples[1], "' Sub-Sample Definition"), format = 'latex', booktabs = TRUE) %>%
    kable_styling(full_width = TRUE, latex_options = c("scale_down")) %>%
    add_footnote("'S' = Sub-Samples")
#'
#' `r tufte::newthought("\\Large{Included populations}")`
#'
#' \tufteskip
#'
### LOOP - 'sf.incl' OUTPUT ####
for (i in 1:length(samples)) {
    print(
        paste0("\n\n\\noindent'**_", samples[i], "_' Sub-Sample**:\n",
               pander(as.list(sf.incl[sf.incl$S == samples[i], 1])))
    )
}
#'
#'
#' `r tufte::newthought("\\Large{Excluded populations}")`
#'
#' \tufteskip
#'

### LOOP - 'sf.excl' OUTPUT ####
for (i in 1:length(samples)) {
    print(
        paste0("\n\n\\noindent'**_", samples[i], "_' Sub-Sample**:\n",
               pander(as.list(sf.excl[sf.excl$S == samples[i], 1])))
    )
}
#'
#'
#' ## Sampling Methods
#'
#+ smthds
S1.smthds <- c("Purposive")
S2.smthds <- c("Purposive", "Probability", "Random Sampling")
# STACK - smthds ---------------------------------------------------

S3.smthds <- c("Purposive", "Dependent Probability")
m2.sampMthds <- list("S1-Sampling" = S1.smthds,
                     "S2-Sampling" = S2.smthds,
                     "S3-Sampling" = S3.smthds) %>% stack()

# TBL - 'sampMthds' -------------------------------------------
sampMthds <- m2.sampMthds
sampMthds[, 2] <- gsub("-\\w+", "", sampMthds[, 2], perl = TRUE)
names(sampMthds) <- c("Sampling Methods", "Sub-Sample")
kable(sampMthds[, c(2, 1)], align = c("r", "l"), col.names = c("S", names(sf)[1]), caption = paste0("'", samples[1], "' Sub-Sample Definition"), format = 'latex', booktabs = TRUE) %>%
    kable_styling(full_width = TRUE, latex_options = c("scale_down"))
#'
#'
#'
#' ## Data Collection Methods (DC)
#'
#+ dc

S1.dc <- c("Naturalistic observation", "Comparative evaluation")
S2.dc <- c("Self-report survey", "Key informant survey")
S3.dc <- c("Longitudinal", "Self-report survey", "Telephone survey", "Telephone interview")

# STACK - dc ---------------------------------------------------------
m3.dc <- list("S1-DC" = S1.dc,
              "S2-DC" = S2.dc,
              "S3-DC" = S3.dc) %>% stack()
dc2 <- m3.dc
dc2[, 2] <- gsub("-\\w+", "", dc2[, 2], perl = TRUE)
names(dc2) <- c("Data Collection Methods", "Sub-Sample")
kable(dc2[, c(2, 1)], align = c("r", "l"), col.names = c("S", names(sf)[1]), caption = paste0("'", samples[1], "' Sub-Sample Definition"), format = 'latex', booktabs = TRUE) %>%
    kable_styling(full_width = TRUE, latex_options = c("scale_down"))
#'
#' > "The design is naturalistic in the sense that it examines programs as they are implemented in their respective communities, and it is comparative in the sense that systems of a range of components and linkages are selected" (p. 43).
#'
#'
#'
#'
#' ## Measures - Quantitative (QT)
#'
#+ qt

# quaNT measures -----------------------------------------------------
S1.qt <- NA
S2.qt <- c("Millon Clinical Multiaxial Inventory (MCMI)", "Michigan Alcoholism Screening Test (MAST)")
S3.qt <- c("Conflict Tactics Scales (CTS)", "Severity of Violence against Women Scales (SVAWS)", "Abusive Behavior Inventory (ABI)", "Psychological Maltreatment of Women Inventory (PMWI)", "'Would you say that your life is generally better, worse, or the same?' (p. 49)", "'How safe [the woman] felt at that point' (p. 49)", "'How likely it was that [the womens\'] partners would hit them in the next few months (using a likert-type scale)' (p. 49)")

# STACK - m4.measures.qt ---------------------------------------------------
m4.measures.qt <- list("S1-QuantMeasures" = S1.qt,
                       "S2-QuantMeasures" = S2.qt,
                       "S3-QuantMeasures" = S3.qt) %>% stack()

measures.qt2 <- m4.measures.qt
measures.qt2[, 2] <- gsub("-\\w+", "", measures.qt2[, 2], perl = TRUE)
names(measures.qt2) <- c("Quantitative Measures", "Sub-Sample")
kable(measures.qt2[, c(2, 1)], align = c("r", "l"), col.names = c("S", names(sf)[1]), caption = paste0("'", samples[1], "' Sub-Sample Definition"), format = 'latex', booktabs = TRUE) %>%
    kable_styling(full_width = TRUE, latex_options = c("scale_down"))
#'
#'
MeasuresTypes <- list(MCMI = "Self-report",
                      MAST = "Self-report",
                      CTS = c("Self-report", "informant-report"),
                      SVAWS = "Self-report",
                      ABI =c("Self-report", "informant-report"),
                      PMWI = "Self-report") %>% stack()

kable(MeasuresTypes[, c(2, 1)], align = c("r", "l"), col.names = c("Quantative Measure", "Type"), caption = paste0("'", samples[1], "' Sub-Sample Definition"), format = 'latex', booktabs = TRUE) %>%
    kable_styling(full_width = TRUE, latex_options = c("scale_down"))
#'
#' ## Measures - Qualitative (QL)
#'
#+ ql

# quaL measures ------------------------------------------------------
S1.ql <- "Field Notes"
S2.ql <- NA
S3.ql <- c("'Open-ended question about how the relationship was going' (p. 49)", "'Descriptions of any conflicts and their circumstances' (p. 49)", "'The nature of battering injuries and medical assistance received for those injuries' (p. 49)", "'The woman\'s response to the abuse' (p. 49)")

# STACK - m5.measures.ql ---------------------------------------------------
m5.measures.ql <- list("S1-QualMeasures" = S1.ql,
                       "S2-QualMeasures" = S2.ql,
                       "S3-QualMeasures" = S3.ql) %>% stack()

m5.measures.ql2 <- m5.measures.ql
m5.measures.ql2[, 2] <- gsub("-\\w+", "", m5.measures.ql2[, 2], perl = TRUE)
names(m5.measures.ql2) <- c("Qualitative Measures", "Sub-Sample")
kable(m5.measures.ql2[, c(2, 1)], align = c("r", "l"), col.names = c("S", names(sf)[1]), caption = paste0("'", samples[1], "' Qualitative Measures"), format = 'latex', booktabs = TRUE) %>%
    kable_styling(full_width = TRUE, latex_options = c("scale_down"))
#'
#'
#'
#' ## Longitudinal Methods (LT)
#'
#+ lt

# STACK - S3.lt ---------------------------------------------------
S3.lt <- list(T1 = "Two weeks after partners\' program intake",
              T2 = "3 months after partners\' program intake",
              T3 = "6 months after partners\' program intake",
              T4 = "9 months after partners\' program intake",
              T5 = "12 months after partners\' program intake",
              T6 = "15 months after partners\' program intake") %>% stack()
Ntimepoints <- 6
TS <- 15 ## months ##
TS.unit <- "Months"

kable(S3.lt[, c(2, 1)], col.names = c("Time-Point", "Description"), align = c("r", "l"))
#'
#'
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

vlabs <- c("Intervention Components Continuum", "Methodology", "Qualitative Measures", "Quantitative Measures", "Sampling Methods", "Sampling Frame", "Sampling Frame Includes", "Sampling Frame Excludes")
gondolf1999 <- mthds
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

