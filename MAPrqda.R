#' ---
#' title: "MAP - RQDA"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
# source("../SETUP.R")
# knitr::opts_chunk$set(
#     tidy = TRUE,
#     echo = TRUE,
#     fig.keep = 'high',
#     fig.show = 'hold',
#     results = 'asis'
# )
# rpm()
#'
#' \Frule
#'
#' # RQDA Project
#'
#+ openProj, echo=-2, results='hide'
library(RQDA)
# RQDA()
# openProject("data/RQDA/comps.RQDA", updateGUI = TRUE)
openProject("data/RQDA/comps.RQDA")
#'
#' # Codings Retreival
#'
#+ misc, echo=FALSE, eval=FALSE
# cases <- RQDAQuery("SELECT `_rowid_`,* FROM `cases` ORDER BY `id`")
# library(Riley)
# Rdt(cases)
# closeProject(); RQDA(); openProject("data/RQDA/comps.RQDA", updateGUI = TRUE); caseAttr <- getAttr("case") ## "getAttr()" only works when GUI is open ##
#'
#'
#+ RQDAQueries, echo=TRUE
ctbl1 <- getCodingTable()[, c("cid", "codename", "index1")]
    ## all codings ##
names(ctbl1)[2] <- "code" ## for compatability later ##

cbk <- RQDAQuery("SELECT `id`, `name` FROM `freecode`") ## codelist only (without categories) ##

cases1 <-
    RQDAQuery("SELECT `name` AS `case`, `id` AS `caseid`
              FROM `cases`
              ORDER BY `caseid`")
caseids1 <-
    RQDAQuery("SELECT `caseid`, `selfirst`
              FROM `caselinkage`
              ORDER BY `caseid`")
codecats1 <-
    RQDAQuery("SELECT `name` AS `cat`, `catid`
              FROM `codecat`
              ORDER BY `catid`")
codecats2 <-
    RQDAQuery(
        "SELECT treecode.cid AS cid, freecode.name AS code, treecode.catid
        FROM `treecode`
        INNER JOIN `freecode` WHERE
        freecode.id = treecode.cid AND treecode.status = 1 ORDER BY `catid`"
    )

caseAttr.rm <-
    RQDAQuery("SELECT `value` AS `RM`, `caseID` AS `caseid`
              FROM `caseAttr`
              WHERE `variable`='RMV'
              ORDER BY `caseid`")

caseAttr.srch <-
    RQDAQuery("SELECT `value` AS `scat`, `caseID` AS `caseid`
              FROM `caseAttr`
              WHERE `variable`='SEARCH'
              ORDER BY `caseid`")

#'
#' # Data Wrangling \& Cleaning
#'
#+ ctbl, echo=TRUE
    ## Could've used cbind() in the next 3 lines bc the combined dataframes
    ## all (1) have the same number of rows (1/case) *and*
    ## (2) are all ordered by "caseid" (see RQDAQueries for each above)...
    ## I use merge() instead bc this avoids inadvert duplicate columns ##
caseAttr <- merge(caseAttr.rm, caseAttr.srch, by = "caseid")
caseids2 <- merge(cases1, caseids1, by = "caseid")
caseids <- merge(caseids2, caseAttr, by = "caseid")

    ## I use merge() for the remaining df combinations below
    ## bc the two dfs have differing numbers of rows ##

ctbl.m <- merge(caseids, ctbl1, by.x = "selfirst", by.y = "index1")
    ## this only works because ALL codings were done at the case-level,
    ## so all codings for a given case have the same start- and end-points...
    ## Since "getCodingTable()" returns the start- and end-points by default
    ## I included the start-points ("selfirst") for codings retrieved via
    ## the case-specific see queries (see above) so that these could be
    ## used as merging-indices with the "index1" column in the table
    ## returned from "getCodingTable()" ("ctbl1"; see queries above) ##

codecats <- merge(codecats2, codecats1, by = "catid")

ctbl <- merge(ctbl.m, codecats, by = c("cid", "code"))
ctbl <-
    ctbl[, c("caseid", "case", "RM", "scat", "cid", "code", "catid", "cat")]
    ## reordering columns for data-organizational purposes ...
    ## also removed the "selfirst" column since it's no longer needed ##
ct.srch <- ctbl[ctbl$cat == "SEARCH", ] ## for later ##
ctbl$cat <- ifelse(ctbl$cat == "SEARCH", NA_character_, ctbl$cat)
ctbl <- na.omit(ctbl)
    ## Removing any codings for codes in the "SEARCH" category since the
    ## values for those codes (i.e., "S3" & "S4") are already reflected
    ## in the "scat" attribute variable (see above) ##
#'
#' \newpage
#' ## Excluded Cases
#'
#' `r tufte::newthought("The below procedures")` are conducted to remove cases that will be excluded from the present review. Excluded cases include:
#'
#' 1. Non-empirical items, such as literature reviews, intervention descriptions or proposals (with no corresponding evaluation), etc.<!--(code = `"NON-EMPIRICAL"`)-->
#' 2. Empirical studies conducted with non-U.S. samples.<!--(code = `"NON-US"`)-->
#' 3. Items not available via Portland State University's library nor interlibrary loan.<!--(code = `"NOT_AVAILABLE"`)-->
#'
#' The above-described codes were included in a code-category (`"z"`) designated for excluded cases.
#'
#'
#+ exclude
# Rdt(ctbl)
t.cse <- table(caseids$RM) ## "0" = include; "1" = exclude ##
dimnames(t.cse)[[1]] <- c("Include", "Exclude")
t.cse

ctbl$RM <- as.numeric(ctbl$RM)

ctbl.z <- within(ctbl, {
    RM2 <- ifelse(ctbl$RM == 1, NA_character_, 0)
})
ctbl.z1 <- ctbl.z[ctbl.z$RM == 1, -length(ctbl.z)]
    ## subsetted dataframe containing only excluded cases ("rmv1 == 1") ##
ctbl.z1 <- ctbl.z1[ctbl.z1$cat == "z", ]
    ## subsetted df containing only codings for the "z"
    ## category's codes, which I used to code for exclusion criteria ##

table(ctbl.z1$code) ## tabulated reasons for exclusion from review ##

ctbl <- na.omit(ctbl.z)[, c("caseid", "case", "scat", "cid", "code", "catid", "cat")]
    ## remove NAs created in the new "RM2" column...and remove the
    ## "RM" and "RM2" columns bc they are no longer needed ##

ntot <- length(unique(ctbl.z$case)) ## N_{total} ##
nexcl <- length(unique(ctbl.z1$case)) ## N_{excluded} ##
nincl <- length(unique(ctbl$case))## N_{included} ##
#'
#' \newpage
#'
#' # Descriptives
#'
#' \[ \scriptstyle{N_{total} = `r ntot`; ~~ N_{excluded} = `r nexcl`} \]
#' \[ \mathpzc{N_{included} = `r nincl`} \]
#'
#'  -----
#'
#+ echo=FALSE
# knitr::opts_chunk$set(echo = FALSE)
library(vcd);
library(kableExtra);
library(dplyr)
#'
#' `r tufte::newthought("\\large{Search Categories}")`
#'
ct.srch$scat <- ifelse(ct.srch$scat == "S3", "IPV Interventions", "LGBTQ-IPV Research")

t.srch <- Rtdf(ct.srch$scat, names = c("Category", "Frequency"))
t.srch
srch.t <- table(ct.srch$scat)
prop.test(srch.t)
#'
#'
#+ out.width=".5\\linewidth"
catpal <- c(adjustcolor(mypal[16], alpha.f = 0.8), adjustcolor(mypal[18], alpha.f = 0.9)) ## muted dark blue and muted medium gray ##

barplot(t.srch[, 2], names.arg = t.srch[, 1], pch = 19, col = catpal, ylab = "Frequency", xlab = "Category")
#'
#' \newpage
#' `r tufte::newthought("\\large{Topics}")`
#'
labs <- list(
    top = c("Approach Eval.", "Community Capacity", "CCR", "IPV-Consequences", "IPV-Dynamics", "Help-Seeking", "Int. - General", "Int. - Descriptions", "Int. - Efficay", "Int. - Proposal", "Measures", "Eval. Methods", "Perp. Char.", "Program/Policy Devel.", "Outsiders' Persp.", "Key Stakeholders' Persp.", "Victims' Persp.", "Program Eval.", "Protective Factors", "Policy", "Prevalence", "Risk Factors", "System Response"),
    mo = c("MTA", "MM", "QL", "QT"),
    ql = c("Case Study", "Focus Groups", "Group Interviews", "1-on-1 Interviews", "Multiple QL Methods", "Participant Obsv.", "QL Survey"),
    qt = c("Secondary Data", "Experimental", "Longitudinal", "Multiple QT Methods", "Client Records", "Police Records", "QT Survey", "Cross-Sectional"),
    mm = c("Experimental", "Focus Groups", "1-on-1 Interviews", "Longitudinal", "QL Survey", "QT Survey", "Cross-Sectional"),
    pop = c("African Americans", "'At Risk' Populations", "Asian Americans", "Cis-Gender", "College Students", "Couples", "Non-IPV Crime Victims", "Disabled  Persons", "Female/Women/Girls", "General Population", "Graduate Students", "Heterosexuals", "IPV-Perpetratords", "IPV-Victims/Survivors", "Latin*/Hispanic", "Males/Men/Boys", "CB Practitioners", "CB Practitioners - IPV", "Int. Programs" , "Parents", "Racial Minorities", "Sexual Minorities (SM)", "SM - Bisexuals", "SM - Gay", "SM - Lesbian", "SM - Queer", "SM - Transgender", "System Entities", "Urban-Specific", "Children/Youth")
)
labs2 <- list(
    top = c("Intervention Approach Evaluation (Eval.)", "Community Capacity", "Coordinated Community Response (CCR)", "IPV Consequences", "IPV Dynamics", "Help-Seeking", "IPV Interventions (Int.) - General", "IPV Interventions (Int.) - Description", "IPV Interventions (Int.) - Efficay", "IPV Interventions (Int.) - Proposal", "Measures", "Program Evaluation (Eval.) Methods", "Perpetrator (Perp.) Characteristics (Char.)", "Program/Policy Development (Devel.)", "Outsiders' Perspectives (Persp.)", "Key Stakeholders' Perspectives (Persp.)", "Victims' Perspectives (Persp.)", "Program/Policy Evaluation (Eval.) - General", "Protective Factors", "Policy", "IPV Prevalence", "Risk Factors", "System Response"),
    mo = c("Meta-Analysis (MTA)", "Mixed-Methods (MM)", "Qualitative (QL)", "Quantitative (QT)"),
    ql = c("Case Study", "Focus Groups", "Group Interviews", "1-on-1 Interviews", "Multiple Qualitative (QL) Methods", "Participant Observation (Obsv.)", "Qualitative (QL) Survey"),
    qt = c("Secondary Data", "Experimental", "Longitudinal", "Multiple Quantitative (QT) Methods", "Client Records", "Police Records", "Quantitative (QT) Survey", "Cross-Sectional"),
    mm = c("Experimental", "Focus Groups", "1-on-1 Interviews", "Longitudinal", "Qualitative (QL) Survey", "Quantitative (QT) Survey", "Cross-Sectional"),
    pop = c("African Americans", "'At Risk' Populations", "Asian Americans", "Cis-Gender", "College Students", "Couples", "Non-IPV Crime Victims", "Disabled  Persons", "Female/Women/Girls", "General Population", "Graduate Students", "Heterosexuals", "IPV-Perpetrators", "IPV-Victims/Survivors", "Latinos/Latinas and/or Hispanic-Americans (Latin*/Hispanic)", "Males/Men/Boys", "Community-Based (CB) Practitioners", "CB Practitioners - IPV-Specific", "IPV Intervention (Int.) Programs" , "Parents", "Racial Minorities", "Sexual Minorities (SM)", "SM - Bisexuals", "SM - Gay", "SM - Lesbian", "SM - Queer", "SM - Transgender", "System Entities", "Urban-Specific", "Children/Youth")
)
ct.top <- ctbl[ctbl$cat == "TOPIC",]
t.top <- Rtdf(ct.top$code, names = c("Topic", "Frequency"))
t.top[, 1] <- labs2$top
kable(t.top, booktabs = T, format = "latex") %>% kable_styling(position = "float_right")

ft.top <- ftable(ct.top[, c("code", "scat")], row.vars = 1)
ftm.top <- matrix(ft.top, nrow = length(unique(t.top[, 1])), byrow = FALSE)
dimnames(ftm.top) <- list(Topic = labs$top, scat = c("IPV Interventions", "LGBTQ-IPV Research"))
#'
#+ out.width=".55\\linewidth"
# dotchart(t.top[, 2], labels = labs$top, pch = 19, lcolor = mypal[20], xlab = "Frequency", cex = 0.8, xlim = c(1, 24))

Rdotchart(ftm.top, labels = labs$top, pch = 19, gcolor = mypal[20], xlab = "Frequency", cex = 0.7, gcex = 0.75, gfont = 2, pt.cex = 1.125, color = c(rep(catpal[1], nrow(ftm.top)), rep(catpal[2], nrow(ftm.top))))

#'
#' \newpage
#' `r tufte::newthought("\\large{Overarching Methododology}")`
#'
ct.mo <- ctbl[ctbl$cat == "M-OVERALL", ]
t.mo <- Rtdf(ct.mo$code, names = c("Method(s)", "Frequency"))
t.mo[, 1] <- labs2$mo
kable(t.mo, align = c("l", "r"))

ft.mo <- ftable(ct.mo[, c("code", "scat")], row.vars = 1)
ftm.mo <- matrix(ft.mo, nrow = length(unique(t.mo[, 1])), byrow = FALSE)
dimnames(ftm.mo) <- list(Methodology = labs$mo, scat = c("IPV Interventions", "LGBTQ-IPV Research"))

# barplot(t.mo[, 2], names.arg = labs$mo, pch = 19, main = "Overarching Methodology", cex.names = 0.9)

mosaic(ftm.mo, labeling_args = list(gp_labels = gpar(fontsize = 9), gp_varnames = gpar(fontsize = 11, fontface = 2), set_varnames = c(scat = "Search Category"), pos_labels = c("center", "center"), just_labels = c("center", "right")), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T), highlighting = 2, highlighting_fill = catpal)#, margins = unit(6, "lines"))
#'
#' \newpage
#' `r tufte::newthought("\\large{Qualitative Methods}")`
#'
ct.ql <- ctbl[ctbl$cat == "M-QL", ]
t.ql <- Rtdf(ct.ql$code, names = c("Qualitative Method", "Frequency"))
ft.ql <- ftable(ct.ql[, c("code", "scat")], row.vars = 1)
ftm.ql <- matrix(ft.ql, nrow = length(unique(t.ql[, 1])), byrow = FALSE)
dimnames(ftm.ql) <- list("Qualitative Method(s)" = labs$ql, scat = c("IPV Interventions", "LGBTQ-IPV Research"))

t.ql[, 1] <- labs2$ql
t.ql

mosaic(ftm.ql, labeling_args = list(gp_labels = gpar(fontsize = 7), gp_varnames = gpar(fontsize = 10, fontface = 2), set_varnames = c(scat = "Search Category"), pos_labels = c("center", "center"), just_labels = c("center", "right")), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T), highlighting = 2, highlighting_fill = catpal, margins = unit(6, "lines"))
#'
#' \newpage
#' `r tufte::newthought("\\large{Quantitative Methods}")`
#'
ct.qt <- ctbl[ctbl$cat == "M-QT", ]
t.qt <- Rtdf(ct.qt$code, names = c("Quantitative Method", "Frequency"))
ft.qt <- ftable(ct.qt[, c("code", "scat")], row.vars = 1)
ftm.qt <- matrix(ft.qt, nrow = length(unique(t.qt[, 1])), byrow = FALSE)
dimnames(ftm.qt) <- list("Quantitative Method(s)" = labs$qt, scat = c("IPV Interventions", "LGBTQ-IPV Research"))

t.qt[, 1] <- labs2$qt
t.qt

mosaic(ftm.qt, labeling_args = list(gp_labels = gpar(fontsize = 7), gp_varnames = gpar(fontsize = 10, fontface = 2), set_varnames = c(scat = "Search Category"), pos_labels = c("center", "center"), just_labels = c("center", "right")), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T), highlighting = 2, highlighting_fill = catpal, margins = unit(5, "lines"))
#'
#' \newpage
#' `r tufte::newthought("\\large{Mixed-Methods}")`
#'
ct.mm <- ctbl[ctbl$cat == "M-MM", ]
t.mm <- Rtdf(ct.mm$code, names = c("Methods", "Frequency"))
ft.mm <- ftable(ct.mm[, c("code", "scat")], row.vars = 1)
ftm.mm <- matrix(ft.mm, nrow = length(unique(t.mm[, 1])), byrow = FALSE)
dimnames(ftm.mm) <- list("Mixed-Methods" = labs$mm, scat = c("IPV Interventions", "LGBTQ-IPV Research"))

t.mm[, 1] <- labs2$mm
t.mm

mosaic(ftm.mm, labeling_args = list(gp_labels = gpar(fontsize = 7), gp_varnames = gpar(fontsize = 10, fontface = 2), set_varnames = c(scat = "Search Category"), pos_labels = c("center", "center"), just_labels = c("center", "right")), rot_varnames = c(0, -90, 0, -90), rot_labels = rep(0, 4), alternate_labels = c(F, F), tl_labels = c(T, F), tl_varnames = c(F, T), highlighting = 2, highlighting_fill = catpal, margins = unit(5, "lines"))
#'
#' \newpage
#' `r tufte::newthought("\\large{Populations}")`
#'
ct.pop <- ctbl[ctbl$cat == "POPULATION", ]
t.pop <- Rtdf(ct.pop$code, names = c("Population", "Frequency"))
ft.pop <- ftable(ct.pop[, c("code", "scat")], row.vars = 1)
ftm.pop <- matrix(ft.pop, nrow = length(unique(t.pop[, 1])), byrow = FALSE)
dimnames(ftm.pop) <- list("Populations" = labs$pop, scat = c("IPV Interventions", "LGBTQ-IPV Research"))

t.pop[, 1] <- labs2$pop
t.pop
Rdotchart(ftm.pop, labels = labs$pop, pch = 19, gcolor = mypal[20], xlab = "Frequency", cex = 0.7, gcex = 0.75, gfont = 2, pt.cex = 1.125, color = c(rep(catpal[1], nrow(ftm.pop)), rep(catpal[2], nrow(ftm.pop))))

pop.df <- ft.pop %>% as.data.frame

lrm <- glm(Freq ~ scat + , data = pop.df)
dat.p1 <- with(pop.df,
               data.frame(Freq = mean(Freq), scat = factor(scat)))
# dat.p1
dat.p1$frqP <- predict(lrm, newdata = dat.p1, type = "response")
# dat.p1
dat.p2 <- with(pop.df, data.frame(Freq = rep(seq(from = 0, to = 65, length.out = 30), 2), scat = factor(scat)))
dat.p3 <- cbind(dat.p2, predict(lrm, newdata = dat.p2, type = "link", se = TRUE))
dat.p3 <- within(dat.p3, {
    PredictedProb <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
})
library(ggplot2); library(ggthemes)
ggplot(dat.p3, aes(x = Freq, y = PredictedProb)) +
    geom_ribbon(aes(ymin = LL, ymax = UL, fill = scat), alpha = 0.2) +
    geom_line(aes(colour = scat), size = 1) + scale_fill_manual(values = catpal) + scale_colour_manual(values = catpal) + thm_Rtft()

#'
#'
#'
#' \newpage
#'
#+ closeProj, results='hide'
CleanProject(); closeProject()
#'
