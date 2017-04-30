#' ---
#' title: "CP Methods - RQDA"
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

# OPEN RQDA PROJECT --------------------------------------------------------

library(RQDA)
openProject("data/RQDA/cpmethods.RQDA")
# RQDA()
# openProject("data/RQDA/cpmethods.RQDA", updateGUI = TRUE)
#'
#+ RQDAQueries, echo=TRUE

# RQDA SQL Queries --------------------------------------------------------

## ctbl1 & cbk ========================================================

ctbl1 <- getCodingTable()[, c("cid", "codename", "index1")] ## all codings (sans coded text) ##
names(ctbl1)[2] <- "code" ## for compatability later ##

cbk <- RQDAQuery("SELECT `name` as `code`, `memo` as `clab`
                 FROM `freecode`
                 WHERE `status` = 1") ## codelist only (without categories) ##

## cases ========================================================

cases1 <-
    RQDAQuery("SELECT `name` AS `case`, `id` AS `caseid`
              FROM `cases`
              ORDER BY `caseid`")
caseids1 <-
    RQDAQuery("SELECT `caseid`, `selfirst`
              FROM `caselinkage`
              ORDER BY `caseid`")
caseAttr.rm <-
    RQDAQuery("SELECT `value` AS `RM`, `caseID` AS `caseid`
              FROM `caseAttr`
              WHERE `variable`='RMV'
              ORDER BY `caseid`")

caseAttr.section <-
    RQDAQuery("SELECT `value` AS `scat`, `caseID` AS `caseid`
              FROM `caseAttr`
              WHERE `variable`='SECTION'
              ORDER BY `caseid`")

## codecats ========================================================

codecats1 <-
    RQDAQuery("SELECT `name` AS `cat`, `catid`
              FROM `codecat`
              ORDER BY `catid`")
codecats2 <-
    RQDAQuery(
        "SELECT treecode.cid AS cid, treecode.catid AS catid,
        freecode.name AS code, freecode.memo AS clab
        FROM `treecode`
        INNER JOIN `freecode` WHERE
        freecode.id = treecode.cid AND treecode.status = 1 ORDER BY `catid`"
    )

#'
#' # Data Wrangling \& Cleaning
#'
#+ ctbl, echo=TRUE

# DATA WRANGLING --------------------------------------------------------
## ctbl ========================================================

caseAttr <- merge(caseAttr.rm, caseAttr.srch, by = "caseid")
caseids2 <- merge(cases1, caseids1, by = "caseid")
caseids <- merge(caseids2, caseAttr, by = "caseid")

ctbl2 <- merge(caseids, ctbl1, by.x = "selfirst", by.y = "index1")

codecats <- merge(codecats2, codecats1, by = "catid")

ctbl <- merge(ctbl2, codecats, by = c("cid", "code"))
ctbl <-
    ctbl[, c("caseid", "case", "cid", "code", "catid", "cat")] #"clab",
#'
#' \newpage
#'
#+ closeProj, results='hide'

# CLOSE RQDA PROJECT --------------------------------------------------------

CleanProject(); closeProject()
#'
