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

# OPEN RQDA PROJECT --------------------------------------------------------

library(RQDA)
openProject("data/RQDA/comps.RQDA")
# RQDA()
# openProject("data/RQDA/comps.RQDA", updateGUI = TRUE)
# openProject("data/RQDA/MAP-new.RQDA", updateGUI = TRUE)
# openProject("data/RQDA/MAP-new.RQDA")
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

# RQDA SQL Queries --------------------------------------------------------

## ctbl1 & cbk ========================================================

ctbl1 <- getCodingTable()[, c("cid", "codename", "index1")] ## all codings (sans coded text) ##
names(ctbl1)[2] <- "code" ## for compatability later ##

cbk <- RQDAQuery("SELECT `name` as `code`, `memo` as `clab`
                 FROM `freecode`
                 WHERE `status` = 1") ## codelist only (without categories) ##
cbk <- within(cbk, {
    code <- gsub("FG-\\w+", "FG", code)
    # code <- gsub("EXP-\\w+", "EXP", code)
    # code <- gsub("LT-\\w+", "LT", code)
    code <- gsub("SVY-QL-MM", "SVY-QL", code)
    code <- gsub("SVY-QT-MM", "SVY-QT", code)
    # code <- gsub("XS-\\w+", "XS", code)
    code <- gsub("IVW-\\w+", "IVW", code)
    # code <- gsub("SMIN-\\w+", "SMIN", code)
    code <- gsub("HET", NA, code)
})
# ctbl1 <- merge(ctbl1, cbk, all.y = FALSE, all.x = TRUE)

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

caseAttr.srch <-
    RQDAQuery("SELECT `value` AS `scat`, `caseID` AS `caseid`
              FROM `caseAttr`
              WHERE `variable`='SEARCH'
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

    ## Could've used cbind() in the next 3 lines bc the combined dataframes
    ## all (1) have the same number of rows (1/case) *and*
    ## (2) are all ordered by "caseid" (see RQDAQueries for each above)...
    ## I use merge() instead bc this avoids inadvert duplicate columns ##
caseAttr <- merge(caseAttr.rm, caseAttr.srch, by = "caseid")
caseids2 <- merge(cases1, caseids1, by = "caseid")
caseids <- merge(caseids2, caseAttr, by = "caseid")

    ## I use merge() for the remaining df combinations below
    ## bc the two dfs have differing numbers of rows ##

ctbl2 <- merge(caseids, ctbl1, by.x = "selfirst", by.y = "index1")
    ## this only works because ALL codings were done at the case-level,
    ## so all codings for a given case have the same start- and end-points...
    ## Since "getCodingTable()" returns the start- and end-points by default
    ## I included the start-points ("selfirst") for codings retrieved via
    ## the case-specific see queries (see above) so that these could be
    ## used as merging-indices with the "index1" column in the table
    ## returned from "getCodingTable()" ("ctbl1"; see queries above) ##

codecats <- merge(codecats2, codecats1, by = "catid")

ctbl <- merge(ctbl2, codecats, by = c("cid", "code"))
ctbl <-
    ctbl[, c("caseid", "case", "RM", "scat", "cid", "code", "catid", "cat")] #"clab",
    ## reordering columns for data-organizational purposes ...
    ## also removed the "selfirst" column since it's no longer needed ##

# ct.scat <- ctbl[ctbl$cat == "SEARCH", ] ## for later ##

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

# ctbl - EXCLUDE --------------------------------------------------------

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

ctbl <- na.omit(ctbl.z)[, c("caseid", "case", "scat", "cid", "code", "catid", "cat")]# "clab",
    ## remove NAs created in the new "RM2" column...and remove the
    ## "RM" and "RM2" columns bc they are no longer needed ##

# ctbl - DESCRIPTIVES --------------------------------------------------------

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
#' \newpage
#'
#+ closeProj, results='hide'

# CLOSE RQDA PROJECT --------------------------------------------------------

CleanProject(); closeProject()
#'
