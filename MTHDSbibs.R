#' ---
#' title: "MAP - Bibliography"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
# SETUP --------------------------------------------------------------

source("../SETUP.R")
knitr::opts_chunk$set(
    tidy = TRUE,
    echo = FALSE,
    fig.keep = 'high',
    fig.show = 'asis',
    results = 'asis',
    tidy.opts = list(comment = FALSE),
    echoRule = NULL,
    echoRuleb = NULL,
    fig.height = 5,
    fig.path = "graphics/bibs/rplot-")#, dev = 'png',
# fig.retina = 4
# rpm()
#+ bibdf
### FUN - 'Rbibkeys()' ####
Rbibkeys <- function(bib) {
    keys <- bib[grep("\\@.*?\\{.*?,", bib, perl = TRUE)]
    keys <- gsub("\\@\\w+\\{(.*?)", "\\1", keys, perl = TRUE)
    keys <- keys[!grepl("\\%.*?,", keys, perl = TRUE)]
    keys <- gsub(" ", NA_character_, keys)
    keys <- gsub(",", "", keys)
    return(keys)
}

# BIB --------------------------------------------------------------

bib <- readLines("MAP.bib")
BIBKEY <- Rbibkeys(bib)

library(bib2df)
bibdf <- bib2df("MAP.bib")

### n.init ####
n.init <- nrow(bibdf)

ID <- seq(1:nrow(bibdf))
MAP.au <- cbind(BIBKEY, bibdf[, "AUTHOR"])
## bibdf[,2] ##
#'
#+ MAP
# MAP ----------------------------------------------------------------
MAP <- cbind(ID,
             BIBKEY,
             bibdf[, c("YEAR", "TITLE", "JOURNAL", "ABSTRACT")]) %>%
    as.data.frame()
## bibdf[c(3:5, 8)] ##
names(MAP)[-1] <- tolower(names(MAP)[-1])
#'
#+ MAP_RQDA, results='hide', fig.keep='none', fig.show='none'
# MAP-RQDA ----------------------------------------------------------------
source("MAPrqda.R", echo = FALSE)
#'
csid <- caseids[, c("caseid", "case", "RM", "scat")]
## caseids[, -3] ##
csid$case <- factor(csid$case)
