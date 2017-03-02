#' ---
#' title: "BibTeX Entry Types and Fields"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R")
knitr::opts_chunk$set( tidy = TRUE, echo = TRUE, cache = FALSE, fig.keep = 'high', fig.show = 'hold', results = 'asis', autodep = TRUE, Rplot = TRUE, dev = 'pdf', fig.path = 'graphics/rplot-', fig.width = 7, fig.height = 7, out.width = "\\linewidth" )
rpm()
#'
#' \newpage
#'
#+ journals, echo=TRUE
source("journals.R", echo = TRUE)
dat$JOURNAL <- sapply(dat$JOURNAL, tolower) ## "dat" created in "journals.R" ##
#'
#+ bibdf, echo=TRUE
library(bib2df)

bib <- readLines("MAP.bib")
keys <- bib[grep("\\@.*?\\{.*?,", bib, perl = TRUE)]
keys <- gsub("\\@\\w+\\{(.*?)", "\\1", keys, perl = TRUE)
keys <- keys[!grepl("\\%.*?,", keys, perl = TRUE)]
keys <- gsub(" ", NA_character_, keys)
keys <- gsub(",", "", keys) %>% data.frame
names(keys) <- "BIBKEY"
bibdf <- bib2df("MAP.bib")
MAP <- cbind(keys, bibdf) %>% data.frame
MAP$JOURNAL <- sapply(MAP$JOURNAL, tolower)
MAP.cp <- MAP$JOURNAL %in% dat$JOURNAL
MAP.cpp <- MAP[MAP.cp, ]

yr <- cbind(density(MAP$YEAR)$x, density(MAP$YEAR)$y*200)
hist(MAP$YEAR, col = adjustcolor(mypal[16], alpha = 0.75), main = " ", xlab = "Year Published", lwd=.5, right = FALSE); lines(yr, lwd = 2, col = mypal[20], lty = 3)
#'