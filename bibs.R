#' ---
#' title: "BibTeX Entry Types and Fields"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R")
knitr::opts_chunk$set(
    tidy = TRUE,
    echo = TRUE,
    cache = FALSE,
    fig.keep = 'high',
    fig.show = 'hold',
    results = 'asis',
    autodep = TRUE,
    Rplot = TRUE,
    dev = 'pdf',
    fig.path = 'graphics/rplot-',
    fig.width = 7,
    fig.height = 7,
    out.width = "\\linewidth"
    )
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

bib4 <- readLines("MAP-4.bib") ## Search 4 ('S4') ##
keys4 <- bib4[grep("\\@.*?\\{.*?,", bib4, perl = TRUE)]
keys4 <- gsub("\\@\\w+\\{(.*?)", "\\1", keys4, perl = TRUE)
keys4 <- keys4[!grepl("\\%.*?,", keys4, perl = TRUE)]
keys4 <- gsub(" ", NA_character_, keys4)
keys4 <- gsub(",", "", keys4) %>% data.frame
names(keys4) <- "BIBKEY"
bibdf4 <- bib2df("MAP-4.bib")
MAP4 <- cbind(keys4, bibdf4) %>% data.frame
MAP4$citedBy <- c(0, 0, 2, 2, 0, 0, 3, 4, 2, 4, 4, 2, 4, 3, 67, 8, 18, 9, 4, 17, 44, 34, 35, 20, 55, 12, 24, 29, 24, 46, 229, 76, 29, 9, 12, 94, 9, 141, 14, 24)
MAP4$JOURNAL <- sapply(MAP4$JOURNAL, tolower)
MAP4$Hscore <- car::recode(MAP4$JOURNAL, rc)

MAP4.cp <- MAP4[MAP4$JOURNAL %in% dat$JOURNAL, ] %>% data.frame


yr <- cbind(density(MAP4$YEAR)$x, density(MAP4$YEAR)$y*200)
hist(MAP4$YEAR, col = adjustcolor(mypal[16], alpha = 0.75), main = " ", xlab = "Year Published", lwd=.5, right = FALSE); lines(yr, lwd = 2, col = mypal[20], lty = 3)

ct <- cbind(density(MAP4$citedBy)$x, density(MAP4$citedBy)$y*1400)
hist(MAP4$citedBy, col = adjustcolor(mypal[16], alpha = 0.75), main = " ", xlab = expression(N[Citations]), lwd=.5); lines(ct, lwd = 2, col = mypal[20], lty = 3)

with(MAP4, {lm2 <- lm(citedBy ~ YEAR); cl <- mpal(citedBy, p = cols3); bg <- adjustcolor(cl, alpha.f = 0.6); plot(YEAR, citedBy, col = cl, bg = bg, pch = 21, xlab = "Year Published", ylab = expression(N[Citations])); abline(lm2, lwd = 2.5, col = mypal[18])})
#'
#'