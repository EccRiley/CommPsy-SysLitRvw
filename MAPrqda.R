#' ---
#' title: "MAP - RQDA"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
# setwd("../../GitHub/comps-git")

source("../SETUP.R")
knitr::opts_chunk$set(
    tidy = FALSE,
    echo = FALSE,
    fig.keep = 'high',
    fig.show = 'hold',
    results = 'asis'
)
#'
library(RQDA); RQDA()
openProject("data/comps.RQDA", updateGUI = TRUE)
