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
