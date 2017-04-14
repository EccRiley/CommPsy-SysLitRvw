#' ---
#' title: "MAP - Literature Description"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
# SETUP --------------------------------------------------------------

source("bibs.R")
knitr::opts_chunk$set(
    tidy = TRUE,
    echo = TRUE,
    fig.keep = 'high',
    fig.show = 'hold',
    results = 'asis',
    tidy.opts = list(comment = FALSE),
    echoRule = NULL,
    echoRuleb = TRUE
)
rpm()
#'
#'
hca.s3F <- ctbl.m[ctbl.m$scat == "IPV Interventions", c("case", "cat", "caseid", "year", "jrnl", "cid")]
hca.s3F$jrnl <- as.numeric(factor(hca.s3F$jrnl))
hca.s3m <- hca.s3F[hca.s3F$cat == "M-OVERALL", -1:-2] %>% droplevels()
hca.s3m$caseid %in% hca.s3F$casid
hca.s3m <- apply(hca.s3m, 2, as.double)

means <- apply(hca.s3m,2,mean)
sds <- apply(hca.s3m,2,sd)
hca.s3m <- scale(hca.s3m,center = means,scale = sds)
head(hca.s3m) %>% round(2)

dist.s3m <- dist(hca.s3m)
Rmsmm(dist.s3m)

clust.s3m <- hclust(dist.s3m, method = "ward.D2")
names(clust.s3m)
clust.s3m$merge
clust.s3m$height
clust.s3m$order
clust.s3m$labels <- unique(hca.s3F[, 1])
# clust.s3m$labels

plot(clust.s3m)#, labels = unique(hca.s3F$case), sub = ' ', xlab = ' ', main = " ",
     # frame.plot = F, col = 19, cex = .75, cex.main = 0.75,
     # cex.lab = 0.75, cex.axis = 0.65)
abline(a = 9, b = 0, lty = 3, lwd = 1.5,
       col = adjustcolor(pal_my[19], alpha.f = 0.75))
rect.hclust.s3m(clust.s3m, h = 9, border = "#3B0C67") -> rhcl
