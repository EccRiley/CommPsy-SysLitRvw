#' ---
#' title: "Cluster Analysis - Full Codebook"
#' author: "Rachel Smith-Hunter"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#' \Frule
#'
#+ echo=FALSE
knitr::opts_template$set(invisible = list(echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE, fig.keep='none', fig.show='none'))
knitr::opts_chunk$set(fig.path="graphics/cluster/rplot-")
#'
#+ srcbibs, opts.label='invisible'
source("bibs_html.R", verbose = FALSE)
#'
dat <- cb[!duplicated(cb), ]
dat <- within(dat, {

})
dat$bibkey <- paste0("@", dat$bibkey)

Rtdf(dat$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(align = c("l", "r"))

mat <- ftable(dat$bibkey, dat$cid) %>% as.matrix
matpr <- ftable(dat$bibkey, dat$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##
matt <- t(mat)

kable(Rmsmm(as.data.frame(matpr)))
kable(Rmsmm(as.data.frame(matt)))

dist <- dist(matt)
Rmsmm(dist)


clust <- hclust(dist, method = "ward.D2")
names(clust)
clust$merge
clust$height
clust$order
clust$labels

#+ hclust_all, fig.fullwidth=TRUE
palette(pal_my); par(mar = c(0, 2, 2, 1))
plot(clust, sub = ' ', xlab = ' ', main = " ",
     frame.plot = F, col = 19, cex = .75, cex.main = 0.75,
     cex.lab = 0.75, cex.axis = 0.65)
abline(a = 7, b = 0, lty = 3, lwd = 1.5,
       col = adjustcolor(pal_my[19], alpha.f = 0.75))
rect.hclust(clust, h = 7, border = "#3B0C67") -> rhcl

Grps <- cutree(clust, 4)
barplot(table(Grps), col = , border = 18, main = " ", xlab = ' ', cex = 1.25, cex.main = 1.15, cex.lab = 1, cex.axis = 0.85)
#'
#' \newpage
#'
as.data.frame(table(Grps)) %>%
    dplyr::rename(Group = Grps, Nmembers = Freq) %>% ## 'rename()' {dplyr} args: [output]=[input]
    kable(caption = "4-Group Solution Membership Counts")
members <- sapply(unique(Grps),function(g) paste(car::recode(rownames(matt), rec.cid2clab)[Grps == g]))

names(members) <- paste0("Group.", seq_along(members))
members <- t(t(members))
names(members) <- "Group Members"
#'
#'
#+ echo=FALSE
kable(data.frame(members[1, ]), caption = "Group-1 Members")

kable(data.frame(members[2, ]), caption = "Group-2 Members")

kable(data.frame(members[3, ]), caption = "Group-3 Members")

kable(data.frame(members[4, ]), caption = "Group-4 Members")
#'
#' \newpage
#'
#+ kclust_all
library(cluster)
kfit <- kmeans(matt, 4)
palette(pal_sci)
clusplot(matt, kfit$cluster, main = '4-Cluster K-Means Solution',
         color = T, shade = T,
         labels = 0, lines = 1, col.p = pal_my[19], font.main = 3, verbose = T, span = T)
kable(t(kfit$centers), caption = "Per Variable Cluster Means ('centers') for 4-Cluster K-Means Solution")
#'
#' \newpage
#'
#' # References
#'
#' \refs
#'
