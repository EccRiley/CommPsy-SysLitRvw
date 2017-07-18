#' ---
#' title: "Cluster Analysis - Focal Populations \\& Sampling Frames"
#' author: "Rachel Smith-Hunter"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#' \Frule
#'
#+ echo=FALSE
knitr::opts_template$set(invisible = list(echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE, fig.keep='none', fig.show='none'))
knitr::opts_chunk$set(fig.path="graphics/cluster/rplot-pops-")
#'
#+ srcbibs, opts.label='invisible'
source("bibs.R")
#'
#' \newpage
#'
#' # Cluster - Populations
#'
pop <- cb[cb$cat == "POPULATION", ] %>% droplevels()
pop$bibkey <- paste0("@", pop$bibkey)

Rtdf(pop$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(align = c("l", "r"))

popmat <- ftable(pop$bibkey, pop$cid) %>% as.matrix
popmatpr <- ftable(pop$bibkey, pop$clab) %>% as.matrix()

popmatt <- t(popmat)

kable(Rmsmm(as.data.frame(popmatpr)))
kable(Rmsmm(as.data.frame(popmatt)))

popdist <- dist(popmat)
Rmsmm(popdist)


popclust <- hclust(popdist, method = "ward.D2")
# names(popclust)
# popclust$merge
# popclust$height
# popclust$order
# popclust$labels

palette(pal_my)
par(mar = c(0,2,2,1))
plot(popclust, sub = ' ', xlab = ' ', main = "Populations Clusters",
     frame.plot = F, col = 19, cex = .75, cex.main = 0.75,
     cex.lab = 0.75, cex.axis = 0.65)
abline(a = 3, b = 0, lty = 3, lwd = 1.5,
       col = adjustcolor(pal_my[19], alpha.f = 0.75))
rect.hclust(popclust, h = 3, border = "#3B0C67") -> rhcl

popGrps <- cutree(popclust, 3)
barplot(table(popGrps), col = pal_sci[1], border = pal_sci[1], main = "Cluster Member Counts - Populations", xlab = ' ', cex = 1.25, cex.main = 1.15, cex.lab = 1, cex.axis = 0.85)

as.data.frame(table(popGrps)) %>%
    dplyr::rename(Group = popGrps, Nmembers = Freq) %>% ## 'rename()' {dplyr} args: [output]=[input]
    kable(caption = "3-Group Solution Membership Counts (Populations)")
popmembers <- sapply(unique(popGrps),function(g) paste(rownames(popmat)[popGrps == g]))
names(popmembers) <- paste0("Group.", seq_along(popmembers))
popmembers <- t(t(popmembers))
names(popmembers) <- "Group Members"
pander(popmembers, caption = "Group Memberships Resulting from 4-Group Hierarchical Cluster Solution (Populations)")

library(cluster)
popkfit <- kmeans(popmat, 3)
palette(pal_sci)
clusplot(popmat, popkfit$cluster, main = '3-Cluster K-Means Solution (Populations)',
         color = T, shade = T,
         labels = 0, lines = 1, col.p = pal_my[19], font.main = 3, verbose = T, span = T)
pander(t(popkfit$centers), caption = "Per Variable Cluster Means ('centers') for 3-Cluster K-Means Solution (Populations)")

popkpam <- pam(popmat, 3) ## k = n-groups ##
popsil <- silhouette(popkpam)
plot(popsil, main = "Silhouette Plot of 3-Cluster Solution\n(Populations)", font.main = 3, do.n.k = T, col = mpal(1:4, p = grays), cex = 0.5)

popCluster <- c(seq(1:3))

popkpam.clus <- round(popkpam$clusinfo, 2)
popkpam.clus <- cbind(popCluster, popkpam.clus)

popkpam.cwidth <- popkpam$silinfo[2]
popkpam.cwidth <- cbind(popCluster, round(popkpam.cwidth[[1]], 2))

popkpam.sil <- round(popkpam$silinfo[[1]], 5)
popCase <- rownames(popkpam.sil)
popkpam.sil <- cbind(popCase, popkpam.sil)

kable(popkpam.clus, col.names = c("Cluster", "Size", "$Max_{Dissimilarity}$", "$\\mu_{Dissimilarity}$", "Diameter", "Separation"), align = c("c", rep("r", ncol(popkpam.clus) - 1)), caption = "K-pam Cluster Descriptives (Populations)")

names(popkpam$silinfo) <- c("Cluster Width", "$\\mu_{Cluster Width}", "\\mu_{Width_{Overall}}")
kable(popkpam.cwidth, caption = "Cluster Widths for 3-Cluster PAM Solution (Populations)", col.names = c("Cluster", "Width"), align = c("c", "r"))

kable(popkpam.sil, col.names = c("Case", "Cluster", "Neighbor", "Silhouette Width"), caption = "Silouette Information Per Case (Populations)", row.names = FALSE)
#'
#' \newpage
#'
#' # References
#'
#' \refs
#'
