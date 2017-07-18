#' ---
#' title: "Cluster Analysis - Sampling Settings \\& Methods"
#' author: "Rachel Smith-Hunter"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#' \Frule
#'
#+ echo=FALSE
knitr::opts_template$set(invisible = list(echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE, fig.keep='none', fig.show='none'))
knitr::opts_chunk$set(fig.path="graphics/cluster/rplot-sampMethods-")
#'
#+ srcbibs, opts.label='invisible'
source("bibs.R")
#'
#' \newpage
#'
#' # Cluster - Sampling Methods
#'

sm <- cb[cb$cat == "M-SAMPLING", ] %>% droplevels()
# sm2 <- cb[cb$cat == "M-SETTINGS", ] %>% droplevels()
# sm <- rbind(sm1, sm2)
sm$bibkey <- paste0("@", sm$bibkey)

Rtdf(sm$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(align = c("l", "r"))

smmat <- ftable(sm$bibkey, sm$cid) %>% as.matrix
smmatpr <- ftable(sm$bibkey, sm$clab) %>% as.matrix ## "ks4" == "bibkeys - s4" ##

smmatt <- t(smmat)

kable(Rmsmm(as.data.frame(smmatpr)))
kable(Rmsmm(as.data.frame(smmatt)))

smdist <- dist(smmat)
Rmsmm(smdist)


smclust <- hclust(smdist, method = "ward.D2")
# names(smclust)
# smclust$merge
# smclust$height
# smclust$order
# smclust$labels

palette(pal_my)
par(mar = c(0,2,2,1))
plot(smclust, sub = ' ', xlab = ' ', main = "Sampling Methods Clusters",
     frame.plot = F, col = 19, cex = .75, cex.main = 0.75,
     cex.lab = 0.75, cex.axis = 0.65)
abline(a = 3, b = 0, lty = 3, lwd = 1.5,
       col = adjustcolor(pal_my[19], alpha.f = 0.75))
rect.hclust(smclust, h = 3, border = "#3B0C67") -> rhcl

smGrps <- cutree(smclust, 3)
barplot(table(smGrps), col = pal_sci[1], border = pal_sci[1], main = "Cluster Member Counts - Sampling Methods", xlab = ' ', cex = 1.25, cex.main = 1.15, cex.lab = 1, cex.axis = 0.85)

as.data.frame(table(smGrps)) %>%
    dplyr::rename(Group = smGrps, Nmembers = Freq) %>% ## 'rename()' {dplyr} args: [output]=[input]
    kable(caption = "3-Group Solution Membership Counts (Sampling Methods)")
smmembers <- sapply(unique(smGrps),function(g) paste(rownames(smmat)[smGrps == g]))
names(smmembers) <- paste0("Group.", seq_along(smmembers))
smmembers <- t(t(smmembers))
names(smmembers) <- "Group Members"
pander(smmembers, caption = "Group Memberships Resulting from 4-Group Hierarchical Cluster Solution (Sampling Methods)")

library(cluster)
smkfit <- kmeans(smmat, 3)
palette(pal_sci)
clusplot(smmat, smkfit$cluster, main = '3-Cluster K-Means Solution (Sampling Methods)',
         color = T, shade = T,
         labels = 0, lines = 1, col.p = pal_my[19], font.main = 3, verbose = T, span = T)
pander(t(smkfit$centers), caption = "Per Variable Cluster Means ('centers') for 3-Cluster K-Means Solution (Sampling Methods)")

smkpam <- pam(smmat, 3) ## k = n-groups ##
smsil <- silhouette(smkpam)
plot(smsil, main = "Silhouette Plot of 3-Cluster Solution\n(Sampling Methods)", font.main = 3, do.n.k = T, col = mpal(1:4, p = grays), cex = 0.5)

smCluster <- c(seq(1:3))

smkpam.clus <- round(smkpam$clusinfo, 2)
smkpam.clus <- cbind(smCluster, smkpam.clus)

smkpam.cwidth <- smkpam$silinfo[2]
smkpam.cwidth <- cbind(smCluster, round(smkpam.cwidth[[1]], 2))

smkpam.sil <- round(smkpam$silinfo[[1]], 5)
smCase <- rownames(smkpam.sil)
smkpam.sil <- cbind(smCase, smkpam.sil)

kable(smkpam.clus, col.names = c("Cluster", "Size", "$Max_{Dissimilarity}$", "$\\mu_{Dissimilarity}$", "Diameter", "Separation"), align = c("c", rep("r", ncol(smkpam.clus) - 1)), caption = "K-pam Cluster Descriptives (Sampling Methods)")

names(smkpam$silinfo) <- c("Cluster Width", "$\\mu_{Cluster Width}", "\\mu_{Width_{Overall}}")
kable(smkpam.cwidth, caption = "Cluster Widths for 3-Cluster PAM Solution (Sampling Methods)", col.names = c("Cluster", "Width"), align = c("c", "r"))

kable(smkpam.sil, col.names = c("Case", "Cluster", "Neighbor", "Silhouette Width"), caption = "Silouette Information Per Case (Sampling Methods)", row.names = FALSE)
#'
#' \newpage
#'
#' # References
#'
#' \refs
#'
