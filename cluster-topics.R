#' ---
#' title: "Cluster Analysis - Research Topics"
#' author: "Rachel Smith-Hunter"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#' \Frule
#'
#+ echo=FALSE
knitr::opts_template$set(invisible = list(echo = FALSE, results = 'hide', message = FALSE, warning = FALSE, cache = FALSE, fig.keep = 'none', fig.show = 'none'))

knitr::opts_chunk$set(fig.path = "graphics/cluster/rplot-tops-")
#'
#+ srcbibs, opts.label='invisible'
source("bibs.R")
#'
#' \newpage
#'
#' # Cluster - Topics
#'
top <- cb[cb$cat == "TOPIC", ] %>% droplevels()
top <- within(top, {
    cid <- ifelse(top$cid %in% tpFilter, cid, NA)
})

top <- within(top, {
    bibkey2 <- ifelse(bibkey == "boal2014barriers", "boala2014barriers", bibkey)
    bibkey2 <- ifelse(bibkey2 == "boal2014impact", "boalb2014impact", bibkey2)
    bibkey2 <- gsub("(\\w+)(\\d{4})\\w+", "\\1 (\\2)", bibkey2)
    bibkey2 <- ifelse(bibkey2 == "boala (2014)", "boal (2014a)", bibkey2)
    bibkey2 <- ifelse(bibkey2 == "boalb (2014)", "boal (2014b)", bibkey2)
    bibkey2 <- sapply(bibkey2, RtCap, USE.NAMES = FALSE)
    bibkey3 <- paste0("@", bibkey)
    clab <- gsub(" \\(General\\) ", "", clab)
    clab <- gsub(" \\(General\\)", "", clab)
    clab <- gsub("IPV Victimization", "IPV - V", clab)
    clab <- gsub("IPV Perpetration", "IPV - P", clab)
    clab <- gsub("IPV Perpetrator", "IPV - P", clab)
    clab <- gsub("Victims'/Survivors'", "IPV - V", clab)
    clab <- gsub("IPV Intervention", "Intervention", clab)
    clab <- gsub("Intervention Program", "Intervention", clab)
    clab <- gsub("Intervention/Prevention", "Intervention", clab)
    clab <- gsub("Evaluation", "Eval.", clab)
    clab <- gsub("Interventions", "Intervention", clab)
    clab <- gsub("Coordinated Community Response to IPV", "CCR", clab)
    clab <- gsub("Key Stakeholders", "Stakeholders", clab)
})
rec.clab2cid2 <- paste0("\"", top$clab, "\" = \"", top$cid, "\"", collapse = "; ")

top <- na.omit(top)
# top$bibkey.pr <- paste0("@", top$bibkey)
# rec.key2pr <- paste0("\"", top$bibkey, "\" = \"", top$bibkey.pr, "\"", collapse = "; ")

Rtdf(top$clab, names = c(" ", "$N_{Articles}$")) %>%
    kable(align = c("l", "r"))

topmat <- ftable(top$bibkey2, top$cid) %>% as.matrix()
topmatpr <- ftable(top$bibkey3, top$clab) %>% as.matrix() ## "ks4" == "bibkeys - s4" ##

# topmatt <- t(topmat)
topmatt <- ftable(top$bibkey2, top$clab) %>% as.matrix() %>% t() ## "ks4" == "bibkeys - s4" ##
topmattpr <- ftable(top$bibkey3, top$clab) %>% as.matrix() %>% t() ## "ks4" == "bibkeys - s4" ##

kable(Rmsmm(as.data.frame(topmatpr)))
kable(Rmsmm(as.data.frame(topmattpr)))

topdist <- dist(topmat)
Rmsmm(topdist)
topclust <- hclust(topdist, method = "ward.D2")
# names(topclust)
# topclust$merge
# topclust$height
# topclust$order
# topclust$labels <- paste0("@", topclust$labels)

palette(pal_my)
par(mar = c(0,2,0.7,0))
plot(topclust, sub = ' ', xlab = ' ', main = "Bibkeys Clustered by Topics",
     frame.plot = F, col = 20, col.main = 18, col.axis = 18,
     cex = 0.8, cex.main = 1, cex.axis = 0.65, lwd = 1.5,
     font.main = 3)
abline(a = 3, b = 0, lty = 3, lwd = 1.5,
       col = adjustcolor(pal_my[19], alpha.f = 0.75))
rect.hclust(topclust, h = 3, border = "#3B0C67") -> rhcl
par(op)
topGrps <- cutree(topclust, 3)
barplot(table(topGrps), col = pal_sci[1], border = pal_sci[1], main = "Cluster Member Counts - Topics", xlab = ' ', cex = 1.25, cex.main = 1.15, cex.lab = 1, cex.axis = 0.85)

as.data.frame(table(topGrps)) %>%
    dplyr::rename(Group = topGrps, Nmembers = Freq) %>% ## 'rename()' {dplyr} args: [output]=[input]
    kable(caption = "3-Group Solution Membership Counts (Topics)")
topmembers <- sapply(unique(topGrps),function(g) paste(rownames(topmat)[topGrps == g]))
names(topmembers) <- paste0("Group.", seq_along(topmembers))
topmembers <- t(t(topmembers))
names(topmembers) <- "Group Members"
pander(topmembers, caption = "Group Memberships Resulting from 4-Group Hierarchical Cluster Solution (Topics)")

library(cluster)
topkfit <- kmeans(topmat, 5)
p <- pal_sci[c(1, 2, 3, 5, 8)]
clusplot(topmat, topkfit$cluster, main = '3-Cluster K-Means Solution (Topics)',
         color = T, shade = T, plotchar = F, cex.txt = 0.5,
         col.clus = p, pch = 21, bg = adjustcolor(pal_my[19], alpha.f = .25),
         labels = 4, lines = 0, col.p = pal_my[19], font.main = 3, verbose = T, span = T)
pander(t(topkfit$centers), caption = "Per Variable Cluster Means ('centers') for 3-Cluster K-Means Solution (Topics)")

topkpam <- pam(topmat, 3) ## k = n-groups ##
topsil <- silhouette(topkpam)
plot(topsil, main = "Silhouette Plot of 3-Cluster Solution\n(Topics)", font.main = 3, do.n.k = T, col = mpal(1:4, p = grays), cex = 0.5)

topCluster <- c(seq(1:3))

topkpam.clus <- round(topkpam$clusinfo, 2)
topkpam.clus <- cbind(topCluster, topkpam.clus)

topkpam.cwidth <- topkpam$silinfo[2]
topkpam.cwidth <- cbind(topCluster, round(topkpam.cwidth[[1]], 2))

topkpam.sil <- round(topkpam$silinfo[[1]], 5)
topCase <- rownames(topkpam.sil)
topkpam.sil <- cbind(topCase, topkpam.sil)

kable(topkpam.clus, col.names = c("Cluster", "Size", "$Max_{Dissimilarity}$", "$\\mu_{Dissimilarity}$", "Diameter", "Separation"), align = c("c", rep("r", ncol(topkpam.clus) - 1)), caption = "K-pam Cluster Descriptives (Topics)")

names(topkpam$silinfo) <- c("Cluster Width", "$\\mu_{Cluster Width}", "\\mu_{Width_{Overall}}")
kable(topkpam.cwidth, caption = "Cluster Widths for 3-Cluster PAM Solution (Topics)", col.names = c("Cluster", "Width"), align = c("c", "r"))

kable(topkpam.sil, col.names = c("Case", "Cluster", "Neighbor", "Silhouette Width"), caption = "Silouette Information Per Case (Topics)", row.names = FALSE)

#'
#' -----
#'
#' # Which topics cluster with other topics?
#'
#'
top2dist <- dist(topmatt)

Rmsmm(top2dist)


top2clust <- hclust(top2dist, method = "ward.D2")
# names(top2clust)
# top2clust$merge
# top2clust$height
# top2clust$order
# top2clust$labels

palette(pal_my)
par(mar = c(0,2,0.7,0))
plot(top2clust, sub = ' ', xlab = ' ', main = "Topics Clustered by Bibkeys",
     frame.plot = F, col = 20, col.main = 18, col.axis = 18,
     cex = 0.65, cex.main = 1, cex.axis = 0.65, lwd = 1.5,
     font.main = 3)
abline(a = 4, b = 0, lty = 3, lwd = 1.5,
       col = adjustcolor(pal_sci[2], alpha.f = 0.75))
# rect.hclust(top2clust, h = 4,
            # border = adjustcolor(pal_sci[2], alpha.f = 0.75)) -> rhcl.h4
abline(a = 3.5, b = 0, lty = 3, lwd = 1.5,
       col = adjustcolor(pal_sci[4], alpha.f = 0.75))
# rect.hclust(top2clust, h = 3.5,
            # border = adjustcolor(pal_sci[4], alpha.f = 0.75)) -> rhcl.h35
abline(a = 3, b = 0, lty = 3, lwd = 1.5,
       col = adjustcolor(pal_sci[5], alpha.f = 0.75))
# rect.hclust(top2clust, h = 3,
            # border = adjustcolor(pal_sci[5], alpha.f = 0.75)) -> rhcl.h3
abline(a = 2.8, b = 0, lty = 3, lwd = 1.5,
       col = adjustcolor(pal_sci[8], alpha.f = 0.75))
rect.hclust(top2clust, h = 2.8,
            border = adjustcolor(pal_sci[8], alpha.f = 0.75)) -> rhcl.h25
# names(rhcl.h25) <- paste0("c", seq_along(rhcl.h25))
# f <- function(x) { car::recode(x, rec.clab2cid2) }
par(op)
top2clusts <- list()
for (i in 1:length(rhcl.h25)) {
    top2clusts[[i]] <- car::recode(names(rhcl.h25[[i]]), rec.clab2cid2)
}
 # <- if
# for (i in 1:length(top2clusts)) {
#     top2clusts[[i]] <- top[top$cid %in% top2clusts[[i]], "bibkey"] %>% unique()
# }

top2 <- within(top, {
    tp_c1 <- ifelse(cid %in% top2clusts[[1]], 1, 0)
    tp_c2 <- ifelse(cid %in% top2clusts[[2]], 1, 0)
    tp_c3 <- ifelse(cid %in% top2clusts[[3]], 1, 0)
    tp_c4 <- ifelse(cid %in% top2clusts[[4]], 1, 0)
    tp_c5 <- ifelse(cid %in% top2clusts[[5]], 1, 0)
    tp_clust <- ifelse(cid %in% top2clusts[[1]], "C-1", NA)
    tp_clust <- ifelse(cid %in% top2clusts[[2]], "C-2", tp_clust)
    tp_clust <- ifelse(cid %in% top2clusts[[3]], "C-3", tp_clust)
    tp_clust <- ifelse(cid %in% top2clusts[[4]], "C-4", tp_clust)
    tp_clust <- ifelse(cid %in% top2clusts[[5]], "C-5", tp_clust)
})

top2bib <- top2[, c("bibkey2", "code", paste0("tp_c", 1:5), "tp_clust")]
# top2bib <- top2bib[!duplicated(top2bib), ]
topmatbib <- ftable(top2bib$bibkey2, top2bib$tp_clust) %>% as.matrix()

barcol <- adjustcolor(pal_sci[c(2, 4, 5, 8)], alpha.f = 0.65)
top2Grps <- cutree(top2clust, 5)
barplot(table(top2Grps), col = barcol, border = pal_sci[c(2, 4, 5, 8)], main = "Cluster Member Counts - Topics", xlab = ' ', cex = 1.25, cex.main = 1.15, cex.lab = 1, cex.axis = 0.85)

# as.data.frame(table(top2Grps)) %>%
    # dplyr::rename(Group = top2Grps, Nmembers = Freq) %>% ## 'rename()' {dplyr} args: [output]=[input]
    # kable(caption = "3-Group Solution Membership Counts (Topics)")
# top2members <- sapply(unique(top2Grps),function(g) paste(car::recode(rownames(topmatt), rec.cid2clab)[top2Grps == g]))
# names(top2members) <- paste0("Group.", seq_along(top2members))
# top2members <- t(t(top2members))
# names(top2members) <- "Group Members"
# pander(top2members, caption = "Group Memberships Resulting from 4-Group Hierarchical Cluster Solution (Topics)")

top2kpam <- pam(topmatt, 5) ## k = n-groups ##
top2sil <- silhouette(top2kpam)
plot(top2sil, main = "Silhouette Plot of 3-Cluster Solution\n(Topics)", font.main = 3, do.n.k = T, col = mpal(1:4, p = grays), cex = 0.5)

top2Cluster <- c(seq(1:3))

top2kpam.clus <- round(top2kpam$clusinfo, 2)
top2kpam.clus <- cbind(top2Cluster, top2kpam.clus)

top2kpam.cwidth <- top2kpam$silinfo[2]
top2kpam.cwidth <- cbind(top2Cluster, round(top2kpam.cwidth[[1]], 2))

top2kpam.sil <- round(top2kpam$silinfo[[1]], 5)
top2Case <- rownames(top2kpam.sil)
top2kpam.sil <- cbind(top2Case, top2kpam.sil)

kable(top2kpam.clus, col.names = c("Cluster", "Size", "$Max_{Dissimilarity}$", "$\\mu_{Dissimilarity}$", "Diameter", "Separation"), align = c("c", rep("r", ncol(top2kpam.clus) - 1)), caption = "K-pam Cluster Descriptives (Topics)")

names(top2kpam$silinfo) <- c("Cluster Width", "$\\mu_{Cluster Width}", "\\mu_{Width_{Overall}}")
kable(top2kpam.cwidth, caption = "Cluster Widths for 3-Cluster PAM Solution (Topics)", col.names = c("Cluster", "Width"), align = c("c", "r"))

kable(top2kpam.sil, col.names = c("Case", "Cluster", "Neighbor", "Silhouette Width"), caption = "Silouette Information Per Case (Topics)", row.names = FALSE)

#'
#' \newpage
#'
#+
topbibdist <- dist(topmatbib)
topbibclust <- hclust(topbibdist, method = "ward.D2")
palette(pal_my)
par(mar = c(0,2,0.7,0))
plot(topbibclust, sub = ' ', xlab = ' ', main = "Bibkey Clusters based on Topics Clusters",
     frame.plot = F, col = 20, col.main = 18, col.axis = 18,
     cex = 0.65, cex.main = 1, cex.axis = 0.65, lwd = 1.5,
     font.main = 3)
abline(a = 2.8, b = 0, lty = 3, lwd = 1.5,
       col = pal_sci[8])
# rect.hclust(top2clust, h = 2.8,
#             border = adjustcolor(pal_my[18], alpha.f = 0.75))
rect.hclust(hclust(dist(topmatbib), method = "ward.D2"),
            h = 2.8, border = pal_sci[8]) -> rhcl_bib

topmatbib2 <- ftable(top2$bibkey3, top2bib$tp_clust) %>% as.matrix()
# topmatbib2 <- ifelse(topmatbib2 == 0, topmatbib2)
kable(topmatbib2, caption = "Bibkey Clusters based on Topics Clusters", format.args = list(zero.print = ".") )

c1 <- names(rhcl.h25[[1]]) %>% as.list
c2 <- names(rhcl.h25[[2]]) %>% as.list
c3 <- names(rhcl.h25[[3]]) %>% as.list
c4 <- names(rhcl.h25[[4]]) %>% as.list
c5 <- names(rhcl.h25[[5]]) %>% as.list

cat(tufte::newthought("'C-1 Topics:"))

pander(c1)

cat(tufte::newthought("'C-2 Topics:"))

pander(c2)

cat(tufte::newthought("'C-3 Topics:"))

pander(c3)

cat(tufte::newthought("'C-4 Topics:"))

pander(c4)

cat(tufte::newthought("'C-5 Topics:"))

pander(c5)

topmatcodes <- ftable(top2$clab, top2$tp_clust) %>% as.matrix()

topmatcodespr <- ifelse(topmatcodes == 0, "$\\cdot$", "$\\checkmark$")
kable(topmatcodespr, caption = "Topic Clusters' Membership")

library(cluster)
topbib2fanny <- fanny(topmatbib, 5)

ktopbib <- fanny(topmatbib, 2)
clusplot(topmatbib, ktopbib$cluster, main = '3-Cluster K-Means Solution (Topics)',
         color = T, shade = T, plotchar = F, cex.txt = 0.5,
         col.clus = p, pch = 21, bg = adjustcolor(pal_my[19], alpha.f = .25),
         labels = 2, lines = 0, col.p = pal_my[19], font.main = 3, verbose = T, span = T)
pander(t(topkfit$centers), caption = "Per Variable Cluster Means ('centers') for 3-Cluster K-Means Solution (Topics)")

topmatbib3 <- ftable(top2$bibkey2, top2bib$tp_clust) %>% as.matrix()
tmb_c1 <- ktopbib$cluster[ktopbib$cluster == 1] %>% names()
tmb_c2 <- ktopbib$cluster[ktopbib$cluster == 2] %>% names()

tmb_c1mat <- topmatbib3[rownames(topmatbib3) %in% tmb_c1, ]
apply(tmb_c1mat, 2, sum)
tmb_c2mat <- topmatbib3[rownames(topmatbib3) %in% tmb_c2, ]
apply(tmb_c2mat, 2, sum)
# topmatbib2 <- ifelse(topmatbib2 == 0, topmatbib2)
kable(topmatbib3[rownames(topmatbib3) %in% tmb_c1, ], caption = "Cluster - 1 Bibkeys", format.args = list(zero.print = ".") )
kable(topmatbib3[rownames(topmatbib3) %in% tmb_c2, ], caption = "Cluster - 2 Bibkeys", format.args = list(zero.print = ".") )

levels(top2$scat) <- c("IPV Interventions", "SMW-Inclusive")
ftable(top2$tp_clust, top2$scat)

top2$tmb_clust <- ifelse(top2$bibkey2 %in% tmb_c1, "A", "B")
top2$tmb_c1 <- ifelse(top2$bibkey2 %in% tmb_c1, 1, 0)
top2$tmb_c2 <- ifelse(top2$bibkey2 %in% tmb_c2, 1, 0)

ftable(top2[top2$tmb_clust == "A", c("bibkey2", "code")]) %>% as.matrix() %>% pander()

ftable(top2[top2$tmb_clust == "B", c("bibkey2", "code")]) %>% as.matrix() %>% pander()

#'
#' \newpage
#'
#' # References
#'
#' \refs
