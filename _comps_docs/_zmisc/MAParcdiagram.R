# ctbl.gr <- cb[, c("caseid", "bibkey", "scat", "cpv", "journal", "cat", "code", "year")]

source("bibs.R")
tp <- cb[cb$cat == "TOPIC", ]
# tp$code <- droplevels(tp$code)
# tp$clab <- droplevels(tp$clab)
# tp$bibkey <- droplevels(tp$bibkey)

edges.tp1 <- cbind(tp$scat, tp$code)
edges.tp2 <- cbind(tp$scat, tp$cpv)
library(arcdiagram)
arcplot(edges.tp1)
arcplot(edges.tp2)

source("MAPlvls.R")
sedges <- cbind(snet$from, snet$to)
svals <- V(snetg)$Freq
sdeg <- degree(snetg)
sarcs <- .5*(snetft %>% matrix())
sarcs <- ifelse(sarcs == 0, NA, sarcs) %>% na.omit()

arcplot(sedges, col.arcs = hsv(0, 0, 0.1, 0.06), pch.nodes = 21, bg.nodes = snetcol, cex.nodes = log(sdeg[c("Micro", "Meso-Exo", "Exo-Macro")])+1, col.nodes = snetcol, lwd.arcs = sarcs, line = 1, cex.labels = 0.8)

ledges <- cbind(lnet$from, lnet$to)
lvals <- V(lnetg)$Freq
ldeg <- degree(lnetg)
larcs <- .85*(lnetft %>% matrix())
larcs <- ifelse(larcs == 0, NA, larcs) %>% na.omit()

arcplot(ledges, col.arcs = hsv(0, 0, 0.1, 0.075), pch.nodes = 21, bg.nodes = lnetcol, cex.nodes = log(ldeg[c("Individual", "Relationship", "Community", "Societal")]), col.nodes = lnetcol, lwd.arcs = larcs, line = 1, cex.labels = 0.8)

MAPeco <- merge(MAP, mplvls1, by.x = "bibkey", by.y = "key", all = TRUE)[, c("bibkey", "jrnl", "year", "journal", "scat", "prop", "j.loc", "j.year", "SJR", "Hindex", "cpv", "l1", "l2", "l3", "l4", "exo_macro", "meso_exo", "micro", "nlvls", "nsys")]
