source("QCA.R")


cbt <- cb[cb$cat == "TOPIC",
          c("bibkey", "scat", "cat", "code", "clab")] %>% droplevels()
cbt <- cbt[!duplicated(cbt), ]
tlabs <- paste0(seq(1:length(levels(cbt$clab))), " = ", levels(cbt$clab))

lt <- mplvls[, 1:4]
lt$id <- rownames(lt)

lcbt <- merge(lt, cbt, by.x = "id", by.y = "bibkey", all = FALSE)
lcbt.t <- lcbt[, c("code", "l1", "l2", "l3", "l4")] %>% dplyr::rename("id" = code)
lcbt.t.nd <- lcbt.t[!duplicated(lcbt.t), ]



tlnet1 <- lcbt.t[, 1:2] %>% Rtdf(names = c(names(lcbt.t[,1:2]), "Freq"))
tlnet1$l1 <- as.numeric(tlnet1$l1)
tlnet1$l1 <- ifelse(tlnet1$l1 == 1, NA, "l1")
tlnet1$Freq <- ifelse(tlnet1$Freq == 0, NA, tlnet1$Freq)
tlnet1 <- na.omit(tlnet1)
# tlnet1 <- tlnet1[!duplicated(tlnet1$id), ]
names(tlnet1) <- c("from", "to", "Freq")
tlnet2 <- lcbt.t[, c(1, 3)] %>% Rtdf(names = c(names(lcbt.t[, c(1, 3)]), "Freq"))
tlnet2$l2 <- as.numeric(tlnet2$l2)
tlnet2$l2 <- ifelse(tlnet2$l2 == 1, NA, "l2")
tlnet2$Freq <- ifelse(tlnet2$Freq == 0, NA, tlnet2$Freq)
tlnet2 <- na.omit(tlnet2)
names(tlnet2) <- c("from", "to", "Freq")

tlnet3 <- lcbt.t[, c(1, 4)] %>% Rtdf(names = c(names(lcbt.t[,c(1, 4)]), "Freq"))
tlnet3$l3 <- as.numeric(tlnet3$l3)
tlnet3$l3 <- ifelse(tlnet3$l3 == 1, NA, "l3")
tlnet3$Freq <- ifelse(tlnet3$Freq == 0, NA, tlnet3$Freq)
tlnet3 <- na.omit(tlnet3)
names(tlnet3) <- c("from", "to", "Freq")

tlnet4 <- lcbt.t[, c(1, 5)] %>% Rtdf(names = c(names(lcbt.t[,c(1, 5)]), "Freq"))
tlnet4$l4 <- as.numeric(tlnet4$l4)
tlnet4$l4 <- ifelse(tlnet4$l4 == 1, NA, "l4")
tlnet4$Freq <- ifelse(tlnet4$Freq == 0, NA, tlnet4$Freq)
tlnet4 <- na.omit(tlnet4)
names(tlnet4) <- c("from", "to", "Freq")

tlnet0 <- rbind(tlnet1, tlnet2, tlnet3, tlnet4)
tlnet0$clab <- recode(tlnet0$from, rec.code2clab) ## "rec.code2clab" is from "MAPrqda.R"
# tlnet <- tlnet0[!duplicated(tlnet0), c("from", "to", "clab", "Freq")]

#+ tlnet_llabs
library(car)
llabs <- c("'l1' = '.Individual'; 'l2' = '.Relationship'; 'l3' = '.Community'; 'l4' = '.Societal'")
tlnet$to <- car::recode(tlnet$to, llabs)

#+ tlfrq

# tlfrq ----------------

tlfrq1 <- tlnet[, 1] %>% as.character()
tlfrq2 <- tlnet[, 2]
tlfrq3 <- c(tlfrq1, tlfrq2)
tlfrq <- Rtdf(tlfrq3, names = c("lvl", "Freq"))

tv1 <- tlnet[, 3] %>% as.character()
tv2 <- tlnet[, 2]
tv3 <- c(tv1, tv2)
tv <- Rtdf(tv3, names = c("id", "Freq"))
tv[, 1] <- as.character(tv[, 1])
# `tlnetg` ----------------

library(igraph)
tlnetg <- graph_from_data_frame(tlnet[, 1:2], directed = FALSE, vertices = tlfrq)
V(tlnetg)$size <- V(tlnetg)$Freq*3
tlnetcol <- mpal(tlfrq, a = 0.8)[-1:-4] %>% adjustcolor(alpha.f = 0.5)
tvclrs <- c(adjustcolor(vclrs[1:4], alpha.f = 0.65), tlnetcol)
V(tlnetg)$color <- tvclrs
# E(tlnetg)$width <- 0.25
E(tlnetg)$frq <- tlnet$Freq
E(tlnetg)$width <- log(tlnet$Freq) + 1
# V(tlnetg)$name[-1:-4] <- seq(1:length(tv[-1:-4, 1]))
V(tlnetg)$name[1:4] <- gsub("\\.", "", V(tlnetg)$name[1:4])
tindex.g <- V(tlnetg)$name %>% length()
tlblsize <- c(log(V(tlnetg)$size[1:4])*0.125, log(V(tlnetg)$size[5:tindex.g])*0.325)
#'
#'
#+ echo=FALSE
# panderOptions("p.wrap", "")
# tlabs <- gsub("\\n", "", tlabs)
tlabs <- gsub("&", "\\\\&", tlabs)
# tlabs1 <- paste0(tlabs[1:length(tlabs)-1], sep = ", ")
# deparse(tlabs1)
tlabs <- paste(tlabs, collapse = ", ")

tlnetg_cap <- paste0("Network Diagram Showing Relations among Substantive Research Topics (numbered graph nodes) Covered & Ecological Levels of Analysis (named graph nodes) Involved among the Reviewed Literature: \\textit{", tlabs, "}")
# PLOTS - `tlnetg` (layout-0 & layout-2) ----------------

#'
#+ arc_tlnetg, out.height='4in', fig.cap=tlnetg_cap

par(mar = rep(0, 4))
ltl <- layout_with_fr(tlnetg) %>% norm_coords()
plot(tlnetg, rescale = T, layout = ltl, edge.arrow.size = .2, vertex.label.color = "#1a1e22", vertex.frame.color = c(vclrs[1:4], rep(NA, length(tlnetcol))), vertex.label.cex = tlblsize)



l1tpsum <- tlnet[tlnet$to == "l1", "Freq"] %>% sum()
l2tpsum <- tlnet[tlnet$to == "l2", "Freq"] %>% sum()
l3tpsum <- tlnet[tlnet$to == "l3", "Freq"] %>% sum()
l4tpsum <- tlnet[tlnet$to == "l4", "Freq"] %>% sum()

l1tpWgtMu <- l1tpsum / length(unique(tlnet$from))
l2tpWgtMu <- l2tpsum / length(unique(tlnet$from))
l3tpWgtMu <- l3tpsum / length(unique(tlnet$from))
l4tpWgtMu <- l4tpsum / length(unique(tlnet$from))

l1tpRawMu <- tlnet[tlnet$to == "l1", "Freq"] %>% mean()
l2tpRawMu <- tlnet[tlnet$to == "l2", "Freq"] %>% mean()
l3tpRawMu <- tlnet[tlnet$to == "l3", "Freq"] %>% mean()
l4tpRawMu <- tlnet[tlnet$to == "l4", "Freq"] %>% mean()

tpMuAll <- mean(tlnet$Freq)
l1tpRawMu/tpMuAll
l1tpWgtMu/tpMuAll
l2tpRawMu/tpMuAll
l2tpWgtMu/tpMuAll
