# tmaplvls <- t(maplvls[, -1]) %>% data.frame()
# rownames(tmaplvls) <- eco[, 1]

# mplvls$lvl <- ifelse(mplvls[, 1] == 1, lvls[1], NA)

# flvl <- function(x, lvls, yval = 1, nval = NA) {
#     ## this is essentially a way to create a single (factor) variable/column
#     ## from a set of dichotomous (1, 0) variables/columns
#     ## - 'x' is the set of dichotomous vars;
#     ## 'lvls' is the set of values to code each var in 'x' where x$var == 1
#     ## thus, ncol(x) & length(lvls) should be the same length ##
#     nvar <- max(apply(x, 1, sum))
#     # y <- vector(mode = 'character', length = nvar)
#     y <- x
#     y[, 1] <- ifelse(x[, 1] == 1, lvls[1], NA)
#     for (i in 1:length(lvls)) {
#         # y[i] <- rep(NA, nrow(x))
#         y[, i] <- ifelse(x[, i] == 1, lvls[i], y[, i])
#     }
#     return(y)
# }
# xlvls <- flvl(x = x, lvls = lvls)

# x$id <- rownames(x)
# xlong <- reshape(x, varying = 1:4, direction = 'long', sep = "")
# xlong <- dplyr::rename(xlong, lvl = time, ynlvl = l)

# l <- layout.sphere(xnetg)
# plot(xnetg, rescale = T, layout = l, edge.arrow.size = .2, vertex.label.color = "#1a1e22")
# ln <- layout.norm(l)#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
# plot(xnetg, rescale = F, layout = ln, edge.arrow.size = .2, vertex.label.color = "#1a1e22")

# l <- layout.mds(xnetg)
# plot(xnetg, rescale = T, layout = l, edge.arrow.size = .2, vertex.label.color = "#1a1e22")
# ln <- layout.norm(l)#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
# plot(xnetg, rescale = F, layout = ln, edge.arrow.size = .2, vertex.label.color = "#1a1e22")

# l <- layout.davidson.harel(xnetg)
# plot(xnetg, rescale = T, layout = l, edge.arrow.size = .2, vertex.label.color = "#1a1e22")
# ln <- layout.norm(l)#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
# plot(xnetg, rescale = F, layout = ln, edge.arrow.size = .2, vertex.label.color = "#1a1e22")

#
# l <- layout.drl(xnetg)
# plot(xnetg, rescale = T, layout = l, edge.arrow.size = .2, vertex.label.color = "#1a1e22")
# ln <- layout.norm(l)#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
# plot(xnetg, rescale = F, layout = ln, edge.arrow.size = .2, vertex.label.color = "#1a1e22")

# l <- layout.kamada.kawai(xnetg)
# plot(xnetg, rescale = T, layout = l, edge.arrow.size = .2, vertex.label.color = "#1a1e22")
# ln <- layout.norm(l)#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
# plot(xnetg, rescale = F, layout = ln, edge.arrow.size = .2, vertex.label.color = "#1a1e22")

#
# l <- layout.lgl(xnetg)
# plot(xnetg, rescale = T, layout = l, edge.arrow.size = .2, vertex.label.color = "#1a1e22")
# ln <- layout.norm(l)#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
# plot(xnetg, rescale = F, layout = ln, edge.arrow.size = .2, vertex.label.color = "#1a1e22")

# lnetm <- merge(lnet, lnetft, all.x = TRUE)
# V(lnetg)$label.size <- (V(lnetg)$Freq*0.02)*0.65
# plot(lnetg, rescale = TRUE)

# arcplot(as.matrix(llong))

#+ keysnet2, fig.fullwidth=TRUE
# library(igraph)
# lsys <- c("'Individual' = 'Micro'; 'Relationship' = 'Micro-Meso'; 'Community' = 'Meso-Exo'; 'Societal' = 'Exo-Macro'")
# slong <- within(llong1, {
#     lvl <- car::recode(lvl, lsys)
# })
#
# slongv <- within(llongv, {
#     x <- car::recode(x, lsys)
# })
#
# slongg <- graph_from_data_frame(slong, directed = FALSE, vertices = slongv)
#
# V(slongg)$size <- V(slongg)$Freq
# V(slongg)$color <- adjustcolor(V(slongg)$vclr, alpha.f = 0.5)
# V(slongg)$frame.color <- V(slongg)$border
# #mpal(1:length(unique(llongv3[, 1])))
#
# # lv <- cbind(names(V(llongg)), V(llongg)$Freq) %>% data.frame()
# # rep(V(llongg)$Freq, V(llongg)$Freq)
# # lv$Freq <- as.integer(lv$Freq)
# E(slongg)$width <- 0.35
#
# lsfr <- layout_with_fr(slongg)
# lsn <- norm_coords(lsfr)#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
# plot(slongg, rescale = T, layout = lsn, edge.arrow.size = 0.5, vertex.label.color = "#1a1e22", vertex.label.cex = log(V(slongg)$size)*0.35)

# llongv1$vclr <- ifelse(llongv1$lvl == "Individual", vclrs[1], llongv1$lvl)
# llongv1$vclr <- ifelse(llongv1$lvl == "Relationship", vclrs[2], llongv1$vclr)
# llongv1$vclr <- ifelse(llongv1$lvl == "Community", vclrs[3], llongv1$vclr)
# llongv1$vclr <- ifelse(llongv1$lvl == "Societal", vclrs[4], llongv1$vclr)
# llongv1$Freq <- ifelse(llongv1$Freq == 0, NA, llongv1$Freq)
# llongv <- na.omit(llongv1)
# llongv1$lvl <- car::recode(llongv1$lvl, llabs1)
# llongv2$key <- gsub("(\\w+)\\d{4}\\w+", "\\1", llongv2$key)
# llongv2$key <- sapply(llongv2$key, RtCap)
# llongv <- merge(llongv3, llong1, by.y = "id", by.x = "x", all = T)

# vclrs <- mpal(1:length(unique(llong[, 2])))
# llongv3 <- Rtdf(llong[, "scat"], names = c("x", "Freq"))
# scat <- car::recode(scat, sclabs)

# llongv$vclr <- ifelse(llongv$x == "Individual", vclrs[1], NA)
# llongv$vclr <- ifelse(llongv$x == "Relationship", vclrs[2], llongv$vclr)
# llongv$vclr <- ifelse(llongv$x == "Community", vclrs[3], llongv$vclr)
# llongv$vclr <- ifelse(llongv$x == "Societal", vclrs[4], llongv$vclr)


# llongv$vclr <- ifelse(llongv$x == "IPV Interventions Research", catpal[1], catpal[2])

# llongv$border <- ifelse(llongv$x == "Individual", vclrs[1], NA)
# llongv$border <- ifelse(llongv$x == "Relationship", vclrs[2], llongv$vclr)
# llongv$border <- ifelse(llongv$x == "Community", vclrs[3], llongv$vclr)
# llongv$border <- ifelse(llongv$x == "Societal", vclrs[4], llongv$vclr)
# llongv <- llongv[, c("id", "Freq", "vclr")]

#mpal(1:length(unique(llongv3[, 1])))

# lv <- cbind(names(V(llongg)), V(llongg)$Freq) %>% data.frame()
# rep(V(llongg)$Freq, V(llongg)$Freq)
# lv$Freq <- as.integer(lv$Freq)
# lvnames0 <- attributes(E(llongg))$vnames
# id <- gsub("(\\w+)\\d{4}\\w+", "\\1", id[])
# id <- sapply(id, RtCap)
#, ymin = -1.5, ymax = 2, xmin = -1.5, xmax = 2)
# MAPeco <- merge(MAP, mplvls1, by.x = "bibkey", by.y = "key", all = TRUE)[, c("bibkey", "jrnl", "year", "journal", "scat", "prop", "j.loc", "j.year", "SJR", "Hindex", "cpv", "l1", "l2", "l3", "l4", "exo_macro", "meso_exo", "micro", "nlvls", "nsys")]
# llongv01$cvclr <- ifelse(is.na(llongv01$cvclr), llongv01$vclr, llongv01$cvclr)

# llongbi <- rbind(llongbi1, llongbi2)
#
# llongbig <- graph_from_data_frame(llongbi, directed = T, vertices = llongv)
#
# lbivnames0 <- vertex_attr(llongbig, "name")
# lbivnames1 <- gsub("(\\w+)\\d{4}\\w+", "\\1", lbivnames0)
#
# lbivnames <- sapply(lbivnames1, RtCap, USE.NAMES = FALSE)
#
# V(llongbig)$name <- lbivnames
#
# V(llongbig)$size <- V(llongbig)$Freq+1
# V(llongbig)$color <- adjustcolor(V(llongbig)$vclr, alpha.f = 0.65)
# V(llongbig)$frame.color <- V(llongbig)$vclr
# E(llongbig)$width <- 0.35
#
# kindex.big <- V(llongbig)$name %>% length()-4 ## same as `kindex` above, but for the igraph data ##
# lbiblsize <- c(log(V(llongbig)$size[1:kindex.big])*0.45, log(V(llongbig)$size[-1:-kindex.big])*0.15) ## currently, this is actually identitcal, and tehrefore unneccessarily repetative of, to the llongg vertex attributes, but included here, as usual, for reusability/reproducibility purposes
#
# # PLOT - `llongbig` ---------------------------------------------------------
#
# lbfr <- layout_with_fr(llongbig)
# lbn <- norm_coords(lbfr)
# plot(llongbig, rescale = T, layout = lbn, edge.arrow.size = 0.075, vertex.label.color = V(llongbig)$cvclr, vertex.label.cex = lblsize)

# #+ keysnet_lvls2, fig.fullwidth = TRUE
# llongdg <- graph_from_data_frame(llong[, c(2, 1)], directed = T, vertices = llongv)
#
# ldvnames0 <- vertex_attr(llongdg, "name")
# ldvnames1 <- gsub("(\\w+)\\d{4}\\w+", "\\1", ldvnames0)
#
# ldvnames <- sapply(ldvnames1, RtCap, USE.NAMES = FALSE)
#
# V(llongdg)$name <- ldvnames
#
# V(llongdg)$size <- V(llongdg)$Freq+1
# V(llongdg)$color <- adjustcolor(V(llongdg)$vclr, alpha.f = 0.65)
# V(llongdg)$frame.color <- V(llongdg)$vclr
# E(llongdg)$width <- 0.35
#
# kindex.dg <- V(llongdg)$name %>% length()-4 ## same as `kindex` above, but for the igraph data ##
# ldblsize <- c(log(V(llongdg)$size[1:kindex.dg])*0.45, log(V(llongdg)$size[-1:-kindex.dg])*0.125) ## currently, this is actually identitcal, and tehrefore unneccessarily repetative of, to the llongg vertex attributes, but included here, as usual, for reusadlity/reproducidlity purposes
#
# # PLOT - `llongdg` ---------------------------------------------------------
#
# ldfr <- layout_with_fr(llongdg) %>% norm_coords()
# plot(llongdg, rescale = T, layout = ldfr, edge.arrow.size = 0.075, vertex.label.color = V(llongdg)$cvclr, vertex.label.cex = lblsize)
#

# plvl <- sapply(mplvls[, 1:4], table) %>% data.frame()
# apply(plvl, 2, Rbinom)
# sapply(mplvls[, c(1, 2)], table) %>% chisq.test
# sapply(mplvls[, c(1, 3)], table) %>% chisq.test
# sapply(mplvls[, c(1, 4)], table) %>% chisq.test
# sapply(mplvls[, c(2, 3)], table) %>% chisq.test
# sapply(mplvls[, c(2, 4)], table) %>% chisq.test
# sapply(mplvls[, c(3, 4)], table) %>% chisq.test


# aledges <- cbind(as.character(alnet$clab), alnet$to)
# allabs0 <- unique(aledges1[, 1])
# alrec <- paste0("'", allabs0, "' = '.", allabs0, "'; ", collapse = "")
