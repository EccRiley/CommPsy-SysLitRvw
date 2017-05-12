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
