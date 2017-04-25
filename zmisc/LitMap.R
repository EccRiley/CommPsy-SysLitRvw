#' ---
#' title: "Geography of Reviewed Research"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
# SETUP --------------------------------------------------------------

source("../SETUP.R")
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
options(scipen = 10)
# rpm()
#'
#'
# USMAP --------------------------------------------------------
# usmap <- read.csv("data/usmap.csv")[, -1]
# write.csv(usmap, "data/usmap.csv", row.names = FALSE) ## there was an extra column ("X") in the csv file (which contained the rownames from the first time I saved the file) that removed by not including that first column and then re-writing the file below ##

usmap <- read.csv("data/usmap.csv")
states = read.csv("data/states.csv", header = T, row.names = 1)
states <- data.frame(state = rownames(states), states)
snames <- aggregate(cbind(long, lat) ~ id, data = usmap,
                    FUN = function(x) mean(range(x)))
states <- merge(snames, states, by.x = "id", by.y = "state")

states$region <- c(3, 4, 4, 3, 4, 4, 1, 3, 3, 3, 3, 4, 4, 2, 2, 2, 2, 3, 3, 1, 3, 1, 2, 2, 3, 2, 4, 2, 4, 1, 1, 4, 1, 3, 2, 2, 3, 4, 1, 1, 3, 2, 3, 3, 4, 1, 3, 4, 3, 2, 4)
states$div <- c(6, 9, 8, 7, 9, 8, 1, 5, 5, 5, 5, 9, 8, 3, 3, 4, 4, 6, 7, 1, 5, 1, 3, 4, 6, 4, 8, 4, 8, 1, 2, 8, 2, 5, 4, 3, 7, 9, 2, 1, 5, 4, 6, 7, 8, 1, 5, 9, 5, 3, 8)
states.s <- states[, c("id", "ST", "region", "div")]
states.rg <- merge(usmap, states.s)
srg <- aggregate(cbind(long, lat) ~ region, data = states.rg,
                 FUN = function(x) mean(range(x)))
srg <- merge(srg, states.s)
srg <- srg[, c("id", "ST", "region", "long", "lat")]
regions <- c("Northeast", "Midwest", "South", "West")
srg <- within(srg, {
    region <- factor(region, labels = regions)
})

sdv <- aggregate(cbind(long, lat) ~ div, data = states.rg,
                 FUN = function(x) mean(range(x)))
sdv <- merge(sdv, states.s)
sdv <- sdv[, c("id", "ST", "div", "long", "lat")]
divisions <- c("New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central", "West South Central", "Mountain", "Pacific")
sdv <- within(sdv, {
    div <- factor(div, labels = divisions)
})

## LABELING CENTROIDS =====================================================

states[10,2] <- 1841536
states[19,2] <- 706827
states[2,2] <- -1344021
states[2,3] <- -1974685
states[37,2] <- 181757
states[24,2] <- 400430
states[23,2] <- 1264168
states[23,3] <- -92122
states[13,2] <- -1174084
states[13,3] <- -0
states[5,2] <- -1750000
states[12,2] <- -350000
states[12,3] <- -2010000
states[21,3] <- -343925
states[21,2] <- 1955994
states[22,2] <- 2230000
states[7,3] <- -6
states[31,2] <- 2120364
states[31,3] <- -255347
states[30,3] <- 209979
states[46,3] <- 309355
states[8,3] <- -362274
states[47,2] <- 1881512
states[18,2] <- 1354662
states[49,2] <- 1657769
states[49,3] <- -520174
states[41,2] <- 1770243
states[41,3] <- -1015010
states[20,3] <- 497611

## Adjust hawaii ##

maxlong <- range(usmap[usmap$id == "California", "long"])[1]
usmap <- within(usmap, {
    long <- ifelse(long <= maxlong, NA, long)
})
usmap <- na.omit(usmap)


# GGPLOT MAPS --------------------------------------------------------

library(ggplot2)
ggbase <- ggplot() +
    geom_map(data = usmap, map = usmap,
             aes(x = long, y = lat, map_id = id, group = group),
             fill = "transparent", color = pal_my[19], size = 0.15)

gg <- ggbase + thm_Rcl_tft()

gg + geom_map(data = states, map = usmap,
                  aes(map_id = id, fill = ST),
                  color = pal_my[19], size = 0.15) +
    scale_fill_manual(values = mpal(1:nlevels(states$ST)), guide=FALSE) +
    geom_text(data = states, aes(long, lat, label = ST), size = 3)

## bibState ========================================================

bibState = list(
    boal2014barriers = c("Oregon"),
    boal2014impact = c("Oregon"),
    contrino2007compliance = c("New York"),
    enriquez2010development = c("Missouri"),
    ermentrout2014this = c("North Carolina"),
    feder2011need = c("Oregon"),
    foshee2004assessing = c("North Carolina"),
    gillum2008benefits = c("Massachusetts"),
    gondolf1999comparison = c("Pennsylvania", "Texas", "Colorado"),
    gregory2002effects = c("Ohio"),
    hendricks2006recidivism = c("Wisconsin"),
    hovell2006evaluation = c("California"),
    howell2015strengthening = c("Michigan"),
    muftic2007evaluation = c("North Dakota"),
    potter2011bringing = c("New Hampshire"),
    price2009batterer = c("Illinois"),
    roffman2008mens = c("Washington"),
    rumptz1991ecological = c("Michigan"),
    silvergleid2006how = c("Oregon"),
    sullivan2002findings = c("Illinois", "Michigan"),
    thompson2000identification = c("Washington"),
    welland2010culturally = c("California"))

bibState <- stack(bibState)

names(bibState) <- c("state", "bibkey")
bibState.t <- Rtdf(bibState$state, names = c("id", "Freq"))
bibSt <- merge(states, bibState.t, by = "id", all = TRUE)

bibRg <- merge(srg, bibState.t, by = "id", all = TRUE)
bibRg.t <- Rtdf(bibRg$region, names = c("region", "Freq"))

bibRg <- merge(srg, bibRg.t, by = "region", all = TRUE)
bibRg <- within(bibRg, {
    lat <- ifelse(region == "West", lat/2, lat)
    long <- ifelse(region == "West", long/1.5, long)
})

bibDv <- merge(sdv, bibState.t, by = "id", all = TRUE)
bibDv.t <- Rtdf(bibDv$div, names = c("div", "Freq"))
bibDv <- merge(sdv, bibDv.t, by = "div", all = TRUE)

## CHLOROPLETH MAPS ========================================================

frq.st <- 1:length(unique(bibSt$Freq))
frq.rg <- 1:length(unique(bibRg$Freq))
frq.dv <- 1:length(unique(bibDv$Freq))

grays2 <- colorRampPalette(pal_my[c(20, 1)])

### PLOT - cloropleths - states ####

ggchl.st <- ggbase + geom_map(data = bibSt, map = usmap,
              aes(map_id = id, fill = Freq),
              color = pal_my[19], size = 0, alpha = 0.5) +
    scale_fill_gradientn(colours = grad(frq.st, p = grblues, alpha = 0.5),
                         na.value = NA, name = expression(N[studies])) +
    guides(fill = guide_legend(barwidth = 2, alpha = 0.5)) +
    geom_text(data = bibSt, aes(long, lat, label = ST),
              size = 3, family = "serif", colour = pal_my[20]) + thm_Rcl_tft(ltitle = TRUE, lpos = "top", ldir = "horizontal") +
    theme(legend.title.align = 0.5, legend.box.spacing = unit(-1, 'cm')) #+
ggchl.st

### ggchloro - regions ####
grblues2 <- colorRampPalette(pal_my[c(2, 16)], alpha = TRUE)
ggchl.rg <- gg + geom_map(data = bibRg, map = usmap,
                              aes(map_id = id, fill = Freq),
                              color = pal_my[19], size = 0, alpha = 0.5) +
    scale_fill_gradientn(colours = grad(frq.rg, p = grblues2, alpha = 0.5),
                         na.value = NA, name = expression(N[studies])) +
    guides(fill = guide_legend(barwidth = 2, alpha = 0.5)) +
    geom_text(data = bibRg, aes(long, lat, label = region),
              size = 10, family = "serif", colour = pal_my[20])
ggchl.rg

ggchl.dv <- ggplot() +
    geom_map(data = states, map = usmap,
             aes(x = long, y = lat, map_id = id),
             color = NA, size = 0.15) +
    geom_path(data = usmapDv,
             aes(x = long, y = lat, group = group), size = 0.5)
    # scale_fill_gradientn(colours = grad(frq.dv, p = grblues2, alpha = 0.5),
                         # na.value = NA, name = expression(N[studies])) +
    # guides(fill = guide_legend(barwidth = 2, alpha = 0.5)) +
    # geom_text(data = bibDv,
    #           aes(long, lat, label = div),
    #           size = 4, family = "serif", colour = pal_my[20])
ggchl.dv

bibDv.s <- bibDv[, c("div", "id", "ST", "Freq")]
usmapDv <- merge(usmap, bibDv.s)
# aggregate(cbind(long, lat) ~ div, data = bibDv,
#           FUN = unique)
# divn <- Rtdf(usmapDv$div, names = c("div", "n"))
# f <- function(x) {
#     y <- data.frame(id = 1:sum(x[, 2]))
#     for (i in 1:nrow(x)) {y$id[1:x[i, 2]] <- paste0(x[i, 1], ".", 1:x[i, 2]))}
# }

# divgrp <- f(divn)
# names(divgrp) <- "divgrp"
# divgrp <- separate(divgrp, divgrp, c("div", "grpn"), remove = FALSE)

# usmapDv <- merge(usmapDv, divn, all = TRUE)

usmapDv$group2 <- paste0(usmapDv$div, ".", as.numeric(usmapDv$group))
ggbase + geom_map(data = bibDv, map = usmapDv,
                      aes(x = long, y = lat, map_id = id,
                          fill = div), size = 0.25, stat = "contour")

    # geom_map(data = bibDv, map = usmap,
    #                 aes(map_id = id, fill = Freq),
    #                 color = pal_my[19], size = 0, alpha = 0.5) +
    # geom_path(color = "black")
    # scale_fill_hue(l = 40) +
    # coord_equal()

ggplot() + geom_map(data = usmapDv, map = usmap,
                          aes(x = long, y = lat, map_id = id, group = group),
                          color = pal_my[19], size = 0, alpha = 0.5)
#     scale_fill_gradientn(colours = grad(frq.dv, p = grblues2, alpha = 0.5),
#                          na.value = NA, name = expression(N[studies])) +
#     guides(fill = guide_legend(barwidth = 2, alpha = 0.5)) +
#     geom_text(data = bibDv, aes(long, lat, label = div),
#               size = 4, family = "serif", colour = pal_my[20])
# ggchl.dv

### ggdensity - points ####

ggdens <- gg +
    # geom_text(data = bibSt, aes(long, lat, label = ST), size = 2.5, colour = mypal[18]) +
    geom_point(data = bibSt, aes(x = long, y = lat, colour = Freq)) +
    geom_point(data = bibSt, aes(x = long, y = lat,
                   size = Freq, colour = Freq, alpha = Freq)) +
    scale_size_continuous(range=c(1, 30), guide = FALSE) +
    scale_fill_gradientn(colours = grad(frq, p = grblutr),
                         na.value = pal_my[1], guide = FALSE)
ggdens
#
# + geom_map(data = bibSt, map = usmap,
#            aes(map_id = id),
#            color = pal_my[19], size = 0.15, fill = pal_my[1]) +
