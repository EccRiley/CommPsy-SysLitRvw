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
# rpm()
#'
#'
# library(devtools)
library(rgdal)
library(sp)
# library(maptools)
maptbl <- read.csv("data/usmap.csv") ## previously created

library(ggplot2)
ggplot(maptbl, aes(long, lat)) +
    geom_polygon(aes(group = group), colour = "white")
cnty <- map('county', plot = FALSE)
counties <- map_data('county', projection = "azequalarea")
us <- map_data('state', projection = "albers")
ggplot(us, aes(long, lat)) +
    geom_polygon(aes(group = group), colour = "white")

library(maps)
US <- map("state")
US.names <- US$names
US.coord <- data.frame(US$x, US$y)
# plot(US.coord, type='l')
# map.cities()
#'
#+ deprecated, echo=FALSE

# DEPRECATED ---------------------------------------------------------

rgdal::readOGR(dsn = "data/us.geojson.txt", layer = "OGRGeoJSON") -> us
sp::spTransform(us, sp::CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")) -> us_aea
us_aea@data$id <- rownames(us_aea@data)
alaska <- us_aea[us_aea$STATEFP10 == "02",]
maptools::elide(alaska, rotate = -50) -> alaska
maptools::elide(alaska, scale = max(apply(bbox(alaska), 1, diff)) / 2.3) -> alaska
maptools::elide(alaska, shift = c(-2200000, -2600000)) -> alaska
sp::proj4string(us_aea) -> proj4string(alaska)
hawaii <- us_aea[us_aea$STATEFP10 == "15",]
maptools::elide(hawaii, rotate = -40) -> hawaii
maptools::elide(hawaii, shift = c(5400000, -1600000)) -> hawaii
sp::proj4string(us_aea) -> proj4string(hawaii)

# us_aea <- us_aea[!us_aea$STATEFP10 %in% c("02", "15", "72"),]
# us_aea <- rbind(us_aea, alaska, hawaii)
#

# map <- fortify(us_aea)#, region = "NAME10")
# summary(map)

#'
map_data('state')
#' Because the code above can become time consuming, particularly with a slow (or no) internet connection, it is  useful to save the data to a local file for later use.
#'
# write.csv(map,file = "usmap.csv")
