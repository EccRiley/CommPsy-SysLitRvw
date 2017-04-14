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
usmap <- read.csv("data/usmap.csv")
states = read.csv("data/states.csv",header = T,row.names = 1)
states <- data.frame(state = rownames(states), states)
####################################
### STATE CENTROIDS (for adding state abbr. labels) ###
snames <- aggregate(cbind(long, lat) ~ id, data = usmap, FUN = function(x)mean(range(x)))
states <- merge(snames,states, by.x = "id", by.y = "state")
states$region <- c(3, 4, 4, 3, 4, 4, 1, 3, 3, 3, 3, 4, 4, 2, 2, 2, 2, 3, 3, 1, 3, 1, 2, 2, 3, 2, 4, 2, 4, 1, 1, 4, 1, 3, 2, 2, 3, 4, 1, 1, 3, 2, 3, 3, 4, 1, 3, 4, 3, 2, 4)
states$div <- c(6, 9, 8, 7, 9, 8, 1, 5, 5, 5, 5, 9, 8, 3, 3, 4, 4, 6, 7, 1, 5, 1, 3, 4, 6, 4, 8, 4, 8, 1, 2, 8, 2, 5, 4, 3, 7, 9, 2, 1, 5, 4, 6, 7, 8, 1, 5, 9, 5, 3, 8)
# Region 1: Northeast
# Region 2: Midwest
# Region 3: South
# Region 4: West
#
# Division 1: New England
# Division 2: Middle Atlantic
# Division 3: East North Central
# Division 4: West North Central
# Division 5: South Atlantic
# Division 5: South Atlantic
# Division 6: East South Central
# Division 7: West South Central
# Division 8: Mountain
# Division 9: Pacific

##########################
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

gg <- ggplot()
gg <- gg + geom_map(data = usmap, map = usmap,
                    aes(map_id = id, group = group),
                    fill = "transparent", color = pal_my[19], size = 0.15)

gg + geom_map(data = states, map = usmap,
                  aes(map_id = id, fill = ST),
                  color = pal_my[19], size = 0.15) + thm_cl_tft() +
    scale_fill_manual(values = mpal(1:nlevels(states$ST)), guide=FALSE) +
    geom_text(data = states, aes(long, lat, label = ST))

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
states2 <- merge(states, bibState, by.x = "id", by.y = "state", all = TRUE)
states2.t <- Rtdf(states2$id)
states3 <- merge(states, bibState.t, by = "id", all = TRUE)
# states3$Freq <- sapply(states3$Freq, Rna)

gg + geom_map(data = states3, map = usmap,
              aes(map_id = id, fill = Freq),
              color = pal_my[19], size = 0.15) + thm_cl_tft() +
    # scale_colour_gradientn(colours = grad(1:length(unique(states3$Freq), p = grblues)), guide = FALSE) +
    geom_text(data = states3, aes(long, lat, label = ST))
names()
