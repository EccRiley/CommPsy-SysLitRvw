#' ---
#' title: "Journals Included in Literature Search"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R")
# knitr::opts_chunk$set(
    # tidy = TRUE,
    # echo = TRUE,
    # fig.keep = 'high',
    # fig.show = 'hold',
    # results = 'asis'
# )
rpm()
#'
#' \Frule
#'
#' # Community Psychology Journals^[http://www.scra27.org/publications/other-journals-relevant-community-psychology/]
#'
#+ tidy=FALSE, echoRule=NULL, Rrule=TRUE, Rerule=NULL
JOURNAL <- c("Action Research",
             "American Journal of Community Psychology",
             "American Journal of Health Promotion",
             "American Journal of Orthopsychiatry",
             "American Journal of Preventive Medicine",
             "American Journal of Public Health",
             "Australian Community Psychologist",
             "Community Development",
             "Community Development Journal",
             "Community Mental Health Journal",
             "Community Psychology in Global Perspective",
             "Cultural Diversity and Ethnic Minority Psychology",
             "Global Journal of Community Psychology Practice",
             "Health Education and Behavior",
             "Health Promotion Practice",
             "Journal of Applied Social Psychology",
             "Journal of Community and Applied Social Psychology",
             "Journal of Community Practice",
             "Journal of Health and Social Behavior",
             "Journal of Prevention and Intervention",
             "Journal of Primary Prevention",
             "Journal of Rural Community Psychology",
             "Journal of Social Issues",
             "Journal of Community Psychology",
             "Psychiatric Rehabilitation Journal",
             "Psychology of Women Quarterly",
             "Psychosocial Intervention",
             "Social Science and Medicine",
             "The Community Psychologist",
             "Transcultural Psychiatry",
             "Progress in Community Health Partnerships: Research, Education, and Action",
             "Journal of Interpersonal Violence",
             "Violence Against Women",
             "Violence and Victims",
             "Journal of Family Violence")
#'
#+ tidy=TRUE, echoRule=NULL, Rrule=NULL, Rerule=TRUE
Year <- c(2003, 1973, 1986, 1930, 1985, 1971, 2006, 2005, 1966, 1965, 2015, 1995, 2010, 1997, 2000, 1971, 1991, 1994, 1967, 1996, 1981, 1980, 1997, 1973, 1996, 1976, 2011, 1967, 1975, 1997, 2007, 1986, 1995, 1986, 1986)

SJR <- c(0.33, 1.237, 0.821, 0.756, 2.764, 2.52, NA, NA, 0.47, 0.467, NA, NA, NA, 1.177, 0.769, 0.639, 0.855, 0.278, 1.727, 0.231, 0.64, NA, 1.467, 0.425, 0.692, 1.216, 0.331, 1.894, NA, 1.141, 0.427, 1.064, 0.851, 0.449, 0.639) ## SJR is for 2015 (most recent available on http://www.scimagojr.com) ##

H <- c(15, 83, 72, 69, 154, 196, NA, NA, 28, 51, NA, NA, NA, 72, 32, 78, 44, 15, 93, 21, 36, NA, 89, 65, 48, 66, 8, 177, NA, 35, 15, 78, 66, 64, 56) ## also retrieved from http://www.scimagojr.com ##

Med <- c(0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0)

Psych <- c(0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1)

SocSci <- c(1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1)

Biz <- c(1, rep(0, length(JOURNAL)-1))
AH <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0)

Nurs <- c(rep(0, 14), 1, rep(0, length(JOURNAL)-15))

HlthPrf <- c(rep(0, 21), 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)

Location <- c("UK", "US", "US", "US", "US", "US", "AU", "UK", "UK", "NL", "IT", "US", "US", "US", "US", "UK", "UK", "US", "US", "US", "US", "US", "UK", "US", "US", "UK", "ES", "UK", "US", "UK", "US", "US", "US", "US", "US")

dat <- data.frame(JOURNAL, Location, Year, SJR, H)
dat$jrnl <- sapply(as.character(dat$JOURNAL), Rabbr)

dat.subj <- data.frame(Med, Psych, SocSci, Biz, AH, Nurs, HlthPrf)
dat.subj$Nsubj <- apply(dat.subj, 1, sum)
dat <- cbind(dat, dat.subj) %>% as.data.frame()
dat.narm <- na.omit(dat) %>% as.data.frame()
names(dat.narm) <- c("Journal", "Location", "Year", "ScimagoJourn.Rating", "H-Index", "Medical", "Psychology", "SocialSciences", "Business", "Arts&Humanities", "Nursing", "HealthProfessions", "N_CoveredTopics")

# cor(dat.narm[, -1:-2]) %>% corrplot::corrplot.mixed(lower = "number", upper = "shade", tl.pos = "lt", diag = "u", bg = NA, col = blgrmg(length(cor(dat.narm[, -1:-2]))), tl.col = pal_my[19])

cl <- mpal(H, p = cols3); bg <- adjustcolor(cl, alpha.f = 0.6)

with(dat, {lm1 <- lm(H ~ SJR); plot(SJR, H, col = cl, bg = bg, pch = 21, xlab = "Scimago Journal Rating", ylab = "H Index"); abline(lm1, lwd = 2.5, col = pal_my[18])})

cor.test(dat$H, dat$SJR)
#'
#'
#' # IPV-Related Journals
#'
#' The following is for determining which IPV-related journals to include in the formal literature searches conducted for this review.
#'
#' The data below contain the publication names and corresponding count of articles from each publication resulting from the inital broad-strokes database search^[The initial broad-strokes IPV-related literature search was conducted using the following command-line search: "`SU('intimate partner violence' OR 'domestic violence' OR 'partner abuse') AND YR($>1965$)`"]
#'

dat2 <- read.csv("data/ipvJournalsSearch.csv")[c(-1, -6, -7), ]
    ## Exclude Theses and Dissertations ##

m.cnt <- mean(dat2[, 2])
s.cnt <- sd(dat2[, 2])

dat2.m <- dat2[dat2[,2] >= m.cnt, ]
dat2.s <- dat2[dat2[,2] >= s.cnt, ]

#+ echo=FALSE

pander(dat2.m, justify = c("right", "left", "right"), caption = "Journals with article counts greater than or equal to the mean of all journal article counts in the 'broad-strokes' database search results set")
pander(dat2.s, justify = c("right", "left", "right"), caption = "Journals with article counts greater than or equal one standard deviation of the distribution for all journal article counts in the 'broad-strokes' database search results set")
