#' ---
#' title: "Journals Included in Literature Search"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R")
knitr::opts_chunk$set(
    tidy = FALSE,
    echo = FALSE,
    fig.keep = 'high',
    fig.show = 'hold',
    results = 'asis'
)
#'
#' \Frule
#'
#' # Community Psychology Journals^[http://www.scra27.org/publications/other-journals-relevant-community-psychology/]

JOURNAL <- c("Action Research", "American Journal of Community Psychology", "American Journal of Health Promotion", "American Journal of Orthopsychiatry", "American Journal of Preventive Medicine", "American Journal of Public Health", "Australian Community Psychologist", "Community Development", "Community Development Journal", "Community Mental Health Journal", "Community Psychology in Global Perspective", "Cultural Diversity and Ethnic Minority Psychology", "Global Journal of Community Psychology Practice", "Health Education and Behavior", "Health Promotion Practice", "Journal of Applied Social Psychology", "Journal of Community and Applied Social Psychology", "Journal of Community Practice", "Journal of Health and Social Behavior", "Journal of Prevention and Intervention", "Journal of Primary Prevention", "Journal of Rural Community Psychology", "Journal of Social Issues", "Journal of Community Psychology", "Psychiatric Rehabilitation Journal", "Psychology of Women Quarterly", "Psychosocial Intervention", "Social Science and Medicine", "The Community Psychologist", "Transcultural Psychiatry", "Progress in Community Health Partnerships: Research, Education, and Action")

Year <- c(2003, 1973, 1986, 1930, 1985, 1971, 2006, 2005, 1966, 1965, 2015, 1995, 2010, 1997, 2000, 1971, 1991, 1994, 1967, 1996, 1981, 1980, 1997, 1973, 1996, 1976, 2011, 1967, 1975, 1997, 2007)

SJR <- c(0.33, 1.237, 0.821, 0.756, 2.764, 2.52, NA, NA, 0.47, 0.467, NA, NA, NA, 1.177, 0.769, 0.639, 0.855, 0.278, 1.727, 0.231, 0.64, NA, 1.467, 0.425, 0.692, 1.216, 0.331, 1.894, NA, 1.141, 0.427)

H <- c(15, 83, 72, 69, 154, 196, NA, NA, 28, 51, NA, NA, NA, 72, 32, 78, 44, 15, 93, 21, 36, NA, 89, 65, 48, 66, 8, 177, NA, 35, 15)

Med <- c(0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0)

Psych <- c(0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0)

SocSci <- c(1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1)

Biz <- c(1, rep(0, 30))

AH <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0)

Nurs <- c(rep(0, 14), 1, rep(0, 16))

HealthProf <- c(rep(0, 21), 1, 0, 0, 1, 0, 0, 0, 1, 0, 0)

Location <- c("UK", "US", "US", "US", "US", "US", "AU", "UK", "UK", "NL", "IT", "US", "US", "US", "US", "UK", "UK", "US", "US", "US", "US", "US", "UK", "US", "US", "UK", "ES", "UK", "US", "UK", "US")

dat <- data.frame(JOURNAL, Location, Year, SJR, H)

dat.subj <- data.frame(Med, Psych, SocSci, Biz, AH, Nurs, HealthProf)
dat.subj$Nsubj <- apply(dat.subj, 1, sum)
dat <- cbind(dat, dat.subj) %>% as.data.frame()
cor(dat[, -1:-2])

dat.narm <- na.omit(dat) %>% as.data.frame()
# cor(dat.narm[, -1:-2]) %>% corrplot::corrplot()

R.mdhist <- function(x, nrow, ncol, pal = mpal(seq(1:ncol(x)), p = cols3), labs = colnames(x), ...) {
	origPar <- par("mfrow")
	par(mfrow = c(nrow, ncol))
	for (i in 1:ncol(x)) {
		hist(x[, i], col = pal[i], freq = FALSE, main = paste0("Histogram of ", labs[i]), xlab = labs[i], ...);
		lines(density(x[, i], na.rm = TRUE), lwd = 2, ...)
		}
	on.exit(par(mfrow = origPar))
}

R.mdhist(x = dat[, 3:5], nrow = 1, ncol = 3, labs = c("Year", "\nScimagoJournal Rating", "H Index"))

cl <- mpal(H, p = cols3); bg <- adjustcolor(cl, alpha.f = 0.6)
with(dat, {lm1 <- lm(H ~ SJR); plot(SJR, H, col = cl, bg = bg, pch = 21, xlab = "Scimago Journal Rating", ylab = "H Index"); abline(lm1, lwd = 2.5, col = mypal[18])})
#'
#' \newpage
#' 
#' # IPV-Related Journals 
#'
#' The following is for determining which IPV-related journals to include in the formal literature searches conducted for this review. 
#'
#' The data below contain the publication names and corresponding count of articles from each publication resulting from the inital broad-strokes database search `r margin_note("The initial broad-strokes IPV-related literature search was conducted using the following command-line search: \"`SU(\"intimate partner violence\" OR \"domestic violence\" OR \"partner abuse\") AND YR(>1965)`\"")`
#'
# distrs <- c("norm", "lnorm", "pois", "exp", "gamma", "nbinom", "geom", "beta", "unif", "logis")
# for (i in 1:length(distrs)) {
	# fitdist(dat2[, 2], distr = distrs[i]) %>% plot
# }
dat2 <- read.csv("data/ipvJournalsSearch.csv")[c(-1, -6, -7), ] ## Exclude Theses and Dissertations ##
fitdist(dat2[, 2], "lnorm") %>% plot
m.cnt <- mean(dat2[, 2])
s.cnt <- sd(dat2[, 2])
dat2.m <- dat2[dat2[,2] >= m.cnt, ]
dat2.m
dat2.s <- dat2[dat2[,2] >= s.cnt, ]
dat2.s
dat2.s$journal %>% as.character %>% print
