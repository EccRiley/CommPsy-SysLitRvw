
### FUN - 'RtCap()' ####
RtCap <- function(x) {
    s0 <- strsplit(x, " ")[[1]]
    nocap <- c("a", "the", "to", "at", "in", "with", "and", "but", "or", "of")
    s1 <- ifelse(!s0 %in% nocap, toupper(substring(s0, 1,1)), s0)
    # s2 <- toupper(substring(s[!s %in% nocap], 1,1))
    s2 <- ifelse(!s0 %in% nocap, substring(s0, 2), "")
    s <- paste(s1, s2, sep="", collapse=" ")
    return(s)
}


### FUN - 'Rabbr()' ####
Rabbr <- function(x) {
    s0 <- strsplit(x, " ")[[1]]
    ex <- c("a", "the", "to", "at", "in", "with", "and", "but", "or", "of", "\\&")
    s1 <- s0[!s0 %in% ex]
    s2 <- substring(s1, 1,1)
    s <- paste(s2, sep = "", collapse = "")
    s <- toupper(s)
    return(s)
}

j.map <- c("Action Research",
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
j.map <- sapply(j.map, tolower, USE.NAMES = FALSE)
#'
#+ tidy=TRUE, echoRule=NULL, Rrule=NULL, Rerule=TRUE

j.loc <- c("UK", "US", "US", "US", "US", "US", "AU", "UK", "UK", "NL", "IT", "US", "US", "US", "US", "UK", "UK", "US", "US", "US", "US", "US", "UK", "US", "US", "UK", "ES", "UK", "US", "UK", "US", "US", "US", "US", "US")

j.year <- c(2003, 1973, 1986, 1930, 1985, 1971, 2006, 2005, 1966, 1965, 2015, 1995, 2010, 1997, 2000, 1971, 1991, 1994, 1967, 1996, 1981, 1980, 1997, 1973, 1996, 1976, 2011, 1967, 1975, 1997, 2007, 1986, 1995, 1986, 1986)

SJR <- c(0.33, 1.237, 0.821, 0.756, 2.764, 2.52, NA, NA, 0.47, 0.467, NA, NA, NA, 1.177, 0.769, 0.639, 0.855, 0.278, 1.727, 0.231, 0.64, NA, 1.467, 0.425, 0.692, 1.216, 0.331, 1.894, NA, 1.141, 0.427, 1.064, 0.851, 0.449, 0.639) ## SJR is for 2015 (most recent available on http://www.scimagojr.com) ##

Hindex <- c(15, 83, 72, 69, 154, 196, NA, NA, 28, 51, NA, NA, NA, 72, 32, 78, 44, 15, 93, 21, 36, NA, 89, 65, 48, 66, 8, 177, NA, 35, 15, 78, 66, 64, 56) ## also retrieved from http://www.scimagojr.com ##

jdat.all <- data.frame(journal = j.map, j.loc, j.year, SJR, Hindex)


jdat.v <- read.csv("data/ipvJournalsSearch.csv")[c(-1, -6, -7), ]
## Exclude Theses and Dissertations ##

m.cnt <- mean(jdat.v[, 2])

jdat.v$journal <- sapply(as.character(jdat.v$journal), tolower)
jdat.v$journal <- gsub(" & ", " and ", jdat.v$journal)

jv.sum <- sum(jdat.v$count)

jdat <- merge(jdat.v, jdat.all)[, c("journal", "j.loc", "j.year", "SJR", "Hindex")]
# jdat$jrnl <- sapply(as.character(jdat$journal), Rabbr)

# jdat.v$j <- as.numeric(factor(jdat.v$journal))

jfv.n <- jdat.v[3, 2]
jiv.n <- jdat.v[2, 2]
vaw.n <- jdat.v[4, 2]
jvv.n <- jdat.v[5, 2]
jv.n <- rbind(jiv.n, jfv.n, vaw.n, jvv.n)

jdat.v.m <- jdat.v[jdat.v[,2] >= m.cnt, ]

j.vpr <-c("Journal of Interpersonal Violence",
          "Violence Against Women",
          "Violence and Victims",
          "Journal of Family Violence")

j.v <- sapply(j.vpr, tolower, USE.NAMES = FALSE)

j.cppr <- c("Action Research",
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
            "Journal of Community Psychology",
            "Journal of Health and Social Behavior",
            "Journal of Prevention and Intervention",
            "Journal of Primary Prevention",
            "Journal of Rural Community Psychology",
            "Journal of Social Issues",
            "Journal of Community Psychology",
            "Psychiatric Rehabilitation Journal",
            "Psychology of Women Quarterly",
            "Social Science and Medicine",
            "The Community Psychologist",
            "Transcultural Psychiatry",
            "Progress in Community Health Partnerships")

j.cp <- sapply(j.cppr, tolower, USE.NAMES = FALSE)
#'
#'
#+ pander_journals, echo=FALSE

j.cpp <- sapply(j.cp, RtCap, USE.NAMES = FALSE)
#'
#' \newpage
#'
jdat.v.m <- dplyr::rename(jdat.v.m, "Journal" = journal, "Count" = count)
rownames(jdat.v.m) <- NULL
kable(jdat.v.m[, 1:2], align = c("l", "r"), caption = "Violence-specific journals with article counts greater than or equal to the mean of all journal article counts in the 'broad-strokes' database search results set.")

j.vp <- sapply(j.v, RtCap, USE.NAMES = FALSE)

