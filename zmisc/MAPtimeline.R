journals <- data.frame(
    j = c("Action Research",
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
          "Cultural Diversity \\& Ethnic Minority Psychology",
          "Global Journal of Community Psychology Practice",
          "Health Education \\& Behavior",
          "Health Promotion Practice",
          "Journal of Applied Social Psychology",
          "Journal of Community \\& Applied Social Psychology",
          "Journal of Community Practice",
          "Journal of Health \\& Social Behavior",
          "Journal of Prevention \\& Intervention",
          "Journal of Primary Prevention",
          "Journal of Rural Community Psychology",
          "Journal of Social Issues",
          "Journal of Community Psychology",
          "Psychiatric Rehabilitation Journal",
          "Psychology of Women Quarterly",
          "Psychosocial Intervention",
          "Social Science \\& Medicine",
          "The Community Psychologist",
          "Transcultural Psychiatry",
          "Progress in Community Health Partnerships: Research, Education, \\& Action",
          "Journal of Interpersonal Violence",
          "Violence Against Women",
          "Violence \\& Victims",
          "Journal of Family Violence"),
    cat = c(rep("Community Psychology", 31), rep("Violence", 4)),
    yr1 = c(2003, 1973, 1986, 1930, 1985, 1971, 2006, 2005, 1966, 1965, 2015, 1995, 2010, 1997, 2000, 1971, 1991, 1994, 1967, 1996, 1981, 1980, 1997, 1973, 1996, 1976, 2011, 1967, 1975, 1997, 2007, 1989, 1995, 1986, 1986)
)
journals$yr2 <- rep(2017, nrow(journals))
journals$j <- sapply(as.character(journals$j), Rabbr)

timeline(journals, text.size = 3, text.color = NA) + scale_fill_manual(values = mpal(1:nrow(journals)), guide = FALSE) + thm_Rtft() + xlim(range(c(journals$yr1, journals$yr2)))


bibkey <- MAP$bibkey
scat <- MAP$scat %>% factor()
jrnl <- MAP$jrnl %>% factor()
codes.tp <- ctbl.m[ctbl.m$cat == "TOPIC", "code"] %>% droplevels()

tl <- data.frame(jrnl, scat)

yr1.s3 <- min(MAP[MAP$scat == "S3", "year"])
yr1.s4 <- min(MAP[MAP$scat == "S4", "year"])
yr2.s3 <- max(MAP[MAP$scat == "S3", "year"])
yr2.s4 <- max(MAP[MAP$scat == "S4", "year"])

tl <- within(tl, {
    yr1 <- ifelse(scat == "S3", yr2.s3, yr2.s4)
    yr2 <- ifelse(scat == "S3", yr1.s3, yr1.s4)
})

