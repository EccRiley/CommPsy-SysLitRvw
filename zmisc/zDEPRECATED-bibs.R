# Rtdf(MAP$journal, names = c("Journal", "$N_{Articles}$")) %>%
#     kable(caption = "Number of articles published in each included journal after restricting search results to only those published in community-psychology specific journals and the _four selected_ violence-related journals")

# t.excl %>% kable(caption = "Number of Articles Removed per Exclusion Criteria", align = c('l', 'r')) #%>% #, format = 'latex', booktabs = TRUE) #%>%
# add_footnote("The inclusion of SMW criterion was only applied to articles returned from the database searches specific to SMW-Inclusive research.")
#'
# MAPv2 <- MAP[!MAPv1 %in% ctbl.z1[, "case"], "bibkey"] %>% as.character()

# MAPv2 <- c("rumptz1991ecological", "gondolf1999comparison", "thompson2000identification", "gregory2002effects", "sullivan2002findings", "foshee2004assessing", "hendricks2006recidivism", "hovell2006evaluation", "silvergleid2006batterer", "contrino2007compliance", "muftic2007evaluation", "gillum2008benefits", "roffman2008mens", "price2009batterer", "enriquez2010development", "welland2010culturally", "feder2011need", "portwood2011evaluation", "potter2011bringing", "boal2014barriers", "boal2014impact", "ermentrout2014this", "howell2015strengthening", "lockhart1994letting", "wise1997comparison", "bernhard2000physical", "giorgio2002speaking", "younglove2002law", "fortunata2003demographic", "mccloskey2003contribution", "glass2004female-perpetrated", "balsam2005relationship", "sorenson2005restraining", "heintz2006intimate", "pattavina2007comparison", "bossarte2008clustering", "glass2008risk", "hassouneh2008influence", "swahn2008measuring", "blosnich2009comparisons", "edelen2009measurement", "oswald2010lesbian", "ackerman2011gender", "davidovic2011impelling", "hardesty2011lesbian/bisexual", "iverson2011contribution", "messinger2011invisible", "porter2011intimate", "gillum2012there", "goldberg2013sexual", "kanuha2013relationships", "witte2013social", "finneran2014antecedents", "lewis2014sexual", "mustanski2014syndemic", "tran2014prevalence", "edwards2015physical", "kubicek2015same-sex", "lewis2015emotional", "sylaska2015disclosure", "witte2015perceived", "wu2015association", "dixon2016association", "edwards2016college", "langenderfer-magruder2016experiences")

# j.vcap <- sapply(j.v, RtCap)
# MAP$journal <- factor(MAP$journal)

# v1v2 <- MAPv1[!MAPv1 %in% MAPv2]
# v1v2 <- paste0("@", v1v2)

# map.v$journal <- droplevels(map.v$journal)

# MAP <- merge(map.cp, map.v, all = TRUE)

# EXCL <- ctbl.z1[ctbl.z1$case %in% MAPrm, c("case", "scat", "code")]
#'
#' <!-- ## Final Inclusion \\& Exclusion Decisions-->
#'
#' <!-- Any articles retained from the above-described database searches and filtering processes that either did not meet the basic inclusion criteria (i.e., U.S.-based empirical research) or were determined to be unrelated to the topics of interest for the current review were excluded from the set of formally reviewed articles ($N_{excluded} = `r length(MAPrm)`$). The final set of `r paste0("$N_{included} = ", nrow(MAP), "$")` is divided into two categories: (C1) IPV intervention evaluations research (`r paste0("$n_{c_{1}} = ", nrow(MAP[MAP$scat == "S3", ]), "$")`), and (C2) community-psychology-specific research specific to SMW-Inclusive and inclusive of sexual minority women (`r paste0("$n_{c_{2}} = ", nrow(MAP[MAP$scat == "S4", ]), "$")`). -->
#'
# ctbl.z1 <- ctbl.z1[ctbl.z1$case %in% MAPrm, ]
#
# library(kableExtra)
#
# MAPv31 <- as.character(MAP$bibkey)
# v3v31 <- MAPv3[!MAPv3 %in% MAPv31]
# v3v31 <- paste0("@", v3v31)

# map.cp$journal <- factor(map.cp$journal)
# levels(map.cp$journal) <- c(levels(map.cp$journal),
#                             j.cp[!j.cp %in% levels(map.cp$journal)])
# levels(map.cp$journal) <- sapply(levels(map.cp$journal), RtCap)
#

# map.v$RM <- ifelse(map.v$RM == 1, NA, 0)
# map.v <- na.omit(map.v)

# clr.cp <- MAP.j[MAP.j$jrnl %in% j.cp, "jrnl"] %>% unique() %>% length()
# clr.v <- MAP.j[MAP.j$jrnl %in% j.v, "jrnl"] %>% unique() %>% length()
# clr.cpv <- c(rep(pcpv[1], 3), ,  rep(pcpv[2], 2), rep(pcpv[1], 2), rep(pcpv[2], 2))
# clr.cpv1 <- levels(factor(MAP.j$Journal))
# clr.cpv <- ifelse(clr.cpv %in% j.cpp, pcpv[1], pcpv[2])
