#+ results='hide', fig.keep='none', fig.show='none'
source("bibs.R")
#'
#' # IPV Interventions
#'
### [MAP-interventions.csv] ####
cpv.s3 <- MAP[MAP$scat == "S3", ]
# write.csv(cpv.s3, "data/MAP-interventions.csv", row.names = FALSE)
cpv.s3
#'
#' \newpage
#'
#'  # LGBTQ-IPV Research
### [MAP-lgbtq.csv] ####
cpv.s4 <- MAP[MAP$scat == "S4", ]
# write.csv(cpv.s3, "data/MAP-lgbtq.csv", row.names = FALSE)
cpv.s4
#'
#' \newpage
#'
