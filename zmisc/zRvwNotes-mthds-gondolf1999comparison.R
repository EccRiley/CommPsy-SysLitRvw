#'---
#'title: "gondolf1999comparison"
#'author: "Riley M. Smith"
#'---
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
rpm()
#'
#+ key, echo=FALSE
bibkey <- "gondolf1999comparison"
paste0("@", bibkey)
#'
#' # Hypotheses
#'
#' H1. "More comprehensive systems ... would have significantly lower reassault rates than less comprehensive systems" (p. 43).
#'     - More comprehensive systems include the following components:
#'         - Added sanctions imposed by post-conviction, as opposed to pre-trial referral
#'         - Extended batterer counseling and observation implied by longer-term BI programs
#'         - Additional alcohol treatment servcices provided
#'         - Victim services that address compounding problems
#'
#' # Sampling Frame (SF):
#'
#+ sf
sf.locations <- list(state = c("PA", "TX", "TX", "CO"), county = c("Allegheny", "Dallas", "Harris", "Denver"), city = c("Pittsburg", " Dallas", " Houston", "Denver"))

L1.sf <- "Batterer Intevention Systems in Pittsburg, PA; Dallas, TX; Houston, TX; and Denver, CO"
L2.sf <- "The first 20-25 male-identified IPV-perpetrators appearing for intake each month at each site (until each site\'s recruitment cap, n = 210 per site, was met)"
L2.sf.incl <- c("Court-mandated IPV-P", "Voluntary IPV-P", "Males", "Adults", "Heterosexuals")
L2.sf.excl <- c("Non-IPV-P", "Non-Males", "Non-Heterosexuals", "Individuals under 18 years of age")

L3.sf <- "Female-identified initial or new (at T1) partners of L2 participants"
L3.sf.incl <- c("IPV-V", "No IPV-V", "Females", "Adults", "Heterosexuals")
L3.sf.excl <- c("Past partners of L2 participants", "Non-Females", "Non-Heterosexuals", "Individuals under 18 years of age")
paste0(sf.locations, "Level-1 Frame:", L1.sf, "Level-2 Frame:", L2.sf, "Level-2 Frame Includes:", L2.incl, "Level-2 Frame Excludes:", L2.excl, "Level-3 Frame:", L3.sf, "Level-3 Frame Excludes:", L3.incl, "Level-3 Frame Excludes:", L3.excl, sep = "\n\n")
#'
#' # Sampling Methods (SM):
#'
#+ sm
L1.sm <- list(BIP = c("Complies with state BIP standards", "collaborates with victim services", "empoloys cognitive-behavioral approach according to evidence-based standards", "40-50+ referrals per month", "operating for 5+ years", "provide training and supervision to new programs"), Components = c("court referral type", "length of intervention", "additional services provided"))
L1.sm.conditions <- list(BIP = "Based on a Well Established Batterer Program", Components = "Range of Intervention System Components")

L2.sm <- c("Purposive", "Probability", "Random Sampling")
L2.sm.conditions <- c("First 20-25 men appearing for intake each month at each of the four research sites")

L3.sm <- ("Probability")
L3.sm.conditions <- c("L3 sampling probability is dependent upon L2 participants")
#'
#' # Data Collection Methods (DC)
#'
#+ dc
L1.dc <- c("Naturalistic observation", "Comparative evaluation")
L2.dc <- c("Longitudinal", "Self-report survey", "Key informant survey")
L3.dc <- c("Longitudinal", "Self-report survey", "Telephone survey", "Telephone interview")
#'
#' > "The design is naturalistic in the sense that it examines programs as they are implemented in their respective communities, and it is comparative in the sense that systems of a range of components and linkages are selected" (p. 43).
#'
#'
#' ## Measures - Quantitative (QT)
#'
#+ qt
L1.qt <- NA
L2.qt <- c("Millon Clinical Multiaxial Inventory (MCMI)", "Michigan Alcoholism Screening Test (MAST)")
L3.qt <- c("Conflict Tactics Scales (CTS)", "Severity of Violence against Women Scales (SVAWS)", "Abusive Behavior Inventory (ABI)", "Psychological Maltreatment of Women Inventory (PMWI)", "'Would you say that your life is generally better, worse, or the same?' (p. 49)", "'How safe [the woman] felt at that point' (p. 49)", "'How likely it was that [the womens\'] partners would hit them in the next few months (using a likert-type scale)' (p. 49)")

MeasuresTypes <- list(MCMI = "Self-report", MAST = "Self-report", CTS = c("Self-report", "informant-report"), SVAWS = "Self-report", ABI =c("Self-report", "informant-report"), PMWI = "Self-report")
#'
#' ## Measures - Qualitative (QL)
#'
#+ ql
L1.ql <- "Field Notes"
L2.ql <- NA
L3.ql <- c("'Open-ended question about how the relationship was going' (p. 49)", "'Descriptions of any conflicts and their circumstances' (p. 49)", "'The nature of battering injuries and medical assistance received for those injuries' (p. 49)", "'The woman\'s response to the abuse' (p. 49)")
#'
#' ## Longitudinal Design/Methods (LT)
#'
#+ lt
L1.lt <- NA
L2.lt <- NA
L3.lt <- list(T1 = "Two weeks after partners\' program intake", T2 = "3 months after partners\' program intake", T3 = "6 months after partners\' program intake", T4 = "9 months after partners\' program intake", T5 = "12 months after partners\' program intake", T6 = "15 months after partners\' program intake")
Ntimepoints <- 6
TS <- 15 ## months ##
TS.unit <- "Months"
#'
