#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
knitr::opts_template$set(invisible = list(echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE, fig.keep='none', fig.show='none'))
#'

#+ src_bibs, opts.label='invisible'
source("bibs.R")
#'

s4all <- MAP2[MAP2$scat == "S4", ]
s4all$bibkey <- as.character(s4all$bibkey)
s4excl <- s4all[!s4all$bibkey %in% MAP$bibkey, ]
# Rdt(s4excl[, c("bibkey", "journal")])
#

cb_s4x <- merge(s4excl, ctbl, by = c("caseid", "scat"))

cb_s4x <- within(cb_s4x, {
    jrnl <- sapply(as.character(journal), Rabbr)
    code <- gsub("FG-\\w+", "FG", code)
    # code <- gsub("EXP-\\w+", "EXP", code)
    # code <- gsub("LT-\\w+", "LT", code)
    code <- gsub("SVY-QL-MM", "SVY-QL", code)
    code <- gsub("SVY-QT-MM", "SVY-QT", code)
    # code <- gsub("XS-\\w+", "XS", code)
    code <- gsub("IVW-\\w+", "IVW", code)
    code <- gsub("SMIN-\\w+", NA, code)
    code <- gsub("HET", NA, code)
})
cb_s4x <- na.omit(cb_s4x) %>% droplevels()
cb_s4x <- cb_s4x[, c("caseid", "scat", "journal", "bibkey", "year", "RM", "case", "cid", "code", "catid", "cat")]

cb_s4x <- merge(cb_s4x, cbk, by = "code")
cb_s4x$code <- factor(cb_s4x$code)
cb_s4x$clab <- factor(cb_s4x$clab)
# x <- s4xtp[, c("bibkey", "clab")] %>% ftable() %>% data.frame()
# x[, 3] %>% unique()

# ftable(s4xql$clab, s4xql$jrnl)
#'
#' \newpage
#'
#+ echo=FALSE
#'

s4xtp <- cb_s4x[cb_s4x$cat == "TOPIC", ] %>% droplevels()
s4xtp <- s4xtp[!duplicated(s4xtp), ]
#
# rec.clab2code <- paste0("\"", s4xtp$clab, "\" = \"", s4xtp$code, "\"", collapse = "; ")
# rec.code2clab <- paste0("\"", s4xtp$code, "\" = \"", s4xtp$clab, "\"", collapse = "; ")
# lvla4.tp <- paste0(seq(1:length(unique(s4xtp$clab))), " = ", car::recode(levels(s4xtp$clab), rec.clab2code))


s4xtp$codelabs <- paste0("**`", car::recode(s4xtp$clab, rec.clab2code), "`** = _", car::recode(s4xtp$code, rec.code2clab), "_")


Rtdf(s4xtp$codelabs, names = c(" ", "$N_{Articles}$")) %>%
    kable(align = c("l", "r"), row.names = TRUE)


cda4.tp <- paste0(seq(1:length(unique(s4xtp$code))), " = ", levels(s4xtp$code))
levels(s4xtp$code) <- seq(1:length(unique(s4xtp$code)))
ks4xtp <- ftable(s4xtp$bibkey, s4xtp$code) %>% as.matrix ## "ks4x" == "bibkeys - s4x" ##
ks4xtp <- ifelse(ks4xtp >= 1, "$\\checkmark$", "$\\cdot$")
z <- paste0("@", rownames(ks4xtp))
ks4xtp <- cbind(z, ks4xtp)
rownames(ks4xtp) <- NULL
#'
#+ eval=FALSE, echo=FALSE

dt4xtp <- ftable(s4xtp$bibkey, s4xtp$code) %>% as.matrix ## "ks4x" == "bibkeys - s4x" ##
dt4xtp <- ifelse(dt4xtp >= 1, "&#10003;", ".")
z <- paste0("@", rownames(dt4xtp))
dt4xtp <- cbind(z, dt4xtp)
rownames(dt4xtp) <- NULL
Rdt(dt4xtp, escape = FALSE, filter = 'top', class = 'cell-border hover')
#'
#' \newpage
#'
#+ echo=FALSE
kable(ks4xtp[, 1:7])

#'
#' \newpage
#'
#+ echo=FALSE
kable(ks4xtp[, 8:13])

# pander(lvla4.tp)
#'
#' \newpage
#'
#' # References
#'
#' \refs
#'
