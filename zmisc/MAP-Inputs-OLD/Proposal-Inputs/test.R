#' ---
#' title: "Test"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---

#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R")
knitr::opts_chunk$set(
    echo = FALSE,
    cache = FALSE,
    results = 'asis'
)
#'
#'
#+ crswk
maj <-
    data.frame(
        n = c(" ",
                "`PSY 610`",
                "`PSY 614`",
                "`PSY 610`",
                "`PSY 510`",
                "\\midrule"),
        ti = c(
            "\\rowgroup{\\textbf{\\textit{Major Area Coursework: Applied Social \\& Community Psychology}}}",
            "Top: Community \\& Social Interventions",
            "Advanced Applied Social Psychology \\& Community Psychology",
            "Top: Program Evaluation",
            "Top: Place-Based Research",
            " "),
        g = c(" ",
                  "B+",
                  "A",
                  "A",
                  "A",
                  " "),
        h = c(" ", rep(4, 4),
                " "),
        t = c(" ",
                 "Fall 2013",
                 "Spring 2014",
                 "Fall 2014",
                 "Winter 2016",
                 " "))
#
# knitr::kable(maj, caption = "_Major Area Coursework: Applied
#       Social \\& Community Psychology_",
#       col.names = c("`CRN`",
#                     "Title",
#                     "Grade",
#                     "Credit Hours",
#                     "Term"),
#       align = c('c', 'l', 'c', 'c', 'l'))
#
prac <-
    data.frame(
        n = c(" ",
                "`PSY 609`",
                "`PSY 609`",
                "\\midrule"),
        ti = c("\\rowgroup{\\textbf{\\textit{Applied Community Psychology Parctica}}}",
                  "Prac: Violence Prevention I",
                  "Prac: Violence Prevention II",
                  " "),
        g = c(" ",
                  "P",
                  "P",
                  " "),
        h = c(" ",
                rep(1, 2),
                " "),
        t = c(" ",
                 "Fall 2013",
                 "Winter 2014",
                 " "))

# knitr::kable(prac, caption = "_Applied Community Psychology Parctica_",
#       col.names = rep(" ", 5),
#       align = c('c', 'l', 'c', 'c', 'l'))
minr <-
    data.frame(
        n = c(" ",
                "`SOC 610`",
                "`SOC 610`",
                "`PSY 510`",
                "`SOC 610`",
                "`PSY 597`",
                "`PSY 510`",
                "_`PSY 610`_",
                "\\midrule"),
        ti = c("\\rowgroup{\\textbf{\\textit{Minor Area Coursework: Mixed Methods Research}}}",
                  "Focus Groups",
                  "Qualitative Data Analysis",
                  "HLM/Mixed Effects Models for Longitudinal Data Analysis",
                  "Mixed Methods Research",
                  "Applied Survey Research",
                  "Categorical Data Analysis",
                  "Structural Equation Modeling",
                  " "),
        g = c(" ",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A-",
                  "A",
                  "_IP_",
                  " "),
        h = c(" ",
                rep(4, 7),
                " "),
        t = c(" ",
                 "Spring 2014",
                 "Fall 2014",
                 "Winter 2015",
                 "Winter 2015",
                 "Spring 2015",
                 "Fall 2016",
                 "Winter 2017",
                 " "))

# knitr::kable(minr, caption = "_Minor Area Coursework: Mixed Methods Research_",
#       col.names = rep(" ", 5),
#       align = c('c', 'l', 'c', 'c', 'l'))
reqr <-
    data.frame(
        n = c(" ",
                "`PSY 621`",
                "`PSY 618`",
                "`PSY 622`",
                "`PSY 624`",
                "\\midrule"),
        ti = c("\\rowgroup{\\textbf{\\textit{Research Methods \\& Statistics Coursework Required by Program}}}",
                  "Univariate Quantitative Methods",
                  "Ethics \\& Professional Issues in Applied Research and Practice",
                  "Multiple Regression \\& Multivariate Quantitative Methods",
                  "Research Design in Applied Psychology",
                  " "),
        g = c(" ",
                  "A-",
                  "A",
                  "B+",
                  "A",
                  " "),
        h = c(" ",
                rep(c(5, 4), 2),
                " "),
        t = c(" ",
                 "Fall 2013",
                 "Winter 2014",
                 "Winter 2014",
                 "Spring 2014",
                 " "))

all <- rbind(maj, prac, minr, reqr) %>% as.data.frame()
kable(all)

crswk <-
    data.frame(
        n = c(" ",
                "`PSY 610`",
                "`PSY 614`",
                "`PSY 610`",
                "`PSY 510`",
                " ",
                "`PSY 609`",
                "`PSY 609`",
                " ",
                "`SOC 610`",
                "`SOC 610`",
                "`PSY 510`",
                "`SOC 610`",
                "`PSY 597`",
                "`PSY 510`",
                "_`PSY 610`_",
                " ",
                "`PSY 621`",
                "`PSY 618`",
                "`PSY 622`",
                "`PSY 624`"
        ),
        ti = c("\\rowgroup{\\textbf{\\textit{Major Area Coursework: Applied Social \\& Community Psychology}}}",
                  "Top: Community \\& Social Interventions",
                  "Advanced Applied Social Psychology \\& Community Psychology",
                  "Top: Program Evaluation",
                  "Top: Place-Based Research",
                  "\\rowgroup{\\textbf{\\textit{Applied Community Psychology Parctica}}}",
                  "Prac: Violence Prevention I",
                  "Prac: Violence Prevention II",
                  "\\rowgroup{\\textbf{\\textit{Minor Area Coursework: Mixed Methods Research}}}",
                  "Focus Groups",
                  "Qualitative Data Analysis",
                  "HLM/Mixed Effects Models for Longitudinal Data Analysis",
                  "Mixed Methods Research",
                  "Applied Survey Research",
                  "Categorical Data Analysis",
                  "Structural Equation Modeling",
                  "\\rowgroup{\\textbf{\\textit{Research Methods \\& Statistics Coursework Required by Program}}}",
                  "Univariate Quantitative Methods",
                  "Ethics \\& Professional Issues in Applied Research and Practice",
                  "Multiple Regression \\& Multivariate Quantitative Methods",
                  "Research Design in Applied Psychology"
        ),
        g = c(" ",
                  "B+",
                  "A",
                  "A",
                  "A",
                  " ",
                  "P",
                  "P",
                  " ",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A-",
                  "A",
                  "_IP_",
                  " ",
                  "A-",
                  "A",
                  "B+",
                  "A"
        ),
        h = c(" ",
                rep(4, 4),
                " ",
                rep(1, 2),
                " ",
                rep(4, 7),
                " ",
                rep(c(5, 4), 2)
        ),
        t = c(" ",
                 "Fall 2013",
                 "Spring 2014",
                 "Fall 2014",
                 "Winter 2016",
                 " ",
                 "Fall 2013",
                 "Winter 2014",
                 " ",
                 "Spring 2014",
                 "Fall 2014",
                 "Winter 2015",
                 "Winter 2015",
                 "Spring 2015",
                 "Fall 2016",
                 "Winter 2017",
                 " ",
                 "Fall 2013",
                 "Winter 2014",
                 "Winter 2014",
                 "Spring 2014"
        )
    )

# knitr::kable(all,
#              col.names = c("`CRN`",
#                            "Title",
#                            "Grade",
#                            "Credit Hours",
#                            "Term"),
#              align = c('c', 'l', 'c', 'c', 'l'))





knitr::kable(all2,
             col.names = c("`CRN`",
                           "Title",
                           "Grade",
                           "Credit Hours",
                           "Term"),
             align = c('c', 'l', 'c', 'c', 'l'))
