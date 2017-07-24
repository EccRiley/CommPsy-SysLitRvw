f1 <- "MAP-v13-2.Rmd"
f2 <- "MAP-v13-2-untex.Rmd"

yml <- "---
title: |
  | Major Area Paper
  | (v13.1)
author: \"Rachel M. Smith\"
date: \"`r format(Sys.Date(), '%d %B %Y')`\"
fignos-plus-name: \"Figure \"
tablenos-plus-name: \"Table \"
---"

ymlline <- 10

ex <- data.frame(
	a = c("`\\{\\{(.*?)\\}\\}`",
		"(\\\\)+chisq",
		"(\\\\)+url\\{(.*?)\\}",
		"(\\\\)+%",
        "(\\\\)+refs",
		"(\\\\)+Frule",
        "\\.pdf",
        "(\\\\)+rowgroup\\[.*?\\]\\{(.*?)\\}",
		"\\\\part\\{(.*?)\\} <!\\-\\- (Part I{1,})\\. \\-\\->",
        "(\\\\)+&",
        "(\\\\)+tufteskip",
        "`r tufte::newthought\\(\"(.*?)\")`"
        ),
	b = c("\\1",
		"\\\\chi^{2}",
		"\\1",
		"%",
		"",
		"",
        "\\.png",
        "\\1",
		"# \\2. \\1",
        "&",
        "",
        "**\\1.**"
		))
        # "\n> \\1\n"


# library(Riley)
source("../Riley/R/Rretex.R")
source("../Riley/R/Runtex.R")

Runtex(file = f1, addlexpr = ex, yml = yml, ymlline = ymlline, cat = TRUE, catFile = f2)

.Rword(f2)
