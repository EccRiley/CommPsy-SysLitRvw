file <- "MAP-v12.Rmd"

yml <- "---
title: |
| Major Area Paper
| (v12.1)
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
        "^$^$^$",
		"^$^$^$",
        "\\.pdf",
        "(\\\\)+rowgroup\\[.*?\\]\\{(.*?)\\}",
		"\\\\part\\{(.*?)\\} <!\\-\\- (Part I{1,})\\. \\-\\->",
        "(\\\\)+part\\{(.*)\\}",
        "(\\\\)+&",
        "(\\\\)+tufteskip"
		),
	b = c("\\1",
		"\\\\chi^{2}",
		"\\1",
		"%",
		"",
		"",
		"",
		"",
        "\\.png",
        "\\1",
		"# \\2. \\1",
        "# \\1",
        "&",
        ""))


# library(Riley)
source("../Riley/R/Rretex.R")
source("../Riley/R/Runtex.R")

Runtex(file = file, addlexpr = ex, yml = yml, ymlline = ymlline, cat = TRUE, catFile = "MAP-v12-untex4.Rmd")
