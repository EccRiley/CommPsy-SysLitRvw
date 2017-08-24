f1 <- "z.R"
f2 <- "zz.R"

yml <- "#' ---
#' title: \"Appendix C: Results from Qualitative Comparative Analyses\"
#' author: \"Riley M. Smith\"
#' date: \"`r format(Sys.Date(), '%d %B %Y')`\"
#' ---"

ymlline <- 5

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
        "`r tufte::newthought\\(\"(.*?)\")`",
		"source\\(bibs\\.R\\)"
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
        "**\\1.**",
		"source\\(z\\.R\\)"
		))
        # "\n> \\1\n"


# library(Riley)
source("../Riley/R/Rretex.R")
source("../Riley/R/Runtex.R")

Runtex(file = f1, addlexpr = ex, yml = yml, ymlline = ymlline, cat = TRUE, catFile = f2)

# .Rword(f2)
