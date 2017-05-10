file <- "MAP-v6 copy.Rmd"
ex <- data.frame(
	a = c("`\\{\\{(.*?) \\}\\}`",
		"\\\\chisq",
		"\\\\url\\{(.*?)\\}",
		"\\\\%",
        "\\\\refs",
		"\\\\doublespacing",
		"\\\\Frule",
		"^$^$^$",
		"^$^$^$"),
	b = c("\\1",
		"\\\\chi^{2}",
		"\\1",
		"%",
		"",
		"",
		"",
		"",
		""))


library(Riley)

Runtex(file = file, addlexpr = ex, cat = TRUE, catFile = "MAP-v6-untex.Rmd")
