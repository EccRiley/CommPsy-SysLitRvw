file <- "MAP-v6 copy.Rmd"
ex <- data.frame(
	a = c("`\\{\\! (.*?) \\!\\}`",
		"\\\\chisq",
		"\\\\url\\{(.*?)\\}",
		"\\\\%",
		"\\\\parindent=\\-1\\.75em",
		"\\\\setlength\\{\\\\parskip\\}\\{0\\.25\\\\baselineskip\\}",
		"\\\\doublespacing",
		"\\\\Frule",
		" \n",
		"\n\n\n"),
	b = c("\\1",
		"\\\\chi^{2}",
		"\\1",
		"%",
		" ",
		" ",
		" ",
		" ",
		"\n",
		"\n"))


library(Riley)

Runtex(file = file, addlexpr = ex, cat = TRUE, catFile = "MAP-v6-untex-2.Rmd")

