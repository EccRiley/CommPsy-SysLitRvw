file <- "MAP-v9-1-untex.Rmd"
ex <- data.frame(
	a = c("`\\{\\{(.*?)\\}\\}`",
		"\\\\chisq",
		"\\\\url\\{(.*?)\\}",
		"\\\\%",
        "\\\\refs",
		"\\\\doublespacing",
		"\\\\Frule",
        "^$^$^$",
		"^$^$^$",
        "\\.pdf",
        "\\\\rowgroup\\[.*?\\]\\{(.*?)\\}",
        "\\\\part\\{(.*)\\}",
        "\\\\&",
        "\\\\tufteskip"),
	b = c("\\1",
		"\\\\chi^{2}",
		"\\1",
		"%",
		"",
		"",
		"",
		"",
		"",
        "\\.png",
        "\\1",
        "# \\1",
        "&",
        ""))


library(Riley)

Runtex(file = file, addlexpr = ex, cat = TRUE, catFile = "MAP-v9-docx.Rmd")
