f <- function(x, bib) {
	bib <- readLines(bib)
	y <- bib[grep(paste0(x, "=\\{.*?\\},"), bib, perl = TRUE)]
	fz <- function(z) {
		gsub("[ ]+", z, "=\\{(.*?)\\},", "\\1", paste0("y$", z), perl = TRUE)
	}
	yz <- sapply(y, fz)
	return(yz)
}

bib <- "MAP.bib"
x <- c("address", "annote", "author", "booktitle", "chapter", "crossref", "edition", 
	"editor", "howpublished", "institution", "journal", "key", "month", "note", "number", 
	"organization", "pages", "publisher", "school", "series", "title", "type", "volume", 
	"year")

bib1 <- sapply(x, f, bib = bib)