# 
# RtufteB <- function(x, ...) {rmarkdown::render(x, output_format = "tufte::tufte_book", ...)}
# RtufteH <- function(x, ...) {rmarkdown::render(x, output_format = "tufte::tufte_handout", ...)}
# RtufteHtml <- function(x, ...) {rmarkdown::render(x, output_format = "tufte::tufte_html", ...)}
# Rbeam <- function(x, ...) {rmarkdown::render(x, output_format = "beamer_presentation", ...)}
# Rword <- function(x, ...) {rmarkdown::render(x, output_format = "word_document", ...)}
# Rpdf <- function(x, ...) {rmarkdown::render(x, output_format = "pdf_document", ...)}
# Rnb <- function(x, ...) {rmarkdown::render(x, output_format = "html_notebook", ...)}
# Rhtml <- function(x, ...) {rmarkdown::render(x, output_format = "html_document", ...)}
# Repub <- function(x, ...) { ## Rendering epubs requires a second step with the .epub output to generate the kindle .mobi format ##
# 	rmarkdown::render(x, output_format = "bookdown::epub_book", ...)
# 	p <- gsub("\\.Rmd", "\\.epub", x)
# 	bookdown::kindlegen(p)
# 	}


RtftB <- function(x, ...) {rmarkdown::render(x, output_format = "tufte::tufte_book", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
RtftH <- function(x, ...) {rmarkdown::render(x, output_format = "tufte::tufte_handout", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
RtftHt <- function(x, ...) {rmarkdown::render(x, output_format = "tufte::tufte_html", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
RtftHt2 <- function(x, ...) {rmarkdown::render(x, output_format = "bookdown::tufte_html2", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
Rbeam <- function(x, ...) {rmarkdown::render(x, output_format = "beamer_presentation", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
Rword <- function(x, ...) {rmarkdown::render(x, output_format = "word_document", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
Rpdf <- function(x, ...) {rmarkdown::render(x, output_format = "pdf_document", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
Rnb <- function(x, ...) {rmarkdown::render(x, output_format = "html_notebook", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
Rhtml <- function(x, ...) {rmarkdown::render(x, output_format = "html_document", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
Rhtml2 <- function(x, ...) {rmarkdown::render(x, output_format = "bookdown::html_document2", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
RhMat <- function(x, ...) {rmarkdown::render(x, output_format = "rmdformats::material", ...); print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))}
Repub <- function(x, ...) { ## Rendering epubs requires a second step with the .epub output to generate the kindle .mobi format ##
	rmarkdown::render(x, output_format = "bookdown::epub_book", ...)
#  p <- gsub("\\.Rmd", "\\.epub", x)
#  bookdown::kindlegen(p)
#  print(format(Sys.time(), '%n[%d %b. %I:%M%p]'))
	}