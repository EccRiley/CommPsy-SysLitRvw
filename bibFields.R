#' ---
#' title: "BibTeX Entry Types and Fields"
#' author: "Riley M. Smith"
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#'
#+ setup, echo=FALSE, results='hide', message=FALSE, warning=FALSE, cache=FALSE
source("../SETUP.R")
knitr::opts_chunk$set(
    tidy = FALSE,
    echo = FALSE,
    cache = FALSE,
    fig.keep = 'high',
    fig.show = 'hold',
    results = 'asis',
    autodep = TRUE,
    Rplot = TRUE,
    dev = 'pdf',
    fig.path = 'graphics/rplot-',
    fig.width = 7,
    fig.height = 7,
    out.width = "\\linewidth"
    )
#'
#' # BibEntry Types
#'
#' - article
#' - book
#' - booklet
#' - conference
#' - inbook
#' - incollection
#' - inproceedings
#' - manual
#' - mastersthesis
#' - misc
#' - phdthesis
#' - proceedings
#' - techreport
#' - unpublished
#'
#' # BibEntry Fields
#'
#' - address
#' - annote
#' - author
#' - booktitle
#' - chapter
#' - crossref
#' - edition
#' - editor
#' - howpublished
#' - institution
#' - journal
#' - key
#' - month
#' - note
#' - number
#' - organization
#' - pages
#' - publisher
#' - school
#' - series
#' - title
#' - type
#' - volume
#' - year
#'
#'
#' | Type | Description
#' | :----- | :-----
#' | article | An article from a journal or magazine. Required fields: author, title, journal, year. Optional fields: volume, number, pages, month, note.
#' | book | A book with an explicit publisher. Required fields: author or editor, title, publisher, year. Optional fields: volume or number, series, address, edition, month, note.
#' | booklet | A work that is printed and bound, but without a named publisher or sponsering institution. Required field: title. Optional fields: author, howpublished, address, month, year. note.
#' | conference | The same as INPROCEEDINGS, included for Scribe compatibility.
#' | inbook | A part of a book, which may be a chapter (or section or whatever) and/or a range of pages. Required fields: author or editor, title, chapter and/or pages, publisher, year. Optional fields: volume or number, series, type, address, edition, month, note.
#' | incollection | A part of a book having its own title. Required fields: author, title, booktitle, publisher, year. Optional fields: editor, volume or number, series, type, chapter, pages, address, edition, month, note.
#' | inproceedings | An article in a conference proceedings. Required fields: author, title, booktitle, year. Optional fields: editor, volume or number, series, pages, address, month, organization, publisher, note.
#' | manual | Technical documentation. Required field: title. Optional fields: author, organization, address, edition, month, year, note.
#' | mastersthesis | A Master’s thesis. Required fields: author, title, school, year. Optional fields: type, address, month, note.
#' | misc | Use this type when nothing else fits. Required fields: none. Optional fields: author, title, howpublished, month, year, note.
#' | phdthesis | A PhD thesis. Required fields: author, title, school, year. Optional fields: type, address, month, note.
#' | proceedings | The proceedings of a conference. Required fields: title, year. Optional fields: editor, volume or number, series, address, month, organization, publisher, note.
#' | techreport | A report published by a school or other institution. Required fields: author, title, institution, year. Optional fields: type, address, month, note.
#' | unpublished | A document having an author and title, but not formally published. Required fields: author, title, note. Optional fields: month, year.
#'
#' Table: BibEntry Types
#'
#' ewpage
#'
#' | Field | Definition
#' | :----- | :-----
#' | address | Usually the address of the publisher or other type of institution. For major publishing houses, van Leunen recommends omitting the information entirely. For small publishers, on the other hand, you can help the reader by giving the complete address.
#' | annote | An annotation. It is not used by the standard bibliography styles, but may be used by others that produce an annotated bibliography.
#' | author | The name(s) of the author(s), in the format described in the LATEX book. Remember, all names are separated with the and keyword, and not commas.
#' | booktitle | Title of a book, part of which is being cited. See the LATEX book for how to type titles. For book entries, use the title field instead.
#' | chapter | A chapter (or section or whatever) number.
#' | crossref | The database key of the entry being cross referenced.
#' | edition | The edition of a book for example, “Second”. This should be an ordinal, and should have the first letter capitalized, as shown here; the standard styles convert to lower case when necessary.
#' | editor | Name(s) of editor(s), typed as indicated in the LATEX book. If there is also an author field, then the editor field gives the editor of the book or collection in which the reference appears.
#' | howpublished | How something strange has been published. The first word should be capital- ized.
#' | institution | The sponsoring institution of a technical report.
#' | journal | A journal name.
#' | key | Used for alphabetizing, cross-referencing, and creating a label when the author information is missing. This field should not be confused with the key that appears in the \cite command and at the beginning of the database entry.
#' | month | The month in which the work was published or, for an unpublished work, in which it was written. You should use the standard three-letter abbreviation, as described in Appendix B.1.3 of the LATEX book.
#' | note | Any additional information that can help the reader. The first word should be capitalized.
#' | number | Any additional information that can help the reader. The first word should be capitalized.
#' | organization | The organization that sponsors a conference or that publishes a manual.
#' | pages | One or more page numbers or range of numbers, such as 42–111 or 7,41,73–97 or 43+ (the ‘+’ in this last example indicates pages following that don’t form a simple range). BibTEX requires double dashes for page ranges (--).
#' | publisher | The publisher’s name.
#' | school | The name of the school where a thesis was written.
#' | series | The name of a series or set of books. When citing an entire book, the the title field gives its title and an optional series field gives the name of a series or multi-volume set in which the book is published.
#' | title | The work’s title, typed as explained in the LATEX book.
#' | type | The type of a technical report, for example, “Research Note”.
#' | volume | The volume of a journal or multi-volume book.
#' | year | The year of publication or, for an unpublished work, the year it was written. Generally it should consist of four numerals, such as 1984, although the stan- dard styles can handle any year whose last four nonpunctuation characters are numerals, such as ‘(about 1984)’.
#'
#'
#' Table: BibEntry Fields
#'
#'
#' ewpage
#'
#+
bib <- readLines("MAP.bib")

keys <- bib[grep("\\@\\w+\\{.*?,", bib, perl = TRUE)]
Etype <- gsub("\\@(.*?)\\{.*?,", "\\@\\1", keys, perl = TRUE)
keys <- gsub("\\@\\w+\\{(.*?)", "\\@\\1", keys, perl = TRUE)


address <- bib[grep("address=\\{.*?\\},", bib, perl = TRUE)]
annote <- bib[grep("annote=\\{.*?\\},", bib, perl = TRUE)]
author <- bib[grep("author=\\{.*?\\},", bib, perl = TRUE)]
booktitle <- bib[grep("booktitle=\\{.*?\\},", bib, perl = TRUE)]
chapter <- bib[grep("chapter=\\{.*?\\},", bib, perl = TRUE)]
crossref <- bib[grep("crossref=\\{.*?\\},", bib, perl = TRUE)]
edition <- bib[grep("edition=\\{.*?\\},", bib, perl = TRUE)]
editor <- bib[grep("editor=\\{.*?\\},", bib, perl = TRUE)]
howpublished <- bib[grep("howpublished=\\{.*?\\},", bib, perl = TRUE)]
institution <- bib[grep("institution=\\{.*?\\},", bib, perl = TRUE)]
journal <- bib[grep("journal=\\{.*?\\},", bib, perl = TRUE)]
key <- bib[grep("key=\\{.*?\\},", bib, perl = TRUE)]
month <- bib[grep("month=\\{.*?\\},", bib, perl = TRUE)]
note <- bib[grep("note=\\{.*?\\},", bib, perl = TRUE)]
number <- bib[grep("number=\\{.*?\\},", bib, perl = TRUE)]
organization <- bib[grep("organization=\\{.*?\\},", bib, perl = TRUE)]
pages <- bib[grep("pages=\\{.*?\\},", bib, perl = TRUE)]
publisher <- bib[grep("publisher=\\{.*?\\},", bib, perl = TRUE)]
school <- bib[grep("school=\\{.*?\\},", bib, perl = TRUE)]
series <- bib[grep("series=\\{.*?\\},", bib, perl = TRUE)]
title <- bib[grep("title=\\{.*?\\},", bib, perl = TRUE)]
type <- bib[grep("type=\\{.*?\\},", bib, perl = TRUE)]
volume <- bib[grep("volume=\\{.*?\\},", bib, perl = TRUE)]
year <- bib[grep("year=\\{.*?\\},", bib, perl = TRUE)]


# f <- function(x, bib) {
	# bib <- readLines(bib)
	# y <- bib[grep(paste0(x, "=\\{.*?\\},"), bib, perl = TRUE)]
	# # y <- gsub("[ ]+", x, "=\\{(.*?)\\},", "\\1", y, perl = TRUE)
	# return(y)
# }

# bib <- "MAP.bib"
x <- c("address", "annote", "author", "booktitle", "chapter", "crossref", "edition", 
	"editor", "howpublished", "institution", "journal", "key", "month", "note", "number", 
	"organization", "pages", "publisher", "school", "series", "title", "type", "volume", 
	"year")

# bib1 <- sapply(x, f, bib = bib)

address <- gsub("[ ]+address=\\{(.*?)\\},", "\\1", address, perl = TRUE)
annote <- gsub("[ ]+annote=\\{(.*?)\\},", "\\1", annote, perl = TRUE)
author <- gsub("[ ]+author=\\{(.*?)\\},", "\\1", author, perl = TRUE)
booktitle <- gsub("[ ]+booktitle=\\{(.*?)\\},", "\\1", booktitle, perl = TRUE)
chapter <- gsub("[ ]+chapter=\\{(.*?)\\},", "\\1", chapter, perl = TRUE)
crossref <- gsub("[ ]+crossref=\\{(.*?)\\},", "\\1", crossref, perl = TRUE)
edition <- gsub("[ ]+edition=\\{(.*?)\\},", "\\1", edition, perl = TRUE)
editor <- gsub("[ ]+editor=\\{(.*?)\\},", "\\1", editor, perl = TRUE)
howpublished <- gsub("[ ]+howpublished=\\{(.*?)\\},", "\\1", howpublished, perl = TRUE)
institution <- gsub("[ ]+institution=\\{(.*?)\\},", "\\1", institution, perl = TRUE)
journal <- gsub("[ ]+journal=\\{(.*?)\\},", "\\1", journal, perl = TRUE)
month <- gsub("[ ]+month=\\{(.*?)\\},", "\\1", month, perl = TRUE)
note <- gsub("[ ]+note=\\{(.*?)\\},", "\\1", note, perl = TRUE)
number <- gsub("[ ]+number=\\{(.*?)\\},", "\\1", number, perl = TRUE)
organization <- gsub("[ ]+organization=\\{(.*?)\\},", "\\1", organization, perl = TRUE)
pages <- gsub("[ ]+pages=\\{(.*?)\\},", "\\1", pages, perl = TRUE)
publisher <- gsub("[ ]+publisher=\\{(.*?)\\},", "\\1", publisher, perl = TRUE)
school <- gsub("[ ]+school=\\{(.*?)\\},", "\\1", school, perl = TRUE)
series <- gsub("[ ]+series=\\{(.*?)\\},", "\\1", series, perl = TRUE)
title <- gsub("[ ]+title=\\{(.*?)\\},", "\\1", title, perl = TRUE)
type <- gsub("[ ]+type=\\{(.*?)\\},", "\\1", type, perl = TRUE)
volume <- gsub("[ ]+volume=\\{(.*?)\\},", "\\1", volume, perl = TRUE)
year <- gsub("[ ]+year=\\{(.*?)\\},", "\\1", year, perl = TRUE)

x <- list(address = address, annote = annote, author = author, booktitle = booktitle, chapter = chapter, crossref = crossref, edition = edition, editor = editor, howpublished = howpublished, institution = institution, journal = journal, key = key, month = month, note = note, number = number, organization = organization, pages = pages, publisher = publisher, school = school, series = series, title = title, type = type, volume = volume, year = year)


#' # Reduce to Journals Specific to 
source("journals.R", echo = TRUE)
bibdf <- bib2df("MAP.bib")
bibdf$JOURNAL <- sapply(bibdf$JOURNAL, tolower)
dat$Title <- sapply(dat$Title, tolower)
bibdf.cp <- bibdf[bibdf$JOURNAL %in% dat$Title, ]

