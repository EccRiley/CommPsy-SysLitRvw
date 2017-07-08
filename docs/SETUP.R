options(prompt = "> ", continue = "... ", width = 60, scipen = 10, error = NULL, knitr.kable.NA = "-")
auto.loads <- c("Riley", "tidyr", "plyr", "dplyr", "devtools",
                "rmarkdown", "knitr", "tufte", "formatR", "pander",
                "bibtex", "knitcitations",
                "scales", "sysfonts", "extrafont", "showtext")
sshhh <- function(a.package)
{
    suppressWarnings(suppressPackageStartupMessages(library(a.package,
                                                            character.only = TRUE)))
}
invisible(sapply(auto.loads, sshhh))
library(extrafont)
library(sysfonts)
loadfonts(quiet = TRUE)
quartzFonts(serif = quartzFont(c("ETBembo-RomanOSF", "ETBembo-BoldLF",
                                 "ETBembo-DisplayItalic", "ETBembo-SemiBoldOSF")))
options(bib.loc = "auxDocs/REFs.bib", citation_format = "pandoc", knitr.table.format = "pandoc",
        xtable.floating = FALSE, scipen = 0, digits = 3, zero.print = ".",
        formatR.arrow = TRUE, DT.options = list(pageLength = 200, language = list(search = "Filter:")),
        bib.loc = "auxDocs/REFs.bib", citation_format = "pandoc", knitr.table.format = "pandoc",
        formatR.arrow = TRUE, xtable.comment = FALSE, xtable.booktabs = TRUE,
        xtable.caption.placement = "top", xtable.caption.width = "\\linewidth",
        xtable.sanitize.text.function = function(x)
        {
            x
        }, xtable.table.placement = "hb", xtable.floating = FALSE)
knitr::opts_chunk$set(warning = FALSE, message = FALSE, comment = ">>",
                      fig.showtext = TRUE, highlight = TRUE, background = "#f2f2f2", size = "footnotesize",
                      cache = FALSE, autodep = TRUE, fig.path = "graphics/rplot-", fig.width = 7,
                      fig.height = 7, out.width = "\\linewidth")
devtools::session_info()
seed <- 42
set.seed(seed)
panderOptions("digits", 3)
panderOptions("round", 3)
panderOptions("missing", "-")
panderOptions("keep.line.breaks", TRUE)
panderOptions("table.style", "multiline")
panderOptions("table.alignment.default", "left")
panderOptions("table.split.table", 160)
panderOptions("table.split.cells", 50)
panderOptions("table.continues", "")
panderOptions("p.copula", ", and ")
cite_options(citation_format = "pandoc", style = "pandoc", hyperlink = "to.bib",
             max.names = 6, longnamesfirst = TRUE, check.entries = TRUE)
today <- format(Sys.Date(), "%d %B %Y")
origin.xls <- as.Date("1899-12-30")
origin.r <- as.Date("1970-01-01")
font.add("ETBembo", regular = "/Library/Fonts/ETBembo-RomanOSF.ttf", bold = "/Library/Fonts/ETBembo-SemiBoldOSF.ttf",
         italic = "/Library/Fonts/ETBembo-DisplayItalic.ttf")
font.add("Palatino", "/Library/Fonts/Palatino")
library(showtext)
showtext.auto(enable = TRUE)
# pal_my <- c("#eef0f2", "#d1d6dc", "#da00d1", "#a700a0", "#74006f", "#dee800",
           # "#adb500", "#00f485", "#00b965", "#006d3b", "#00d8d8", "#008b8b", "#003f3f",
           # "#053efa", "#0434d4", "#021860", "#7a8998", "#56636f", "#2e363e", "#181C20")
options(palette = pal_my)
# mypal3 <- pal_my[3:16]
# mypal2 <- pal_my[c(4, 9, 12, 15)]
# mypal22 <- c("#29B78B", "#0434D4", "#00E6AE", "#01AFD7", "#A700A0")
# pal_my.a75 <- sapply(pal_my, adjustcolor, alpha.f = 0.75)
# pal_my.a50 <- sapply(pal_my, adjustcolor, alpha.f = 0.5)
# mb <- colorRampPalette(pal_my[c(5, 16)])
# magblu <- mb(10)
# pdxpal <- c("#8b9535", "#ffffff", "#373737")
# ynpal <- pal_my[c(10, 1, 2)]
# bmon_pal <- pal_my[c(16, 1, 17)]
# bmon_pal2 <- pal_my[c(16, 18, 17)]
# bstds_pal <- pal_my[c(5, 1, 17)]
# bstds_pal2 <- pal_my[c(5, 18, 17)]
# stmon_pal <- pal_my[c(17, 16, 5)]
# bmon_palX <- pal_my[c(17, 16)]
# bstds_palX <- pal_my[c(17, 5)]
R.adjCol <- function(x, a)
{
    adjustcolor(x, alpha = a)
}
# stmon_pal2 <- as.character(lapply(stmon_pal, R.adjCol, a = 0.9))
# stmon_pal.ls <- list(Standards = pal_my[17], Monitoring = pal_my[16], `Standards \\&\nMonitoring` = pal_my[5])
# pal_HC <- c("#7cb5ec", "#434348", "#90ed7d", "#f7a35c", "#8085e9", "#f15c80",
#             "#e4d354", "#8085e8", "#8d4653", "#91e8e1")
# pal_HCd <- c("#2b908f", "#90ee7e", "#f45b5b", "#7798BF", "#aaeeee", "#ff0066",
#              "#eeaaee", "#55BF3B", "#DF5353", "#7798BF", "#aaeeee")
# pal_sci <- c("#0b2b4a", "#114477", "#44aa88", "#77112b", "#de1f51", "#ddd10a")
# pal_nord <- list(
#     polar = c("#2e343f", "#2e343f", "#2e343f", "#2e343f"),
#     snow = c("#d8dee8", "#e5e9ef", "#eceff4", "#eceff4"),
#     frost = c("#8fbcbb", "#88c0ce", "#81a1be", "#5e81a8"),
#     aurora = c("#bf606b", "#d08674", "#ebca93", "#a3be91", "#b48eab")
# )
# clrss <- colorRampPalette(pal_sci, alpha = T)
grad <- function(x, p = cols1, alpha = 1)
{
    if (class(x) == "data.frame")
        l <- nrow(x)
    else l <- length(x)
    adjustcolor(p(l), alpha.f = alpha)
}
Rformat <- function(x, big.mark = ",", ...)
{
    format(x, big.mark = big.mark)
}
library(pander)
# capwords <- function(s, strict = FALSE)
# {
#     cap <- function(s) paste(toupper(substring(s, 1, 1)), {
#         s <- substring(s, 2)
#         if (strict)
#             tolower(s)
#         else s
#     }, sep = "", collapse = " ")
#     sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
# }
Rformats <- list(big = ",", dgts = 0)

# mp <- function(..., plotlist = NULL, file, cols = 1, layout = NULL)
# {
#     library(grid)
#     plots <- c(list(...), plotlist)
#     numPlots <- length(plots)
#     if (is.null(layout))
#     {
#         layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), ncol = cols,
#                          nrow = ceiling(numPlots/cols))
#     }
#     if (numPlots == 1)
#     {
#         print(plots[[1]])
#     }
#     else
#     {
#         grid.newpage()
#         pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#         for (i in 1:numPlots)
#         {
#             matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#             print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                             layout.pos.col = matchidx$col))
#         }
#     }
# }
# sedit <- function(x)
# {
#     file.edit(x)
#     source(x)
# }
# R.rrefs <- function(bib_main = "REFs.bib", bib_r = "rrefs.bib", perl = TRUE)
# {
#     r_refs(file = bib_main, append = TRUE)
#     r_refs(file = bib_r, append = FALSE)
#     rrefs <- R.cite_r(file = bib_r)
#     rrefs <- gsub(" and the R-packages", ";", rrefs, perl = perl)
#     rrefs <- gsub("\\], and", "\\];", rrefs, perl = perl)
#     rrefs <- gsub("\\],", "\\];", rrefs, perl = perl)
#     rrefs <- strsplit(rrefs, "; ", perl = perl)
#     rrefs <- data.frame(rrefs)
#     names(rrefs) <- "R-References"
#     return(kable(rrefs, col.names = ""))
# }
# R.percTEX <- function(x, n = 14)
# {
#     if (length(x) == 0)
#         return(character())
#     y <- round(x/n, digits = getOption("digits"))
#     paste0(comma(y * 100), "\\%")
# }
# R.Euvenn <- function(x, vcol, ...)
# {
#     evenn <- venneuler(x)
#     evenn$labels <- NA
#     plot(evenn, col = vcol, alpha = 0.75, ...)
#     text(evenn$centers, labels = colnames(x), col = "#c0c0c0", cex = 2)
#     return(evenn)
# }
# R.Evenn <- function(x, vcol, labs = rev(names(x)), al = 0.5, lcex = 1,
#                     adj = NULL, ...)
# {
#     A <- x[[2]]
#     AB <- x[[1]]
#     evenn <- venneuler(c(A = A, `A&B` = AB))
#     evenn$labels <- c("", "")
#     plot(evenn, col = vcol, alpha = al, ...)
#     text(evenn$centers, labels = labs, col = pal_my[1], cex = lcex, adj = adj)
#     return(evenn)
# }
is_html_output <- function(...) knitr:::is_html_output(...)
is_latex_output <- function(...) knitr:::is_latex_output(...)
# R.regTEX <- function(txt, op = 1)
# {
#     if (op == 1)
#     {
#         TX <- gsub("\\*\\*(.*?)\\*\\*", "\\\\textbf\\{\\1\\}", txt, perl = TRUE)
#         TX <- gsub("_(.*?)_", "\\\\textit\\{\\1\\}", TX, perl = TRUE)
#         TX <- gsub("`(.*?)`", "\\\\texttt\\{\\1\\}", TX, perl = TRUE)
#     }
#     else
#     {
#         TX <- gsub("\\\\textbf\\{(.*?)\\}", "\\*\\*\\1\\*\\*", txt, perl = TRUE)
#         TX <- gsub("\\\\textit\\{(.*?)\\}", "_\\1_", TX, perl = TRUE)
#         TX <- gsub("\\\\texttt\\{(.*?)\\}", "`\\1`", TX, perl = TRUE)
#     }
#     return(TX)
# }
# R.rspss <- function(x, vlabs = FALSE, df = TRUE, ...)
# {
#     read.spss(x, use.value.labels = vlabs, to.data.frame = df)
# }
# knitr::knit_hooks$set(Rplot = function(before, options, envir)
# {
#     if (before)
#     {
#         palette(pal_my)
#         par(bg = "transparent", font.main = 3, family = "ETBembo")
#     }
# })
knitr::knit_hooks$set(Rrule = function(before, options, envir)
{
    if (is_latex_output())
    {
        if (before)
        {
            if (!is.null(options$Rrule))
            {
                "\\Rrule\n\n"
            }
        }
        else
        {
            if (!is.null(options$Rerule))
            {
                "\n\n\\Rerule"
            }
        }
    }
})
knitr::opts_hooks$set(echoRule = function(options)
{
    if (options$echo == FALSE)
    {
        options$Rrule <- NULL
        options$Rerule <- NULL
    }
    else
    {
        options$Rrule <- TRUE
        options$Rerule <- TRUE
    }
    options
})
knitr::opts_chunk$set(Rplot = TRUE, echoRule = TRUE)
knitr::knit_hooks$set(Rruleb = function(before, options, envir)
{
    if (is_latex_output())
    {
        if (before)
        {
            "\\Rruleb\n\n"
        }
        else
        {
            "\n\n\\Reruleb"
        }
    }
})
knitr::opts_hooks$set(echoRuleb = function(options)
{
    if (options$echo == FALSE)
    {
        options$Rruleb <- NULL
    }
    else
    {
        options$Rruleb <- TRUE
    }
    options
})
knitr::opts_chunk$set(echoRuleb = NULL)
RtftB <- function(x, ...)
{
    rmarkdown::render(x, output_format = "tufte::tufte_book", ...)
    print(format(Sys.time(), "%n[%d %b. %I:%M%p]"))
}
RtftH <- function(x, ...)
{
    rmarkdown::render(x, output_format = "tufte::tufte_handout", ...)
    print(format(Sys.time(), "%n[%d %b. %I:%M%p]"))
}
RtftHt <- function(x, ...)
{
    rmarkdown::render(x, output_format = "tufte::tufte_html", ...)
    print(format(Sys.time(), "%n[%d %b. %I:%M%p]"))
}
RtftHt2 <- function(x, ...)
{
    rmarkdown::render(x, output_format = "bookdown::tufte_html2", ...)
    print(format(Sys.time(), "%n[%d %b. %I:%M%p]"))
}
Rbeam <- function(x, ...)
{
    rmarkdown::render(x, output_format = "beamer_presentation", ...)
    print(format(Sys.time(), "%n[%d %b. %I:%M%p]"))
}
Rword <- function(x, ...)
{
    rmarkdown::render(x, output_format = "word_document", ...)
    print(format(Sys.time(), "%n[%d %b. %I:%M%p]"))
}
Rpdf <- function(x, ...)
{
    rmarkdown::render(x, output_format = "pdf_document", ...)
    print(format(Sys.time(), "%n[%d %b. %I:%M%p]"))
}
Rnb <- function(x, ...)
{
    rmarkdown::render(x, output_format = "html_notebook", ...)
    print(format(Sys.time(), "%n[%d %b. %I:%M%p]"))
}
Rhtml <- function(x, ...)
{
    rmarkdown::render(x, output_format = "html_document", ...)
    print(format(Sys.time(), "%n[%d %b. %I:%M%p]"))
}
Rhtml2 <- function(x, ...)
{
    rmarkdown::render(x, output_format = "bookdown::html_document2", ...)
    print(format(Sys.time(), "%n[%d %b. %I:%M%p]"))
}
