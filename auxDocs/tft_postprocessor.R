tft <- function(..., knitr, opts_hooks, opts_knit, i, hook_plot_md, knit_engines) {

    format = html_document(theme = NULL, ...)

    # when fig.margin = TRUE, set fig.beforecode = TRUE so plots are moved before
    # code blocks, and they can be top-aligned
    ohooks = knitr::opts_hooks$set(fig.margin = function(options) {
        if (isTRUE(options$fig.margin)) options$fig.beforecode = TRUE
        options
    })
    post_processor = format$post_processor
    format$post_processor = function(metadata, input, output, clean, verbose) {

        if (is.function(post_processor))
            output = post_processor(metadata, input, output, clean, verbose)

        knitr::opts_hooks$restore(ohooks)

        x = readUTF8(output)
        fn_label = paste0(knitr::opts_knit$get('rmarkdown.pandoc.id_prefix'), 'fn')
        footnotes = tufte:::parse_footnotes(x)#, fn_label)
        notes = footnotes$items
        # replace footnotes with sidenotes
        for (i in seq_along(notes)) {
            num = sprintf(
                '<a href="#%s%d" class="footnoteRef" id="%sref%d"><sup>%d</sup></a>',
                fn_label, i, fn_label, i, i
            )
            con = sprintf(paste0(
                '<label for="tufte-sn-%d" class="margin-toggle sidenote-number"></label>',
                # '<label for="tufte-sn-%d" class="margin-toggle sidenote-number">%d</label>',
                '<input type="checkbox" id="tufte-sn-%d" class="margin-toggle">',
                '<span class="sidenote">%s</span>'
                # '<span class="sidenote"><span class="sidenote-number">%d</span> %s</span>'
            ), i, i, notes[i])
            x = gsub_fixed(num, con, x)
        }
        # remove footnotes at the bottom
        if (length(footnotes$range)) x = x[-footnotes$range]

        # replace citations with margin notes
        x = margin_references(x)

        # place figure captions in margin notes
        x[x == '<p class="caption">'] = '<p class="caption marginnote shownote">'

        # move </caption> to the same line as <caption>; the previous line should
        # start with <table
        for (i in intersect(grep('^<caption>', x), grep('^<table', x) + 1)) {
            j = 0
            while (!grepl('</caption>$', x[i])) {
                j = j + 1
                x[i] = paste0(x[i], x[i + j])
                x[i + j] = ''
            }
        }
        # place table captions in the margin
        r = '^<caption>(.+)</caption>$'
        for (i in grep(r, x)) {
            # the previous line should be <table> or <table class=...>
            if (!grepl('^<table( class=.+)?>$', x[i - 1])) next
            cap = gsub(r, '\\1', x[i])
            x[i] = x[i - 1]
            x[i - 1] = paste0(
                '<p><!--\n<caption>-->', '<span class="marginnote shownote">',
                cap, '</span><!--</caption>--></p>'
            )
        }

        # add an incremental number to the id of <label> and <input> for margin notes
        r = '(<label|<input type="checkbox") (id|for)(="tufte-mn)-(" )'
        m = gregexpr(r, x)
        j = 1
        regmatches(x, m) = lapply(regmatches(x, m), function(z) {
            n = length(z)
            if (n == 0) return(z)
            if (n %% 2 != 0) warning('The number of labels is different with checkboxes')
            for (i in seq(1, n, 2)) {
                if (i + 1 > n) break
                z[i + (0:1)] =  gsub(r, paste0('\\1 \\2\\3-', j, '\\4'), z[i + (0:1)])
                j <<- j + 1
            }
            z
        })

        # restore blockquote footer from <span class="blockquote footer">
        r = '^<p><span class="blockquote footer">(.+)</span></p>$'
        i = grep(r, x)
        x[i] = gsub(r, '<footer>\\1</footer>', x[i])

        writeUTF8(x, output)
        output
    }

    if (is.null(format$knitr$knit_hooks)) format$knitr$knit_hooks = list()
    format$knitr$knit_hooks$plot = function(x, options) {
        # make sure the plot hook always generates HTML code instead of ![]()
        if (is.null(options$out.extra)) options$out.extra = ''
        fig_margin = isTRUE(options$fig.margin)
        fig_fullwd = isTRUE(options$fig.fullwidth)
        if (fig_margin || fig_fullwd) {
            if (is.null(options$fig.cap)) options$fig.cap = ' ' # empty caption
        } else if (is.null(options$fig.topcaption)) {
            # for normal figures, place captions at the top of images
            options$fig.topcaption = TRUE
        }
        res = knitr::hook_plot_md(x, options)
        if (fig_margin) {
            res = gsub_fixed('<p class="caption">', '<!--\n<p class="caption marginnote">-->', res)
            res = gsub_fixed('</p>', '<!--</p>-->', res)
            res = gsub_fixed('</div>', '<!--</div>--></span></p>', res)
            res = gsub_fixed(
                '<div class="figure">', paste0(
                    '<p>', '<span class="marginnote shownote">', '<!--\n<div class="figure">-->'
                ), res
            )
        } else if (fig_fullwd) {
            res = gsub_fixed('<div class="figure">', '<div class="figure fullwidth">', res)
            res = gsub_fixed(
                '<p class="caption">', '<p class="caption marginnote shownote">', res
            )
        }
        res
    }

    knitr::knit_engines$set(marginfigure = function(options) {
        options$type = 'marginnote'
        if (is.null(options$html.tag)) options$html.tag = 'span'
        options$html.before = marginnote_html()
        eng_block = knitr::knit_engines$get('block')
        eng_block(options)
    })

    format$inherits = 'html_document'

    format

}

Rhtout <- function() {
    htouts <- list.files(pattern = "\\.html")
    htouts_in <- sapply(htouts, readLines, encoding = "UTF-8")
    for (i in 1:length(htouts_in)){
        htouts_in[[i]] <- gsub("about_files/tufte\\-css\\-2015\\.12\\.29/tufte\\.css",
                               "auxDocs/riley.css", htouts_in[[i]],
                               fixed = FALSE, perl = TRUE, useBytes = TRUE)
        htouts_in[[i]] <- gsub("about_files",
                               "auxDocs", htouts_in[[i]],
                               fixed = FALSE, perl = TRUE, useBytes = TRUE)

        writeLines(htouts_in[[i]], htouts[i], useBytes = TRUE)
    }
}

Rsite <- function(...) {
    rmarkdown::render_site(output_format = tufte::tufte_html(self_contained = FALSE), encoding = 'UTF-8', ...)
    tft()
    Rhtout()
}
# gsub("(.*?)\\.html", "\\1_2.html", htouts_in[1]
