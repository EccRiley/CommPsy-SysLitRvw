library(scales)
library(knitr)

s1psy <- 4386
s1wos <- 3376
s1mn <- min(c(s1psy = s1psy, s1wos = s1wos))
s1mx <- max(c(s1psy = s1psy, s1wos = s1wos))
s1 <- range(c(s1psy = s1psy, s1wos = s1wos))
s2psy <- 337
s2wos <- 686
s2mn <- min(c(s2psy = s2psy, s2wos = s2wos))
s2mx <- max(c(s2psy = s2psy, s2wos = s2wos))
s2 <- range(c(s2psy = s2psy, s2wos = s2wos))
s3psy <- 32
s3wos <- 46
s3mn <- min(c(s3psy = s3psy, s3wos = s3wos))
s3mx <- max(c(s3psy = s3psy, s3wos = s3wos))
s3 <- range(c(s3psy = s3psy, s3wos = s3wos))
s4psy <- 18
s4wos <- 34
s4mn <- min(c(s4psy = s4psy, s4wos = s4wos))
s4mx <- max(c(s4psy = s4psy, s4wos = s4wos))
s4 <- range(c(s4psy = s4psy, s4wos = s4wos))
s5psy <- 0
s5wos <- 4
s5mn <- min(c(s5psy = s5psy, s5wos = s5wos))
s5mx <- max(c(s5psy = s5psy, s5wos = s5wos))
s5 <- range(c(s5psy = s5psy, s5wos = s5wos))
s6psy <- 0
s6wos <- 2
s6mn <- min(c(s6psy = s6psy, s6wos = s6wos))
s6mx <- max(c(s6psy = s6psy, s6wos = s6wos))
s6 <- range(c(s6psy = s6psy, s6wos = s6wos))


n <- c(s1psy, s1wos, s2psy, s2wos, s3psy, s3wos, s4psy, s4wos, s5psy, s5wos, s6psy, s6wos)
srch.n <- sort(rep(1:6, 2))
names(n) <- paste0("s", srch.n, c("psy", "wos"))
db <- ifelse(grepl("psy", names(n)), "psy", "wos")
srch.mn <- c(s1mn, s1mn, s2mn, s2mn, s3mn, s3mn, s4mn, s4mn, s5mn, s5mn, s6mn, s6mn)
srch.mx <- c(s1mx, s1mx, s2mx, s2mx, s3mx, s3mx, s4mx, s4mx, s5mx, s5mx, s6mx, s6mx)

srchdf <- data.frame(srch.n, db, n, srch.mn, srch.mx)

db.n <- data.frame(wos = c(s1wos, s2wos, s3wos, s4wos, s5wos, s6wos), psy = c(s1psy, s2psy, s3psy, s4psy, s5psy, s6psy))
db.n$mn <- apply(db.n, 1, min)
db.n$mx <- apply(db.n, 1, max)
db.n$whchmn <- apply(db.n[, 1:2], 1, which.min)
db.n$whchmx <- apply(db.n[, 1:2], 1, which.max)
db.n <- within(db.n, {
    whchmn <- ifelse(whchmn == 1, "Web of Science", "PsychINFO")
    whchmx <- ifelse(whchmx == 1, "Web of Science", "PsychINFO")
})

dbsrch <- data.frame(n = c("1.", "2.", "3.", "4.", "5.", "6."),
                     srch = c("\\textbf{IPV, Domestic Violence, or Partner Abuse} - \\textit{General}",
                              "--- \\textit{Interventions}",
                              "--- \\textit{Intervention Evaluations}",
                              "\\textbf{Female Same-Sex/Same-Gender IPV, Domestic Violence, or Partner Abuse} - \\textit{General}",
                              "--- \\textit{Interventions}",
                              "--- \\textit{Intervention Evaluations}"),
                     nres = c(
                         paste0(comma(db.n$mn[1]), ", ", comma(db.n$mx[1]),
                                " (\\textit{", db.n$whchmx[1], "})"),
                         paste0(comma(db.n$mn[2]), ", ", comma(db.n$mx[2]),
                                " (\\textit{", db.n$whchmx[2], "})"),
                         paste0(comma(db.n$mn[3]), ", ", comma(db.n$mx[3]),
                                " (\\textit{", db.n$whchmx[3], "})"),
                         paste0(comma(db.n$mn[4]), ", ", comma(db.n$mx[4]),
                                " (\\textit{", db.n$whchmx[4], "})"),
                         paste0(comma(db.n$mn[5]), ", ", comma(db.n$mx[5]),
                                " (\\textit{", db.n$whchmx[5], "})"),
                         paste0(comma(db.n$mn[6]), ", ", comma(db.n$mx[6]),
                                " (\\textit{", db.n$whchmx[6], "})"))) #,
                     # mxdb = c(db.n$whchmx))

names(dbsrch) <- c(" ",
                   "Database Search",
                   "\\footnotesize{$Range_{N_{Results}}$ (\\textit{Database with most results})}") #,
                   # "Database with most results")


# names(dbsrch) <- c("", "Database Search", "$Range_{N_{Results}}$")
kable(dbsrch, caption = "Descriptions of database searches conducted with corresponding ranges of the number of results returned {#tbl:dbsrch}", align = c("r", "l", "c"))
