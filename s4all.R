
s4all <- MAP2[MAP2$scat == "S4", ]
s4all$bibkey <- as.character(s4all$bibkey)
s4excl <- s4all[!s4all$bibkey %in% MAP$bibkey, ]
