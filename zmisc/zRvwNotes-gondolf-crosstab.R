source("../SETUP.R")
rpm()

reassault <- list(physical = c("Any Reassault",
                               "Any Severe Ressault",
                               "Any Repeated Reassault",
                               "Any Injury"),
                  nonPhysical = c("Any Controlling Behavior",
                                  "Any Verbal Abuse",
                                  "Any Threats"))
total <- 665
site <- c("Pittsburg", "Dallas", "Houston", "Denver")
n <- c(180, 144, 160, 174)
np <- round(n/total, 1)
np14 <- round(n[c(1, 4)]/sum(n[c(1, 4)]), 1)

reassault <- c(63, 52, 48, 47)
chisq.test(reassault, p = np, correct = FALSE)
chisq.test(reassault, correct = FALSE)
reassault14 <- reassault[c(1, 4)]
chisq.test(reassault14, p = np14)
severe <- c(41, 38, 33, 21)
chisq.test(severe, p = np, correct = T)

repeated <- c(43, 34, 25, 19)
injury<- c(35, 37, 33, 23)
controlling <- c(78, 67, 75, 79)
verbal <- c(140, 101, 102, 117)
threats <- c(81, 60, 72, 70)

dat.yes <- cbind(site, n, reassault, severe, repeated,
                 injury, controlling, verbal, threats) %>%
    data.frame(row.names = site)

RA.p <- reassault/n
# chisq.test(RA.p, p = np, correct = FALSE)
chisq.test(RA.p correct = FALSE)
RA.p14 <- RA.p[c(1, 4)]
chisq.test(RA.p14, p = np14, correct = FALSE)

SV.p <- severe/n
RP.p <- repeated/n
IJ.p <- injury/n
CT.p <- controlling/n
VB.p <- verbal/n
TH.p <- threats/n

dat.yes.p <- cbind(site, n, RA.p, SV.p, RP.p, IJ.p, CT.p, VB.p, TH.p) %>%
    data.frame(row.names = site)

dat.y.ra <- dat.yes[, c("site", "reassault")]
dat.y.ra$reassault <- as.integer(dat.y.ra$reassault)
xtabs(reassault ~ ., dat.y.ra) %>% summary()

dnoreassault <- n - reassault
nosevere <- n - severe
norepeated <- n - repeated
noinjury <- n - injury
nocontrolling <- n - controlling
noverbal <- n - verbal
nothreats <- n - threats

nRA.p <- noreassault/n
nSV.p <- nosevere/n
nRP.p <- norepeated/n
nIJ.p <- noinjury/n
nCT.p <- nocontrolling/n
nVB.p <- noverbal/n
nTH.p <- nothreats/n

dat.no <- cbind(site, n, noreassault, nosevere, norepeated,
                noinjury, nocontrolling, noverbal, nothreats) %>%
    data.frame(row.names = site)

dat.no.p <- cbind(site, n, nRA.p, nSV.p, nRP.p, nIJ.p, nCT.p, nVB.p, nTH.p) %>%
    data.frame(row.names = site)

maxN <- max(n)
ndiff <- maxN - n

ra.PB <- c(rep(1, reassault[1]), rep(0, noreassault[1]), rep(NA, ndiff[1]))
ra.DL <- c(rep(1, reassault[2]), rep(0, noreassault[2]), rep(NA, ndiff[2]))
ra.HT <- c(rep(1, reassault[3]), rep(0, noreassault[3]), rep(NA, ndiff[3]))
ra.DV <- c(rep(1, reassault[4]), rep(0, noreassault[4]), rep(NA, ndiff[4]))
ra <- data.frame(ra.PB, ra.DL, ra.HT, ra.DV)
names(ra) <- c("Pittsburg", "Dallas", "Houston", "Denver")

sv.PB <- c(rep(1, severe[1]), rep(0, nosevere[1]), rep(NA, ndiff[1]))
sv.DL <- c(rep(1, severe[2]), rep(0, nosevere[2]), rep(NA, ndiff[2]))
sv.HT <- c(rep(1, severe[3]), rep(0, nosevere[3]), rep(NA, ndiff[3]))
sv.DV <- c(rep(1, severe[4]), rep(0, nosevere[4]), rep(NA, ndiff[4]))
sv <- data.frame(sv.PB, sv.DL, sv.HT, sv.DV)
names(sv) <- c("Pittsburg", "Dallas", "Houston", "Denver")

rp.PB <- c(rep(1, repeated[1]), rep(0, norepeated[1]), rep(NA, ndiff[1]))
rp.DL <- c(rep(1, repeated[2]), rep(0, norepeated[2]), rep(NA, ndiff[2]))
rp.HT <- c(rep(1, repeated[3]), rep(0, norepeated[3]), rep(NA, ndiff[3]))
rp.DV <- c(rep(1, repeated[4]), rep(0, norepeated[4]), rep(NA, ndiff[4]))
rp <- data.frame(rp.PB, rp.DL, rp.HT, rp.DV)
names(rp) <- c("Pittsburg", "Dallas", "Houston", "Denver")

ij.PB <- c(rep(1, injury[1]), rep(0, noinjury[1]), rep(NA, ndiff[1]))
ij.DL <- c(rep(1, injury[2]), rep(0, noinjury[2]), rep(NA, ndiff[2]))
ij.HT <- c(rep(1, injury[3]), rep(0, noinjury[3]), rep(NA, ndiff[3]))
ij.DV <- c(rep(1, injury[4]), rep(0, noinjury[4]), rep(NA, ndiff[4]))
ij <- data.frame(ij.PB, ij.DL, ij.HT, ij.DV)
names(ij) <- c("Pittsburg", "Dallas", "Houston", "Denver")

ct.PB <- c(rep(1, controlling[1]), rep(0, nocontrolling[1]), rep(NA, ndiff[1]))
ct.DL <- c(rep(1, controlling[2]), rep(0, nocontrolling[2]), rep(NA, ndiff[2]))
ct.HT <- c(rep(1, controlling[3]), rep(0, nocontrolling[3]), rep(NA, ndiff[3]))
ct.DV <- c(rep(1, controlling[4]), rep(0, nocontrolling[4]), rep(NA, ndiff[4]))
ct <- data.frame(ct.PB, ct.DL, ct.HT, ct.DV)
names(ct) <- c("Pittsburg", "Dallas", "Houston", "Denver")

vb.PB <- c(rep(1, verbal[1]), rep(0, noverbal[1]), rep(NA, ndiff[1]))
vb.DL <- c(rep(1, verbal[2]), rep(0, noverbal[2]), rep(NA, ndiff[2]))
vb.HT <- c(rep(1, verbal[3]), rep(0, noverbal[3]), rep(NA, ndiff[3]))
vb.DV <- c(rep(1, verbal[4]), rep(0, noverbal[4]), rep(NA, ndiff[4]))
vb <- data.frame(vb.PB, vb.DL, vb.HT, vb.DV)
names(vb) <- c("Pittsburg", "Dallas", "Houston", "Denver")

th.PB <- c(rep(1, threats[1]), rep(0, nothreats[1]), rep(NA, ndiff[1]))
th.DL <- c(rep(1, threats[2]), rep(0, nothreats[2]), rep(NA, ndiff[2]))
th.HT <- c(rep(1, threats[3]), rep(0, nothreats[3]), rep(NA, ndiff[3]))
th.DV <- c(rep(1, threats[4]), rep(0, nothreats[4]), rep(NA, ndiff[4]))
th <- data.frame(th.PB, th.DL, th.HT, th.DV)
names(th) <- c("Pittsburg", "Dallas", "Houston", "Denver")
