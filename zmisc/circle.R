#'
#' # First, some basic geometry
#'
#' ## Area of a circle ("$A$"):
#' \[ A = \pi r^{2} \]
#'
#' where $r$ is the radius.
#'
#' ## Radius of a circle ("$r$"):
#'
#' \[ r = \sqrt{\frac{A}{\pi}} \]
#'
#' # Now a circle (drawn in base `R` graphics):
#'
radius <- 1
cir <- c(-1, 1)
theta <- seq(0, 2*pi, length = 200)
plot(cir, cir*2, type = 'n')
lines(x = radius*cos(theta), y = radius*sin(theta))
#'
#'
# MAPtl <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv", header = TRUE, sep = "\t")
radius <- sqrt(MAP$SJR/pi)

symbols(MAP$year, jitter(MAP$prop, amount = sd(MAP$prop)), circles = radius, inches = 0.35, fg = NA, bg = mpal(MAPtl, a = 0.25))

symbols(MAP$year, circles = radius, inches = 0.5, fg = NA, bg = mpal(MAPtl, a = 0.25))

plot(MAPtl$year, jitter(MAPtl$prop, amount = sd(MAPtl$prop)))
text(MAPtl$year, MAPtl$prop, MAPtl$state, cex=0.5)
jitter(MAPtl$prop, amount = sd(MAPtl$prop))
