#' # Generating a plain-text directory map 
#'
#' Below is a minimally-modified version of [@jennybc's](https://gist.github.com/jennybc) ["twee()" function](https://gist.github.com/jennybc/2bf1dbe6eb1f261dfe60). Specific modifications are commented inline, but in short, the only difference between "`twee2()`" (below) and the original "`twee()`" (further below and available [@jennybc's original gist](https://gist.github.com/jennybc/2bf1dbe6eb1f261dfe60)) is the added option to write the output to either or both the "`R-console`" (or save as an object) or a specified "`file`".
#'
#' The added output args/options were motivated by my attempts to make my own project management workflow more efficient. Specifically, I typically include a directory map in the README's I create in any shared project directories for which I am the primary administrator, and this made the whole process of constructing README's SO much easier.
#'
#+ modTwee
twee2 <- function(dir = getwd(), level = Inf, f = NULL, quiet = FALSE) {
    ## ^changed "path" arg to "dir" to avoid confusion (mostly as part of my own workflow)^ ##
    ## ^added "file = """ to args list for use in later "cat(...)" line^ ##
  fad <-
    list.files(path = dir, recursive = TRUE, no.. = TRUE, include.dirs = TRUE)

  fad_split_up <- strsplit(fad, "/")

  too_deep <- lapply(fad_split_up, length) > level
  fad_split_up[too_deep] <- NULL

  jfun <- function(x) {
    n <- length(x)
    if(n > 1)
      x[n - 1] <- "|__"
    if(n > 2)
      x[1:(n - 2)] <- "   "
    x <- if(n == 1) c("-- ", x) else c("   ", x)
    x
  }
  fad_subbed_out <- lapply(fad_split_up, jfun)
  y <- unlist(lapply(fad_subbed_out, paste, collapse = "")) ## saved main output to a new object ##
  if (!is.null(f)) { ## added conditional step to write output to a specified file ("f") ##
  	cat(y, sep = "\n", file = f)
  }
  if (!quiet) { ## added option to return the output quietly (i.e., if writing to a file and not wanting the output to also print to the console) ##
  	return(y) ## return output/print to console only if "quiet == FALSE" ##
  }
}
#'
#' ## Example usage
#'
#+ twee2_example
twee2()
twee2(f = "dirMap.txt")
twee2(f = "dirMap.txt", quiet = TRUE)
dirmap <- twee2()
dirmap
dirmap <- twee2(f = "dirMap.txt")
dirmap
dirmap <- twee2(f = "dirMap.txt", quiet = TRUE)
dirmap ## yields NULL, but output still written to file ##
readLines("dirMap.txt")
#'
#' -----
#'
#' # Original `FUN` Description:

> "quick-and-dirty ersatz Unix tree command in R inspired by this one-liner: `ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'` found here (among many other places): http://serverfault.com/questions/143954/how-to-generate-an-ascii-representation-of-a-unix-file-hierarchy"

#' ## Original `FUN`: 
#'
#+ origTwee
# twee <- function(path = getwd(), level = Inf) {
#
#   fad <-
#     list.files(path = path, recursive = TRUE,no.. = TRUE, include.dirs = TRUE)
#
#   fad_split_up <- strsplit(fad, "/")
#
#   too_deep <- lapply(fad_split_up, length) > level
#   fad_split_up[too_deep] <- NULL
#
#   jfun <- function(x) {
#     n <- length(x)
#     if(n > 1)
#       x[n - 1] <- "|__"
#     if(n > 2)
#       x[1:(n - 2)] <- "   "
#     x <- if(n == 1) c("-- ", x) else c("   ", x)
#     x
#   }
#   fad_subbed_out <- lapply(fad_split_up, jfun)
#
#   cat(unlist(lapply(fad_subbed_out, paste, collapse = "")), sep = "\n")
# }
#'