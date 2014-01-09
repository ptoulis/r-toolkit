## Log utilities.
kLogFile <- "rtools-logs.txt"
kCurrentLogLevel = 10
kLogToStdOut = T
#  FINE DEBUG  INFO  WARNING   ERROR
#   0     1      2     3        4
library(stringr)
if (file.exists(file=kLogFile)) {
  file.remove(file=kLogFile)
}

logthis <- function(x, level) {
  wline <- function(vec) {
    if(kLogToStdOut) {
      print(sprintf("%s", str_c(vec, collapse=" ")))
    } else {
      if(file.exists(kLogFile))
        write(str_c(vec, collapse="\t"), file=kLogFile, append=T)
    }
  }
  if (level >= kCurrentLogLevel) {
    preamble <- sprintf("%s::", date())
    if(is.character(x)) {
      wline(c(preamble, x))
    } else if(is.data.frame(x)) {
      wline(c(preamble, "DATA FRAME"))
      wline(c("\t", names(x)))
      logthis(as.matrix(x), level=level)
    } else if(is.matrix(x)) {
      if(nrow(x) > 0)
        for(i in 1:nrow(x))
          wline(c("\t", as.vector(x[i, ])))
    } else if (is.table(x)) {
      wline(c(preamble, "TABLE"))
      wline(names(x))
      wline(as.numeric(x))
    } else if(is.list(x)) {
      wline(c(preamble, "LIST"))
      wline(names(x))
      for(i in names(x))
        logthis(x[[i]], level)
    } else if(is.vector(x)) {
      wline(c(preamble, x))
    } else {
      wline(c(preamble, x))
    }
  }
}

logfine <- function(x) logthis(x, level=0)
logdebug <- function(x) logthis(x, level=1)
loginfo <- function(x) logthis(x, level=2)
logwarning <- function(x) logthis(x, level=3)
logerror <- function(x) logthis(x, level=4)