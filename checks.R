# Copyright 2013 Panos Toulis
# Author: Panos Toulis(ptoulis@fas.harvard.edu)
#
# Simple checks for comparing objects (e.g. lists), including
# logical checks, set checks and statistical tests.

stop.now <- function(x, y, msg) {
  compact <- function(a) {
    if(is.vector(a))
      return(paste(head(a, min(length(a), 80)), collapse=", "))
  }
  dump.frames()
  calls <- names(last.dump)
  calls <- sapply(calls, function(x) {
    strsplit(x, ":")[[1]][1]
  })
  stop("\n>> ", sprintf("[FAIL]: %s", paste(msg, collapse=" : ")), "\n",
       ">> Execution path: ", paste(calls, collapse=" -> "),
        "\n>> Operands:\n", 
        sprintf("x = %s...", compact(x)), "\n", 
        sprintf("y = %s...", compact(y)))
}

CHECK_identical <- function(x, y, msg="Identical vector check") {
  CHECK_TRUE(identical(x, y), msg)
}

CHECK_notNA <- function(x, msg="Not-NA check") {
  if(any(is.na(x)))
    stop.now(x, "n/a (ignore)", c("INPUT has NA", msg))
  if(any(is.null(x)))
    stop.now(x, "n/a (ignore)", c("INPUT has NULL", msg))
}

CHECK_INPUT <- function(x, msg="n/a") {
  if(length(x) < 1) {
    stop.now(x, "n/a (ignore)", c("INPUT empty", msg))
  }
  if(any(is.na(x)))
    stop.now(x, "n/a (ignore)", c("INPUT has NA", msg))
  if(any(is.null(x)))
    stop.now(x, "n/a (ignore)", c("INPUT has NULL", msg))
}

CHECK_TRUE <- function(x, msg="n/a") {
  if (!x)
    stop.now(x, "n/a (ignore)", c("Logical test failed", msg))
}

CHECK_EQ <- function(x, y, msg="n/a") {
  CHECK_INPUT(x)
  CHECK_INPUT(y)
  if(length(x) != length(y))
    stop.now(length(x), length(y), c("Not equal lengths", msg))
  if (!all(x==y)) stop.now(x, y, c("Arrays not equal.", msg)) 
}

CHECK_GE <- function(x, y, msg="n/a") {
  if (any(x < y))
    stop.now(x, y, c("Some values are > y", msg))
}

CHECK_SETEQ <- function(x, y, msg="n/a") {
  if (!setequal(x, y))
    stop.now(x,y, c("Sets not equal", msg))
}

CHECK_DISJOINT <- function(x, y, msg="n/a") {
  if(length(intersect(x, y)) > 0)
    stop.now(x, y, c("Sets are not disjoint", msg))
}

CHECK_UNIQUE <- function(x, msg="n/a") {
  if (any(duplicated(x)))
    stop.now(x, "na", c("Duplicates exist", msg))
}

CHECK_MEMBER <- function(element, y, msg="n/a") {
  if (!all(is.element(element, y))) {
    stop.now(element, y, sprintf("Element not member of list : %s", msg))
  }
}

CHECK_NEAR <- function(x, y, tol=1e-2, msg="n/a") {
  CHECK_INPUT(x)
  CHECK_INPUT(y)
  if (any(abs(x-y) > tol))
    stop.now(x, y, c(sprintf("Failed at tolerance %.6f", tol), msg))
}

CHECK_INTERVAL <- function(x, min, max, msg="n/a") {
  if(any(x < min) | any(x > max))
    stop.now(x, c(min, max), c("Not in interval", msg))
}

CHECK_EXCEPTION <- function(expr, msg="n/a") {
  out <- tryCatch(eval(expr), error=function(err) { return("ERROR") })
  CHECK_TRUE(out == "ERROR", msg=c("Did not throw exception", msg))
}
bootstrap.mean = function(x) sd(replicate(1000, { mean(sample(x, replace=T)) }))
CHECK_MU0 <- function(x, mu0, msg="n/a") {
  # Will bootstrap x to see if mean(x) = mu0
  if (!is.vector(x)) {
    print(x)
    stop("x needs to be a vector")
  }
  boot.se = bootstrap.mean(x)
  CHECK_INTERVAL(mu0, min=mean(x) - 2 * boot.se, max=mean(x) + 2*boot.se,
                 msg=c("Not in bootstrap inteval", msg))
}

CHECK_ERROR <- function(expr, msg="n/a") {
  x = try(eval(expr), silent=T)
  if(!inherits(x, "try-error"))
    stop(sprintf("ERROR did not occur: %s", msg))
}